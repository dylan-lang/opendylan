Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *current-debug-target* = #f;

define class <project-execution-context> (<object>)
  constant slot context-interactive-sources-table :: <table> = make(<table>);
  constant slot context-execution-context, 
    required-init-keyword: execution-context:;
  slot context-last-transaction-id = #f;
  constant slot context-debug-target, 
    required-init-keyword: debug-target:;
end;

define abstract class <interactive-project> (<project>)
  constant slot project-execution-contexts = make(<stretchy-vector>);
end;

define function project-target-execution-context(project :: <interactive-project>,
						 target)
 => (ec :: false-or(<project-execution-context>));
  any?(method(c) target & c.context-debug-target == target & c end, 
       project.project-execution-contexts)
end;

define function 
    context-interactive-sources(context :: <project-execution-context>)
 => sr*;
  let interactive-sources = #();

  for(sr in context.context-interactive-sources-table)
    interactive-sources := pair(sr, interactive-sources)
  end;
  interactive-sources
end;

define method project-canonical-source-records(project :: <interactive-project>)
 => (records :: <sequence>);
/*
  let interactive-sources = #();

  for(context in project.project-execution-contexts)
    interactive-sources := 
      concatenate(context.context-interactive-sources-table, interactive-sources)
  end;
  concatenate(#(), next-method(), interactive-sources)
*/
  let context = project-target-execution-context(project, *current-debug-target*);
  let interactive-sources = 
    if(context)
      context-interactive-sources(context)
    else
      #()
    end;
  concatenate(#(), next-method(), interactive-sources)
end;

define method project-current-debug-target-setter(debug-target,
						  p :: <interactive-project>)
 => (debug-target)
  if (debug-target = #f & ~p.%project-closed?)
    with-lock($pm-lock)
      debug-out(#"project-manager", "Current debug target: %=\n", *current-debug-target*);
      do(%project-close-interactive-context, p.all-used-projects);
      %project-close-interactive-context(p);
    end;
  else
    // we assume here that we have been called before with debug-target = #f
    debug-assert(p.ensure-project-database,
		 "Cannot run application without %s.ddb present", 
		 p.project-library-name);
    *current-debug-target* := debug-target;
  end;

  debug-out(#"project-manager", 
	    "Setting current debug target for %s to %=\n", p.project-name, debug-target);
  *current-debug-target* := debug-target;
end;

define function 
    %project-close-interactive-context(project :: <interactive-project>,
				       #key context = #f)
  let context = context | project-target-execution-context(project, *current-debug-target*);
  if(context)
    release-execution-context(context.context-execution-context);
    debug-out(#"project-manager",
	      "Closed an execution context for: %s\n", project.project-name);
    remove!(project.project-execution-contexts, context);
      // TO DO: don't remove old records
  end
end;

define method 
    project-close-compilation-contexts(project :: <interactive-project>)
  for(c in project.project-execution-contexts)
    %project-close-interactive-context(project, context: c);
  end;
  next-method()
end;

define class <debug-target-not-set-error> (<simple-condition>, <error>) end;


define function verify-execution-target(project :: <interactive-project>)
 => context;
  with-compiler-transaction
    unless(*current-debug-target*)
      signal(make(<debug-target-not-set-error>, 
		  format-string: "Project Manager: No debug target set",
		  format-arguments: #()));
    end;
    unless(project-target-execution-context(project, *current-debug-target*))
      add!(project.project-execution-contexts,
	   make(<project-execution-context>,
		execution-context: 
		  establish-execution-context(project-current-compilation-context(project),
					      *current-debug-target*),
		debug-target: *current-debug-target*));
      debug-out(#"project-manager",
		"Opened new execution context for: %s\n", project.project-name);
      do(verify-execution-target, project.directly-used-projects);
    end;
    project-target-execution-context(project, *current-debug-target*)
  end;
end function;

define function project-execution-context(project :: <interactive-project>)
 => context;
  let context = project-target-execution-context(project, *current-debug-target*);
  context & context.context-execution-context
end;

define function project-last-transaction-id(project :: <interactive-project>)
 => id;
  let context = project-target-execution-context(project, *current-debug-target*);
  context & context.context-last-transaction-id
end;

define method evaluate-expression
    (project :: <interactive-project>, runtime-context, module :: <symbol>,
     source :: <byte-string>)
 => id;
  verify-execution-target(project);

  let record = make(<interactive-source-record>,
		    project: project,
		    module: module,
		    source: as(<byte-vector>, source));

  let context = project-target-execution-context(project, *current-debug-target*);

  let id =
    execute-source(context.context-execution-context,
		   runtime-context,
		   list(record));
  // execution successful
  
  context.context-last-transaction-id := id;
  record.interactive-source-record-id := id;
  context.context-interactive-sources-table[id] := record;
  id
end method;

define method macroexpand-expression
    (project :: <user-project>, module :: <symbol>, source :: <byte-string>,
     #key expansion-stream, trace-stream)
 => ()
  //--- We have to use <interactive-source-record> since it is the only
  //--- one that takes a string!
  let record = make(<interactive-source-record>,
		    project: project,
		    module: module,
		    source: as(<byte-vector>, source));

  let context = project.project-browsing-context;

  macroexpand-source(context, record,
		     expansion-stream: expansion-stream,
		     trace-stream: trace-stream)
end;

define function parse-expression(project :: <interactive-project>,
				 runtime-context,
				 module :: <symbol>,
				 source :: <byte-string>)
 => (well? :: <boolean>, warnings :: <sequence>);

  verify-execution-target(project);

  let record = make(<interactive-source-record>,
		    project: project,
		    module: module,
		    source: as(<byte-vector>, source));

  let context = project-target-execution-context(project, *current-debug-target*);

  source-complete?(context.context-execution-context,
		   runtime-context,
		   list(record));
end;


define method project-id-interactive-record(project :: <interactive-project>,
					    id)
 => (sr :: false-or(<interactive-source-record>));
  any?(method(c) 
	   element(c.context-interactive-sources-table, id, default: #f)
       end,
       project.project-execution-contexts)
end method;

define method project-interactive-execution-notes(project :: <interactive-project>,
						  #key record-id)
  let context = project-target-execution-context(project, *current-debug-target*);
  context & execution-transaction-notes(context.context-execution-context,
					context.context-last-transaction-id)
end;

define method project-interaction-allowed?(project :: <interactive-project>)
 => (well? :: <boolean>);
  block()
    verify-execution-target(project);

    let context = project-target-execution-context(project, *current-debug-target*);
    execution-context-interactive?(context.context-execution-context)
  exception(<debug-target-not-set-error>)
    #f
  end
end;
    
