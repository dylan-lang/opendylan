Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <project-execution-context> (<object>)
  constant slot context-interactive-sources-table :: <table> = make(<table>);
  constant slot context-interactive-sources :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  constant slot context-execution-context :: <context>, 
    required-init-keyword: execution-context:;
  slot context-last-transaction-id = #f;
  constant slot context-debug-target :: <debug-target>,
    required-init-keyword: debug-target:;
end class <project-execution-context>;

define function project-target-execution-context
    (project :: <dfmc-project>, target :: <debug-target>)
 => (ec :: false-or(<project-execution-context>));
  any?(method (c) target & c.context-debug-target == target & c end, 
       project.project-execution-contexts)
end function project-target-execution-context;

define method project-canonical-source-records
    (project :: <dfmc-project>)
 => (records :: <sequence>)
  let workspace = project.project-workspace;
  let debug-target = workspace.workspace-debug-target;
  let context = project-target-execution-context(project, debug-target);
  if (context)
    let interactive-sources = context.context-interactive-sources;
    concatenate(#(), next-method(), interactive-sources)
  else
    next-method()
  end
end method project-canonical-source-records;

define method project-current-debug-target-setter
    (debug-target :: <debug-target>, project :: <dfmc-project>)
 => (debug-target :: <debug-target>)
  let workspace = project.project-workspace;
  if (debug-target = #f & ~project.%project-closed?)
    with-lock ($pm-lock)
      do(%project-close-interactive-context, project.all-used-projects);
      %project-close-interactive-context(p);
    end
  else
    debug-assert(project.ensure-project-database,
		 "Cannot run application without %s.ddb present",
		 p.project-library-name)
  end;
  workspace.workspace-debug-target := debug-target
end method project-current-debug-target-setter;

define function %project-close-interactive-context
    (project :: <dfmc-project>, #key context = #f) => ()
  let workspace = project.project-workspace;
  let debug-target = workspace.workspace-debug-target;
  let context
    = context 
        | project-target-execution-context(project, debug-target);
  if (context)
    // TO DO: don't remove old records
    release-execution-context(context.context-execution-context);
    remove!(project.project-execution-contexts, context)
  end
end function %project-close-interactive-context;

define method project-close-compilation-contexts
    (project :: <dfmc-project>) => ()
  for (c :: <context> in project.project-execution-contexts)
    %project-close-interactive-context(project, context: c);
  end;
  next-method()
end method project-close-compilation-contexts;

define function verify-execution-target
    (project :: <dfmc-project>)
 => (context :: <context>)
  let workspace = project.project-workspace;
  let debug-target = workspace.workspace-debug-target;
  debug-assert(debug-target, "Project Manager: No debug target set");
  with-compiler-lock ()
    project-target-execution-context(project, debug-target)
      | begin
	  let context = project.project-current-compilation-context;
	  let execution-context
	    = establish-execution-context(context, debug-target);
	  add!(project.project-execution-contexts,
	       make(<project-execution-context>,
		    execution-context: execution-context,
		    debug-target: debug-target));
	  do(verify-execution-target, project.directly-used-projects);
	  execution-context
	end;
  end
end function verify-execution-target;

define function project-execution-context
    (project :: <dfmc-project>)
 => (context :: false-or(<context>))
  let workspace = project.project-workspace;
  let debug-target = workspace.workspace-debug-target;
  let context = project-target-execution-context(project, debug-target);
  context & context.context-execution-context
end function project-execution-context;

define function project-last-transaction-id
    (project :: <dfmc-project>)
 => (id :: false-or(<transaction-id>))
  let workspace = project.project-workspace;
  let debug-target = workspace.workspace-debug-target;
  let context = project-target-execution-context(project, debug-target);
  context & context.context-last-transaction-id
end function project-last-transaction-id;

define method evaluate-expression
    (project :: <dfmc-project>,
     runtime-context :: <context>,
     module :: <symbol>,
     source :: <byte-string>)
 => (id :: <transaction-id>)
  let context = verify-execution-target(project);
  let record
    = make(<interactive-source-record>,
	   project: project,
	   module: module,
	   source: as(<byte-vector>, source));
  let id
    = execute-source(context.context-execution-context,
		     runtime-context,
		     list(record));
  // execution successful
  
  context.context-last-transaction-id := id;
  record.interactive-source-record-id := id;
  context.context-interactive-sources-table[id] := record;
  add!(context.context-interactive-sources, record);
  id
end method evaluate-expression;

define function parse-expression
    (project :: <dfmc-project>,
     runtime-context :: <context>,
     module :: <symbol>,
     source :: <byte-string>)
 => (well? :: <boolean>, warnings :: <sequence>)
  let context = verify-execution-target(project);
  let record
    = make(<interactive-source-record>,
	   project: project,
	   module: module,
	   source: as(<byte-vector>, source));
  source-complete?(context.context-execution-context,
		   runtime-context,
		   list(record))
end function parse-expression;

define method project-id-interactive-record
    (project :: <dfmc-project>, id :: <transaction-id>)
 => (sr :: false-or(<interactive-source-record>))
  any?(method (context :: <project-execution-context>)
	 element(context.context-interactive-sources-table, id, default: #f)
       end,
       project.project-execution-contexts)
end method project-id-interactive-record;

//--- What does this return?
define method project-interactive-execution-notes
    (project :: <dfmc-project>, #key record-id)
  let workspace = project.project-workspace;
  let debug-target = workspace.workspace-debug-target;
  let context = project-target-execution-context(project, debug-target);
  context & execution-transaction-notes(context.context-execution-context,
					context.context-last-transaction-id)
end method project-interactive-execution-notes;

define method project-interaction-allowed?
    (project :: <dfmc-project>) => (interactive? :: <boolean>)
  block ()
    let context = verify-execution-target(project);
    execution-context-interactive?(context.context-execution-context)
  exception (<debug-target-not-set-error>)
    #f
  end
end method project-interaction-allowed?;
