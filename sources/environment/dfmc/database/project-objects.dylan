Module:    dfmc-environment-database
Synopsis:  DFM compiler database project information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project objects

define sealed method source-record-top-level-forms
    (server :: <dfmc-database>, sr :: <source-record>, #key project)
 => (source-forms :: <sequence>)
  let forms = make(<stretchy-vector>);
  let project
    = if (project)
	project.project-proxy
      else
	let library 
	  = find-source-record-library(server.server-project, sr);
	library.library-compiler-project;
      end;
  let context = browsing-context(server, project);
  for (form :: <source-form> in dfmc/source-record-top-level-forms(context, sr))
    let object = make-environment-object-for-source-form(server, form);
    object & add!(forms, object)
  end;
  forms
end method source-record-top-level-forms;

define function do-all-projects
    (function :: <function>, server :: <server>) => ()
  let project = server.server-project.project-proxy;
  if (project)
    function(project);
    do(function, all-used-projects(project))
  end
end function do-all-projects;

define function project-second-context
    (project :: <project>, context :: <context>)
 => (context :: false-or(<context>))
  let original-context = project.ensure-project-database;
  unless (original-context == context)
    original-context
  end
end function project-second-context;

define function more-specific-context
    (context1 :: <context>, context2 :: <context>)
 => (context :: false-or(<context>))
  case
    context1 == context2 =>
      context1;
    member?(context1, context2.all-known-compilation-contexts) =>
      context2;
    member?(context2, context1.all-known-compilation-contexts) =>
      context1;
    otherwise =>
      #f
  end
end function more-specific-context;

// TO DO: use projects not contexts ?
define method do-all-client-contexts
    (function :: <function>, server :: <dfmc-database>, context :: <context>,
     #key both-contexts? = #t)
 => ()
  let main-context = browsing-context(server, server);
  let all-contexts = main-context.all-known-compilation-contexts;
  debug-assert(member?(context, context.all-known-compilation-contexts),
	       "Calling do-all-client-contexts with wrong context %",
	       context.compilation-context-name);
  for (used-context :: <context> in all-contexts)
    if (member?(context, used-context.all-known-compilation-contexts))
      function(used-context);
      if (both-contexts?)
	let used-project = used-context.compilation-context-project;
	let second-context = project-second-context(used-project, used-context);
	second-context & function(second-context)
      end
    end
  end
end method do-all-client-contexts;

define method collect-from-all-client-contexts
    (function :: <function>, server :: <dfmc-database>, context :: <context>,
     #key both-contexts? = #t)
 => (all-results :: <simple-object-vector>)
  let all-results :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-all-client-contexts
    (method (context :: <context>)
       let results = function(context);
       if (results & ~empty?(results))
	 add!(all-results, results)
       end
     end,
     server, context,
     both-contexts?: both-contexts?);
  if (empty?(all-results))
    #[]
  else
    //--- Is there a more efficient way to do this?
    remove-duplicates!
      (apply(concatenate-as, <simple-object-vector>, all-results))
  end
end method collect-from-all-client-contexts;

define method any-results-from-all-client-contexts?
    (function :: <function>, server :: <dfmc-database>, context :: <context>,
     #key both-contexts? = #t)
 => (results? :: <boolean>)
  block (return)
    do-all-client-contexts
      (method (context :: <context>)
	 let results = function(context);
	 unless (empty?(results))
	   return(#t)
	 end
       end,
       server, context,
       both-contexts?: both-contexts?);
    #f
  end
end method any-results-from-all-client-contexts?;

define function find-project-for-library-name
    (server :: <dfmc-database>, name :: <symbol>, #key error? = #t)
 => (project :: false-or(<project>))
  block (return)
    do-all-projects
      (method (project :: <project>)
	 let context = browsing-context(server, project);
	 let definition = context.project-library-definition;
	 if (definition.library-definition-name == name)
	   return(project)
	 end
       end,
       server);
    error? & error("Failed to find project for library %s", name)
  end
end function find-project-for-library-name;

define function compilation-context-name
    (context :: <context>) => (name :: <symbol>)
  let project = context.compilation-context-project;
  project.project-library-name
end function compilation-context-name;
