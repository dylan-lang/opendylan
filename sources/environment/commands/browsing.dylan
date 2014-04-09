Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment object argument parsing

define method parameter-type-name
    (type :: subclass(<environment-object>)) => (name :: <string>)
  //---*** If only we could use environment-object-type-name!
  select (type)
    <class-object>    => "class";
    <function-object> => "function";
    <method-object>   => "method";
    otherwise         => "definition"
  end
end method parameter-type-name;

define method parse-next-argument
    (context :: <environment-context>, type :: subclass(<definition-object>),
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <definition-object>, next-index :: <integer>)
  let stop = stop | text.size;
  if (start < stop)
    let name = copy-sequence(text, start: start, end: stop);
    let project = context.context-project;
    let project-context = context.context-project-context;
    let module = project-context.context-module;
    let object = find-environment-object(project, name, module: module);
    case
      ~object =>
	parse-error("No object named '%s'", name);
      ~instance?(object, type) =>
	parse-error("Incorrect argument type: '%s'",
		    environment-object-display-name(project, object, module));
      otherwise =>
	values(object, stop);
    end
  else
    parse-error("Missing %s argument",
		parameter-type-name(type))
  end
end method parse-next-argument;


/// Module and library argument parsing

define method parameter-type-name
    (type == <library-object>) => (name :: <string>)
  "library"
end method parameter-type-name;

define method parse-next-argument
    (context :: <environment-context>, type == <library-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <library-object>, next-index :: <integer>)
  let (name, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (name)
    let project = context.context-project;
    let library = find-library(project, name);
    if (library)
      values(library, next-index)
    else
      parse-error("No library named '%s'", name)
    end
  else
    parse-error("Missing library argument")
  end
end method parse-next-argument;

define method parameter-type-name
    (type == <module-object>) => (name :: <string>)
  "module"
end method parameter-type-name;

define method parse-next-argument
    (context :: <environment-context>, type == <module-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <module-object>, next-index :: <integer>)
  let (name, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (name)
    let project = context.context-project;
    let library = project.project-library;
    let module = find-module(project, name, library: library);
    if (module)
      values(module, next-index)
    else
      parse-error("No module named '%s'", name)
    end
  else
    parse-error("Missing module argument")
  end
end method parse-next-argument;


/// Browsing properties

define class <library-property> (<project-property>)
end class <library-property>;

define command-property library => <library-property>
  (summary:       "Current library",
   documentation: "The current library.",
   type:          <library-object>)
end command-property library;

define method show-property
    (context :: <environment-context>, property :: <library-property>) => ()
  let project-context = context.context-project-context;
  let project = context.context-project;
  let library = project-context & project-context.context-library;
  if (library)
    message(context, "  Library: %s",
	    environment-object-display-name(project, library, library))
  else
    command-error("No selected library")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <library-property>,
     library :: <library-object>, 
     #key save?)
 => ()
  let project-context = context.context-project-context;
  project-context.context-library := library
end method set-property;

define class <libraries-property> (<project-property>)
end class <libraries-property>;

define command-property libraries => <libraries-property>
  (summary:       "Used libraries",
   documentation: "The libraries used by the current library.")
end command-property libraries;

define method show-property
    (context :: <environment-context>, property :: <libraries-property>) => ()
  let project-context = context.context-project-context;
  let project = context.context-project;
  let library = project-context.context-library;
  let library-project = library-project(project, library);
  let used-libraries
    = project-used-libraries(library-project, library-project);
  for (library :: <library-object> in used-libraries)
    message(context, "  %s",
	    environment-object-display-name(project, library, library))
  end
end method show-property;


// Modules

define class <module-property> (<project-property>)
end class <module-property>;

define command-property module => <module-property>
  (summary:       "Current module",
   documentation: "The current module.",
   type:          <module-object>)
end command-property module;

define method show-property
    (context :: <environment-context>, property :: <module-property>) => ()
  let project-context = context.context-project-context;
  let project = context.context-project;
  let module = project-context & project-context.context-module;
  if (module)
    message(context, "  Module: %s",
	    environment-object-display-name(project, module, module))
  else
    command-error("No selected module")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <module-property>,
     module :: <module-object>, 
     #key save?)
 => ()
  let project-context = context.context-project-context;
  project-context.context-module := module
end method set-property;

define class <modules-property> (<project-property>)
end class <modules-property>;

define command-property modules => <modules-property>
  (summary:       "Library modules",
   documentation: "The modules in the current library.")
end command-property modules;

define method show-property
    (context :: <environment-context>, property :: <modules-property>) => ()
  let project-context = context.context-project-context;
  let project = context.context-project;
  let library = project-context.context-library;
  let module-names
    = map(curry(environment-object-primitive-name, project),
	  namespace-names(project, library, imported?: #f));
  for (name :: <string> in module-names)
    message(context, "  %s", name)
  end
end method show-property;


/// Macroexpand command

define class <macroexpand-code-command> (<project-command>)
  constant slot command-code :: <string>,
    required-init-keyword: code:;
end class <macroexpand-code-command>;

define command-line macroexpand => <macroexpand-code-command>
    (summary:       "macroexpands the given code",
     documentation: "Macroexpands the given code.")
  argument code :: <string> = "the code to macroexpand";
end command-line macroexpand;

define method command-complete?
    (context :: <environment-context>, command :: <macroexpand-code-command>)
 => (complete? :: <boolean>)
  //---*** We have no way to determine this yet!
  let complete? = #t;
  complete?
end method command-complete?;

define method do-execute-command
    (context :: <environment-context>, command :: <macroexpand-code-command>)
 => ()
  let stream  = context.context-server.server-output-stream;
  let project = context.context-project;
  let module  = context.context-project-context.context-module;
  project-macroexpand-code
    (project, module, command.command-code,
     expansion-stream: stream)
end method do-execute-command;


/// Browsing commands

define command-group browsing
    (summary: "browsing commands",
     documentation: "Commands to browse project information.")
  property library;
  property libraries;
  property module;
  property modules;
  command  macroexpand;
end command-group browsing;
