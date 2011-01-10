Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment context

define constant $command-prefix-character = ':';

define class <environment-context> (<server-context>)
  constant slot context-notification :: <notification>
    = make(<notification>, lock: make(<lock>));
  constant slot context-project-contexts :: <object-table>
    = make(<object-table>);
  slot context-project :: false-or(<project-object>) = #f;
end class <environment-context>;

define open abstract class <environment-command> (<basic-command>)
end class <environment-command>;

define open abstract class <environment-property> (<command-property>)
end class <environment-property>;

define open abstract class <command-library> (<object>)
end class <command-library>;

define open generic command-library-prompt
    (context :: <environment-context>, library :: <command-library>)
 => (prompt :: false-or(<string>));

define open generic command-library-default-command-class
    (context :: <environment-context>, library :: <command-library>)
 => (class :: false-or(subclass(<command>)));

define method command-title
    (context :: <environment-context>, command :: <environment-command>)
 => (title :: <string>)
  let command-line = command-line-for-command(context, command);
  command-line.command-info-title  
end method command-title;

define variable *default-command-library* :: false-or(<command-library>) = #f;

define method register-default-command-library
    (library :: <command-library>) => ()
  *default-command-library* := library
end method register-default-command-library;

define method display-command-prompt
    (stream :: <stream>, context :: <environment-context>) => ()
  let library = *default-command-library*;
  let prompt  = library & command-library-prompt(context, library);
  if (prompt)
    format(stream, prompt)
  else
    next-method()
  end
end method display-command-prompt;

define method class-for-command-line
    (context :: <environment-context>, command-line :: <string>,
     #key start = 0, end: stop = #f)
 => (class :: subclass(<command>), next-index :: <integer>)
  let stop :: <integer> = stop | command-line.size;
  case
    //--- Make sure help always works
    (as-lowercase(command-line) == "help") =>
      next-method();
    start < stop & command-line[start] == $command-prefix-character =>
      next-method(context, command-line, start: start + 1, end: stop);
    otherwise =>
      let library = *default-command-library*;
      let default-command
	= library & command-library-default-command-class(context, library);
      if (default-command)
	values(default-command, 0)
      else
	next-method()
      end;
  end
end method class-for-command-line;


/// Command superclasses

define open abstract class <project-command> (<environment-command>)
end class <project-command>;

define open abstract class <project-property> (<environment-property>)
end class <project-property>;

define method ensure-command-available
    (context :: <environment-context>, command :: <project-command>)
 => ()
  unless (context.context-project-context)
    command-error("Project command '%s' requires an open project", 
		  command-title(context, command))
  end
end method ensure-command-available;

define method ensure-property-available
    (context :: <server-context>, property :: <project-property>)
 => ()
  unless (context.context-project-context)
    command-error("Project property '%s' requires an open project", 
		  property.command-info-title)
  end
end method ensure-property-available;


/// Project context

define class <project-context> (<server-context>)
  slot context-project :: <project-object>,
    required-init-keyword: project:;
  slot context-module :: false-or(<module-object>) = #f,
    init-keyword: module:;
  slot context-build-script :: <file-locator> = default-build-script(),
    init-keyword: build-script:;
  slot context-properties :: <list> = #();
  // slot context-last-heading :: false-or(<string>) = #f;
  slot context-last-item-label :: false-or(<string>) = #f;
end class <project-context>;

define method context-project-context
    (context :: <environment-context>, 
     #key project :: false-or(<project-object>) = context.context-project)
 => (project-context :: false-or(<project-context>))
  project & element(context.context-project-contexts, project, default: #f)
end method context-project-context;

define method context-project-context-setter
    (project-context :: false-or(<project-context>),
     context :: <environment-context>, 
     #key project :: <project-object> = context.context-project)
 => (project-context :: false-or(<project-context>))
  let contexts = context.context-project-contexts;
  if (project-context)
    element(contexts, project) := project-context
  else
    remove-key!(contexts, project);
    project-context
  end
end method context-project-context-setter;

define method context-previous-project
    (context :: <environment-context>)
 => (project :: false-or(<project-object>))
  //---*** Use a history...
  let active-projects = key-sequence(context.context-project-contexts);
  let projects = remove(active-projects, context.context-project);
  ~empty?(projects) & projects[0]
end method context-previous-project;

define method context-library
    (context :: <project-context>)
 => (library :: false-or(<library-object>))
  let module = context & context.context-module;
  if (module)
    let project = context.context-project;
    environment-object-library(project, module)
  end
end method context-library;

define method context-library-setter
    (library :: <library-object>, context :: <project-context>)
 => (library :: <library-object>)
  let project = context.context-project;
  let module = library-default-module(project, library);
  context.context-module := module;
  library
end method context-library-setter;
