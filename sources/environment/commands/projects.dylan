Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright: Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

/// Project argument parsing

define method parameter-type-name
    (type == <project-object>) => (name :: <string>)
  "project"
end method parameter-type-name;

define method parse-next-argument
    (context :: <environment-context>, type == <project-object>,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <project-object>, next-index :: <integer>)
  let (name, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (name)
    block (return)
      let locator = as(<file-locator>, name);
      for (project :: <project-object> in open-projects())
        if (environment-object-primitive-name(project, project) = name
              | project.project-filename = locator)
          return(project, next-index)
        end
      end;
      let command
        = make(<open-project-command>, server: context, file: locator);
      let project = execute-command(command);
      if (project)
        values(project, next-index)
      else
        parse-error("File %s is not a project", name)
      end
    end
  else
    parse-error("Missing project argument")
  end
end method parse-next-argument;


/// Project properties

// Project

define class <current-project-property> (<environment-property>)
end class <current-project-property>;

define command-property project => <current-project-property>
  (summary:       "Current project",
   documentation: "The currently active project.",
   type:          <project-object>)
end command-property project;

define method show-property
    (context :: <environment-context>, property :: <current-project-property>)
 => ()
  let project = context.context-project;
  if (project)
    message(context, "Project: %s = %s",
            project.project-name,
            project.project-filename)
  else
    command-error("No open projects")
  end
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <current-project-property>,
     project :: <project-object>,
     #key save?)
 => ()
  context.context-project := project
end method set-property;


// Projects

define class <open-projects-property> (<environment-property>)
end class <open-projects-property>;

define command-property projects => <open-projects-property>
  (summary:       "Open projects",
   documentation: "The set of open projects.")
end command-property projects;

define method show-property
    (context :: <environment-context>, property :: <open-projects-property>)
 => ()
  let projects = open-projects();
  if (~empty?(projects))
    let current-project = context.context-project;
    for (project :: <project-object> in projects)
      message(context, " %s %s = %s",
              if (project = current-project) "*" else " " end,
              project.project-name,
              project.project-filename)
    end
  else
    message(context, "No open projects")
  end
end method show-property;


/// Open project

define class <open-project-command> (<environment-command>)
  constant slot %file :: <file-locator>,
    required-init-keyword: file:;
end class <open-project-command>;

define command-line open => <open-project-command>
    (summary:       "opens the specified project",
     documentation: "Opens the specified project.")
  argument file :: <file-locator> = "the filename of the project";
end command-line open;

define function open-project-from-locator
    (locator :: <file-locator>)
 => (project :: false-or(<project-object>), invalid? :: <boolean>)
  let pathname = merge-locators(locator, working-directory());
  let extension = locator-extension(pathname);
  select (extension by \=)
    lid-file-extension() =>
      values(import-project-from-file(pathname), #f);
    project-file-extension() =>
      values(open-project(pathname), #f);
    executable-file-extension() =>
      values(create-exe-project-from-file(pathname), #f);
    otherwise =>
      if (~extension)
        let library-name = as(<symbol>, locator.locator-base);
        values(find-project-for-library(library-name), #f)
      else
        values(#f, #t)
      end;
  end
end function open-project-from-locator;

define function find-project-for-library
    (library-name :: <symbol>) => (project :: false-or(<project-object>))
  find-project(as(<string>, library-name))
    | begin
        let library-info = find-library-info(library-name);
        if (library-info)
          let location = info-location(library-info);
          location & open-project-from-locator(as(<file-locator>, location))
        end
      end
end function find-project-for-library;

define sealed method do-execute-command
    (context :: <environment-context>, command :: <open-project-command>)
 => (project :: false-or(<project-object>))
  let filename = command.%file;
  let (project, invalid?) = open-project-from-locator(filename);
  case
    project =>
      open-project-compiler-database
        (project,
         warning-callback:     curry(note-compiler-warning, context),
         error-handler:        curry(compiler-condition-handler, context));
      project.project-opened-by-user? := #t;
      context.context-project := project;
      let project-context
        = context.context-project-context
            | begin
                let library = project.project-library;
                let module = library & library-default-module(project, library);
                let project-context
                  = make(<project-context>,
                         project: project,
                         module: module);
                context.context-project-context := project-context
              end;
      message(context, "Opened project %s (%s)", project.project-name,
              project.project-filename);
      project;
    invalid? =>
      command-error("Cannot open '%s' as it is not a project", filename);
    otherwise =>
      command-error("Unable to open project '%s'", filename);
  end
end method do-execute-command;


/// Import project

define class <import-project-command> (<environment-command>)
  constant slot %file :: <file-locator>,
    required-init-keyword: file:;
end class <import-project-command>;

define command-line import => <import-project-command>
    (summary:       "imports a LID file",
     documentation: "Imports a LID file as a project file.")
  argument file :: <file-locator> = "the LID file to be imported";
end command-line import;

define sealed method do-execute-command
    (context :: <environment-context>, command :: <import-project-command>)
 => ()
  let filename = command.%file;
  let project = import-project-from-file(filename);
  if (project)
    message(context, "Imported project %s", filename)
  else
    command-error("Failed to import %s", filename)
  end
end method do-execute-command;


/// Close project

define class <close-project-command> (<environment-command>)
  constant slot %project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  constant slot %all? :: <boolean> = #f,
    init-keyword: all?:;
end class <close-project-command>;

define command-line close => <close-project-command>
    (summary:       "closes the specified project",
     documentation: "Closes the specified project.")
  optional project :: <project-object> = "the project to close";
  flag all = "close all open projects [off by default]";
end command-line close;

define sealed method do-execute-command
    (context :: <environment-context>, command :: <close-project-command>)
 => ()
  local method close
            (project :: <project-object>) => ()
          if (project.application-tethered?)
            let application = project.project-application;
            let filename = application.application-filename.locator-name;
            close-application(project, wait-for-termination?: #t);
            message(context, "Closed application %s", filename)
          end;
          project.project-opened-by-user? := #f;
          close-project(project)
        end;
  let projects = open-projects();
  case
    empty?(projects) =>
      command-error("There are no open projects to close");
    command.%all? =>
      do(close, projects);
      context.context-project := #f;
      message(context, "Closed all projects");
    otherwise =>
      let project = command.%project | context.context-project;
      let name = project.project-name;
      close(project);
      if (project == context.context-project)
        context.context-project := context.context-previous-project
      end;
      message(context, "Closed project %s", name);
  end
end method do-execute-command;


/// Project commands

define command-group project
    (summary: "project commands",
     documentation: "Commands applying to projects.")
  property project;
  property projects;
  command  open;
  command  import;
  command  close;
end command-group project;
