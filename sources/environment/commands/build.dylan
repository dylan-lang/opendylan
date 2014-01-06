Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Build properties

// Compiler back-end

define class <compiler-back-end-property> (<environment-property>)
end class <compiler-back-end-property>;

define command-property compiler-back-end => <compiler-back-end-property>
  (summary:       "current compiler back end",
   documentation: "The current back-end code generator.",
   type:          <symbol>,
   persistent?:   #t)
end command-property compiler-back-end;

define method show-property
  (context :: <environment-context>, property :: <compiler-back-end-property>)
 => ()
  let back-end = session-property(#"compiler-back-end");
  message(context, "Compiler back end: %s", back-end);
end method show-property;

define method set-property
  (context :: <environment-context>, property :: <compiler-back-end-property>,
   back-end :: <symbol>,
   #key save?)
 => ()
  ignore(save?);
  session-property(#"compiler-back-end") := back-end;
end method set-property;

// Compilation mode

define class <compilation-mode-property> (<project-property>)
end class <compilation-mode-property>;

define command-property compilation-mode => <compilation-mode-property>
  (summary:       "compilation mode",
   documentation: "The project's compilation mode.",
   type:          <symbol>,
   persistent?:   #t)
end command-property compilation-mode;

define method show-property
    (context :: <environment-context>, property :: <compilation-mode-property>)
 => ()
  let project = context.context-project;
  message(context, "Compilation mode: %s",
          select (project.project-compilation-mode)
            #"loose"  => "development [Interactive development mode]";
            #"tight"  => "production  [Production mode]";
          end)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <compilation-mode-property>,
     compilation-mode :: <symbol>,
     #key save?)
 => ()
  //--- Need to do a non-persistent version!
  ignore(save?);
  let project = context.context-project;
  let compilation-mode
    = select (compilation-mode)
        #"tight", #"production"  => #"tight";
        #"loose", #"development" => #"loose";
        otherwise =>
          set-error("Unrecognised compilation mode: %s", compilation-mode);
      end;
  project.project-compilation-mode := compilation-mode
end method set-property;


// Build script

define class <build-script-property> (<environment-property>)
end class <build-script-property>;

define command-property build-script => <build-script-property>
  (summary:       "current build script",
   documentation: "The currently active build script.",
   type:          <file-locator>,
   persistent?:   #t)
end command-property build-script;

define method show-property
    (context :: <environment-context>, property :: <build-script-property>)
 => ()
  let project-context = context.context-project-context;
  let build-script
    = if (project-context)
        project-context.context-build-script
      else
        default-build-script()
      end;
  message(context, "Build script: %s", build-script);
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <build-script-property>,
     build-script :: <file-locator>,
     #key save?)
 => ()
  let project-context = context.context-project-context;
  if (project-context)
    project-context.context-build-script := build-script;
  end;
  if (save?)
    default-build-script() := build-script;
  end
end method set-property;

// Target platform

define class <target-platform-property> (<environment-property>)
end class <target-platform-property>;

define command-property target-platform => <target-platform-property>
  (summary:       "current target platform",
   documentation: "The current target platform.",
   type:          <symbol>,
   persistent?:   #t)
end command-property target-platform;

define method show-property
  (context :: <environment-context>, property :: <target-platform-property>)
 => ()
  let project = context.context-project;
  if (project)
    message(context, "Target platform: %s (project)", project.project-platform-name);
  else
    message(context, "Target platform: %s (global)", default-platform-name());
  end if;
end method show-property;

define method set-property
  (context :: <environment-context>, property :: <target-platform-property>,
   platform-name :: <symbol>,
   #key save?)
 => ()
  ignore(save?);
  let project = context.context-project;
  if (project)
    project.project-platform-name := platform-name;
  end if;
  default-platform-name() := platform-name;
  // Also update this in case we have a project loaded.
  let project-context = context.context-project-context;
  if (project-context)
    project-context.context-build-script := default-build-script();
  end;
end method set-property;


/// Build command

define class <abstract-link-command> (<project-command>)
  constant slot %project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  constant slot %build-script :: false-or(<file-locator>) = #f,
    init-keyword: build-script:;
  constant slot %target :: false-or(<symbol>) = #f,
    init-keyword: target:;
  constant slot %force? :: <boolean> = #f,
    init-keyword: force?:;
  constant slot %subprojects? :: <boolean> = #t,
    init-keyword: subprojects?:;
  constant slot %unify? :: <boolean> = #f,
    init-keyword: unify?:;
end class <abstract-link-command>;

define class <build-project-command> (<abstract-link-command>)
  constant slot %clean? :: <boolean> = #f,
    init-keyword: clean?:;
  constant slot %save? :: <boolean> = #t,
    init-keyword: save?:;
  constant slot %link? :: <boolean> = #t,
    init-keyword: link?:;
  constant slot %output :: <sequence> = #[],
    init-keyword: output:;
  constant slot %release? :: <boolean> = #f,
    init-keyword: release?:;
end class <build-project-command>;

define command-line build => <build-project-command>
    (summary:       "builds a project's executable",
     documentation: "Builds the executable for a project.")
  optional project :: <project-object> = "the project to build";
  keyword output :: $keyword-list-type = "debug output types [default none]";
  flag clean         = "do a clean build? [off by default]";
  flag save          = "save the compiler database [save by default]";
  flag link          = "link the executable [link by default]";
  flag release       = "build a standalone release [off by default]";
  flag subprojects   = "build subprojects as well if necessary [on by default]";
  keyword build-script :: <file-locator> = "the (Jam) build script to use";
  keyword target :: <symbol> = "the target [dll or executable]";
  flag force         = "force relink the executable [off by default]";
  flag unify         = "combine the libraries into a single executable [off by default]";
end command-line build;

define method do-execute-command
    (context :: <environment-context>, command :: <build-project-command>)
 => ()
  let project = command.%project | context.context-project;
  let messages = #"internal";
  block ()
    if (build-project
          (project,
           process-subprojects?: command.%subprojects?,
           clean?:               command.%clean?,
           link?:                #f,
           save-databases?:      command.%save?,
           messages:             messages,
           output:               command.%output,
           progress-callback:    curry(note-build-progress, context),
           warning-callback:     curry(note-compiler-warning, context),
           error-handler:        curry(compiler-condition-handler, context)))
      if (command.%link?)
        let project-context = context.context-project-context;
        let build-script
          = command.%build-script | project-context.context-build-script;
        link-project
          (project,
           build-script:         build-script,
           target:               command.%target,
           release?:             command.%release?,
           force?:               command.%force?,
           process-subprojects?: command.%subprojects?,
           unify?:               command.%unify?,
           messages:             messages,
           progress-callback:    curry(note-build-progress, context),
           error-handler:        curry(compiler-condition-handler, context))
      end;
      message(context, "Build of '%s' completed", project.project-name)
    else
      message(context, "Build of '%s' aborted",   project.project-name)
    end
  exception (error :: <file-system-error>)
    command-error("%s", error)
  end
end method do-execute-command;

define method note-build-progress
    (context :: <environment-context>,
     position :: <integer>, range :: <integer>,
     #key heading-label, item-label)
 => ()
  let project-context = context.context-project-context;
  // let last-heading = project-context.context-last-heading;
  // Let's not show headings
  // if (heading-label & ~empty?(heading-label) & heading-label ~= last-heading)
  //   project-context.context-last-heading := heading-label;
  //   message(context, "%s", heading-label)
  // end;
  let last-item-label = project-context.context-last-item-label;
  if (item-label & ~empty?(item-label) & item-label ~= last-item-label)
    project-context.context-last-item-label := item-label;
    message(context, "%s", item-label)
  end
end method note-build-progress;

define method note-compiler-warning
    (context :: <environment-context>, warning :: <warning-object>) => ()
  let project = context.context-project;
  let stream = context.context-server.server-output-stream;
  new-line(stream);
  print-environment-object-name(stream, project, warning, full-message?: #t);
  new-line(stream)
end method note-compiler-warning;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"project-not-found",
     library :: <string>)
 => (filename :: false-or(<file-locator>))
  ignore(handler-type);
  choose-missing-project(context, library: library)
end method compiler-condition-handler;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"project-file-not-found",
     filename :: <string>)
 => (filename :: false-or(<file-locator>))
  ignore(handler-type);
  choose-missing-project(context, filename: as(<file-locator>, filename))
end method compiler-condition-handler;

define function choose-missing-project
    (context :: <environment-context>,
     #key filename :: false-or(<file-locator>),
          library :: false-or(<string>))
 => (filename :: false-or(<file-locator>))
  let prompt
    = format-to-string("Project file for missing '%s':",
                       filename | library | "unknown");
  command-line-choose-file(context.context-server, prompt: prompt)
end function choose-missing-project;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"link-error", message :: <string>)
 => (filename :: singleton(#f))
  command-error("Link failed: %s", message)
end method compiler-condition-handler;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"link-warning", warning-message :: <string>)
 => (filename :: singleton(#f))
  message(context, "%s", warning-message);
end method compiler-condition-handler;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"fatal-error", message :: <string>)
 => (filename :: singleton(#f))
  command-error("Fatal error: %s", message)
end method compiler-condition-handler;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"yes-no",
     message :: <string>)
 => (yes? :: <boolean>)
  command-line-question(context.context-server, message)
end method compiler-condition-handler;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type == #"warning",
     warning-message :: <string>)
 => (yes? :: <boolean>)
  message(context, "warning: %s", warning-message)
end method compiler-condition-handler;

define method compiler-condition-handler
    (context :: <environment-context>,
     handler-type :: <symbol>,
     warning-message :: <string>)
 => (yes? :: <boolean>)
  message(context, "missing handler for %s: %s", handler-type, warning-message)
end method compiler-condition-handler;


/// Link command

define class <link-project-command> (<abstract-link-command>)
end class <link-project-command>;

define command-line link => <link-project-command>
    (summary:       "links a project's executable",
     documentation: "Links the executable for a project.")
  optional project :: <project-object> = "the project to link";
  keyword build-script :: <file-locator> = "the (Jam) build script to use";
  keyword target :: <symbol> = "the target [dll or executable]";
  flag force       = "force relink the executable [off by default]";
  flag subprojects = "link subprojects as well if necessary [on by default]";
  flag unify       = "combine the libraries into a single executable [off by default]";
end command-line link;

define method do-execute-command
    (context :: <environment-context>, command :: <link-project-command>)
 => ()
  let project-context = context.context-project-context;
  let project = command.%project | context.context-project;
  let build-script
    = command.%build-script | project-context.context-build-script;
  let messages = #"internal";
  link-project(project,
               build-script:         build-script,
               target:               command.%target,
               force?:               command.%force?,
               process-subprojects?: command.%subprojects?,
               unify?:               command.%unify?,
               progress-callback:    curry(note-build-progress, context),
               error-handler:        curry(compiler-condition-handler, context),
               messages:             messages)
end method do-execute-command;


/// clean command

define class <clean-command> (<project-command>)
  constant slot %project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  constant slot %subprojects? :: <boolean> = #f,
    init-keyword: subprojects?:;
end class <clean-command>;

define command-line clean => <clean-command>
    (summary:       "remove the project's build products",
     documentation: "Removes the build products for a project.")
  optional project :: <project-object> = "the project";
  flag subprojects = "remove the build products for subprojects as well [off by default]";
end command-line clean;

define method do-execute-command
    (context :: <environment-context>,
     command :: <clean-command>)
 => ()
  let project = command.%project | context.context-project;
  clean-project
    (project,
     process-subprojects?: command.%subprojects?,
     error-handler:        curry(compiler-condition-handler, context))
end method do-execute-command;


///---*** To do

/*
  compile-library
  make-release
  profile (the compiler)
  heap-statistics
  collect-garbage
  room
  break
  dood statistics
  trace-optimizations
*/


/// Build commands

define command-group build
    (summary: "project building commands",
     documentation: "Commands to drive project building.")
  property compiler-back-end;
  property compilation-mode;
  property build-script;
  property target-platform;
  command  build;
  command  link;
  command  clean;
end command-group build;
