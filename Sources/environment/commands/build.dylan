Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Build properties

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
  let linker = context.context-project-context.context-linker;
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


// Linker

define class <linker-property> (<environment-property>)
end class <linker-property>;

define command-property linker => <linker-property>
  (summary:       "Current linker",
   documentation: "The currently active linker.",
   type:          <symbol>,
   persistent?:   #t)
end command-property linker;

define method show-property
    (context :: <environment-context>, property :: <linker-property>)
 => ()
  let project-context = context.context-project-context;
  let linker
    = if (project-context)
	project-context.context-linker
      else
	default-linker()
      end;
  message(context, "Linker: %s",
	  select (linker)
	    #"microsoft" => "microsoft [Microsoft(TM) linker]";
	    #"gnu"       => "gnu       [GNU linker]";
	    otherwise    => format-to-string("Unknown linker %s", linker);
	  end)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <linker-property>, 
     linker :: <symbol>,
     #key save?)
 => ()
  select (linker)
    #"microsoft", #"gnu" =>
      #f;
    otherwise =>
      set-error("Unrecognised linker: %s", linker)
  end;
  let project-context = context.context-project-context;
  if (project-context)
    project-context.context-linker := linker
  end;
  if (save?)
    default-linker() := linker
  end
end method set-property;


/// Build command

define class <abstract-link-command> (<project-command>)
  constant slot %project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  constant slot %linker :: false-or(<symbol>) = #f,
    init-keyword: linker:;
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
  keyword linker :: <symbol> = "the linker to use";
  keyword target :: <symbol> = "the target [dll or exe]";
  flag force         = "force relink the executable [off by default]";
  flag unify         = "combine the libraries into a single executable [off by default]";
end command-line build;

define method do-execute-command
    (context :: <environment-context>, command :: <build-project-command>)
 => ()
  let project = command.%project | context.context-project;
  let messages = if (release-internal?()) #"internal" else #"external" end;
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
	   error-handler:        #f))
      if (command.%link?)
	let project-context = context.context-project-context;
	let linker = command.%linker | project-context.context-linker;
	link-project
	  (project,
	   linker:               linker,
	   target:               command.%target,
	   release?:             command.%release?,
	   force?:               command.%force?,
	   process-subprojects?: command.%subprojects?,
	   unify?:               command.%unify?,
	   messages:             messages,
	   progress-callback:    curry(note-build-progress, context),
	   error-handler:        #f)
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


/// Link command

define class <link-project-command> (<abstract-link-command>)
end class <link-project-command>;

define command-line link => <link-project-command>
    (summary:       "links a project's executable",
     documentation: "Links the executable for a project.")
  optional project :: <project-object> = "the project to link";
  keyword linker :: <symbol> = "the linker to use";
  keyword target :: <symbol> = "the target [dll or exe]";
  flag force       = "force relink the executable [off by default]";
  flag subprojects = "link subprojects as well if necessary [on by default]";
  flag unify       = "combine the libraries into a single executable [off by default]";
end command-line link;

define method do-execute-command
    (context :: <environment-context>, command :: <link-project-command>)
 => ()
  let project-context = context.context-project-context;
  let project = command.%project | context.context-project;
  let linker = command.%linker | project-context.context-linker;
  let messages = if (release-internal?()) #"internal" else #"external" end;
  link-project(project,
	       linker:               linker,
	       target:               command.%target,
	       force?:               command.%force?,
	       process-subprojects?: command.%subprojects?,
	       unify?:               command.%unify?,
	       progress-callback:    curry(note-build-progress, context),
	       error-handler:        #f,
	       messages:             messages)
end method do-execute-command;


/// Remove Build Products command

define class <remove-build-products-command> (<project-command>)
  constant slot %project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  constant slot %subprojects? :: <boolean> = #f,
    init-keyword: subprojects?:;
end class <remove-build-products-command>;

define command-line remove-build-products => <remove-build-products-command>
    (summary:       "links a project's executable",
     documentation: "Links the executable for a project.")
  optional project :: <project-object> = "the project to link";
  flag subprojects = "remove the build products for subprojects as well [off by default]";
end command-line remove-build-products;

define method do-execute-command
    (context :: <environment-context>, 
     command :: <remove-build-products-command>)
 => ()
  let project = command.%project | context.context-project;
  remove-project-build-products
    (project,
     process-subprojects?: command.%subprojects?,
     error-handler:        #f)
end method do-execute-command;


///---*** To do

/*
  compile-library
  remove-build-products
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
  property compilation-mode;
  property linker;
  command  build;
  command  link;
  command  remove-build-products;
end command-group build;
