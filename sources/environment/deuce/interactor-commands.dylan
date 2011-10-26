Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Interactor Commands

/// User-level commands

define class <in-command> (<basic-command>)
  constant slot %module :: false-or(<string>) = #f, init-keyword: module:;
  constant slot %library :: false-or(<string>) = #f, init-keyword: library:;
end;

define command-line in => <in-command>
  (summary: "Switch to module in library",
   documentation:
  "Usage: IN\n"
  "       IN module\n"
  "       IN module:library\n"
  "\n"
  "Switches to the specified module in specified library.\n"
  "All future evaluations will be done in the new module.\n"
  "If no arguments are specified, the current module and library\n"
  "are displayed.\n"
  "If the module is uniquely named within the application, then the\n"
  "library need not be specified.")
  optional module :: <string> = "target module";
  optional library :: <string> = "target library";
end;

define method do-execute-command (context :: <interactor-context>, command :: <in-command>)
  let module = command.%module;
  let library = command.%library;
  let pane = context.interactor;
  let frame = sheet-frame(pane);
  let the-project = frame-current-project(frame);
  local method current-module-and-library-name
	    () => (module :: <string>, library :: <string>)
	  let the-module
	    = frame-current-module(frame);
	  let the-library
	    = environment-object-library(the-project, the-module);
	  values(environment-object-primitive-name(the-project, the-module),
		 environment-object-primitive-name(the-project, the-library))
	end method current-module-and-library-name;
  if (module)
    let the-module
      = find-module(frame-current-project(frame), module,
		    library: library,
		    all-libraries?: #t);
    let the-library
      = the-module & environment-object-library(the-project, the-module);
    case
      ~the-module =>
	message(context, "No such module as '%s'.", module);
      ~library-interactive?(the-project, the-library) =>
	message(context, "Module '%s' is not interactive.", module);
      otherwise =>
	frame-current-module(frame) := the-module;
        context.context-project-context.context-module := the-module;
	update-module-gadget(frame);
	let (module, library) = current-module-and-library-name();
	message(context, "Module set to '%s' of library '%s'.", module, library);
    end
  else
    let (module, library) = current-module-and-library-name();
    message(context, "Currently in module '%s' of library '%s'.", module, library)
  end;
end;

/// Internal commands
// Need replacements for them - should be straightforward!
/*
define imported-interactor-command collect-garbage;

define imported-interactor-command room;

define imported-interactor-command compile-library;

define imported-interactor-command statistics;

define imported-interactor-command save;

define imported-interactor-command flush;

define imported-interactor-command build-locations;

define imported-interactor-command find-library;

define imported-interactor-command all-open-projects;

define imported-interactor-command registries;

define imported-interactor-command update-libraries;

define imported-interactor-command trace-optimizations;

define imported-interactor-command untrace-optimizations;
*/

/// Recovery Protocol
//same here, replacements
/*
define interactor-command
  continue (restart-option)
  description shell/command-description($continue-command)
  documentation shell/command-documentation($continue-command)
  hidden? shell/command-hidden?($continue-command)

  let restart-option = string-to-integer(restart-option | "0");
  let thread   = interactor-remote-thread(pane);
  let restarts = application-thread-restarts(the-project, thread);
  let restart  = element(restarts, restart-option, default: #f);
  if (restart)
    invoke-application-restart(the-project, thread, restart)
  else
    interactor-message(stream, "No such restart %=; please choose another option.", restart-option)
  end
end;

define interactor-command
  abort ()
  description shell/command-description($abort-command)
  documentation shell/command-documentation($abort-command)
  hidden? shell/command-hidden?($abort-command)

  let thread   = interactor-remote-thread(pane);
  let restarts = application-thread-restarts(the-project, thread);
  block (return)
    for (restart in restarts)
      when (application-restart-abort?(the-project, restart))
	invoke-application-restart(the-project, thread, restart);
	return()
      end
    end;
    interactor-message(stream, "No abort restart found; please try another option.")
  end
end;

define interactor-command
  restarts ()
  description shell/command-description($restarts-command)
  documentation shell/command-documentation($restarts-command)
  hidden? shell/command-hidden?($restarts-command)

  let thread    = interactor-remote-thread(pane);
  let restarts  = application-thread-restarts(the-project, thread);
  let abort?    = #f;
  let continue? = #f;
  for (restart in restarts,
       count from 0)
    let abort-restart? = application-restart-abort?(the-project, restart);
    let prefix
      = case
	  ~continue? & ~abort-restart? =>
	    continue? := #t;
	    "(continue) ";
	  ~abort? & abort-restart? =>
	    abort? := #t;
	    "(abort) ";
	  otherwise =>
	    " ";
	end;
    interactor-message(stream, "%=: %s%s", count, prefix,
		       application-restart-message(the-project, restart))
  end;
  format(stream, "\n");
  if (empty?(restarts))
    interactor-message(stream, "Currently no active restarts.")
  else
    interactor-message(stream, "Type :c followed by a number to proceed or type :help for other options.")
  end
end;
*/

define class <describe-command> (<basic-command>)
  constant slot %expression :: <string>, required-init-keyword: expression:;
end;

define command-line describe => <describe-command>
  (summary: "Show the contents of an object",
  documentation:
  "Usage: DESCRIBE expression\n"
  "\n"
  "Evaluates expression, and then shows the contents of the\n"
  "first returned value (if there is one).")
  //---*** Note that this is a fake to get help, see the real
  //---*** implementation in interactor-control.dylan!
  argument expression :: <string> = "expression to evaluate";
end;

define class <show-contents-command> (<basic-command>)
  constant slot %expression :: <string>, required-init-keyword: expression:;
end;

define command-line show-contents => <show-contents-command>
  (summary: "Show the contents of an object",
  documentation:
  "Usage: SHOW-CONTENTS expression\n"
  "\n"
  "Evaluates expression, and then shows the contents of the\n"
  "first returned value (if there is one).")
  //---*** Note that this is a fake to get help, see the real
  //---*** implementation in interactor-control.dylan!
  argument expression :: <string> = "expression to evaluate";
end;

define class <break-command> (<basic-command>)
  constant slot %name :: <string>, required-init-keyword: name:;
end;

define command-line break => <break-command>
  (summary: "Sets a function breakpoint",
  documentation:
  "Usage: BREAK name\n"
  "\n"
  "Sets a breakpoint on the named function.")
  //---*** Note that this is a fake to get help, see the real
  //---*** implementation in interactor-control.dylan!
  argument name :: <string> = "function name where to break"
end;


define command-group interactor into environment
  (summary: "Interactor commands",
   documentation: "Commands for interactor")
  command in;
  command describe;
  command show-contents;
  command break;
end command-group;

