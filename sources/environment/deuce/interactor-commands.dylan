Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Interactor Commands
//---*** andrewa: should replace this with environment-commands protocols

define macro interactor-command-definer
  { define interactor-command ?:name (?args:*)
      description ?description:expression
      documentation ?documentation:expression
      hidden? ?hide:expression
      ?:body
    end }
    => { define shell-command
           ?name (?args)
           description ?description
           documentation ?documentation
           hidden? ?hide
           interactor-command-body ()
             ?body
           end
         end }
end macro interactor-command-definer;

define macro interactor-command-body
  { interactor-command-body () ?:body end }
    => { let ?=pane  = %source(?=context);
	 let ?=frame = sheet-frame(?=pane);
	 let ?=the-project = frame-current-project(?=frame);
	 with-editor-state-bound (buffer = ?=pane)
	   let ?=stream = make(<interval-stream>,
			       interval: buffer,
			       direction: #"output");
	   %sink(?=context) := ?=stream;
	   ?body;
	   queue-redisplay(?=pane, $display-text, centering: 1);
	   redisplay-window(?=pane)
	 end; }
end macro interactor-command-body;

define macro imported-interactor-command-definer
  { define imported-interactor-command ?:name }
    => { begin
	   let command = "$" ## ?name ## "-command";
	   let prev-command-function = shell/command-function(command);
	   let command-function
	     = method (?=context :: <command-loop>, #rest args) => ()
		 interactor-command-body ()
		   apply(prev-command-function, ?=context, args)
	         end
	       end method;
	   shell/command-function(command) := command-function;
	   register-command(*top-level-loop*, command)
         end }
end macro imported-interactor-command-definer;

define function interactor-message
    (stream :: <stream>, string :: <string>, #rest args) => ()
  apply(format, stream, concatenate("// ", string, "\n"), args)
end function interactor-message;


/// User-level commands

define interactor-command
  in (module, library)
  description "Switch to module in library"
  documentation
  "Usage: IN\n"
  "       IN module\n"
  "       IN module:library\n"
  "\n"
  "Switches to the specified module in specified library.\n"
  "All future evaluations will be done in the new module.\n"
  "If no arguments are specified, the current module and library\n"
  "are displayed.\n"
  "If the module is uniquely named within the application, then the\n"
  "library need not be specified."
  hidden? #f

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
	interactor-message(stream, "No such module as '%s'.", module);
      ~library-interactive?(the-project, the-library) =>
	interactor-message(stream, "Module '%s' is not interactive.", module);
      otherwise =>
	frame-current-module(frame) := the-module;
	update-module-gadget(frame);
	let (module, library) = current-module-and-library-name();
	interactor-message(stream, "Module set to '%s' of library '%s'.", module, library);
    end
  else
    let (module, library) = current-module-and-library-name();
    interactor-message(stream, "Currently in module '%s' of library '%s'.", module, library)
  end
end;

define command-argument count;

define interactor-command
  more (count)
  description "Show more of the previously shown contents"
  documentation
  "Usage: MORE\n"
  "       MORE count\n"
  "\n"
  "This command will show the next set of contents from the results\n"
  "of using the \"Show Contents\" command. If count is specified\n"
  "then that many items will be shown, otherwise the default will\n"
  "be used.\n"
  hidden? #f

  let count  = count & string-to-integer(count);
  let subset = interactor-next-subset(pane);
  if (subset)
    interactor-show-contents(pane, subset, count: count)
  else
    interactor-message(stream, "No more contents to show.")
  end
end;

define imported-interactor-command help;

define imported-interactor-command help-all;

define imported-interactor-command parse;

define imported-interactor-command open;

define imported-interactor-command import;

define imported-interactor-command close;

define imported-interactor-command close-all;

define imported-interactor-command link;

define imported-interactor-command build;


/// Internal commands

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


/// Recovery Protocol

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

define interactor-command
  describe ()
  description "Show the contents of an object"
  documentation
  "Usage: DESCRIBE expression\n"
  "\n"
  "Evaluates expression, and then shows the contents of the\n"
  "first returned value (if there is one)."
  hidden? #f

  //---*** Note that this is a fake to get help, see the real
  //---*** implementation in interactor-control.dylan!
  #f
end;

define interactor-command
  show-contents ()
  description "Show the contents of an object"
  documentation
  "Usage: SHOW-CONTENTS expression\n"
  "\n"
  "Evaluates expression, and then shows the contents of the\n"
  "first returned value (if there is one)."
  hidden? #f

  //---*** Note that this is a fake to get help, see the real
  //---*** implementation in interactor-control.dylan!
  #f
end;

define interactor-command
  break ()
  description "Sets a function breakpoint"
  documentation
  "Usage: BREAK name\n"
  "\n"
  "Sets a breakpoint on the named function."
  hidden? #f

  //---*** Note that this is a fake to get help, see the real
  //---*** implementation in interactor-control.dylan!
  #f
end;
