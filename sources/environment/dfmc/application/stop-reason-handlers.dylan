Module:    dfmc-application
Synopsis:  Stop reason handling code
Author:    Bill Chiles, Jason Trenouth, Paul Howard, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <STOP-REASON-ACTION> (internal)

define sealed class <stop-reason-action> (<object>)
  sealed constant slot stop-reason-action-label :: <string>, 
    required-init-keyword: label:;
end class <stop-reason-action>;

define sealed class <stop-reason-report-action> (<stop-reason-action>)
end class <stop-reason-report-action>;

define sealed class <stop-reason-debug-action> (<stop-reason-report-action>)
end class <stop-reason-debug-action>;


/// STOP-REASON-ACTION-DEFINER (internal)

define macro stop-reason-action-definer
  { define stop-reason-action ?name:name (?class:name) ?initargs:* end }
    =>
    { define constant ?name :: <stop-reason-action> = make(?class, ?initargs) }
end macro stop-reason-action-definer;


/// STOP-REASON-ACTIONs (internal)

define stop-reason-action stop-reason-ignore-action (<stop-reason-action>)
  label: "Ignore"
end stop-reason-action;

define stop-reason-action stop-reason-report-action (<stop-reason-report-action>)
  label: "Report"
end stop-reason-action;

define stop-reason-action stop-reason-debug-action (<stop-reason-debug-action>)
  label: "Debug"
end stop-reason-action;

/// $STOP-REASON-TYPES (internal)

define constant $stop-reason-types :: <object-table>
  = make(<object-table>);

define function stop-reason-types
    () => (types :: <object-table>)
  $stop-reason-types
end function stop-reason-types;


/// <STOP-REASON-TYPE> (internal)

define sealed class <stop-reason-type> (<object>)
  sealed constant slot stop-reason-class :: subclass(<stop-reason>), 
    required-init-keyword: class:;
  sealed constant slot stop-reason-label :: <string>, 
    required-init-keyword: label:;
  sealed slot stop-reason-action :: <stop-reason-action>, 
    required-init-keyword: action:;
  sealed constant slot stop-reason-message :: false-or(<string>) = #f,
    init-keyword: message:;
  sealed constant slot stop-reason-argument-callback :: false-or(<function>) = #f,
    init-keyword: argument-callback:;
end class <stop-reason-type>;


/// INITIALIZE (dylan)

define sealed method initialize (type :: <stop-reason-type>, #key)
  next-method();
  element(stop-reason-types(), type.stop-reason-class) := type;
end method initialize;

define sealed domain make (subclass(<stop-reason-type>));
define sealed domain initialize (<stop-reason-type>);


/// SHALLOW-COPY (dylan)

define sealed method shallow-copy (type :: <stop-reason-type>)
 => (new-type :: <stop-reason-type>)
  make(<stop-reason-type>,
       class:   type.stop-reason-class,
       label:   type.stop-reason-label,
       action:  type.stop-reason-action,
       message: type.stop-reason-message,
       argument-callback: type.stop-reason-argument-callback);
end method shallow-copy;


/// STOP-REASON-TYPE-DEFINER (internal) 

define macro stop-reason-type-definer
  { define stop-reason-type ?class:name ?initargs:* end }
    => 
    { make(<stop-reason-type>, class: ?class, ?initargs) }
end macro;


/// STOP-REASON-TYPE (internal) 

define stop-reason-type <stop-reason>
  action:  stop-reason-debug-action,
  message: "Unknown exception.",
  label:   "Unknown exception"
end stop-reason-type;

define stop-reason-type <debugger-generated-stop-reason>
  action:  stop-reason-ignore-action,
  message: "Environment requested pause.",
  label:   "Browser generated"
end stop-reason-type;

define stop-reason-type <debugger-stop-application-stop-reason>
  action:  stop-reason-debug-action,
  message: "User requested pause.",
  label:   "Stop button"
end stop-reason-type;

define stop-reason-type <create-process-stop-reason>
  action:  stop-reason-report-action,
  message: "Application process created: %s.",
  argument-callback: method (application :: <dfmc-application>, stop-reason)
		       application.application-filename
		     end method,
  label:   "Process creation"
end stop-reason-type;

define stop-reason-type <exit-process-stop-reason>
  action:  stop-reason-report-action,
  message: "Application process exited with exit code: %d.",
  argument-callback: method (application, stop-reason)
		       stop-reason.stop-reason-process-exit-code
		     end method,
  label:   "Process exit"
end stop-reason-type;

define stop-reason-type  <RIP-stop-reason>
  action:  stop-reason-report-action,
  message: "The application died (RIP event received).",
  label:   "Process RIP"
end stop-reason-type;

define stop-reason-type <create-thread-stop-reason>
  action:  stop-reason-report-action,
  message: "Application created a thread.",
  label:   "Thread creation"
end stop-reason-type;

define stop-reason-type <exit-thread-stop-reason>
  action:  stop-reason-report-action,
  message: "Application destroyed a thread.",
  label:   "Thread exit"
end stop-reason-type;

define stop-reason-type <load-library-stop-reason>
  action:  stop-reason-report-action,
  message: "Application loaded a library: %s.",
  argument-callback: method (application, stop-reason)
		       stop-reason.stop-reason-library.library-image-name
		     end method,
  label:   "Library load"
end stop-reason-type;

define stop-reason-type <unload-library-stop-reason>
  action:  stop-reason-report-action,
  message: "Application unloaded a library: %s.",
  argument-callback: method (application, stop-reason)
		       stop-reason.stop-reason-library.library-image-name
		     end method,
  label:   "Library unload"
end stop-reason-type;

define stop-reason-type <single-step-stop-reason>
  action:  stop-reason-report-action,
  message: "Application singled stepped.",
  label:   "Single step"
end stop-reason-type;

// phoward modified 8th August 1997
// The invoke-debugger-stop-reason has been divided into
// system-invoke-debugger (meaning a hard breakpoint) and
// system-initialized (meaning an automatic breakpoint).

define stop-reason-type <system-invoke-debugger-stop-reason>
  action:  stop-reason-debug-action,
  message: "Application hit hard coded breakpoint.",
  label:   "Hard-coded break"
end stop-reason-type;

define stop-reason-type <system-initialized-stop-reason>
  action:  stop-reason-report-action,
  message: "System initialized: %s.",
  argument-callback: method (application :: <dfmc-application>, stop-reason)
		       application.application-filename
		     end method,
  label:   "System initialization"
end stop-reason-type;

define stop-reason-type <dylan-invoke-debugger-stop-reason>
  action:  stop-reason-debug-action,
  message: "Dylan error: %s",
  argument-callback: method (application, stop-reason)
		       dylan-error-message-string(stop-reason)
		     end method,
  label:   "Dylan language error"
end stop-reason-type;

define stop-reason-type <access-violation-stop-reason>
  action:  stop-reason-debug-action,
  message: "Access violation.",
  label:   "Access violation"
end stop-reason-type;

define stop-reason-type <array-bounds-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Array bounds exception.",
  label:   "Array bounds exception"
end stop-reason-type;

define stop-reason-type <illegal-instruction-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Illegal instruction exception.",
  label:   "Illegal instruction"
end stop-reason-type;

define stop-reason-type <privileged-instruction-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Privileged instruction exception.",
  label:   "Privileged instruction"
end stop-reason-type;

define stop-reason-type <denormal-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point denormal exception.",
  label:   "Denormal exception"
end stop-reason-type;

define stop-reason-type <float-divide-by-zero-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point division by zero.",
  label:   "Float divided by zero"
end stop-reason-type;

define stop-reason-type <inexact-result-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point inexact result.",
  label:   "Inexact result exception"
end stop-reason-type;

define stop-reason-type <invalid-float-operation-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point invalid operation.",
  label:   "Invalid float operation"
end stop-reason-type;

define stop-reason-type <float-overflow-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point overflow.",
  label:   "Float overflow"
end stop-reason-type;

define stop-reason-type <float-underflow-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point underflow.",
  label:   "Float underflow"
end stop-reason-type;

define stop-reason-type <float-stack-check-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Floating point stack-check error.",
  label:   "Float stack check exception"
end stop-reason-type;

define stop-reason-type <integer-divide-by-zero-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Integer division by zero.",
  label:   "Integer divide by zero"
end stop-reason-type;

define stop-reason-type <noncontinuable-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Noncontinuable error.",
  label:   "Noncontinuable exception"
end stop-reason-type;

// ---*** APPLICATION: <debug-message-stop-reason> not implemented yet?
//
//define stop-reason-type <debug-message-stop-reason>
//  action:  stop-reason-report-action,
//  message: "Dylan Debug message printed.",
//  label:   "Dylan Debug Message"
//end stop-reason-type;

define stop-reason-type <source-step-into-stop-reason>
  action:  stop-reason-debug-action,
  message: "Thread stepped into function call.",
  label:   "Source Code Step"
end stop-reason-type;

define stop-reason-type <source-step-out-stop-reason>
  action:  stop-reason-debug-action,
  message: "Thread stepped out of function call.",
  label:   "Source Code Step"
end stop-reason-type;

define stop-reason-type <source-step-over-stop-reason>
  action:  stop-reason-debug-action,
  message: "Thread stepped over function call.",
  label:   "Source Code Step"
end stop-reason-type;

define stop-reason-type <breakpoint-stop-reason>
  action:  stop-reason-debug-action,
  message: "Breakpoint reached: %s.",
  label:   "Environment breakpoint"
end stop-reason-type;

define stop-reason-type <class-breakpoint-stop-reason>
  action:  stop-reason-debug-action,
  message: "Class breakpoint reached: %s.",
  argument-callback:
    method (application :: <dfmc-application>,
	    stop-reason :: <class-breakpoint-stop-reason>)
      let target = application.application-target-app;
      let path = target.debug-target-access-path;
      let class :: <remote-value> = stop-reason.class-breakpoint-class;

      format-to-string("Allocating %d bytes of %s",
		       stop-reason.class-breakpoint-size,
		       print-dylan-object(target, class))
    end method,
  label:   "Environment class breakpoint"
end stop-reason-type;

define stop-reason-type <source-code-alignment-stop-reason>
  action:  stop-reason-debug-action,
  message: "Thread prepared for interactive evaluation.",
  label:   "Source Code Alignment"
end stop-reason-type;

define stop-reason-type <dylan-debug-message-stop-reason>
  action:  stop-reason-report-action,
  message: "Debug: %s",
  argument-callback: method (application, stop-reason)
		       dylan-debug-message-string(stop-reason)
		     end method,
  label:   "Dylan debug message"
end stop-reason-type;

define stop-reason-type <output-debug-string-stop-reason>
  action:  stop-reason-report-action,
  message: "Debug: %s",
  argument-callback: method (application, stop-reason)
		       stop-reason-debug-string(stop-reason)
		     end method,
  label:   "System debug message"
end stop-reason-type;

define stop-reason-type <unclassified-exception-stop-reason>
  action:  stop-reason-debug-action,
  message: "Unclassified stop reason. (System possibly failed to initialize.)",
  label:   "Unclassified stop reason"
end stop-reason-type;


/// DETERMINE-STOP-REASON-ACTION (internal)

define function determine-stop-reason-action 
    (stop-reason :: <stop-reason>)
 => (action :: <stop-reason-action>)
  let class = stop-reason.object-class;
  let types = stop-reason-types();
  let type :: <stop-reason-type>
    = element(types, class, default: #f)
        | element(types, <stop-reason>);
  type.stop-reason-action
end function determine-stop-reason-action;


/// HANDLE-STOP-REASON (internal)

define function handle-stop-reason
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => (stay-stopped? :: <boolean>)
  let action = determine-stop-reason-action(stop-reason);
  stop-reason-admin(action, application, stop-reason);
  stop-reason-report(action, application, stop-reason);
  stop-reason-stay-stopped?(action, application, stop-reason)
    | begin
	let temporary-stop?
	  = stop-reason-temporary-debug?(application, stop-reason);
	application.application-temporary-stop? := temporary-stop?;
	temporary-stop?
      end
end function handle-stop-reason;


/// Notification protocols (internal)

define method note-application-thread-message
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => ()
  let thread :: false-or(<thread-object>) 
    = stop-reason-thread-object(application, stop-reason);
  let message :: <string> = compose-application-message(application, stop-reason);
  invoke-application-callback
    (application, application-thread-message-callback,
     thread, message)
end method note-application-thread-message;

define inline method note-application-threads-changed
    (application :: <dfmc-application>) => ()
  let project = application.server-project;
  let message = make(<application-threads-changed-message>, project: project);
  broadcast($project-channel, message);
  invoke-application-callback
    (application, application-threads-changed-callback)
end method note-application-threads-changed;

define inline method note-application-process-finished
    (application :: <dfmc-application>, exit-code :: false-or(<integer>)) => ()
  application.application-loaded-dylan-library? := #f;
  application.application-initialized-interactive-threads? := #f;
  invoke-application-callback
    (application, application-process-finished-callback, exit-code)
end method note-application-process-finished;


/// APPLICATION-STOP-REASON-MESSAGE (environment-protocols)

define constant $depth-indentation = 2;

define sealed method application-stop-reason-message
    (application :: <dfmc-application>)
 => (message :: false-or(<string>))
  let stop-reason = application.application-stop-reason;
  stop-reason & compose-application-message(application, stop-reason)
end method application-stop-reason-message;

define method compose-application-message
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => (message :: <string>)
  with-output-to-string (stream)
    print-application-message(stream, application, stop-reason)
  end
end method compose-application-message;

define method print-application-message
    (stream :: <stream>, application :: <dfmc-application>,
     stop-reason :: <stop-reason>)
 => ()
  let type :: false-or(<stop-reason-type>) 
    = element(stop-reason-types(), stop-reason.object-class, default: #f);
  if (type)
    let callback = type.stop-reason-argument-callback;
    let format-argument = callback & callback(application, stop-reason);
    if (format-argument)
      format(stream, type.stop-reason-message, format-argument)
    else
      let message = type.stop-reason-message;
      if (message)
	write(stream, message)
      else
	format(stream, "Known exception without message: %=", stop-reason)
      end
    end
  else
    format(stream, "Unknown exception: %=", stop-reason)
  end
end method print-application-message;

define sealed method print-application-message
    (stream :: <stream>, application :: <dfmc-application>, 
     stop-reason :: <breakpoint-stop-reason>)
 => ()
  if (instance?(stop-reason, <source-step-stop-reason>))
    next-method()
  else
    let need-new-line? = #f;
    local method ensure-new-line ()
	    if (need-new-line?)
	      format(stream, "\n")
	    end;
	    need-new-line? := #t
	  end method ensure-new-line;
    let thread = stop-reason-thread-object(application, stop-reason);
    let breakpoints = current-stop-breakpoints(application, thread);
    for (breakpoint :: <breakpoint-object> in breakpoints)
      let message? :: false-or(<string>) = breakpoint.breakpoint-message?;
      if (message?)
	ensure-new-line();
	print-application-breakpoint-message
	  (stream, application, thread, breakpoint);
	if (~empty?(message?))
	  format(stream, " [%s]", message?)
	end
      end
    end;
    //---*** This is too irritating!
    // if (application-just-interacted?(application, thread))
    //   ensure-new-line();
    //   format(stream, "Interaction returned.\n")
    // end;
    if (application-just-stepped?(application, thread))
      ensure-new-line();
      format(stream, "Application stepped.")
    end;
    if (application.application-just-initialized?)
      ensure-new-line();
      format(stream, "Application initialized: %s.",
	     application.application-filename)
    end;
    if (application.application-reached-interaction-point?)
      ensure-new-line();
      format(stream, "Application ready for interaction: %s.", 
	     application.application-filename)
    end
  end
end method print-application-message;

/// APPLICATION-BREAKPOINT-MESSAGE-ARGUMENT (internal)

define sealed method print-application-breakpoint-message
    (stream :: <stream>, application :: <dfmc-application>, 
     thread :: <thread-object>,  breakpoint :: <breakpoint-object>)
 => ()
  #f
end method print-application-breakpoint-message;

define constant $breakpoint-arguments-prefix = ":";
define constant $breakpoint-values-prefix    = " =>";

define sealed method print-application-breakpoint-message
    (stream :: <stream>, application :: <dfmc-application>,
     thread :: <thread-object>, breakpoint :: <function-breakpoint-object>)
 => ()
  let project = breakpoint.breakpoint-project;
  let info = breakpoint-info(application, breakpoint);
  let directions = breakpoint.breakpoint-directions;
  let depth = thread-message-depth(application, thread);
  let (direction-arrow, current-depth, new-depth)
    = select (info by instance?)
	<breakpoint-entry-info> =>
	  values($breakpoint-arguments-prefix,
		 depth,
		 member?(#"out", directions) & (depth + 1));
	<breakpoint-return-info> =>
	  let new-depth
	    = if (member?(#"in", directions))
		depth - 1
	      else
		depth
	      end;
	  values($breakpoint-values-prefix,
		 new-depth,
		 new-depth);
	otherwise =>
	  values($breakpoint-arguments-prefix,
		 depth,
		 #f);
      end;
  if (new-depth)
    thread-message-depth(application, thread) := new-depth
  end;
  let indentation = current-depth * $depth-indentation;
  format(stream, "%s%d: ",
	 make(<byte-string>, size: indentation, fill: ' '),
	 current-depth);
  print-environment-object-name
    (stream, project, breakpoint.breakpoint-object, qualify-names?: #f);
  format(stream, "%s ", direction-arrow);
  print-breakpoint-info-values(stream, project, info)
end method print-application-breakpoint-message;

define sealed method print-application-breakpoint-message
    (stream :: <stream>, application :: <dfmc-application>, 
     thread :: <thread-object>, 
     breakpoint :: <source-location-breakpoint-object>)
 => ()
  let source-location :: <source-location> = breakpoint.breakpoint-object;
  print-source-line-location
    (source-location.source-location-source-record,
     source-location.source-location-start-line,
     stream)
end method print-application-breakpoint-message;

define method print-breakpoint-info-values
    (stream :: <stream>, project :: <project-object>,
     info :: <breakpoint-entry-info>)
 => ()
  ignore(info-rest-value, info-keyword-values);
  write(stream, "(");
  for (value in info.info-required-values,
       separator = "" then ", ")
    write(stream, separator);
    print-environment-object(stream, project, value, qualify-names?: #f)
  end;
  write(stream, ")")
end method print-breakpoint-info-values;

define method print-breakpoint-info-values
    (stream :: <stream>, project :: <project-object>,
     info :: <breakpoint-return-info>)
 => ()
  write(stream, "(");
  for (value in info.info-return-values,
       separator = "" then ", ")
    write(stream, separator);
    print-environment-object(stream, project, value, qualify-names?: #f)
  end;
  write(stream, ")")
end method print-breakpoint-info-values;


/// THREAD-MESSAGE-DEPTH (internal)

define function thread-message-depth
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (depth :: <integer>)
  let depths = application.application-thread-message-depths;
  element(depths, thread, default: 0);
end function thread-message-depth;

define function thread-message-depth-setter 
    (depth :: <integer>, application :: <dfmc-application>,
     thread :: <thread-object>)
 => (depth :: <integer>)
  let depths = application.application-thread-message-depths;
  element(depths, thread) := depth
end function thread-message-depth-setter;


/// STOP-REASON-REPORT (internal)

define sealed method stop-reason-report
    (type :: <stop-reason-action>, application :: <dfmc-application>,
     stop-reason :: <stop-reason>)
 => ()
  #f
end method stop-reason-report;

define sealed method stop-reason-report
    (type :: <stop-reason-report-action>, application :: <dfmc-application>, 
     stop-reason :: <stop-reason>)
 => ()
  note-application-thread-message(application, stop-reason)
end method stop-reason-report;

define sealed method stop-reason-report
    (type :: <stop-reason-report-action>, application :: <dfmc-application>,
     stop-reason :: <source-code-alignment-stop-reason>)
 => ()
  #f
end method stop-reason-report;

define sealed method stop-reason-report
    (type :: <stop-reason-report-action>, application :: <dfmc-application>,
     stop-reason :: <source-step-stop-reason>)
 => ()
  note-application-thread-message(application, stop-reason)
end method stop-reason-report;

define sealed method stop-reason-report
    (type :: <stop-reason-report-action>, application :: <dfmc-application>, 
     stop-reason :: <breakpoint-stop-reason>)
 => ()
  let thread :: <thread-object> = stop-reason-thread-object(application, stop-reason);
  if (any?(breakpoint-message?, current-stop-breakpoints(application, thread))
	| application-just-stepped?(application, thread)
//---*** andrewa: we don't print interaction messages anymore!
//	| application-just-interacted?(application, thread)
	| application.application-just-initialized?
        | application.application-reached-interaction-point?)
    next-method()
  end if;
end method stop-reason-report;

/// STOP-REASON-STAY-STOPPED? (internal)

define sealed method stop-reason-stay-stopped?
    (type :: <stop-reason-action>, application :: <dfmc-application>, 
     stop-reason :: <stop-reason>)
 => (debug? :: <boolean>)
  #f
end method stop-reason-stay-stopped?;

define sealed method stop-reason-stay-stopped?
    (type :: <stop-reason-debug-action>, application :: <dfmc-application>, 
     stop-reason :: <stop-reason>)
  => (debug? :: <boolean>)
  #t
end method stop-reason-stay-stopped?;

define sealed method stop-reason-stay-stopped?
    (type :: <stop-reason-debug-action>, application :: <dfmc-application>, 
     stop-reason :: <source-code-alignment-stop-reason>)
  => (debug? :: <boolean>)
  #f
end method stop-reason-stay-stopped?;

define sealed method stop-reason-stay-stopped?
    (type :: <stop-reason-debug-action>, application :: <dfmc-application>,
     stop-reason :: <source-step-stop-reason>)
  => (debug? :: <boolean>)
  #t
end method stop-reason-stay-stopped?;

define sealed method stop-reason-stay-stopped?
    (type :: <stop-reason-debug-action>, application :: <dfmc-application>, 
     stop-reason :: <breakpoint-stop-reason>)
 => (debug? :: <boolean>)
  let thread :: <thread-object> = stop-reason-thread-object(application, stop-reason);
  application-just-hit-breakpoint?(application, thread)
    | application-just-stepped?(application, thread)
    | (application-just-interacted?(application, thread)
	 & ~application.application-target-app.application-running-on-code-entry?)
    | (application.application-startup-option == #"interact"
	 & application.application-reached-interaction-point?)
    | (application.application-startup-option == #"debug"
	 & application.application-reached-interaction-point?)
    | (application.application-just-finished-execution?
	 & application.application-pause-before-termination?)
end method stop-reason-stay-stopped?;


/// STOP-REASON-TEMPORARY-DEBUG? (internal)

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>,
     stop-reason :: <stop-reason>)
 => (debug? :: <boolean>)
  #f
end method stop-reason-temporary-debug?;

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>, 
     stop-reason :: <breakpoint-stop-reason>)
 => (debug? :: <boolean>)
  #t
end method stop-reason-temporary-debug?;

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>, 
     stop-reason :: <source-code-alignment-stop-reason>)
 => (debug? :: <boolean>)
  #t
end method stop-reason-temporary-debug?;

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>, 
     stop-reason :: <create-process-stop-reason>)
 => (debug? :: <boolean>)
  #t
end method stop-reason-temporary-debug?;

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>, 
     stop-reason :: <exit-process-stop-reason>)
 => (debug? :: <boolean>)
  #t
end method stop-reason-temporary-debug?;

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>, 
     stop-reason :: <create-thread-stop-reason>)
 => (debug? :: <boolean>)
  #t
end method stop-reason-temporary-debug?;

define sealed method stop-reason-temporary-debug?
    (application :: <dfmc-application>, 
     stop-reason :: <exit-thread-stop-reason>)
 => (debug? :: <boolean>)
  #t
end method stop-reason-temporary-debug?;


/// STOP-REASON-ADMIN (internal)

define sealed method stop-reason-admin 
    (type :: <stop-reason-action>,
     application :: <dfmc-application>,
     stop-reason :: <stop-reason>)
 => ()
end method stop-reason-admin;

define sealed method stop-reason-admin 
    (type :: <stop-reason-action>,
     application :: <dfmc-application>,
     stop-reason :: <create-process-stop-reason>)
 => (stay-stopped? :: <boolean>)
  reset-application(application);
  if (application.profiling-enabled?)
    ensure-profiling-started(application)
  end;
  invoke-application-callback
    (application, application-process-created-callback)
end method stop-reason-admin;

define sealed method stop-reason-admin 
    (type :: <stop-reason-action>,
     application :: <dfmc-application>,
     stop-reason :: <load-library-stop-reason>)
 => (stay-stopped? :: <boolean>)
  let target = application.application-target-app;
  let library = stop-reason.stop-reason-library;
  if (library = target.application-dylan-library)
    application.application-loaded-dylan-library? := #t;
    maybe-initialize-cpu-profiling(application)
  end
end method stop-reason-admin;

define sealed method stop-reason-admin
    (type :: <stop-reason-action>,
     application :: <dfmc-application>,
     stop-reason :: <exit-process-stop-reason>)
 => (stay-stopped? :: <boolean>)
  let project = application.server-project;
  let exit-code = stop-reason.stop-reason-process-exit-code;
  note-application-process-finished(application, exit-code);
  application.application-pause-before-termination?
end method stop-reason-admin;

define sealed method stop-reason-admin
    (type :: <stop-reason-action>,
     application :: <dfmc-application>,
     stop-reason :: <RIP-stop-reason>)
 => (stay-stopped? :: <boolean>)
  let project = application.server-project;
  note-application-process-finished(application, #f);
  #f
end method stop-reason-admin;

define sealed method stop-reason-admin
    (type :: <stop-reason-action>,
     application :: <dfmc-application>,
     stop-reason :: <exit-thread-stop-reason>)
 => (stay-stopped? :: <boolean>)
  let thread :: <thread-object>
    = stop-reason-thread-object(application, stop-reason);
  invoke-application-callback
    (application, application-thread-finished-callback, thread)
end method stop-reason-admin;


/// STOP-REASON-TRANSACTION-ADMIN (internal)
///
/// This function performs some adminstrative work that ultimately requires
/// a lock on the target application object. This work cannot be done in
/// STOP-REASON-ADMIN since that is called by the DM with the target app
/// already locked.

define sealed method stop-reason-transaction-admin 
    (application :: <dfmc-application>,
     stop-reason :: <stop-reason>)
 => ()
end method stop-reason-transaction-admin;

define sealed method stop-reason-transaction-admin
    (application :: <dfmc-application>, 
     stop-reason :: <create-thread-stop-reason>) => ()
  note-application-threads-changed(application)
end method stop-reason-transaction-admin;

define sealed method stop-reason-transaction-admin
    (application :: <dfmc-application>, 
     stop-reason :: <exit-thread-stop-reason>) => ()
  note-application-threads-changed(application)
end method stop-reason-transaction-admin;

define sealed method stop-reason-transaction-admin
    (application :: <dfmc-application>, 
     stop-reason :: <create-process-stop-reason>) => ()
  note-application-threads-changed(application)
end method stop-reason-transaction-admin;

define sealed method stop-reason-transaction-admin
    (application :: <dfmc-application>, 
     stop-reason :: <exit-process-stop-reason>) => ()
  note-application-threads-changed(application)
end method stop-reason-transaction-admin;

/// NB sweeps away transient breakpoints as we encounter them.
define sealed method stop-reason-transaction-admin 
    (application :: <dfmc-application>,
     stop-reason :: <breakpoint-stop-reason>)
 => (stay-stopped? :: <boolean>)
  let thread :: <thread-object>
    = stop-reason-thread-object(application, stop-reason);

  if (application-just-interacted?(application, thread)
      & application.application-temporary-stop?)
    invoke-application-callback
      (application, application-just-interacted-callback, thread)
  end if;

  let breakpoints = current-stop-breakpoints(application, thread);

  // If we came across the special "initial" breakpoint, we need
  // to set the APPLICATION-REACHED-INTERACTION-POINT? property.
  if (any?(method (breakpoint)
	     instance?(breakpoint, <function-breakpoint-object>)
	       & breakpoint.breakpoint-entry-function?
	   end,
	   breakpoints))
    application.application-reached-interaction-point? := #t
  end if;

  //--- hughg, 1998/03/18: We used to try to do this as follows, but
  //--- it fails because the APPLICATION-INITIAL-BREAKPOINT is a
  //--- <GENERIC-FUNCTION-BREAKPOINT> but all members of BREAKPOINTS
  //--- are <METHOD-BREAKPOINT>s, and mapping between the two is
  //--- broken at this time.
  // if (member?(application.application-initial-breakpoint, breakpoints))
  //  application.application-reached-interaction-point? := #t
  // end if;

  for (breakpoint :: <breakpoint-object> in breakpoints)
    if (breakpoint.breakpoint-transient?)
      unless (instance?(breakpoint, <function-breakpoint-object>)
		& instance?(breakpoint-info(application, breakpoint), <breakpoint-entry-info>)
		& member?(#"out", breakpoint.breakpoint-directions))
	destroy-breakpoint(breakpoint)
      end unless;
    end if;
  end for;

  // Now is the time to re-establish function breakpoints.
  if (application.application-just-initialized?)
    let project = application.server-project;
    for (breakpoint :: <breakpoint-object> in project.environment-object-breakpoints)
      server-note-breakpoint-state-changed
         (application, breakpoint, #"destroyed");
      server-note-breakpoint-state-changed
         (application, breakpoint, #"created");
    end for;
    for (breakpoint :: <breakpoint-object> in project.source-location-breakpoints)
      server-note-breakpoint-state-changed
          (application, breakpoint, #"destroyed");
      server-note-breakpoint-state-changed
          (application, breakpoint, #"created");
    end for;
    if (application.application-startup-option ~== #"start")
      // If the application was started via the Debug or Interact
      // route, we must set a transient breakpoint upon the initializer
      // function, given that it exists.
      let entry-function = project.project-start-function;
      if (entry-function)
        application.application-initial-breakpoint
	  := make(<breakpoint-object>, 
		  project: project,
		  object: entry-function, entry-function?: #t,
		  directions: #[#"in"],
		  transient?: #t);
      end if;
    end if;

    // Tell the environment the application is about to "really" start.
    invoke-application-callback(application, application-initialized-callback)
  end if;

  // Any threads that were halfway through initializing should now be
  // fully initialized.
  note-application-threads-changed(application)
end method stop-reason-transaction-admin;


/// START-DEBUGGING-TRANSACTION (internal)
///
///

define function start-debugging-transaction
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => ()
  let just-finished? = application.application-just-finished-execution?;
  stop-reason-transaction-admin(application, stop-reason);
  if (just-finished?)
    invoke-application-callback
      (application, application-finished-execution-callback)
  end;
  case
    just-finished? & application.application-pause-before-termination? =>
      // Just stop without entering the debugger
      #f;
    application.application-temporary-stop? =>
      application.application-temporary-stop? := #f;
      continue-application-runtime
	(application,
	 instance?(stop-reason, <internal-stop-reason>)
	   & stop-reason.stop-reason-thread);
    otherwise =>
      application.application-stop-reason := stop-reason;
      let thread :: false-or(<thread-object>)
	= stop-reason-thread-object(application, stop-reason);
      let startup-opt :: false-or(<application-startup-option>)
	= stop-reason-startup-option(application, stop-reason);
      invoke-application-callback
	(application, application-debugging-callback,
	 thread, startup-opt);
  end
end function start-debugging-transaction;


/// FINISH-DEBUGGING-TRANSACTION (internal)

define function finish-debugging-transaction
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => ()
  application.application-stop-reason := #f;
  application.application-just-initialized? := #f;
  application.application-reached-interaction-point? := #f;
  application.application-just-finished-execution? := #f;
end function finish-debugging-transaction;


/// HANDLE-INTERACTOR-RESULTS (internal)

define function handle-interactor-results
    (application :: <dfmc-application>, thread :: <thread-object>, 
     transaction-id)
 => (stop? :: <boolean>)
  invoke-application-callback
    (application, application-interactive-results-callback,
     thread, transaction-id);
  #t;
end function handle-interactor-results;


/// HANDLE-LIBRARY-INITIALIZATION (internal)
///
/// Pause when starting to initialize the top library
/// if the user has requested it.

define function handle-library-initialization
    (application :: <dfmc-application>,
     thread :: <thread-object>,
     library :: <remote-library>,
     phase :: one-of(#"start", #"end"),
     top-level? :: <boolean>)
 => (stop? :: <boolean>)
  let project = application.server-project;
  let (dll-project?, dll-wrap?) = library-breakpoint-info(application, library);
  let really-top-level? = top-level? & ~dll-project?;
  let dll-under-dll-wrap? = dll-wrap? & dll-project?;
  let stop?
    = if (really-top-level? | dll-under-dll-wrap?)
	select (phase)
	  #"start" =>
	    if (really-top-level?)
	      application.application-just-initialized? := #t
	    end;
	  #"end" =>
	    if (profiling-enabled?(project))
	      stop-profiling-application(project);
	      application.pause-before-termination-flag := #t
	    end;
	    if (dll-under-dll-wrap? | ~application.application-initial-breakpoint)
	      application.application-reached-interaction-point? := #t;
	      if (application.application-startup-option ~== #"start")
		application.pause-before-termination-flag := #f
	      end
	    end;
	    application.application-just-finished-execution? := #t;
	end;
      end;
  /*
  debugger-message("Stopping? %=: top? %= dll? %=, dll-wrap? %=, phase %=",
		   stop?, top-level?, dll-project?, dll-wrap?, phase);
  */
  stop?
end function handle-library-initialization;


define function library-breakpoint-info
    (application :: <dfmc-application>, library :: <remote-library>)
 => (dll-project? :: <boolean>, dll-wrap? :: <boolean>)

  // For purposes of debugging top-level libraries, some
  // libraries are considered self-contained (for example OLE
  // components); the debugger will treat these like real
  // executables as far as initialization is concerned.
  // The debugger manager will have set up a suitable post
  // initialization entry-point for these libraries which
  // will be invoked by some external event

  values(if (library.self-contained-component?) #f
	 else
	   application.server-project.env/project-target-type == #"dll"
	 end,
	 as-lowercase(library.library-core-name) = "dll-wrap")
end function library-breakpoint-info;


/// STOP-REASON-THREAD-OBJECT (internal)

define generic stop-reason-thread-object
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => (thread :: false-or(<thread-object>));

define sealed method stop-reason-thread-object 
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => (thread :: singleton(#f))
  #f
end method stop-reason-thread-object;

define sealed method stop-reason-thread-object 
    (application :: <dfmc-application>, stop-reason :: <internal-stop-reason>)
 => (thread :: <thread-object>)
  remote-thread-thread-object(application, stop-reason.stop-reason-thread)
end method stop-reason-thread-object;

define sealed method stop-reason-thread-object 
    (application :: <dfmc-application>,
     stop-reason :: <debugger-generated-stop-reason>)
 => (thread :: false-or(<thread-object>))
  let client-data = stop-reason-client-data(stop-reason);
  let (thread, startup-option) =
    if (client-data)
      values(client-data.head, client-data.tail)
    else
      values(#f, #f)
    end if;
  thread
end method stop-reason-thread-object;

/// STOP-REASON-STARTUP-OPTION (Internal)

define sealed method stop-reason-startup-option 
    (application :: <dfmc-application>, sr :: false-or(<stop-reason>))
 => (sopt :: false-or(<application-startup-option>))
  #f
end method stop-reason-startup-option;

define sealed method stop-reason-startup-option 
    (application :: <dfmc-application>, sr :: <debugger-generated-stop-reason>)
 => (sopt :: false-or(<application-startup-option>))
  let client-data = stop-reason-client-data(sr);
  let (thread, startup-option) =
    if (client-data)
      values(client-data.head, client-data.tail)
    else
      values(#f, #f)
    end if;
  startup-option
end method stop-reason-startup-option;

/// REMOTE-THREAD-THREAD-OBJECT (internal)

define generic remote-thread-thread-object
    (application :: <dfmc-application>, remote-thread :: false-or(<remote-thread>))
 => (thread :: false-or(<thread-object>));

define sealed method remote-thread-thread-object 
    (application :: <dfmc-application>, remote-thread == #f)
 => (thread == #f)
  #f
end method remote-thread-thread-object;

define sealed method remote-thread-thread-object 
    (application :: <dfmc-application>, remote-thread :: <remote-thread>)
 => (thread :: <thread-object>)
  make-environment-object(<thread-object>, 
			  project: application.server-project, 
			  application-object-proxy: remote-thread)
end method remote-thread-thread-object;


/// REGISTER-DEBUGGER-MANAGER-CALLBACKS (internal)

define function register-debugger-manager-callbacks
    (application :: <dfmc-application>) => ()
  register-stop-reason-callback(application, handle-stop-reason);
  register-debugger-transaction-prolog(application, start-debugging-transaction);
  register-debugger-transaction-epilog(application, finish-debugging-transaction);
  register-interactor-return-callback(application, handle-interactor-results);
  register-library-initialization-callback
    (application, handle-library-initialization);
end function register-debugger-manager-callbacks;
