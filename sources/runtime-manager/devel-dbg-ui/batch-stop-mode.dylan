module:      devel-dbg-ui
synopsis:    The batch "ready to continue" mode for the console debugger
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// REGISTER-STOPPED-THREAD
//    A generic function with methods for <internal-stop-reason> and
//    <external-stop-reason>. Decides which of the application's threads
//    to select as the "current" thread. The user can easily change this
//    selection via the 'in thread' command.

define method register-stopped-thread
    (application :: <application>, stop-reason :: <internal-stop-reason>)

  // For an internal stop-reason, the "stopped thread" is the thread
  // that signalled the stop-reason.

  let index = 1;
  let found = #f;
  application.stopped-thread := 
    stop-reason.stop-reason-thread;

  while ((index <= size (application.application-thread-table)) &
         (~found))
    if (application.application-thread-table[index - 1] ==
        application.stopped-thread)
      found := #t;
    else
      index := index + 1;
    end if
  end while;
  application.selected-thread := index;
end method;

define method register-stopped-thread
    (application :: <application>, stop-reason :: <external-stop-reason>)

  // For an external stop-reason, the debugger stopped the application,
  // freezing all threads together. No thread in particular caused the
  // stop, so we just select the first created thread (the primary).

  application.stopped-thread :=
    application.application-thread-table[0];
  application.selected-thread := 1;
end method;


///// ATTEMPT-TO-RECOVER
//    If the error was a <dylan-invoke-debugger-stop-reason>, then attempt to
//    continue via the most recently installed restart handler.

define method attempt-to-recover () 
     => (succeeded? :: <boolean>, why? :: <string>)
  let sr = *open-application*.most-recent-interesting-stop-reason;
  if (instance?(sr, <dylan-invoke-debugger-stop-reason>))
    let thread = sr.stop-reason-thread;
    let restarts = available-restarts-for-thread(*open-application*, thread);
    if (restarts.size > 0)
      signal-restart-on-thread
        (*open-application*, thread, restarts[0]);
      values(#t, restarts[0].remote-restart-description);
    else
      values(#f, "No restart handlers available for recovery.");
    end if;
  else
    values(#f, "Error was not a dylan error.");
  end if;
end method;


///// BUILD-TOP-OF-STACK
//    Gets ready for displaying backtraces.

define method build-top-of-stack 
    (application :: <application>, thread :: <remote-thread>)

  // If we are stopping "for real", that is, we are entering a
  // debugger transaction, we want to get ready for proper symbolic
  // stack traces.

  let frame = first-stack-frame (application, thread);
  let index = 1;
  application.current-stack := make (<stretchy-vector>, size: 0);
  application.current-stack := 
     add! (application.current-stack, frame);
  application.current-frame-index := index;

  // We might build more than just the top frame. We want to get
  // as far as a <call-frame>

  while (~instance? (frame, <call-frame>))
    frame := previous-stack-frame (application, frame);
    index := index + 1;
    application.current-stack :=
    add! (application.current-stack, frame);
  end while;
end method;


///// PROCESS-STOP-MODE
//    The ready-to-continue callback.
//    An activation of this function represents a debugger transaction.

define method process-stop-mode
    (application :: <application>, stop-reason :: <stop-reason>)
  format-out("\n\n");
  register-stopped-thread (application, stop-reason);
  build-top-of-stack (application,
                      application.stopped-thread);
  application.still-in-stop-mode? := #t;
  ignore(application.language-mode);
  if (*program-failed?*)
    spew-bug-report();

    // Process and print profiler results where approriate.

    if (*open-application*.application-profiler-run)
      stop-profiling(*open-application*);
      process-profile-data(*open-application*);
      format-out("\n");
      print-profile-results(*open-application*);
      *open-application*.application-profiler-run := #f;
      format-out("\n");
    end if;

    // Execute the extra commands.

    for (extra-command in *current-debugger-options*.post-command-sequence)
      debugger-message("Command> %s\n", extra-command);
      set-parse-sequence(tokenize(extra-command));
      execute-debugger-command(build-debugger-command());
      format-out("\n");
    end for;

    // If we are trying to recover from errors, then do that now.
    if (*current-debugger-options*.attempt-recovery?)
      let (can-recover?, why?) = attempt-to-recover();
      if (can-recover?)
        *program-failed?* := #f;
        debugger-message("Able to continue via restart: %s\n", why?);
      else
        debugger-message("Could not recover: %s\n", why?);
        kill-application(*open-application*);
      end if;
    else
      kill-application(*open-application*);
    end if;
  else
    while ((application.still-in-stop-mode?) &
           (*current-debugger-options*.pre-command-sequence-index <
               *current-debugger-options*.pre-command-sequence.size))
      let command-string =
        *current-debugger-options*.pre-command-sequence
          [*current-debugger-options*.pre-command-sequence-index];
      set-parse-sequence(tokenize(command-string));
      execute-debugger-command(build-debugger-command());
      *current-debugger-options*.pre-command-sequence-index :=
         *current-debugger-options*.pre-command-sequence-index + 1;
    end while;
  end if;
end method;

