module:      devel-dbg-ui
synopsis:    The "ready to continue" mode for the console debugger
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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

  // Set up a no-op interrupt handler until the real one comes along
  let handler (<keyboard-interrupt>) =
    method(condition :: <keyboard-interrupt>, next-handler)
    end method;

  // Activate auto-polling of keyboard-interrupts
  keyboard-interrupt-polling-thread?() := #t;

  let did-something = #t;

  // This is our debugger transaction.
  // Do some initial processing, and then just read and execute
  // debugger commands from the console. Keep doing this until
  // the command processor says that we have ended the debugger
  // transaction.

  register-stopped-thread (application, stop-reason);
  build-top-of-stack (application,
                      application.stopped-thread);
  application.still-in-stop-mode? := #t;
  debugger-message ("Application stopped in thread %s.",
                     application.stopped-thread.thread-name);

  while (application.still-in-stop-mode?)
    let thr-index = *open-application*.selected-thread - 1;
    let thr = *open-application*.application-thread-table[thr-index];
    let thrname = thr.thread-name;
    let modename =
      if (*open-application*.language-mode == $language-mode-C)
        "C"
      else
        "Dylan"
      end if;

    if (did-something)
      debugger-message 
        ("%s (%s:%s)",
         *opening-command*.application-filename,
         *open-application*.default-name-context.context-module,
         *open-application*.default-name-context.context-library);
    else
      did-something := #t
    end if;
    let (command, empty?, str) = get-next-debugger-command();
    if (command)
      block()
        execute-debugger-command (command);
        ignore(*current-debugger-options*.attempt-recovery?);
      exception (illegal-access :: <remote-access-violation-error>)
        debugger-message
          ("*** ERROR: Tried to read illegal runtime memory.");
        debugger-message
          ("           Aborted the execution of this command.");
      exception (bp-error :: <debug-point-error>)
        debugger-message
          ("*** ERROR: Failed to set or clear a breakpoint.");
        debugger-message
          ("           Aborted the execution of this command.");
//    exception (pants :: <error>)
//      debugger-message
//        ("*** ERROR: Internal implementation error.");
//      debugger-message
//        ("           Paul needs to know about this!!!!");
//      debugger-message
//        ("           Aborted the execution of this command.");
      exception(condition :: <keyboard-interrupt>)
	debugger-message("%s", condition);
        debugger-message
          ("           Aborted the execution of this command.");
      end block;
      format-out ("\n");
    elseif (empty?)
      did-something := #f;
    else
      debugger-message("Illegally formed command.\n");
    end if;   
  end while;

  // We're moving again. Don't remember anything about the stack.

  *open-application*.current-stack := 
     make (<stretchy-vector>, size: 0);
  *open-application*.current-frame-index := 0;


  enable-stop-thread();
  keyboard-interrupt-polling-thread?() := #f;

end method;

