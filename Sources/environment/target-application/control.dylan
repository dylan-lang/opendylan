module:        target-application-internals
synopsis:      Stopping and starting the application
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// STOP-TARGET-APPLICATION
//    This can be hooked up to a STOP button.
//    Asynchronously pausing running applications

define method stop-target-application 
    (application :: <target-application>, #key client-data = #f)
    => ()
  thread-debug-message("Stopping target application");
  let stop-reason = 
    make(<debugger-stop-application-stop-reason>,
	 client-data: client-data);
  stop-application(application, stop-reason: stop-reason);
end method;

// Asynchronously pausing running applications to do
// internal functions   

define method stop-application-request
    (application :: <target-application>) => ()
  thread-debug-message("Stopping target application temporarily");
  let stop-reason :: <stop-reason> =
    make(<temporary-internal-debugger-transaction-stop>);
  stop-application
    (application, stop-reason: stop-reason);
end method;

// Indicate that a debugger transaction request has been served

define method discard-stop-application-request
    (application :: <target-application>) => ()
  application.application-stopped? := #f;
end method;

// Determines if stopped to execute an internal debugger function

define inline function application-temporary-stop?
    (application :: <target-application>) => (temporary-stop :: <boolean>)
  instance?(application.current-stop-reason,
	    <temporary-internal-debugger-transaction-stop>);
end function;


///// CONTINUE-TARGET-APPLICATION
//    This can be hooked up to a CONTINUE button
//    It is a requirement that this continues the current
//    application; clients will await confirmation of this.

define method continue-target-application
    (application :: <target-application>, remote-thread)
    => ()
  debugger-message("Selected Thread is %=", remote-thread);
  application.application-selected-thread := remote-thread;

  if (current-thread() == application.manager-thread)
    application-continuation-pending(application);
  else
    thread-debug-message("Continuing target application");
    with-lock(application.debugger-transaction)
      thread-debug-message("Releasing debugger-transaction-notification");
      release(application.debugger-transaction-notification);
      thread-debug-message("Waiting for debugger-transaction-complete");
      wait-for(application.debugger-transaction-complete);
    end with-lock;

  end;
end method;

define method application-continuation-pending
    (application :: <target-application>) => ()
  application.debugger-transaction-timeout := 0.2;
end method;


///// TARGET-APPLICATION-STATE
//    Returns a <symbol> describing the state of the application. These
//    symbols are identical to those used by the environment protocols.

define method target-application-state (application :: <target-application>)
     => (state :: <symbol>)
  let state =
  case
    application.in-debugger-transaction? => #"stopped";
    application.under-management? => #"running";
    application.been-managed? => #"closed";
    otherwise => #"uninitialized";
  end case;
  thread-debug-message("target-application-state is %=", state);
  state
end method;


///// KILL-TARGET-APPLICATION
 
define method kill-target-application(application :: <target-application>)
  dm-kill-application(application);
end method;
