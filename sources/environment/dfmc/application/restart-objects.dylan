Module:    dfmc-application
Synopsis:  Serving the restart protocols from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Note
// The application proxy for <restart-object> is a <remote-restart>, defined
// in the DM library.
// A <remote-restart> is only valid during a debugger transaction.


///// APPLICATION-THREAD-RESTARTS (Environment Protocol Method)
//    Returns a sequence of <restart-object> descriptors for the restarts that
//    can be signalled on a thread.

define method application-thread-restarts
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (restarts :: <sequence>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let project = application.server-project;
    let remote-thread = thread.application-object-proxy;
    let remote-restarts 
      = available-restarts-for-thread(target, remote-thread);
    map(method (remote-restart)
	  make-environment-object(<restart-object>,
				  project: project,
				  application-object-proxy: remote-restart)
	end,
	remote-restarts)
  end
end method application-thread-restarts;


///// APPLICATION-RESTART-MESSAGE (Environment Protocol Method)
//    Builds a string to describe a restart operation.

define method application-restart-message
    (application :: <dfmc-application>, rst :: <restart-object>)
 => (description :: <string>)
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let remote-restart = rst.application-object-proxy;
    remote-restart-description(remote-restart)
  end
end method application-restart-message;


///// INVOKE-APPLICATION-RESTART (Environment Protocol Method)
//    Signals a restart operation on a specific thread, and allows the
//    target application to continue.

define method invoke-application-restart
    (application :: <dfmc-application>, thread :: <thread-object>,
     rst :: <restart-object>)
 => ()

  // Get all the DM-type objects out of the higher-level stuff.

  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;
  let remote-restart = rst.application-object-proxy;

  // We must be in a debugger transaction to do this.

  perform-continuing-debugger-transaction
     (application, remote-thread,
      method ()
        // Get the DM to prepare for the restart
        signal-restart-on-thread(target, remote-thread, remote-restart);
      end method);
end method invoke-application-restart;


///// APPLICATION-RESTART-ABORT? (Environment Protocol Method)
//    Enquires whether a restart is an abort restart.

define method application-restart-abort?
    (application :: <dfmc-application>, rst :: <restart-object>)
 => (answer :: <boolean>)
  rst.application-object-proxy.remote-restart-abort?
end method application-restart-abort?;

