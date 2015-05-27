module:     access-path-implementation
synopsis:   Functions for debugee control
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// RESTART
//    This operation is always legal.

define method restart-application (conn :: <local-access-connection>) => ()
  nub-application-restart (conn.connection-process)
end method;

///// KILL-APPLICATION

define method kill-app-on-connection (conn :: <local-access-connection>)
    => (success-code :: <integer>)
  nub-kill-application (conn.connection-process);
end method;

define method register-exit-process-function-on-connection
    (conn :: <local-access-connection>, exit-process :: <remote-symbol>)
    => ()
  nub-register-exit-process-function
    (conn.connection-process, exit-process.remote-symbol-address);
end method;


///// CLOSE-APPLICATION
//    While kill-application terminates the process, this gives the nub implementation
//    the chance to release all OS resources associated with the tether.

define method close-application-on-connection
   (conn :: <local-access-connection>) => ()
  nub-close-application(conn.connection-process)
end method;

///// STOP
//    This operation is only legal if the application is running. 
//    This performs an immediate "stamp on the brakes".

define method stop-application 
    (conn :: <local-access-connection>) => ()
  nub-application-stop(conn.connection-process)
end method;


///// CONTINUE

//  Now takes a resume keyword, which is a set of previously
//  suspended threads to explicitly resume

define method continue-application
    (conn :: <local-access-connection>) => ()
  nub-application-continue (conn.connection-process)
end method;

///// CONTINUE-UNHANDLED

//  Now takes a resume keyword, which is a set of previously
//  suspended threads to explicitly resume

define method unhandled-continue-application 
    (conn :: <local-access-connection>) => ()
  nub-application-continue-unhandled (conn.connection-process)
end method;


///// SUSPEND-THREAD

define method suspend-application-thread 
    (conn :: <local-access-connection>, thread :: <remote-thread>)
  nub-thread-stop(conn.connection-process, thread.nub-descriptor);
end method;


///// RESUME-THREAD

define method resume-application-thread 
    (conn :: <local-access-connection>, thread :: <remote-thread>)
  nub-thread-continue (conn.connection-process, thread.nub-descriptor);
end method;


// Managing permanently debugger-suspended threads in an application.
// Query slots in the debugger NUB.

define method application-thread-permanently-suspended?
    (conn :: <local-access-connection>, thread :: <remote-thread>)
 => (suspended? :: <boolean>)
  nub-thread-suspended?(thread.nub-descriptor);
end method;

define method application-thread-permanently-suspended?-setter
    (suspend? :: <boolean>, conn :: <local-access-connection>,
     thread :: <remote-thread>)
 => (suspend? :: <boolean>)
  if (suspend?)
    nub-thread-suspended(thread.nub-descriptor);
  else
    nub-thread-resumed(thread.nub-descriptor);
  end;
  suspend?
end method;



///// STEP

define method step-application 
    (conn :: <local-access-connection>, n :: <integer>) => ()
  nub-application-step(conn.connection-process, n)
end method;


///// STEP-OVER

define method step-over-application 
    (conn :: <local-access-connection>, n :: <integer>) => ()
  nub-application-step-over(conn.connection-process, n)
end method;


///// STEP-OUT

define method step-out-application
    (conn :: <local-access-connection>) => ()
  nub-application-step-out(conn.connection-process)
end method;
