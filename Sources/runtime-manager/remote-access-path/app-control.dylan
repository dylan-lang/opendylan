module:     remote-access-path
synopsis:   Functions for debugee control
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// RESTART

define method restart-application (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/application-restart (conn.nub)
end method;

///// KILL-APPLICATION


define method kill-app-on-connection (conn :: <remote-access-connection>)
    => (success-code :: <integer>)
  Rtmgr/RemoteNub/kill-application (conn.nub)
end method;


define method register-exit-process-function-on-connection
    (conn :: <remote-access-connection>, exit-process :: <remote-symbol>)
    => ()
  Rtmgr/RemoteNub/register-exit-process-function
    (conn.nub, as-integer(exit-process.remote-symbol-address));
end method;


///// CLOSE-APPLICATION

define method close-application-on-connection
   (conn :: <remote-access-connection>) => ()
  Rtmgr/NubServer/DestroyNub(conn.connection-server, conn.nub);
end method;


///// STOP

define method stop-application 
    (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/application-stop(conn.nub)
end method;


///// CONTINUE

define method continue-application 
    (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/application-continue (conn.nub)
end method;

///// CONTINUE-UNHANDLED

define method unhandled-continue-application 
    (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/application-continue-unhandled (conn.nub)
end method;


///// SUSPEND-THREAD

define method suspend-application-thread 
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
  Rtmgr/RemoteNub/thread-stop(conn.nub, thread.rnub-descriptor);
end method;


///// RESUME-THREAD

define method resume-application-thread 
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
  Rtmgr/RemoteNub/thread-continue(conn.nub, thread.rnub-descriptor);
end method;


// Managing permanently debugger-suspended threads in an application.

define method application-thread-permanently-suspended?
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
 => (suspended? :: <boolean>)
  let suspended? = Rtmgr/RemoteNub/thread-suspendedQ(conn.nub, thread.rnub-descriptor);
  if (suspended? = 0) #f
  else #t
  end;
end method;

define method application-thread-permanently-suspended?-setter
    (suspend? :: <boolean>, conn :: <remote-access-connection>,
     thread :: <remote-thread>)
 => (suspend? :: <boolean>)
  let suspend? =
  if (suspend?)
    Rtmgr/RemoteNub/thread-suspended(conn.nub, thread.rnub-descriptor);
  else
    Rtmgr/RemoteNub/thread-resumed(conn.nub, thread.rnub-descriptor);
  end;
  suspend?
end method;



///// STEP

define method step-application 
    (conn :: <remote-access-connection>, n :: <integer>) => ()
  Rtmgr/RemoteNub/application-step(conn.nub, n)
end method;


///// STEP-OVER

define method step-over-application 
    (conn :: <remote-access-connection>, n :: <integer>) => ()
  Rtmgr/RemoteNub/application-step-over(conn.nub, n)
end method;


///// STEP-OUT

define method step-out-application
    (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/application-step-out(conn.nub)
end method;
