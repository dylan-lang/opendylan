module:     remote-access-path
synopsis:   Implementation of stop reasons
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// WAIT-FOR-STOP-REASON-WITH-TIMEOUT

define method wait-for-stop-reason-with-timeout 
    (conn :: <remote-access-connection>, timeout :: <integer>,
     #key profile-at = #f)
  => (code :: <integer>)
  let code :: <integer> =
    if (profile-at) 
      Rtmgr/RemoteNub/profile-wait-for-stop-reason-with-timeout 
        (conn.nub, timeout, profile-at);
    else
      Rtmgr/RemoteNub/wait-for-stop-reason-with-timeout
        (conn.nub, timeout);
    end if;
  code;
end method;


///// WAIT-FOR-STOP-REASON-NO-TIMEOUT
//    Called if no timeout keyword is supplied.

define method wait-for-stop-reason-no-timeout 
    (conn :: <remote-access-connection>,
     #key profile-at = #f) => (code :: <integer>)
  let code :: <integer> =
    if (profile-at)
      Rtmgr/RemoteNub/profile-wait-for-stop-reason-no-timeout 
        (conn.nub, profile-at);
    else
      Rtmgr/RemoteNub/wait-for-stop-reason-no-timeout(conn.nub);
    end if;
  code;
end method;


///// GET-DEBUG-EVENT-PROCESS-EXIT-CODE

define method get-debug-event-process-exit-code
    (conn :: <remote-access-connection>) => (code :: <integer>)
  let raw-code =
    Rtmgr/RemoteNub/stop-reason-process-exit-code(conn.nub);
  if (instance?(raw-code, <integer>))
    raw-code
  else
    0
  end if;
end method;


///// GET-DEBUG-EVENT-THREAD-EXIT-CODE

define method get-debug-event-thread-exit-code
    (conn :: <remote-access-connection>) => (code :: <integer>)
  let raw-code =
    Rtmgr/RemoteNub/stop-reason-thread-exit-code(conn.nub);
  if (instance?(raw-code, <integer>))
    raw-code
  else
    0
  end if
end method;


///// GET-DEBUG-EVENT-STRING-INFORMATION

define method get-debug-event-string-information
    (conn :: <remote-access-connection>)
       => (addr :: <remote-value>,
           sz :: <integer>,
           unicode? :: <boolean>)
  let string-addr
    = Rtmgr/RemoteNub/stop-reason-debug-string-address(conn.nub);
  let string-len
    = Rtmgr/RemoteNub/stop-reason-debug-string-length(conn.nub);
  let unicode-answer
    = Rtmgr/RemoteNub/stop-reason-debug-string-is-unicode(conn.nub);
  unless(instance?(string-len, <integer>))
    string-len := 0
  end unless;
  values (as-remote-value(string-addr),
          string-len,
          unicode-answer == 1);
end method;


///// GET-DEBUG-EVENT-LIBRARY

define method get-debug-event-library
    (conn :: <remote-access-connection>) => (lib :: <NUBLIBRARY>)
  as-remote-pointer(Rtmgr/RemoteNub/stop-reason-library (conn.nub));
end method;


///// GET-DEBUG-EVENT-THREAD

define method get-debug-event-thread 
    (conn :: <remote-access-connection>)=> (thr :: <NUBTHREAD>)
  as-remote-pointer(Rtmgr/RemoteNub/stop-reason-thread(conn.nub));
end method;


///// GET-DEBUG-EVENT-PROCESS

define method get-debug-event-process 
    (conn :: <remote-access-connection>) => (proc :: <remote-process>)
  let nub-process :: <RNUBHANDLE> = Rtmgr/RemoteNub/stop-reason-process (conn.nub);
  let process = make (<remote-process>,
                      nub-descriptor: as-remote-pointer(nub-process));
  process;
end method;


///// GET-EXCEPTION-ADDRESS

define method get-exception-address (conn :: <remote-access-connection>)
    => (ptr :: <remote-value>)
  as-remote-value(Rtmgr/RemoteNub/stop-reason-exception-address (conn.nub));
end method;


///// EXCEPTION-IS-FIRST-CHANCE?

define method exception-is-first-chance? (conn :: <remote-access-connection>)
    => (answer :: <boolean>)
  let int-answer = Rtmgr/RemoteNub/exception-first-chance (conn.nub);
  int-answer == 1
end method;


///// GET-EXCEPTION-VIOLATION-ADDRESS

define method get-exception-violation-address 
  (conn :: <remote-access-connection>)
    => (ptr :: <remote-value>)
  as-remote-value(Rtmgr/RemoteNub/stop-reason-violation-address (conn.nub));
end method;


///// GET-EXCEPTION-VIOLATION-OP

define method get-exception-violation-op (conn :: <remote-access-connection>)
    => (op :: <integer>)
  Rtmgr/RemoteNub/stop-reason-violation-op (conn.nub);
end method;


///// FIRST-DEBUGGER-INVOCATION?

define method first-debugger-invocation?
    (conn :: <remote-access-connection>) => (well? :: <boolean>)
  let int-answer = Rtmgr/RemoteNub/first-hard-coded-breakpoint(conn.nub);
  int-answer == 1
end method;


///// CONNECTION-CAN-RECEIVE-FIRST-CHANCE

define method connection-can-receive-first-chance
   (conn :: <remote-access-connection>, code :: <integer>)
     => (yes-or-no :: <boolean>)
  let answer
    = Rtmgr/RemoteNub/can-receive-first-chance(conn.nub, code);
  if (answer == 1)
    #t
  else
    #f
  end if
end method;


///// RECEIVING-FIRST-CHANCE?-SETTER

define method connection-set-first-chance
   (conn :: <remote-access-connection>, code :: <integer>) => ()
  Rtmgr/RemoteNub/set-first-chance(conn.nub, code);
end method;

define method connection-unset-first-chance
   (conn :: <remote-access-connection>, code :: <integer>) => ()
  Rtmgr/RemoteNub/unset-first-chance(conn.nub, code);
end method;


///// FIRST-CHANCE-EXCEPTION?

define method connection-thread-stopped-at-first-chance?
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
      => (b :: <boolean>)
  let (code, fchance)
    = Rtmgr/RemoteNub/thread-stop-information(conn.nub, thread.rnub-descriptor);
  if (fchance == 1)
    #t
  else
    #f
  end if
end method;


