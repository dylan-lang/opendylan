module:     access-path-implementation
synopsis:   Implementation of stop reasons
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// WAIT-FOR-STOP-REASON-WITH-TIMEOUT
//    Called if the timeout keyword is supplied.

define method wait-for-stop-reason-with-timeout 
    (conn :: <local-access-connection>, timeout :: <integer>,
     #key profile-at = #f)
  => (code :: <integer>)
  let code :: <integer> =
    if (profile-at) 
      nub-profile-wait-for-stop-reason-with-timeout 
        (conn.connection-process, timeout, profile-at);
    else
      nub-wait-for-stop-reason-with-timeout
        (conn.connection-process, timeout);
    end if;
  code;
end method;


///// WAIT-FOR-STOP-REASON-NO-TIMEOUT
//    Called if no timeout keyword is supplied.

define method wait-for-stop-reason-no-timeout 
    (conn :: <local-access-connection>,
     #key profile-at = #f) => (code :: <integer>)

  let code :: <integer> =
    if (profile-at)
      nub-profile-wait-for-stop-reason-no-timeout (conn.connection-process, profile-at);
    else
      nub-wait-for-stop-reason-no-timeout (conn.connection-process);
    end if;

  code;
end method;


///// GET-DEBUG-EVENT-PROCESS-EXIT-CODE
//    Given that the last received stop reason was an
//    <exit-process-stop-reason>, this returns the exit code.
       
define method get-debug-event-process-exit-code
    (conn :: <local-access-connection>) => (code :: <integer>)
  let raw-code =
    nub-stop-reason-process-exit-code (conn.connection-process);
  if (instance?(raw-code, <integer>))
    raw-code
  else
    0
  end if;
end method;


///// GET-DEBUG-EVENT-THREAD-EXIT-CODE
//    Given that the last received stop reason was an
//    <exit-thread-stop-reason>, this returns the exit code.

define method get-debug-event-thread-exit-code
    (conn :: <local-access-connection>) => (code :: <integer>)
  let raw-code =
    nub-stop-reason-thread-exit-code (conn.connection-process);
  if (instance?(raw-code, <integer>))
    raw-code
  else
    0
  end if
end method;


///// GET-DEBUG-EVENT-STRING-INFORMATION
//    Given that the last received stop reason was an
//    <output-debug-string-stop-reason>, grab the address
//    and length of the string from the debugger nub.

define method get-debug-event-string-information
    (conn :: <local-access-connection>)
       => (addr :: <remote-value>,
           sz :: <integer>,
           unicode? :: <boolean>)
  let string-addr
    = nub-stop-reason-debug-string-address(conn.connection-process);
  let string-len
    = nub-stop-reason-debug-string-length(conn.connection-process);
  let unicode-answer
    = nub-stop-reason-debug-string-is-unicode(conn.connection-process);

  unless(instance?(string-len, <integer>))
    string-len := 0
  end unless;

  values (string-addr,
          string-len,
          unicode-answer == 1);
end method;


///// GET-DEBUG-EVENT-LIBRARY
//    Given that the last received stop reason was one that has to
//    do with a remote-library, this returns a handle on the affected
//    library.

define method get-debug-event-library
    (conn :: <local-access-connection>) => (lib :: <NUBLIBRARY>)
  nub-stop-reason-library (conn.connection-process);
end method;


///// GET-DEBUG-EVENT-THREAD
//    All stop reasons are associated with the thread that generated
//    them. This function returns a handle on that thread.

define method get-debug-event-thread 
    (conn :: <local-access-connection>)=> (thr :: <NUBTHREAD>)
  nub-stop-reason-thread (conn.connection-process);
end method;


///// GET-DEBUG-EVENT-PROCESS
//    This function is currently pointless, since there is only one
//    <remote-process>.

define method get-debug-event-process 
    (conn :: <local-access-connection>) => (proc :: <remote-process>)
  let nub-process = nub-stop-reason-process (conn.connection-process);
  let process = make (<remote-process>,
                      nub-descriptor: nub-process);
  process;
end method;


///// GET-EXCEPTION-ADDRESS
//    Returns the address at which the last exception was encountered.
//    We only currently make use of this for finding out where a
//    breakpoint was hit. We could perhaps make more use of it...

define method get-exception-address (conn :: <local-access-connection>)
    => (ptr :: <remote-value>)
  nub-stop-reason-exception-address (conn.connection-process);
end method;


///// EXCEPTION-IS-FIRST-CHANCE?
//    Decides whether an exception is a first-chance exception

define method exception-is-first-chance? (conn :: <local-access-connection>)
    => (answer :: <boolean>)
  let int-answer = nub-exception-first-chance (conn.connection-process);
  int-answer == 1
end method;


///// GET-EXCEPTION-VIOLATION-ADDRESS
//    Returns the address that the application was trying to access when
//    an access violation occurred.

define method get-exception-violation-address 
  (conn :: <local-access-connection>)
    => (ptr :: <remote-value>)
  nub-stop-reason-violation-address (conn.connection-process);
end method;


///// GET-EXCEPTION-VIOLATION-OP
//    Returns a code for the operation that the application was trying
//    to perform when an access violation occurred.

define method get-exception-violation-op (conn :: <local-access-connection>)
    => (op :: <integer>)
  nub-stop-reason-violation-op (conn.connection-process);
end method;


///// FIRST-DEBUGGER-INVOCATION?
//    Is this the first time that the application has signalled a hard-coded
//    breakpoint?
//    If so, we will assume this breakpoint to be "special", and that it
//    is a signal that the application, and all of its shared libraries,
//    have loaded and performed their system-level initializations. On
//    Windows, this is true for free. Debugger nubs on other platforms,
//    however, might need to force the generation of this stop reason.

define method first-debugger-invocation?
    (conn :: <local-access-connection>) => (well? :: <boolean>)
  let int-answer = nub-first-hard-coded-breakpoint(conn.connection-process);
  int-answer == 1
end method;


///// CONNECTION-CAN-RECEIVE-FIRST-CHANCE
//    Calls the nub to determine whether an exception is first-chance
//    receivable.

define method connection-can-receive-first-chance
   (conn :: <local-access-connection>, code :: <integer>)
     => (yes-or-no :: <boolean>)
  let answer
    = nub-can-receive-first-chance(conn.connection-process, code);
  if (answer == 1)
    #t
  else
    #f
  end if
end method;


///// RECEIVING-FIRST-CHANCE?-SETTER
//    Filters (or unfilters) first-chance occurrences of the given exception
//    type.

define method connection-set-first-chance
   (conn :: <local-access-connection>, code :: <integer>) => ()
  nub-set-first-chance(conn.connection-process, code);
end method;

define method connection-unset-first-chance
   (conn :: <local-access-connection>, code :: <integer>) => ()
  nub-unset-first-chance(conn.connection-process, code);
end method;


///// FIRST-CHANCE-EXCEPTION?
//    Is a thread stopped at a first-chance exception or not?

define method connection-thread-stopped-at-first-chance?
    (conn :: <local-access-connection>, thread :: <remote-thread>)
      => (b :: <boolean>)
  let (code, fchance)
    = nub-thread-stop-information(conn.connection-process, thread.nub-descriptor);
  if (fchance == 1)
    #t
  else
    #f
  end if
end method;
