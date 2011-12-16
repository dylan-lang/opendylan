module:      access-path-implementation
synopsis:    Implementation of debug points.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// Constant codes used by the debugger nub.

define constant $ok                            = 7;
define constant $exists                        = 1;

///// EXPORTED GENERIC FUNCTIONS


define generic enable-breakpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic disable-breakpoint 
    (ap :: <access-path>, address :: <remote-value>) 
      => (success :: <boolean>);

define generic query-breakpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic enable-read-watchpoint 
    (ap :: <access-path>, address :: <remote-value>, size :: <integer>)
      => (success :: <boolean>);

define generic disable-read-watchpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic query-read-watchpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic enable-write-watchpoint 
    (ap :: <access-path>, address :: <remote-value>, size :: <integer>)
      => (success :: <boolean>);

define generic disable-write-watchpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic query-write-watchpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic enable-execute-watchpoint 
    (ap :: <access-path>, address :: <remote-value>, size :: <integer>)
      => (success :: <boolean>);

define generic disable-execute-watchpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);

define generic query-execute-watchpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>);


///// ENABLE-BREAKPOINT

define method enable-breakpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)

  let success-code :: <integer> =
    set-breakpoint-in-application (ap.connection, address);

  // Attempt to map the success code to a legal success boolean.
  (success-code == $ok);
end method;

define open generic set-breakpoint-in-application
    (conn :: <access-connection>, ra :: <remote-value>) 
 => (success :: <integer>);

define method set-breakpoint-in-application
    (conn :: <local-access-connection>, ra :: <remote-value>) 
       => (success :: <integer>)
  nub-set-breakpoint (conn.connection-process, ra);
end method;


///// DISABLE-BREAKPOINT

define method disable-breakpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)

  let success-code :: <integer> =
    clear-breakpoint-in-application (ap.connection, address);

  // Map the returned success code onto a boolean.
  (success-code == $ok);
end method;

define open generic clear-breakpoint-in-application
    (conn :: <access-connection>, ra :: <remote-value>) 
 => (success :: <integer>);

define method clear-breakpoint-in-application
    (conn :: <local-access-connection>, ra :: <remote-value>) 
      => (success :: <integer>)
  nub-clear-breakpoint (conn.connection-process, ra);
end method;

define method recover-breakpoint 
    (ap :: <access-path>, thread :: <remote-thread>)
      => ()

  recover-breakpoint-in-application (ap.connection, thread);

end method;

define open generic recover-breakpoint-in-application
    (conn :: <access-connection>, thread :: <remote-thread>) 
 => ();

define method recover-breakpoint-in-application
    (conn :: <local-access-connection>, thread :: <remote-thread>) 
       => ()
  nub-recover-breakpoint (conn.connection-process, thread.nub-descriptor);
end method;


///// QUERY-BREAKPOINT?

define method query-breakpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)

  let code :: <integer> =
    query-breakpoint-in-application (ap.connection, address);

  // Map the code onto a boolean.
  (code == $exists);
end method;

define open generic query-breakpoint-in-application
    (conn :: <access-connection>, ra :: <remote-value>) 
 => (success :: <integer>);

define method query-breakpoint-in-application
    (conn :: <local-access-connection>, ra :: <remote-value>) 
      => (success :: <integer>)
  nub-query-breakpoint (conn.connection-process, ra);
end method;


///// ENABLE-READ-WATCHPOINT

define method enable-read-watchpoint 
    (ap :: <access-path>, address :: <remote-value>, size :: <integer>) 
      => (success :: <boolean>)
  #f
end method;


///// DISABLE-READ-WATCHPOINT

define method disable-read-watchpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)
  #f
end method;


///// QUERY-READ-WATCHPOINT?

define method query-read-watchpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)
  #f
end method;


///// ENABLE-WRITE-WATCHPOINT

define method enable-write-watchpoint 
    (ap :: <access-path>, address :: <remote-value>, size :: <integer>) 
      => (success :: <boolean>)
  #f
end method;


///// DISABLE-WRITE-WATCHPOINT

define method disable-write-watchpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)
  #f
end method;


///// QUERY-WRITE-WATCHPOINT?

define method query-write-watchpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)
  #f
end method;


///// ENABLE-EXECUTE-WATCHPOINT

define method enable-execute-watchpoint 
    (ap :: <access-path>, address :: <remote-value>, size :: <integer>) 
      => (success :: <boolean>)
  #f
end method;


///// DISABLE-EXECUTE-WATCHPOINT

define method disable-execute-watchpoint 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)
  #f
end method;


///// QUERY-EXECUTE-WATCHPOINT?


define method query-execute-watchpoint? 
    (ap :: <access-path>, address :: <remote-value>)
      => (success :: <boolean>)
  #f
end method;


///// APPLY-THREAD-STEPPING-CONTROL
//    Informs the debugger nub that a thread is required to perform a
//    stepping operation. The client must have calculated one or more
//    instruction addresses that might be the destination of the step,
//    and also have supplied sufficient stack pointer context.

define method apply-thread-stepping-control
    (access-path :: <access-path>, thread :: <remote-thread>,
     locations :: <sequence>, operation :: <integer>,
     #key stack-frame = #f)
       => ()

  // Perform all stepping operations on the topmost stack frame unless
  // otherwise specified.

  unless (stack-frame)
    stack-frame := initialize-stack-trace(access-path, thread)
  end unless;

  // Find out the calling frame of the given frame, if there is one. It is
  // not essential that there be a calling frame, but the debugger nub will
  // happily accept that extra context if it can be provided.

  let calling-frame = previous-frame(access-path, stack-frame);
  let fp = stack-frame.stack-frame-pointer;
  let calling-fp =
    if (calling-frame) calling-frame.stack-frame-pointer
    else stack-frame.stack-frame-pointer
    end if;

  let count :: <integer> = locations.size;

  // Beware, we have an implementation limit on the number of locations
  // that we can set, but no control over the number of locations that
  // the client might be _attempting_ to set. There's not much we can
  // do if there are too many, so just truncate and issue a debug
  // message.

  if (count > $max-stepping-locations)
    nub-debug-message("ACCESS PATH STEPPER: Can't track %d destinations.", count);
    count := $max-stepping-locations;
  end if;

  // Dispatch to the appropriate debugger nub API.

  apply-thread-stepping-control-on-connection
     (access-path, access-path.connection, thread, fp, calling-fp, count,
      locations, operation);

  // And make sure that the thread records its state.
  thread.source-stepping-control-applied? := #t;
end method;

define open generic apply-thread-stepping-control-on-connection
    (ap :: <access-path>, conn :: <access-connection>, thread :: <remote-thread>,
     fp :: <remote-value>, calling-fp :: <remote-value>, count :: <integer>,
     locations :: <sequence>, operation :: <integer>)
 => ();

define method apply-thread-stepping-control-on-connection
    (ap :: <access-path>, conn :: <local-access-connection>, thread :: <remote-thread>,
     fp :: <remote-value>, calling-fp :: <remote-value>, count :: <integer>,
     locations :: <sequence>, operation :: <integer>)
  => ()
  let ap-locations :: <REMOTE-ARG-ARRAY> = ap.stepping-locations-vector;

  // Fill in a pre-allocated vector of addresses with those that the
  // client has calculated.

  for (i from 0 below count)
    pointer-value(ap-locations, index: i)
      := locations[i]
  end for;

  let error-code =
    nub-set-stepping-control-on-thread
      (conn.connection-process, thread.nub-descriptor, fp, calling-fp, count,
       ap-locations, operation);
  values();
end method;


///// REMOVE-ALL-STEPPING-CONTROL-FOR-THREAD
//    Removes all stepping traps that have been applied on a
//    particular thread. This should be called by the client upon
//    receipt of the <source-step-stop-reason>, or upon the receipt
//    of any other stop reason that the client wishes to abort the
//    step as a result of.

define method remove-all-stepping-control-for-thread
    (path :: <access-path>, thread :: <remote-thread>) => ()
  if (thread.source-stepping-control-applied?)
    remove-all-stepping-control-for-thread-on-connection
        (path.connection, thread);
    thread.source-stepping-control-applied? := #f
  end if
end method;

define open generic remove-all-stepping-control-for-thread-on-connection
    (conn :: <access-connection>, thread :: <remote-thread>) => ();

define method remove-all-stepping-control-for-thread-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>) => ()
  let error-code =
    nub-clear-stepping-control-on-thread(conn.connection-process, thread.nub-descriptor);
  values();
end method;

