module:      access-path-implementation
synopsis:    Implementation of debug points.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// ENABLE-BREAKPOINT

define method set-breakpoint-in-application
    (conn :: <local-access-connection>, ra :: <remote-value>) 
       => (success :: <integer>)
  nub-set-breakpoint (conn.connection-process, ra);
end method;


///// DISABLE-BREAKPOINT

define method clear-breakpoint-in-application
    (conn :: <local-access-connection>, ra :: <remote-value>) 
      => (success :: <integer>)
  nub-clear-breakpoint (conn.connection-process, ra);
end method;

define method recover-breakpoint-in-application
    (conn :: <local-access-connection>, thread :: <remote-thread>) 
       => ()
  nub-recover-breakpoint (conn.connection-process, thread.nub-descriptor);
end method;


///// QUERY-BREAKPOINT?

define method query-breakpoint-in-application
    (conn :: <local-access-connection>, ra :: <remote-value>) 
      => (success :: <integer>)
  nub-query-breakpoint (conn.connection-process, ra);
end method;


///// APPLY-THREAD-STEPPING-CONTROL
//    Informs the debugger nub that a thread is required to perform a
//    stepping operation. The client must have calculated one or more
//    instruction addresses that might be the destination of the step,
//    and also have supplied sufficient stack pointer context.

define method apply-thread-stepping-control-on-connection
    (ap :: <access-path>, conn :: <local-access-connection>, thread :: <remote-thread>,
     fp :: <remote-value>, calling-fp :: <remote-value>, count :: <integer>,
     locations :: <sequence>, operation :: <integer>)
  => ()
  let ap-locations :: <REMOTE-ARG-ARRAY> = conn.stepping-locations-vector;

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

define method remove-all-stepping-control-for-thread-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>) => ()
  let error-code =
    nub-clear-stepping-control-on-thread(conn.connection-process, thread.nub-descriptor);
  values();
end method;

