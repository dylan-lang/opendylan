module:      remote-access-path
synopsis:    Implementation of debug points.
author:      Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// ENABLE-BREAKPOINT

define method set-breakpoint-in-application
    (conn :: <remote-access-connection>, ra :: <remote-value>) 
       => (success :: <integer>)
  Rtmgr/RemoteNub/set-breakpoint(conn.nub, as-integer(ra));
end method;


///// DISABLE-BREAKPOINT

define method clear-breakpoint-in-application
    (conn :: <remote-access-connection>, ra :: <remote-value>) 
      => (success :: <integer>)
  Rtmgr/RemoteNub/clear-breakpoint(conn.nub, as-integer(ra));
end method;


define method recover-breakpoint-in-application
    (conn :: <remote-access-connection>, thread :: <remote-thread>) 
       => ()
  Rtmgr/RemoteNub/recover-breakpoint(conn.nub, thread.rnub-descriptor);
end method;


///// QUERY-BREAKPOINT?

define method query-breakpoint-in-application
    (conn :: <remote-access-connection>, ra :: <remote-value>) 
      => (success :: <integer>)
  Rtmgr/RemoteNub/query-breakpoint(conn.nub, as-integer(ra));
end method;


///// APPLY-THREAD-STEPPING-CONTROL

define method apply-thread-stepping-control-on-connection
    (ap :: <access-path>, conn :: <remote-access-connection>, thread :: <remote-thread>,
     fp :: <remote-value>, calling-fp :: <remote-value>, count :: <integer>,
     locations :: <sequence>, operation :: <integer>)
  => ()
  let ap-locations :: <RTARGET-ADDRESS-SEQ> = ap.%stepping-locations-remote-vector;

  // Fill in a pre-allocated vector of addresses with those that the
  // client has calculated.

  for (i from 0 below count)
    ap-locations[i]
      := as-integer(locations[i])
  end for;

  let error-code =
    Rtmgr/RemoteNub/set-stepping-control-on-thread
      (conn.nub, thread.rnub-descriptor,
       as-integer(fp), as-integer(calling-fp), count,
       ap-locations, operation);
  values();
end method;


///// REMOVE-ALL-STEPPING-CONTROL-FOR-THREAD

define method remove-all-stepping-control-for-thread-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>) => ()
  let error-code =
    Rtmgr/RemoteNub/clear-stepping-control-on-thread
      (conn.nub, thread.rnub-descriptor);
  values();
end method;

