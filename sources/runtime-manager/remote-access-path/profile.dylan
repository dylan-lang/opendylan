module:    remote-access-path
synopsis:  Access path support for profiling
author:    Keith Dennison, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// INFORM-PROFILING-STARTED (Added by phoward, 1-APR-1997)

define method inform-profiling-started-on-connection
    (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/inform-profiling-started(conn.nub);
end method;


///// INFORM-PROFILING-STOPPED (Added by phoward, 1-APR-1997)

define method inform-profiling-stopped-on-connection
    (conn :: <remote-access-connection>) => ()
  Rtmgr/RemoteNub/inform-profiling-stopped(conn.nub);
end method;


///// GET-PROCESS-WALL-CLOCK-TIME

define method get-process-wc-time-on-connection
  (conn :: <remote-access-connection>)
     => (timer :: <integer>)
  Rtmgr/RemoteNub/get-process-wall-clock-time(conn.nub);
end method;
