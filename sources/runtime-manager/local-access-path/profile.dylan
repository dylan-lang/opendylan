module:    access-path-implementation
synopsis:  Access path support for profiling
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// INFORM-PROFILING-STARTED (Added by phoward, 1-APR-1997)
//    Calls a function in the debugger nub allowing it to make any
//    necessary preparations to begin a profiling run.

define method inform-profiling-started-on-connection
    (conn :: <local-access-connection>) => ()
  nub-inform-profiling-started(conn.connection-process);
end method;


///// INFORM-PROFILING-STOPPED (Added by phoward, 1-APR-1997)
//    Calls a function in the debugger nub, allowing it to do any
//    cleanups that are necessary after a profiling run has ended.

define method inform-profiling-stopped-on-connection
    (conn :: <local-access-connection>) => ()
  nub-inform-profiling-stopped(conn.connection-process);
end method;


///// GET-PROCESS-WALL-CLOCK-TIME

define method get-process-wc-time-on-connection
  (conn :: <local-access-connection>)
     => (timer :: <integer>)
  nub-get-process-wall-clock-time(conn.connection-process);
end method;
