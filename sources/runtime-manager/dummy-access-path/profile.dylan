module:    access-path-implementation
synopsis:  Access path support for profiling
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// INFORM-PROFILING-STARTED
//    Calls a function in the debugger nub allowing it to make any
//    necessary preparations to begin a profiling run.

define method inform-profiling-started (ap :: <access-path>) => ()
end method;


///// INFORM-PROFILING-STOPPED
//    Calls a function in the debugger nub, allowing it to do any
//    cleanups that are necessary after a profiling run has ended.

define method inform-profiling-stopped (ap :: <access-path>) => ()
end method;
