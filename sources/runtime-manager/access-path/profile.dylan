module:    access-path-implementation
synopsis:  Access path support for profiling
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* 

Moved all this to debugger-manager

define constant <instruction-pointers> = <simple-object-vector>;

define variable *old-thread-cpu-times* = make(<table>);


define class <profile-snapshot> (<object>)

  slot weight :: <integer>,
       required-init-keyword: weight:;

  slot instruction-pointers :: <instruction-pointers>,
       required-init-keyword: instruction-pointers:;
end;


define method take-thread-snapshot
    (ap :: <access-path>, thread :: <remote-thread>,
     stack-depth :: false-or(<integer>))
 => (profile-snapshot :: <profile-snapshot>)


  // Find the number of frames on the stack so we can allocate a simple
  // object vector big enough to hold the instruction addresses from
  // the frames.

  let number-of-stack-frames = number-of-frames-on-stack (ap, thread);
  if (number-of-stack-frames = 0) 
    number-of-stack-frames := 1;
  end if;


  // Limit the depth to which the stack is traced if required.

  if (stack-depth)
    if (stack-depth < number-of-stack-frames)
      number-of-stack-frames := stack-depth;
    end if;
  else
    stack-depth := number-of-stack-frames;
  end if;


  // Step through the stack frames up to the maximum depth, collecting
  // the instruction pointer for each frame.

  let ips = make (<instruction-pointers>, size: stack-depth);
  let stack-frame = initialize-stack-trace (ap, thread);
  for (index from 0, while: stack-frame)
    ips[index] := frame-instruction-address (ap, stack-frame);
    stack-frame := previous-frame (ap, stack-frame);
  end for;


  // Calculate the weight. This is the cpu time in milliseconds used by
  // the thread since the last snapshot.

  let old-cpu-time = element (*old-thread-cpu-times*, thread, default: #f);
  let new-cpu-time = get-thread-cpu-time (ap, thread);
  let the-weight = if (old-cpu-time) new-cpu-time - old-cpu-time else 0 end;
  *old-thread-cpu-times*[thread] := new-cpu-time;

  make(<profile-snapshot>, weight: the-weight, instruction-pointers: ips)
end method;

*/

///// INFORM-PROFILING-STARTED (Added by phoward, 1-APR-1997)
//    Calls a function in the debugger nub allowing it to make any
//    necessary preparations to begin a profiling run.

define method inform-profiling-started (ap :: <access-path>) => ()
  inform-profiling-started-on-connection(ap.connection);
end method;

define open generic inform-profiling-started-on-connection
    (conn :: <access-connection>) => ();


///// INFORM-PROFILING-STOPPED (Added by phoward, 1-APR-1997)
//    Calls a function in the debugger nub, allowing it to do any
//    cleanups that are necessary after a profiling run has ended.

define method inform-profiling-stopped (ap :: <access-path>) => ()
  inform-profiling-stopped-on-connection(ap.connection);
end method;

define open generic inform-profiling-stopped-on-connection
    (conn :: <access-connection>) => ();


///// GET-PROCESS-WALL-CLOCK-TIME

define method get-process-wall-clock-time
  (ap :: <access-path>)
     => (timer :: <integer>)
  get-process-wc-time-on-connection(ap.connection);
end method;

define open generic get-process-wc-time-on-connection
  (conn :: <access-connection>)
     => (timer :: <integer>);
