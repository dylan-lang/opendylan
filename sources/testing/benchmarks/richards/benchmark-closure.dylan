module: richards
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline function benchmark-closure (fn :: <function>)
  fn(); // run it once to warm up
  // garbage-collect(); // cleanup
  profiling (cpu-time-seconds, cpu-time-microseconds, allocation)
    for (i from 0 below 10)
      fn();
    end;
  results
    format-out("Time:  %s.%s\n"
	       "Space: %s\n",
	       cpu-time-seconds,
	       integer-to-string(cpu-time-microseconds, size: 6),
	       allocation);
  end profiling;
end function;
