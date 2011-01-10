module:    test-profiler
synopsis:  A <counter> class to associate names with counts
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A counter object associates a count with a name.
//
define class <counter> (<object>)
  slot name :: <string>, required-init-keyword: name:;
  slot value :: <integer> = 0;
end;


// Increment a counter by one.
//
define method increment (counter :: <counter>) => ()
  counter.value := counter.value + 1;
end method;


// Add a specified integer value to the counter.
//
define method increment-by (counter :: <counter>, inc :: <integer>) => ()
  counter.value := counter.value + inc;
end method;
