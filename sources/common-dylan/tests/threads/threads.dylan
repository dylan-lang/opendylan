module:       common-dylan-test-suite
Synopsis:     testing of threads
Author:       Tony Mann, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////
// Create a single thread and then join it. Make sure it returns the correct
// thread object and return values.
//
define test single-thread-join(description: "join-thread (single thread)")
  let thread = make (<thread>,
                     name: "join test",
                     function: method () sleep(10); values(1,2,3) end);
  let (rthread, a, b, c) = join-thread(thread);
  check-true("Returned thread object is correct", rthread == thread);
  check-equal("First thread return value", a, 1);
  check-equal("Second thread return value", b, 2);
  check-equal("Third thread return value", c, 3);
end test;


//////////
// Test join-thread when given a vector of thread objects to join.
//
define test multiple-thread-join(description: "join-thread (multiple threads)")
  let n = 10;
  let thread-maker
    = method (i :: <integer>) => (thread :: <thread>)
        make(<thread>,
             name: concatenate("join test ", integer-to-string(i)),
             function: method ()
                         let b = i * 10;
                         sleep(5);
                         format-l("Thread %d returning\n", i);
                         values(i, b)
                       end method);
      end method;
  let vec = make(<vector>, size: n);
  for (i from 0 below n)
    vec[i] := thread-maker(i);
  end for;
  let (thread, vec-index, b) = apply(join-thread, vec);
  for (i from 0 below n)
    if (i ~== vec-index)
      join-thread(vec[i]);
    end if;
  end for;
  check-true("Consistency of returned values", vec-index * 10 == b);
  check-true("Returned the right thread object", vec[vec-index] == thread);
end test;


//////////
// Make sure current-thread returns the correct thread object.
//
define test current-thread-test(description: "current-thread")
  let thread = make(<thread>, name: "current-thread test",
                              function: method() current-thread() end);
  let (rthread, result) = join-thread(thread);
  check-true("returns correct thread object", thread == result);
end test;


//////////
// Just run thread-yield.
//
define test yield-test(description: "thread-yield")
  thread-yield();
end test;


define suite threads-suite (description: "Threads")
  test single-thread-join;
  test multiple-thread-join;
  test current-thread-test;
  test yield-test;
end suite threads-suite;
