Module:       common-dylan-test-suite
Synopsis:     suite to test simple locks
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant *print-lock* = make(<lock>);

define method format-l (#rest args)
  with-lock (*print-lock*)
    apply(format-out, args);
  end with-lock;
end method;


//////////
// Testing basic claiming and releasing of locks. Also check that
// owned? returns the correct results for when the lock is and is not
// owned.
//
define test simple-lock-owning
    (description: "Basic claiming/releasing of simple lock")
  let lock = make(<lock>);
  check-true("<lock> is a simple lock", instance?(lock, <simple-lock>));
  check-false("Lock not owned before claim", owned?(lock));
  check-true("Wait-for lock", wait-for(lock));
  check-true("Lock owned after claim", owned?(lock));
  release(lock);
  check-false("Lock not owned after release", owned?(lock));
  check-true("Wait-for lock with timeout", wait-for(lock, timeout: 10));
  release(lock);
end test simple-lock-owning;


//////////
// Checks that when a lock which is already owned is claimed a second time
// that an error is raised. This is what I think should happen in our
// implementation, but I don't think the threads library specifies what
// should happen in this case.
//
define test simple-lock-multiple-claim
    (description: "Claiming simple lock when already owned")
  let lock = make(<simple-lock>);
  check-true("First claim", wait-for(lock));
  check-condition("Second claim", <error>, wait-for(lock));
end test simple-lock-multiple-claim;


//////////
// Try to claim a lock while another thread owns it. The claim succeeds
// only when the lock is released by the other thread;
//
define test simple-lock-claim
    (description: "Claim a lock from another thread")
  let lock = make(<simple-lock>);
  let thread = make(<thread>, name: "Test thread",
                    function: method ()
                                wait-for(lock);
                                sleep(2);
                                release(lock);
                              end method);
  sleep(1);
  let result = wait-for(lock);
  join-thread(thread);
  check-true("Wait for lock owned by other thread", result);
  check-true("Really did get lock", owned?(lock));
end test simple-lock-claim;


//////////
// Wait for a lock which another thread has already claimed, with a timeout.
// The wait should return with a timeout error (#f).
//
define test simple-lock-claim-timeout
    (description: "timed wait for lock")
  let lock = make(<simple-lock>);
  let thread = make(<thread>, name: "Test thread",
                    function: method ()
                                wait-for(lock);
                                sleep(3);
                                release(lock);
                              end method);
  sleep(1);
  let result = wait-for(lock, timeout: 1);
  join-thread(thread);
  check-false("Wait for lock timeout", result);
end test simple-lock-claim-timeout;


//////////
// Create a number of threads which increment a counter. The number of
// threads is set by 'n', and each thread increments the counter
// 'iterations' times. A lock is used to prevent threads from accessing
// the counter simultaneously. The main thread waits for all the incrementer
// threads to terminate before checking that the counter has the expected
// value.
//
define test counter-threads
    (description: "Multiple threads incrementing a counter")
  let n = 70;
  let iterations = 10;
  let vec = make(<vector>, size: n);
  let counter = 0;
  let lock = make(<lock>);
  let incrementer = method ()
                      for (i from 1 to iterations)
                        with-lock(lock)
                          let temp = counter;
                          thread-yield();
                          counter := temp + 1;
                        end with-lock;
                      end for;
                    end method;

  counter := 0;
  for (i from 0 below n)
    vec[i] := make(<thread>, name: concatenate("Counter thread ",
                                               integer-to-string(i)),
                   function: incrementer);
  end for;
  for (i from 0 below n)
    join-thread(vec[i]);
  end for;
  check-equal("Value of counter", counter, n * iterations);
end test counter-threads;


//////////
// Create a number of threads which increment a counter. Access to the counter
// is protected by a lock to prevent more than one thread from updating the
// counter at the same time. Threads use a timed wait on the lock before
// attempting to increment the counter. Thus a thread does not necessarily
// succeed at incrementing the counter on each iteration. The main thread
// checks that the counter has been incremented the correct number of times
// according to the number of successful increments the threads executed.
//
define test counter-threads-timeout
    (description: "Multiple threads incrementing a counter with lock timeouts")
  let n = 10;
  let iterations = 10;
  let vec = make(<vector>, size: n);
  let counter = 0;
  let lock = make(<lock>);
  let incrementer = method ()
                      let result = 0;
                      for (i from 1 to iterations)
                        if (wait-for(lock, timeout: 0.01))
                          let temp = counter;
                          sleep(0.003);
                          counter := temp + 1;
                          release(lock);
                          result := result + 1;
                        end if;
                      end for;
                      result
                   end method;

  for (i from 0 below n)
    vec[i] := make(<thread>, name: concatenate("Counter thread",
                                               integer-to-string(i)),
                   function: incrementer);
  end for;
  let results = 0;
  for (i from 0 below n)
    let (thread, result) = join-thread(vec[i]);
    results := results + result;
  end for;
  check-equal("Value of counter", counter, results);
end test counter-threads-timeout;


define suite simple-locks-suite ()
  test simple-lock-owning;
  test simple-lock-claim;
  test simple-lock-claim-timeout;
  test counter-threads;
  test counter-threads-timeout;
end suite simple-locks-suite;
