Module:       common-dylan-test-suite
Synopsis:     testing recursive locks
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////
// Checks the basic claiming and releasing of recursive locks, and tests
// that owned? returns the correct values when the lock is and is not
// owned.
//
define test recursive-lock-basics
    (description: "Basic claiming/releasing of lock")

  let lock = make(<recursive-lock>);
  check-false("Lock not owned before claim", owned?(lock));
  check-true("Wait-for lock", wait-for(lock));
  check-true("Lock owned after claim", owned?(lock));
  release(lock);
  check-false("Lock not owned after release", owned?(lock));
  check-true("Wait-for lock with timeout", wait-for(lock, timeout: 10));
  release(lock);

end test;


//////////
// Make sure that the lock can be claimed recursively and that when it is
// ownership is retained. Check the lock only becomes free when it is
// released the same number of times as it was claimed.
//
define test recursive-lock-multiple-claim
    (description: "Claiming recursive lock multiple times")

  let lock = make(<recursive-lock>);
  for (i from 1 to 5)
    check-true(concatenate("Multiple claim ", integer-to-string(i)),
               wait-for(lock));
    check-true(concatenate("owned after claim ", integer-to-string(i)),
               owned?(lock));
  end for;
  for (i from 1 to 5)
    check-true(concatenate("owned before multiple release ", integer-to-string(i)),
               owned?(lock));
    release(lock);
  end for;
  check-false("not owned after final release", owned?(lock));

end test;


//////////
// Try to claim a lock which is already owned by another thread. The claim only
// succeeds when the other thread releases the lock.
//
define test recursive-lock-claim
    (description: "wait for a recursive lock which is owned by another thread")

  let lock   = make(<recursive-lock>);
  let thread = make(<thread>, name: "test thread",
                    function: method()
                                wait-for(lock);
                                sleep(2);
                                release(lock);
                                end method);

  sleep(1);
  let result = wait-for(lock);
  join-thread(thread);
  check-true("Wait for lock owned by other thread", result);
end test;


//////////
// Wait for a lock which another thread has already claimed, with a timeout.
// The wait should return with a timeout error (#f).
//
define test recursive-lock-claim-timeout
    (description: "timed wait for recursive lock")

  let lock = make(<recursive-lock>);
  let thread = make(<thread>, name: "Test thread",
                    function: method ()
                                wait-for(lock);
                                sleep(3);
                                release(lock);
                              end method);
  sleep(1);
  let result = wait-for(lock, timeout: 0.5);
  join-thread(thread);
  check-false("Wait for lock timeout", result);

end test;


define suite recursive-locks-suite (description: "Recursive locks")
  test recursive-lock-basics;
  test recursive-lock-multiple-claim;
  test recursive-lock-claim;
  test recursive-lock-claim-timeout;
end suite recursive-locks-suite;
