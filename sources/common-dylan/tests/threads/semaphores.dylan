Module:       common-dylan-test-suite
Synopsis:     testing semaphores
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////
// Check basic claiming and releasing of semaphores.
//
define test semaphore-basics
    (description: "Basic semaphore claiming/releasing")

  let semaphore = make(<semaphore>, initial-count: 0, maximum-count: 10);
  release(semaphore);
  check-true("Wait-for semaphore", wait-for(semaphore));

end test;


//////////
// Create another thread which claims the semaphore. Wait for a bit before
// claiming the semaphore. The claim only succeeds when the other thread
// releases the semaphore.
//
define test semaphore-claim
    (description: "Claim semaphore after another thread releases it")

  let semaphore = make(<semaphore>, initial-count: 1, maximum-count: 1);
  let thread = make(<thread>, name: "Semaphore test thread",
                    function: method ()
                                wait-for(semaphore);
                                sleep(2);
                                release(semaphore);
                              end method);

  sleep(1);
  let result = wait-for(semaphore);
  join-thread(thread);
  check-true("Wait-for semaphore", result);

end test;


//////////
// Claim the semaphore lots of times. The final claim should timeout. Then
// release the semaphore the same number of times it was successfully claimed.
// A further release should raise an error.
//
define test semaphore-multiple-claim-release
    (description: "Claim and release semaphore multiple times")

  let semaphore = make(<semaphore>, initial-count: 10, maximum-count: 10);
  for (i from 1 to 10)
    check-true(concatenate("wait-for semaphore", integer-to-string(i)),
               wait-for(semaphore));
  end for;

  check-false("Timeout waiting for semaphore",
              wait-for(semaphore, timeout: 1));

  for (i from 1 to 10)
    release(semaphore);
  end for;

//  check-condition("<count-exceeded-error>", <count-exceeded-error>,
//                  release(semaphore));

end test;


//////////
// The idea of this test is to simulate access to some limited resource e.g.
// a buffer. Each thread needs to use a space in the buffer, but only a
// limited number can do so simultaneously.
//
define test resource-test
    (description: "Lots of threads accessing shared resources")

  let n = 20;
  let threads = make(<vector>, size: n);
  let results = make(<vector>, size: n);
  let iterations = 10;
  let semaphore = make(<semaphore>, initial-count: 5, maximum-count: 5);

  let threadfunction = method ()
                         let result = #t;
                         for (i from 1 to iterations)
                           sleep(0.1);
                           result := result & wait-for(semaphore);
                           sleep(0.5);
                           release(semaphore);
                         end for;
                         result;
                       end method;

  for (i from 0 below n)
    threads[i] := make(<thread>,
                       name: concatenate("Semaphore thread ",
                                         integer-to-string(i)),
                       function: threadfunction);
  end for;

  for (i from 0 below n)
    let (thread, result) = join-thread(threads[i]);
    results[i] := result;
  end for;

  for (i from 0 below n)
    check-true(concatenate(integer-to-string(i), " semaphore thread result"),
               results[i]);
  end for;

end test;


define suite semaphores-suite (description: "Semaphores")
  test semaphore-basics;
  test semaphore-claim;
  test semaphore-multiple-claim-release;
  test resource-test;
end suite semaphores-suite;
