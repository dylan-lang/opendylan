Module:       common-dylan-test-suite
Synopsis:     testing notifications
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////
// A few basic checks.
//
define test notification-basics
    (description: "")

  let lock = make(<lock>);
  let notification = make(<notification>, lock: lock);

  check-true("Associated lock", notification.associated-lock == lock);
  with-lock(lock)
    release(notification);
    check-true("Lock owned after release", owned?(lock));
    release-all(notification);
    check-true("Lock owned after release-all", owned?(lock));
  end with-lock;

end test;


//////////
// Test releasing a notification. The main thread waits for the notification
// after starting another thread which does the release.
//
define test notification-release
    (description: "")

  let lock = make(<lock>);
  let notification = make(<notification>, lock: lock);
  let thread = make(<thread>, name: "Test thread",
                    function: method()
                                sleep(1);
                                with-lock(lock)
                                  release(notification);
                                end with-lock;
                              end method);
  let result = #f;
  with-lock(lock)
    result := wait-for(notification);
  end with-lock;
  join-thread(thread);
  check-true("Wait-for notification", result);

end test;

//////////
// Test release-all on a notification. Set up lots of threads which wait for
// a notification, then release them all.
//
define test notification-release-all
    (description: "Testing of release-all")

  let n = 10;
  let threads = make(<vector>, size: n);
  let results = make(<vector>, size: n);
  let lock = make(<lock>);
  let notification = make(<notification>, lock: lock);
  let threadfunction = method ()
                         sleep(1.0);
                         with-lock(lock)
                           wait-for(notification, timeout: 10)
                         end with-lock;
                       end method;

  for (i from 0 below n)
    threads[i] := make(<thread>, function: threadfunction,
                       name: concatenate("Notification Thread ",
                                         integer-to-string(i)));
  end for;

  sleep(3);
  with-lock(lock)
    release-all(notification);
  end with-lock;

  for (i from 0 below n)
    let (thread, result) = join-thread(threads[i]);
    results[i] := result;
  end for;

  for (i from 0 below n)
    check-true(concatenate(integer-to-string(i)," notification thread result"),
               results[i]);
  end for;

end test;


//////////
// Make sure a timed wait for a notification will timeout.
//
define test notification-wait-timeout ()

  let lock = make(<lock>);
  let notification = make(<notification>, lock: lock);

  with-lock(lock)
    check-false("Wait-for notification with timeout",
                wait-for(notification, timeout: 1))
  end with-lock;

end test;


//////////
// Use notifications to control access to a queue. The main thread creates
// a number of feeders for the queue and one reader. Each feeder adds numbers
// in increasing order to the queue. The output from the reader should be
//
define constant *queue* = make(<deque>);
define constant *qlock* = make(<lock>);
define constant *something-queued* = make(<notification>, lock: *qlock*);

define method put-on-queue (object) => ()
  with-lock(*qlock*)
    if (*queue*.empty?)
      release-all(*something-queued*);
    end;
    push-last(*queue*, object);
  end with-lock;
end method;

define method get-from-queue () => (object)
  with-lock(*qlock*)
    while (*queue*.empty?)
      wait-for(*something-queued*)
    end;
    pop(*queue*);
  end with-lock;
end method;

define test queue-test ()

  let nfeeders = 5;
  let nreaders = 1;
  let iterations = 10;

  let feeders = make(<vector>, size: nfeeders);
  let readers = make(<vector>, size: nreaders);

  let makefeeder = method (n :: <integer>)
        method ()
          for (i from 1 to iterations)
            put-on-queue(concatenate
                           (concatenate
                              (concatenate(integer-to-string(n), " feeder. Item "),
                               integer-to-string(i)),
                            "\n"));
          end for;
        end method;
      end method;

  let reader = method ()
                 for (i from 1 to (iterations * nfeeders))
                   format-l(get-from-queue());
                 end for;
               end method;

  for (i from 0 below nfeeders)
    feeders[i] := make(<thread>, name: concatenate("Feeder ",
                                                   integer-to-string(i)),
                       function: makefeeder(i));
  end for;
  for (i from 0 below nreaders)
    readers[i] := make(<thread>, name: concatenate("Reader ",
                                                   integer-to-string(i)),
                       function: reader);
  end for;

  for (i from 0 below nfeeders)
    join-thread(feeders[i]);
  end for;
  for (i from 0 below nreaders)
    join-thread(readers[i]);
  end for;

end test;


//////////
// Testing monitor-style programming
// 

define variable *counter* = 0;
define constant *mlock* = make(<lock>);
define constant *non-zero* = make(<notification>, lock: *mlock*);


define method reset-counter ()
  *counter* := 0;
end method;


define method debug-counter ()
  let count = *counter*;
  format-l("%d - new value from %s\n", 
           count, current-thread().thread-name);
  count;
end method;


define method increment-count ()
  with-lock (*mlock*)
    let count = *counter*;
    if (count == 0) release-all(*non-zero*) end;
    *counter* := count + 1;
    debug-counter();
  end with-lock;
end method;

define method decrement-count ()
  with-lock (*mlock*)
    let count = *counter*;   // for debugging
    while (*counter* == 0) 
      wait-for(*non-zero*) 
    end;
    *counter* := *counter* - 1;
    debug-counter();
  end with-lock;
end method;


define test monitor-test ()

  let decrementers = 2;
  let incrementers = 2;
  let tries = 10;
  let vec = make(<vector>, size: 4);

  reset-counter();
  // first make a whole load of decrement threads
  for (i from 0 below decrementers)
    let name = concatenate("Decrementer ", integer-to-string(i));
    vec[i] := make(<thread>, name: name, function:
         method ()
           for (j from 1 to tries)
             sleep(1.0);
             decrement-count();
           end for;
           format-l(concatenate(name, " finished\n"));
         end);
  end for;

  // Now make the incrementer threads
  for (i from 0 below incrementers)
    let name = concatenate("Incrementer ", integer-to-string(i));
    vec[i + 2] := make(<thread>, name: name, function:
         method ()
           for (j from 1 to tries)
             sleep(1.5);
             increment-count();
           end for;
           format-l(concatenate(name, " finished\n"));
         end);
  end for;

  // Wait for all the threads to finish
  for (i from 0 below 4)
    join-thread(vec[i]);
  end for;

  check-true("*counter* is zero", *counter* == 0);

end test;


define method fixup (n :: <integer>)
  // Fixup any screwup with the notification
  with-lock (*mlock*)
    for (i from 1 to n) release(*non-zero*) end;
  end with-lock;
end method;


//////////
//
//
define suite notifications-suite ()
  test notification-basics;
  test notification-release;
  test notification-release-all;
  test notification-wait-timeout;
  test queue-test;
  test monitor-test;
end suite;
