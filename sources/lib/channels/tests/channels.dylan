Module: channels-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TO TEST:
/// actual uses in environment
/// typical uses
/// mapper

define test channels-test-1 (name: "Default (object is callback)",
			    description: "Check channel callbacks called only once while tuned-in.")
  let channel = make(<channel>);
  let count1 = 1;
  let count2 = 2;
  local method inc-count1 (message) count1 := count1 + message end method;
  local method inc-count2 (message) count2 := count2 + message end method;

  broadcast(channel, 1);
  check-true("Count1 not yet changed", count1 = 1);
  check-true("Count2 not yet changed", count2 = 2);

  tune-in(channel, inc-count1);
  broadcast(channel, 1);
  check-true("Count1 now 2", count1 = 2);
  check-true("Count2 not yet changed", count2 = 2);

  tune-in(channel, inc-count2);
  broadcast(channel, 1);
  check-true("Count1 now 3", count1 = 3);
  check-true("Count2 now 3", count2 = 3);

  tune-out(channel, inc-count1);
  broadcast(channel, 1);
  check-true("Count1 still 3", count1 = 3);
  check-true("Count2 now 4", count2 = 4);

  tune-out(channel, inc-count2);
  broadcast(channel, 1);
  check-true("Count1 still 3", count1 = 3);
  check-true("Count2 still 4", count2 = 4);
end test;

define test channels-test-2 (name: "Object plus callback",
			     description: "Make unique object as receiver key.")
  let channel = make(<channel>);
  let object = pair(#f, #f);
  let count = 0;

  tune-in(channel, object, callback: method (message) count := count + message end method);
  broadcast(channel, 1);
  check-true("Count now 1", count = 1);

  tune-out(channel, object);
  broadcast(channel, 1);
  check-true("Count still 1", count = 1);
end test;

define test channels-test-3 (name: "No message passed (channel property)")
  let channel = make(<channel>, message?: #f);
  let count = 0;
  local method inc-count () count := count + 2 end method;
  tune-in(channel, inc-count);
  broadcast(channel, 1);
  check-true("Count now 2", count = 2);
end test;

define test channels-test-4 (name: "No message passed (receiver property)")
  let channel = make(<channel>);
  let count = 0;
  local method inc-count () count := count + 2 end method;
  tune-in(channel, inc-count, message?: #f);
  broadcast(channel, 1);
  check-true("Count now 2", count = 2);
end test;

define test channels-test-5 (name: "Receiver passed (channel property)")
  let channel = make(<channel>, receiver?: #t);
  let object = pair(-1, -1);
  let maybe-object = #f;
  local method inc-count (message, passed-object) maybe-object := passed-object end method;
  tune-in(channel, object, callback: inc-count);
  broadcast(channel, 1);
  check-true("Receiver passed", object == maybe-object);
end test;

define test channels-test-6 (name: "Receiver passed (receiver property)")
  let channel = make(<channel>);
  let object = pair(-1, -1);
  let maybe-object = #f;
  local method set-maybe-object (message, passed-object) maybe-object := passed-object end method;
  tune-in(channel, object, receiver?: #t, callback: set-maybe-object);
  broadcast(channel, 1);
  check-true("Receiver passed", object == maybe-object);
end test;

define test channels-test-7 (name: "Matches message type")
  let channel = make(<channel>);
  let message = list(1);
  let maybe-message = #f;
  local method set-maybe-message (passed-message) maybe-message := passed-message end method;

  tune-in(channel, set-maybe-message, message-type: <list>);
  broadcast(channel, vector(1));
  check-true("Maybe-message still #F", maybe-message == #f);

  broadcast(channel, message);
  check-true("Maybe-message now message", maybe-message = message);
end test;

define test channels-test-8 (name: "Channel defines callback")
  let count = 0;
  local method inc-count (message) count := count + message end method;
  let channel = make(<channel>, callback: inc-count);
  tune-in(channel, #f);
  tune-in(channel, #t);
  broadcast(channel, 1);
  check-true("Count now 2", count = 2);
end test;

define test channels-test-9 (name: "Receiver overrides channel")
  let count = 0;
  let receiver = #f;
  local method inc-count (message) count := count + message end method;
  let channel = make(<channel>, callback: inc-count);
  tune-in(channel, #f);
  tune-in(channel, #t, message?: #f, receiver?: #t, callback: method (x) receiver := x; count := count + 9 end method);
  broadcast(channel, 1);
  check-true("Count now 10", count = 10);
  check-true("Receiver now #t", receiver = #t);
end test;

define test channels-test-10 (name: "Broadcaster passes extra arguments")
  let channel = make(<channel>);
  let count = 0;
  local method inc-count (message, extra-arg) count := count + message + extra-arg end method;
  tune-in(channel, inc-count);
  broadcast(channel, 1, 99);
  check-true("Count now 100", count = 100);
end test;

define test channels-test-11 (name: "Override channel")
  let channel = make(<channel>);
  let count = 0;
  local method inc-count (receiver) count := count + receiver; #t end method;
  tune-in(channel, 1);
  tune-in(channel, 2);
  let result = broadcast(override-channel(channel,
					  mapper: every?,
					  message?: #f,
					  receiver?: #t,
					  callback: inc-count),
			 #"ignored");
  check-true("Result now #T", result = #t);
  check-true("Count now 3", count = 3);
end test;

define suite channels-tests-suite ()
  test channels-test-1;
  test channels-test-2;
  test channels-test-3;
  test channels-test-4;
  test channels-test-5;
  test channels-test-6;
  test channels-test-7;
  test channels-test-8;
  test channels-test-9;
  test channels-test-10;
  test channels-test-11;
end suite;

run-test-application(channels-tests-suite);
