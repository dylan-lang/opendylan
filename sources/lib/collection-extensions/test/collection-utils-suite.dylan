Module:    collection-extensions-test
Synopsis:  The test suite for the collection-utilities module.
Author:    Matthias Hölzl (tc@xantira.com)
Copyright: See below.

// Copyright.
// =========

// Copyright (C) 2004 Dr. Matthias Hölzl.

//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".

// If you need to receive this library under another license contact
// the author (tc@xantira.com).

define test singleton-array-1 (description: "Singleton? for arrays.")
  check-false("Singleton? on empty array", singleton?(#[]));
  check-true("Singleton? on #[1]", singleton?(#[1]));
  check-true("Singleton? on #[#[1,2,3]]", singleton?(#[#[1,2,3]]));
  check-false("Singleton? on #[1,2]", singleton?(#[1,2]));
end test singleton-array-1;

define test singleton-list-1 (description: "Singleton? for lists")
  check-false("Singleton? on empty array", singleton?(#()));
  check-true("Singleton? on #(1)", singleton?(#(1)));
  check-true("Singleton? on #(#(1,2,3))", singleton?(#(#(1,2,3))));
  check-false("Singleton? on #(1,2)", singleton?(#(1,2)));
end test singleton-list-1;

define suite collection-utilities-suite
    (description: "Tests for the collection-utilities module.")
  test singleton-array-1;
  test singleton-list-1;
end suite collection-utilities-suite;

