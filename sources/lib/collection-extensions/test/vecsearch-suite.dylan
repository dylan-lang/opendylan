Module:    collection-extensions-test
Synopsis:  The test suite for the vector-search module.
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

define test find-first-key-1
    (description: "Find-first-key for various vectors.")
  let v = #[1, 2, 3, 2, 3, 2, 1];
  check-equal("Find first key without keyword args.",
               find-first-key(v, curry(\=, 2)), 1);
  check-equal("Find non-existing key.",
              find-first-key(v, curry(\=, 6)), #f);
  check-equal("Find non-existing key with fail = #t.",
              find-first-key(v, curry(\=, 6), failure: #t), #t);
  check-equal("Find key with start = 2.",
              find-first-key(v, curry(\=, 2), start: 2), 3);
  check-equal("Find key with start = 3.",
              find-first-key(v, curry(\=, 2), start: 3), 3);
  check-equal("Fail because of start argument.",
              find-first-key(v, curry(\=, 3), start: 5), #f);
  check-equal("Fail because of end argument.",
              find-first-key(v, curry(\=, 3), end: 2), #f);
  check-equal("Fail because of start and end argument.",
              find-first-key(v, curry(\=, 1), start: 1, end: 6), #f);
end test find-first-key-1;

define test find-last-key-1
    (description: "Find-last-key for various vectors.")
  let v = #[1, 2, 3, 2, 3, 2, 1];
  check-equal("Find last key without keyword args.",
               find-last-key(v, curry(\=, 2)), 5);
  check-equal("Find non-existing key.",
              find-last-key(v, curry(\=, 6)), #f);
  check-equal("Find non-existing key with fail = #t.",
              find-last-key(v, curry(\=, 6), failure: #t), #t);
  check-equal("Find key with end = 5.",
              find-last-key(v, curry(\=, 2), end: 5), 3);
  check-equal("Find key with end = 4.",
              find-last-key(v, curry(\=, 2), end: 4), 3);
  check-equal("Find key with end = 3.",
              find-last-key(v, curry(\=, 2), end: 3), 1);
  check-equal("Fail because of start argument.",
              find-last-key(v, curry(\=, 3), start: 5), #f);
  check-equal("Fail because of end argument.",
              find-last-key(v, curry(\=, 3), end: 2), #f);
  check-equal("Fail because of start and end argument.",
              find-last-key(v, curry(\=, 1), start: 1, end: 6), #f);
end test find-last-key-1;

define suite vector-search-suite
    (description: "Test suite for the vector-search module.")
  test find-first-key-1;
  test find-last-key-1;
end suite vector-search-suite;
