Module:    collection-extensions-test
Synopsis:  The test suite for the self-organizing-list module.
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

define function self-organizing-list-==-test (sol :: <self-organizing-list>)
  let l1 = list(1, 2, 3);
  let l2 = list(1, 2, 3);
  assert(l1 ~== l2, "Two distinct lists are identical?");
  sol[l1] := "foo";
  check-equal("element l1 is \"foo\"",
              element(sol, l1, default: #f), "foo");
  check-equal("element l2 is not present", 
              element(sol, l2, default: #f), #f);
  sol[l2] := "bar";
  check-equal("element l1 is \"foo\"",
              element(sol, l1, default: #f), "foo");
  check-equal("element l2 is \"bar\"", 
              element(sol, l2, default: #f), "bar");
end function self-organizing-list-==-test;

define function self-organizing-list-=-test (sol :: <self-organizing-list>)
  let l1 = list(1, 2, 3);
  let l2 = list(1, 2, 3);
  assert(l1 ~== l2, "Two distinct lists are identical?");
  sol[l1] := "foo";
  check-equal("element l1 is \"foo\"",
              element(sol, l1, default: #f), "foo");
  check-equal("element l2 is \"foo\"", 
              element(sol, l2, default: #f), "foo");
  sol[l2] := "bar";
  check-equal("element l1 is \"bar\"",
              element(sol, l1, default: #f), "bar");
  check-equal("element l2 is \"bar\"", 
              element(sol, l2, default: #f), "bar");
end function self-organizing-list-=-test;

define test self-organizing-list-with-==
    (description: "Test self-organizing-list with == as test function")
  let sol = make(<self-organizing-list>, test: \==);
  self-organizing-list-==-test(sol);
end test self-organizing-list-with-==;

define test self-organizing-list-with-=
    (description: "Test self-organizing list with = as test function")
  let sol = make(<self-organizing-list>, test: \=);
  self-organizing-list-=-test(sol);
end test self-organizing-list-with-=;

define test self-organizing-list-default-test-is-==
    (description: "Test whether default test is ==")
  let sol = make(<self-organizing-list>);
  self-organizing-list-==-test(sol);
end test self-organizing-list-default-test-is-==;

define test self-organizing-list-iteration
    (description: "Iterate over a <self-organizing-list>")
  let s1 = make(<self-organizing-list>);
  let s2 = make(<self-organizing-list>);
  s1[1] := "foo";
  s1[2] := "bar";
  s1[3] := "foobar";
  for (elt keyed-by k in s1)
    s2[k] := elt;
  end for;
  check-equal("s2 contains element 1", s2[1], "foo");
  check-equal("s2 contains element 2", s2[2], "bar");
  check-equal("s2 contains element 3", s2[3], "foobar");
  check-equal("s2 contains no additional elements.",
              sort(key-sequence(s2)), #(1, 2, 3));
end test self-organizing-list-iteration;

define suite self-organizing-list-suite
    (description: "Test suite for the self-organizing-list module.")
  test self-organizing-list-with-==;
  test self-organizing-list-with-=;
  test self-organizing-list-default-test-is-==;
  test self-organizing-list-iteration;
end suite self-organizing-list-suite;
