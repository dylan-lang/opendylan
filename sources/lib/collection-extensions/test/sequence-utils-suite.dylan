Module:    collection-extensions-test
Synopsis:  The test suite for the sequence-utilities module.
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


define test push-test (description: "Push! macro")
  let x = #();
  push!(x, 1);
  check-equal("Push integer on empty list", x, #(1));
  push!(x, 2);
  check-equal("Push integer on non-empty list", x, #(2, 1));
end test push-test;

define test pop-test (description: "Pop! macro")
  let x = list(1);
  check-equal("Pop singleton result", pop!(x), 1);
  check-equal("Pop singleton location", x, #());
end test pop-test;

define test split-empty-vector
    (description: "Split-at called on an empty vector")
  let result = split-at(#(), ',');
  check("Split of empty list is singleton", singleton?, result);
  check("Split of empty list has empty member", empty?, result[0]);
end test split-empty-vector;

define test split-string-1
    (description: "Split a string without commas")
  let result = split-at("abc", ',');
  check("Split is singleton", singleton?, result);
  check-equal("Member equal to whole string", as(<string>, result[0]), "abc");
end test split-string-1;

define test split-string-2
    (description: "Split a string with 2 commas")
  let result = split-at("a,bc,def", ',');
  check-equal("Split contains three elements", 3, result.size);
  check-equal("First string is \"a\"", "a", result[0]);
  check-equal("Second string is \"bc\"", "bc", result[1]);
  check-equal("Thrid string is \"def\"", "def", result[2]);
end test split-string-2;

define test split-string-3
    (description: "Split a string with trailing comma")
  let result = split-at("a,bc,def,", ',');
  check-equal("Split contains three elements", 4, result.size);
  check-equal("First string is \"a\"", "a", result[0]);
  check-equal("Second string is \"bc\"", "bc", result[1]);
  check-equal("Thrid string is \"def\"", "def", result[2]);
  check-equal("Thrid string is empty", "", result[3]);
end test split-string-3;

define test split-string-4
    (description: "Split a string with 3 semicolons")
  let result = split-at("\"a A\";\"b B\";c;d", ';');
  check-equal("Split contains three elements", 4, result.size);
  check-equal("First string is \"a A\"", "\"a A\"", result[0]);
  check-equal("Second string is \"b B\"", "\"b B\"", result[1]);
  check-equal("Thrid string is \"c\"", "c", result[2]);
  check-equal("Thrid string is \"d\"", "d", result[3]);
end test split-string-4;


define suite sequence-utilities-suite
    (description: "Test suite for the sequence-utilities module.")
  test push-test;
  test pop-test;
  test split-empty-vector;
  test split-string-1;
  test split-string-2;
  test split-string-3;
  test split-string-4;
end suite sequence-utilities-suite;
