Module:    collection-extensions-test
Synopsis:  The test suite for the subseq module.
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

define variable *dickens* =
    "It was the best of times, it was the worst of times, "
    "it was the age of wisdom, it was the age of foolishness, "
    "it was the epoch of belief, it was the epoch of incredulity...\n"
    "Charles Dickens, A Tale of Two Cities.";

define test string-subsequence
    (description: "Subsequences of strings.")
  let str1 = "foobar";
  check-equal("Subsequence(str1, start: 0, end: 3) is \"foo\"", 
              subsequence(str1, start: 0, end: 3), "foo");
  check-equal("Subsequence(str1, start: 3, end: 7) is \"bar\"", 
              subsequence(str1, start: 3, end: 7), "bar");
  check-equal("Default start is 0",
              subsequence(str1, end: 3), "foo");
  check-equal("Default end is end of string",
              subsequence(str1, start: 3), "bar");
  check-equal("Subsequence from the middle of a string.",
              subsequence(*dickens*, start: 53, end: 77),
              "it was the age of wisdom");
  check-equal("Subsequence \"Charles Dickens\".",
              subsequence(*dickens*, start: 173, end: 188),
              "Charles Dickens");
end test string-subsequence;

define test string-subsequence-mutation
    (description: "Mutation of string subsequences.")
  let str1 = copy-sequence("foobar");
  let sub = subsequence(str1, start: 3, end: 7);
  check-equal("Sub is \"bar\"", sub, "bar");
  sub[2] := 'z';
  check-equal("Sub is \"baz\"", sub, "baz");
  check-equal("Str1 is \"foobaz\"", str1, "foobaz");
  str1[3] := 'f'; str1[4] := 'o'; str1[5] := 'o';
  check-equal("Sub is \"foo\"", sub, "foo");
  check-equal("Str1 is \"foofoo\"", str1, "foofoo");
end test string-subsequence-mutation;

define function integer-subsequence-test (type :: subclass(<mutable-sequence>))
  let $size = 5;
  let c = make(type, size: $size, fill: 99);
  for (i from 0 below $size)
    c[i] := 2 * i;
  end for;
  let s1 = subsequence(c);
  let s2 = subsequence(c, start: 1, end: 3);
  check-equal("S1.size is $size", s1.size, $size);
  check-equal("S2.size is 2", s2.size, 2);
  for (elt in s1, i from 0)
    check-equal("Element in s1 is 2*i", elt, 2 * i);
  finally
    check-equal("Did $Size iterations", i, $size);
  end for;
  for (elt in s2, i from 0)
    check-equal("Element  in s2 is 2*i + 2", elt, 2 * i + 2);
  finally
    check-equal("Did 2 iterations", i, 2);
  end for;
end function integer-subsequence-test;

define function integer-subsequence-mutation-test (type :: subclass(<mutable-sequence>))
  let $size = 5;
  let c = make(type, size: $size);
  for (i from 0 below $size)
    c[i] := 2 * i;
  end for;
  let s1 = subsequence(c);
  let s2 = subsequence(c, start: 1, end: 3);
  for (i from 0 below $size)
    s1[i] := 3 * i + 1;
  end for;
  for (elt in s1, i from 0)
    check-equal("Element in s1 is 3*i+1", elt, 3* i + 1);
  finally
    check-equal("Did $Size iterations", i, $size);
  end for;
  for (elt in s2, i from 0)
    check-equal("Element in s2 is 3*i+4", elt, 3* i + 4);
  finally
    check-equal("Did $Size iterations", i, 2);
  end for;
  for (elt in c, i from 0)
    check-equal("Element in c is 3*i+1", elt, 3* i + 1);
  finally
    check-equal("Did $Size iterations", i, $size);
  end for;
end function integer-subsequence-mutation-test;

define test list-subsequence
    (description: "Integer test for lists")
  integer-subsequence-test(<list>);
  // List subsequences are not mutable.
end test list-subsequence;

define test vector-subsequence
    (description: "Integer test for vectors")
  integer-subsequence-test(<vector>);
  integer-subsequence-mutation-test(<vector>);
end test vector-subsequence;

define test simple-vector-subsequence
    (description: "Integer test for vectors")
  integer-subsequence-test(<simple-vector>);
  integer-subsequence-mutation-test(<simple-vector>);
end test simple-vector-subsequence;

define test deque-subsequence
    (description: "Integer test for vectors")
  integer-subsequence-test(<deque>);
  // Deque subsequences are not mutable.
end test deque-subsequence;

define suite subseq-suite
    (description: "Test suite for the subseq module.")
  test string-subsequence;
  test string-subsequence-mutation;
  test list-subsequence;
  test vector-subsequence;
  test simple-vector-subsequence;
  test deque-subsequence;
end suite subseq-suite;
