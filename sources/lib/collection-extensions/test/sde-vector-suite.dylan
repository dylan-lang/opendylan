Module:    collection-extensions-test
Synopsis:  The test suite for the sde-vector module.
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

define test make-<sde-vector>-1
    (description: "Creation of <sde-vector>s")
  check("Actually got a <sde-vector>",
        instance?, make(<sde-vector>), <sde-vector>);
  check("Create empty vector by default",
        empty?, make(<sde-vector>));
end test make-<sde-vector>-1;

define test sde-vector-size-1
    (description: "Size of sde-vectors")
  check-equal("Size of default vector is 0",
              size(make(<sde-vector>)), 0);
  check-equal("Size is size: argument",
              size(make(<sde-vector>, size: 23)), 23);
end test sde-vector-size-1;

define function sde-element-test (v :: <sde-vector>)
  let $bound = 5;
  for (i from - $bound to $bound)
    v[i] := 2 * i;
  end for;
  for (i from - $bound to $bound, c from 0)
    check-equal("Element i is 2*i", v[i], 2 * i);
  finally
    check-equal("Did 2*$bound iterations.", c, 2 * $bound + 1);
  end for;
end function sde-element-test;

define test sde-element-1
    (description: "Test element and element-setter")
  let v = make(<sde-vector>, size: 11);
  sde-element-test(v);
end test sde-element-1;

define test sde-element-2
    (description: "Test element and element-setter on empty vector")
  let v = make(<sde-vector>);
  check-true("V is initially empty", v.empty?);
  sde-element-test(v);
  check-equal("V contains 11 elements", v.size, 11);
end test sde-element-2;

define test sde-element-3
    (description: "Accessing non-existing elements raises an error")
  let v = make(<sde-vector>);
  check-condition("Non existing element", <error>, v[1]);
end test sde-element-3;

define test sde-iteration-1
    (description: "Iteration over SDE-vectors")
  let v = make(<sde-vector>);
  let $bound = 6;
  for (i from - $bound to $bound)
    v[i] := 3 * i + 1;
  end for;
  for (i from -$bound to $bound)
    check-equal("V[i] = 3*i+1", v[i], 3 * i + 1);
  end for;
  for (j keyed-by i in v)
    check-equal("Value is 3*i+1", j, 3 * i + 1);
  end for;
end test sde-iteration-1;

define suite sde-vector-suite
    (description: "Test suite for the sde-vector module.")
  test make-<sde-vector>-1;
  test sde-vector-size-1;
  test sde-element-1;
  test sde-element-2;
  test sde-element-3;
  test sde-iteration-1;
end suite sde-vector-suite;
