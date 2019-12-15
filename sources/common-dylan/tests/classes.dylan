Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class test cases

// Note: Locators are tested in system-test-suite.

define test test-<format-string-condition> ()
  //---*** Fill this in...
end test;

define test test-<simple-condition> ()
  //---*** Fill this in...
end test;

define test test-<arithmetic-error> ()
  //---*** Fill this in...
end test;

define not-inline function force-integer-division-by-zero (i :: <integer>)
  /* Commented out due to https://github.com/dylan-lang/opendylan/issues/633
  floor/(3, i)
  */
end;

define not-inline function force-float-division-by-zero (f :: <float>)
  3 / f
end;

define test test-<division-by-zero-error> ()
  check-condition("floor/(1, 0) signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  force-integer-division-by-zero(0));
  check-condition("1.0s0 / 0.0s0 signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  force-float-division-by-zero(0.0s0));
  check-condition("1.0d0 / 0.0d0 signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  force-float-division-by-zero(0.0d0));
end test;

define not-inline function force-domain-error (f :: <float>)
  sqrt(f)
end;

define test test-<arithmetic-domain-error> ()
  check-condition("sqrt(-1.0) signals <arithmetic-domain-error>",
                  <arithmetic-domain-error>,
                  force-domain-error(-1.0));
end test;

define not-inline function force-integer-overflow (x :: <integer>, i :: <integer>)
  x + i
end;

define not-inline function force-float-overflow (x :: <float>, f :: <float>)
  x * f
end;

define test test-<arithmetic-overflow-error> ()
  check-condition("$maximum-integer + 1 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  force-integer-overflow($maximum-integer, 1));
  check-condition("$minimum-integer - 1 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  force-integer-overflow($minimum-integer, -1));
  check-condition("1.0s20 * 1.0s20 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  force-float-overflow(1.0s20, 1.0s20));
  check-condition("1.0d160 * 1.0d160 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  force-float-overflow(1.0d160, 1.0d160));
end test;

define not-inline function force-float-underflow (x :: <float>, f :: <float>)
  x * f
end;

define test test-<arithmetic-underflow-error> ()
  check-condition("1.0s-20 * 1.0s-20 signals <arithmetic-underflow-error>",
                  <arithmetic-underflow-error>,
                  force-float-underflow(1.0s-20, 1.0s-20));
  check-condition("1.0d-160 * 1.0d-160 signals <arithmetic-underflow-error>",
                  <arithmetic-underflow-error>,
                  force-float-underflow(1.0d-160, 1.0d-160));
end test;

define sideways method make-test-instance
    (class == <stretchy-sequence>) => (object)
  make(<stretchy-sequence>)
end method make-test-instance;

define test test-<stretchy-sequence> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <stretchy-object-vector>) => (object)
  make(<stretchy-object-vector>)
end method make-test-instance;

define test test-<stretchy-object-vector> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <object-deque>) => (object)
  make(<object-deque>)
end method make-test-instance;

define test test-<object-deque> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <string-table>) => (object)
  make(<string-table>)
end method make-test-instance;

define test test-<string-table> ()
  //---*** Fill this in...
end test;


/// simple-random classes

define test test-<random> ()
  //---*** Fill this in...
end test;


/// simple-profiling classes

define test test-<profiling-state> ()
  //---*** Fill this in...
end test;

define suite common-dylan-classes-test-suite ()
  test test-<format-string-condition>;
  test test-<simple-condition>;
  test test-<arithmetic-error>;
  test test-<division-by-zero-error>;
  test test-<arithmetic-domain-error>;
  test test-<arithmetic-overflow-error>;
  test test-<arithmetic-underflow-error>;
  test test-<stretchy-sequence>;
  test test-<stretchy-object-vector>;
  test test-<object-deque>;
  test test-<string-table>;
  test test-<random>;
end;
