Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class test cases

define locators-protocol class-test <locator> ()
  //---*** Fill this in...
end class-test <locator>;

define common-extensions class-test <format-string-condition> ()
  //---*** Fill this in...
end class-test <format-string-condition>;

define common-extensions class-test <simple-condition> ()
  //---*** Fill this in...
end class-test <simple-condition>;

define common-extensions class-test <arithmetic-error> ()
  //---*** Fill this in...
end class-test <arithmetic-error>;

define not-inline function force-integer-division-by-zero (i :: <integer>)
  /* Commented out due to https://github.com/dylan-lang/opendylan/issues/633
  floor/(3, i)
  */
end;

define not-inline function force-float-division-by-zero (f :: <float>)
  3 / f
end;

define common-extensions class-test <division-by-zero-error> ()
  check-condition("floor/(1, 0) signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  force-integer-division-by-zero(0));
  check-condition("1.0s0 / 0.0s0 signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  force-float-division-by-zero(0.0s0));
  check-condition("1.0d0 / 0.0d0 signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  force-float-division-by-zero(0.0d0));
end class-test <division-by-zero-error>;

define not-inline function force-domain-error (f :: <float>)
  sqrt(f)
end;

define common-extensions class-test <arithmetic-domain-error> ()
  check-condition("sqrt(-1.0) signals <arithmetic-domain-error>",
                  <arithmetic-domain-error>,
                  force-domain-error(-1.0));
end class-test <arithmetic-domain-error>;

define not-inline function force-integer-overflow (x :: <integer>, i :: <integer>)
  x + i
end;

define not-inline function force-float-overflow (x :: <float>, f :: <float>)
  x * f
end;

define common-extensions class-test <arithmetic-overflow-error> ()
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
end class-test <arithmetic-overflow-error>;

define not-inline function force-float-underflow (x :: <float>, f :: <float>)
  x * f
end;

define common-extensions class-test <arithmetic-underflow-error> ()
  check-condition("1.0s-20 * 1.0s-20 signals <arithmetic-underflow-error>",
                  <arithmetic-underflow-error>,
                  force-float-underflow(1.0s-20, 1.0s-20));
  check-condition("1.0d-160 * 1.0d-160 signals <arithmetic-underflow-error>",
                  <arithmetic-underflow-error>,
                  force-float-underflow(1.0d-160, 1.0d-160));
end class-test <arithmetic-underflow-error>;

define sideways method make-test-instance
    (class == <stretchy-sequence>) => (object)
  make(<stretchy-sequence>)
end method make-test-instance;

define common-extensions class-test <stretchy-sequence> ()
  //---*** Fill this in...
end class-test <stretchy-sequence>;

define sideways method make-test-instance
    (class == <stretchy-object-vector>) => (object)
  make(<stretchy-object-vector>)
end method make-test-instance;

define common-extensions class-test <stretchy-object-vector> ()
  //---*** Fill this in...
end class-test <stretchy-object-vector>;

define sideways method make-test-instance
    (class == <object-deque>) => (object)
  make(<object-deque>)
end method make-test-instance;

define common-extensions class-test <object-deque> ()
  //---*** Fill this in...
end class-test <object-deque>;

define sideways method make-test-instance
    (class == <string-table>) => (object)
  make(<string-table>)
end method make-test-instance;

define common-extensions class-test <string-table> ()
  //---*** Fill this in...
end class-test <string-table>;


/// simple-random classes

define simple-random class-test <random> ()
  //---*** Fill this in...
end class-test <random>;


/// simple-profiling classes

define common-extensions class-test <profiling-state> ()
  //---*** Fill this in...
end class-test <profiling-state>;
