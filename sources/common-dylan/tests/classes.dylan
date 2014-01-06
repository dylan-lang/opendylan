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

/// NOTE: These rather strange expressions are used to prevent the compiler from
///       attempting any compile-time optimizations of the original expressions.

define common-extensions class-test <division-by-zero-error> ()
  /* Commented out due to https://github.com/dylan-lang/opendylan/issues/633
  check-condition("floor/(1, 0) signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  begin
                    let x :: <integer> = 0;
                    for (i from 0 below 1)
                      x := x + 1;
                      x := floor/(x, 0);
                    end
                  end);
  */
  // Placeholder so we don't forget the above bug.
  check-true("floor/(1, 0) signals <division-by-zero-error>", #f);

  check-condition("1.0s0 / 0.0s0 signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  begin
                    let x :: <single-float> = 0.0s0;
                    for (i from 0 below 1)
                      x := x + 1.0s0;
                      x := x / 0.0s0;
                    end
                  end);
  check-condition("1.0d0 / 0.0d0 signals <division-by-zero-error>",
                  <division-by-zero-error>,
                  begin
                    let x :: <double-float> = 0.0d0;
                    for (i from 0 below 1)
                      x := x + 1.0d0;
                      x := x / 0.0d0;
                    end
                  end);
end class-test <division-by-zero-error>;

define common-extensions class-test <arithmetic-overflow-error> ()
  check-condition("$maximum-integer + 1 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  begin
                    let x :: <integer> = $maximum-integer - 1;
                    for (i from 0 below 2)
                      x := x + 1;
                    end
                  end);
  check-condition("$minimum-integer - 1 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  begin
                    let x :: <integer> = $minimum-integer + 1;
                    for (i from 0 below 2)
                      x := x - 1;
                    end
                  end);
  check-condition("1.0s20 * 1.0s20 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  begin
                    let x :: <single-float> = 1.0s0;
                    for (i from 0 below 2)
                      x := x * 1.0s20;
                    end;
                    x
                  end);
  check-condition("1.0d160 * 1.0d160 signals <arithmetic-overflow-error>",
                  <arithmetic-overflow-error>,
                  begin
                    let x :: <double-float> = 1.0d0;
                    for (i from 0 below 2)
                      x := x * 1.0d160;
                    end;
                    x
                  end);
end class-test <arithmetic-overflow-error>;

define common-extensions class-test <arithmetic-underflow-error> ()
  check-condition("1.0s-20 * 1.0s-20 signals <arithmetic-underflow-error>",
                  <arithmetic-underflow-error>,
                  begin
                    let x :: <single-float> = 1.0s0;
                    for (i from 0 below 2)
                      x := x * 1.0s-20;
                    end;
                    x
                  end);
  check-condition("1.0d-160 * 1.0d-160 signals <arithmetic-underflow-error>",
                  <arithmetic-underflow-error>,
                  begin
                    let x :: <double-float> = 1.0d0;
                    for (i from 0 below 2)
                      x := x * 1.0d-160;
                    end;
                    x
                  end);
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
