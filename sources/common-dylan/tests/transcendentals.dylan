Module:       common-dylan-test-suite
Synopsis:     Tests for transcendentals:common-dylan
Author:       Andy Armstrong, Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant tests

define test test-$single-pi ()
  check-equal("$single-pi is pi",
              3.14159265358979323846264338327950288419716939937511s0,
              $single-pi);
end test;

define test test-$double-pi ()
  check-equal("$double-pi is pi",
              3.14159265358979323846264338327950288419716939937511d0,
              $double-pi);
end test;

define test test-$single-e ()
  check-equal("$single-e is e",
              2.71828182845904523536028747135266249775724709369996s0,
              $single-e);
end test;

define test test-$double-e ()
  check-equal("$double-e is e",
              2.71828182845904523536028747135266249775724709369996d0,
              $double-e);
end test;


/// Function tests

// The tests below are partially based on the elementary functions
// tests found at http://netlib.org/elefunt and described in detail in:
//
//   Cody, William J. and William Waite. Software Manual for the Elementary
//   Functions.

define test test-sin ()
  // ---*** Fill this in.
end test;

define test test-cos ()
  // ---*** Fill this in.
end test;

define test test-tan ()
  // ---*** Fill this in.
end test;

define test test-sincos ()
  // ---*** Fill this in.
end test;

define test test-asin ()
  // ---*** Fill this in.
end test;

define test test-acos ()
  // ---*** Fill this in.
end test;

define test test-atan ()
  // ---*** Fill this in.
end test;

define test test-atan2 ()
  // ---*** Fill this in.
end test;

define test test-sinh ()
  // ---*** Fill this in.
end test;

define test test-cosh ()
  // ---*** Fill this in.
end test;

define test test-tanh ()
  // ---*** Fill this in.
end test;

define test test-asinh ()
  // ---*** Fill this in.
end test;

define test test-acosh ()
  // ---*** Fill this in.
end test;

define test test-atanh ()
  // ---*** Fill this in.
end test;

define test test-hypot
    (expected-to-fail?: method () $os-name == #"darwin" end,
     expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295")
  assert-equal(5.0s0, hypot(3.0s0, 4.0s0));
  assert-equal(5.0s0, hypot(3.0s0, 4.0d0));
  assert-equal(5.0d0, hypot(3.0d0, 4.0d0));
  assert-equal(5.0d0, hypot(3.0d0, 4.0s0));
  assert-no-errors(
    begin
      let z = hypot(1.0d154, 1.0d154);
      assert-not-equal(#"infinite", classify-float(z));
    end
  );
end test;

define test test-log ()
  check-condition("log(-1) errors",
                  <error>,
                  log(-1));

  check-condition("log(-0.5) errors",
                  <error>,
                  log(-0.5));

  // ---*** Fill this in
end test;

define test test-exp ()
  // These really ought to be exact
  check-equal("exp(0.0s0) = 1.0s0",
              1.0s0, exp(0.0s0));

  check-equal("exp(0.0d0) = 1.0d0",
              1.0d0, exp(0.0d0));

  // ---*** Fill this in
end test;

define test test-logn ()
  // ---*** Fill this in
end test;

define test test-ilog2 ()
  check-equal("ilog2(1) = 0",
              0, ilog2(1));
  check-equal("ilog2(32) = 5",
              5, ilog2(32));
  check-equal("ilog2(33) = 5",
              5, ilog2(32));
  check-condition("ilog2(-1) errors",
                  <error>,
                  ilog2(-1));
end test;

define test test-^ ()
end test;


define test test-sqrt
    (expected-to-fail?: method () $os-name == #"darwin" end,
     expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295")
  check-condition("sqrt(-1) errors",
                  <error>,
                  sqrt(-1));

  check-condition("sqrt(-1.0s0) errors",
                  <error>,
                  sqrt(-1.0s0));

  check-condition("sqrt(-1.d0) errors",
                  <error>,
                  sqrt(-1.d0));


end test;

define test test-isqrt ()
  check-condition("isqrt(-1) errors",
                  <error>,
                  isqrt(-1));

  // Compare isqrt to floor(sqrt)
  for (arg = 2 then arg * 3 + 5,
       while: (arg < floor/($maximum-integer, 3)))
    check-equal(format-to-string("isqrt(%=) = floor(sqrt(%=))", arg, arg),
                isqrt(arg),
                floor(sqrt(as(<single-float>, arg))));
  end;
end test;

define suite transcendentals-test-suite ()
  test test-$single-pi;
  test test-$double-pi;
  test test-$single-e;
  test test-$double-e;
  test test-sin;
  test test-cos;
  test test-tan;
  test test-sincos;
  test test-asin;
  test test-acos;
  test test-atan;
  test test-atan2;
  test test-sinh;
  test test-cosh;
  test test-tanh;
  test test-asinh;
  test test-acosh;
  test test-atanh;
  test test-hypot;
  test test-log;
  test test-exp;
  test test-logn;
  test test-ilog2;
  test test-^;
  test test-sqrt;
  test test-isqrt;
end suite;
