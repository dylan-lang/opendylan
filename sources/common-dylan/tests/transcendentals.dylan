Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong, Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant tests

define transcendentals constant-test $single-pi ()
  check-equal("$single-pi is pi",
              3.14159265358979323846264338327950288419716939937511s0,
              $single-pi);
end constant-test $single-pi;

define transcendentals constant-test $double-pi ()
  check-equal("$double-pi is pi",
              3.14159265358979323846264338327950288419716939937511d0,
              $double-pi);
end constant-test $double-pi;

define transcendentals constant-test $single-e ()
  check-equal("$single-e is e",
              2.71828182845904523536028747135266249775724709369996s0,
              $single-e);
end constant-test $single-e;

define transcendentals constant-test $double-e ()
  check-equal("$double-e is e",
              2.71828182845904523536028747135266249775724709369996d0,
              $double-e);
end constant-test $double-e;


/// Function tests

// The tests below are partially based on the elementary functions
// tests found at http://netlib.org/elefunt and described in detail in:
//
//   Cody, William J. and William Waite. Software Manual for the Elementary
//   Functions. 

define transcendentals function-test sin ()
  // ---*** Fill this in.
end function-test sin;

define transcendentals function-test cos ()
  // ---*** Fill this in.
end function-test cos;

define transcendentals function-test tan ()
  // ---*** Fill this in.
end function-test tan;

define transcendentals function-test asin ()
  // ---*** Fill this in.
end function-test asin;

define transcendentals function-test acos ()
  // ---*** Fill this in.
end function-test acos;

define transcendentals function-test atan ()
  // ---*** Fill this in.
end function-test atan;

define transcendentals function-test atan2 ()
  // ---*** Fill this in.
end function-test atan2;

define transcendentals function-test sinh ()
  // ---*** Fill this in.
end function-test sinh;

define transcendentals function-test cosh ()
  // ---*** Fill this in.
end function-test cosh;

define transcendentals function-test tanh ()
  // ---*** Fill this in.
end function-test tanh;

define transcendentals function-test asinh ()
  // ---*** Fill this in.
end function-test asinh;

define transcendentals function-test acosh ()
  // ---*** Fill this in.
end function-test acosh;

define transcendentals function-test atanh ()
  // ---*** Fill this in.
end function-test atanh;

define transcendentals function-test log ()
  check-condition("log(-1) errors",
                  <error>,
                  log(-1));

  check-condition("log(-0.5) errors",
                  <error>,
                  log(-0.5));

  // ---*** Fill this in
end function-test log;

define transcendentals function-test exp ()
  // These really ought to be exact
  check-equal("exp(0.0s0) = 1.0s0",
              1.0s0, exp(0.0s0));

  check-equal("exp(0.0d0) = 1.0d0",
              1.0d0, exp(0.0d0));

  // ---*** Fill this in
end function-test exp;

define transcendentals function-test logn ()
  // ---*** Fill this in
end function-test logn;

define transcendentals function-test \^ ()
end function-test \^;


define transcendentals function-test sqrt ()
  check-condition("sqrt(-1) errors",
                  <error>,
                  sqrt(-1));

  check-condition("sqrt(-1.0s0) errors",
                  <error>,
                  sqrt(-1.0s0));

  check-condition("sqrt(-1.d0) errors",
                  <error>,
                  sqrt(-1.d0));


end function-test sqrt;

define transcendentals function-test isqrt ()
  check-condition("isqrt(-1) errors",
                  <error>,
                  isqrt(-1));

  // Compare isqrt to floor(sqrt)
  for(arg = 2 then arg * 3 + 5,
      while: (arg < floor/($maximum-integer, 3) ) )
    check-equal(format-to-string("isqrt(%=) = floor(sqrt(%=))", arg, arg),
                isqrt(arg),
                floor(sqrt(arg)));
  end;
end function-test isqrt;
