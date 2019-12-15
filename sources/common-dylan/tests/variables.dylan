Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant testing

define test test-$unsupplied ()
  //---** What can we do here?
end test;

define test test-$unfound ()
  //---** What can we do here?
end test;


/// Streams

define test test-*standard-input* ()
  //---*** Fill this in!
end test;

define test test-*standard-output* ()
  //---*** Fill this in!
end test;

define test test-*standard-error* ()
  //---*** Fill this in!
end test;

define test test-*debug-output* ()
  //---*** Fill this in!
end test;


/// Numerics

define test test-$single-float-epsilon ()
  check-true("1.0s0 + $single-float-epsilon differs from 1.0s0",
             1.0s0 + $single-float-epsilon ~= 1.0s0);
  check-true("1.0s0 + scale-float($single-float-epsilon, -1) does not differ"
               " from 1.0s0",
             1.0s0 + scale-float($single-float-epsilon, -1) = 1.0s0);
end test;

define test test-$double-float-epsilon ()
  check-true("1.0d0 + $double-float-epsilon differs from 1.0d0",
             1.0s0 + $double-float-epsilon ~= 1.0d0);
  check-true("1.0d0 + scale-float($double-float-epsilon, -1) does not differ"
               " from 1.0d0",
             1.0d0 + scale-float($double-float-epsilon, -1) = 1.0d0);
end test;

define test test-$minimum-single-float-exponent ()
  //---*** Fill this in...
end test;

define test test-$maximum-single-float-exponent ()
  //---*** Fill this in...
end test;

define test test-$minimum-double-float-exponent ()
  //---*** Fill this in...
end test;

define test test-$maximum-double-float-exponent ()
  //---*** Fill this in...
end test;

define suite common-dylan-variables-test-suite ()
  test test-$unsupplied;
  test test-$unfound;
  test test-*standard-input*;
  test test-*standard-output*;
  test test-*standard-error*;
  test test-*debug-output*;
  test test-$single-float-epsilon;
  test test-$double-float-epsilon;
  test test-$minimum-single-float-exponent;
  test test-$maximum-single-float-exponent;
  test test-$minimum-double-float-exponent;
  test test-$maximum-double-float-exponent;
end;
