Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constant testing

define common-extensions constant-test $unsupplied ()
  //---** What can we do here?
end constant-test $unsupplied;

define common-extensions constant-test $unfound ()
  //---** What can we do here?
end constant-test $unfound;


/// Streams

define streams-protocol constant-test *standard-input* ()
  //---*** Fill this in!
end constant-test *standard-input*;

define streams-protocol constant-test *standard-output* ()
  //---*** Fill this in!
end constant-test *standard-output*;

define streams-protocol constant-test *standard-error* ()
  //---*** Fill this in!
end constant-test *standard-error*;

define streams-protocol constant-test *debug-output* ()
  //---*** Fill this in!
end constant-test *debug-output*;


/// Numerics

define common-extensions constant-test $single-float-epsilon ()
  check-true("1.0s0 + $single-float-epsilon differs from 1.0s0",
             1.0s0 + $single-float-epsilon ~= 1.0s0);
  check-true("1.0s0 + scale-float($single-float-epsilon, -1) does not differ"
               " from 1.0s0",
             1.0s0 + scale-float($single-float-epsilon, -1) = 1.0s0);
end constant-test $single-float-epsilon;

define common-extensions constant-test $double-float-epsilon ()
  check-true("1.0d0 + $double-float-epsilon differs from 1.0d0",
             1.0s0 + $double-float-epsilon ~= 1.0d0);
  check-true("1.0d0 + scale-float($double-float-epsilon, -1) does not differ"
               " from 1.0d0",
             1.0d0 + scale-float($double-float-epsilon, -1) = 1.0d0);
end constant-test $double-float-epsilon;

define common-extensions constant-test $minimum-single-float-exponent ()
  //---*** Fill this in...
end constant-test $minimum-single-float-exponent;

define common-extensions constant-test $maximum-single-float-exponent ()
  //---*** Fill this in...
end constant-test $maximum-single-float-exponent;

define common-extensions constant-test $minimum-double-float-exponent ()
  //---*** Fill this in...
end constant-test $minimum-double-float-exponent;

define common-extensions constant-test $maximum-double-float-exponent ()
  //---*** Fill this in...
end constant-test $maximum-double-float-exponent;

