Module: common-dylan-test-suite
Synopsis: Tests for ../numerics.dylan


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

define test test-<division-by-zero-error>
    (expected-to-fail-reason:
       "C back-end doesn't install signal handlers."
       " https://github.com/dylan-lang/opendylan/issues/633")
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

define test test-integer-length ()
  for (i from 0 below 27)
    let v1 = ash(1, i) - 1;
    check-equal(format-to-string("integer-length(%d) is %d", v1, i),
                i, integer-length(v1));

    let v2 = ash(1, i);
    check-equal(format-to-string("integer-length(%d) is %d", v2, i + 1),
                i + 1, integer-length(v2));

    let v3 = - ash(1, i);
    check-equal(format-to-string("integer-length(%d) is %d", v3, i),
                i, integer-length(v3));

    let v4 = -1 - ash(1, i);
    check-equal(format-to-string("integer-length(%d) is %d", v4, i + 1),
                i + 1, integer-length(v4));
  end for;
end test;

// Ensure that a number is written to memory in order to dispose of
// any hidden bits in floating-point intermediate values

define variable *temp* :: <float> = 0.0;

define not-inline function store-float (x :: <float>) => (the-x :: <float>)
  *temp* := x;
  sequence-point();
  *temp*;
end function;

define test test-float-radix-single-float ()
  // Based on the algorithm in:
  //   Malcolm, M. A. Algorithms to reveal properties of floating-point
  //   arithmetic. Comm. ACM 15, 11 (Nov. 1972), 949-951.
  //   http://doi.acm.org/10.1145/355606.361870
  // with improvements (namely the use of store-float) suggested in:
  //   Gentlemen, W. Morven, Scott B. Marovich. More on algorithms
  //   that reveal properties of floating point arithmetic units.
  //   Comm. ACM 17, 5 (May 1974), 276-277.
  //   http://doi.acm.org/10.1145/360980.361003

  // Test successive powers of two until we find the first one in
  // the region where integers are no longer exactly representable
  let a :: <single-float>
    = for (a :: <single-float> = 1.0s0 then a + a,
           while: store-float(store-float(a + 1.0s0) - a) = 1.0s0)
      finally
        a
      end for;
  // Add successive powers of two to a until we find the successor
  // floating point number beyond a; a and its successor differ by
  // beta
  let ibeta :: <integer>
    = for (b :: <single-float> = 1.0s0 then b + b,
           while: zero?(store-float(store-float(a + b) - a)))
      finally
        floor(store-float(store-float(a + b) - a))
      end;

  check-equal("float-radix for <single-float> matches ibeta",
              ibeta, float-radix(1.0s0));
end test;

define test test-float-radix-double-float ()
  // Test successive powers of two until we find the first one in
  // the region where integers are no longer exactly representable
  let a :: <double-float>
    = for (a :: <double-float> = 1.0d0 then a + a,
           while: store-float(store-float(a + 1.0d0) - a) = 1.0d0)
      finally
        a
      end for;
  // Add successive powers of two to a until we find the successor
  // floating point number beyond a; a and its successor differ by
  // beta
  let ibeta :: <integer>
    = for (b :: <double-float> = 1.0d0 then b + b,
           while: zero?(store-float(store-float(a + b) - a)))
      finally
        floor(store-float(store-float(a + b) - a))
      end;

  check-equal("float-radix for <double-float> matches ibeta",
              ibeta, float-radix(1.0d0));
end test;

define test test-float-digits ()
  check-true("float-digits(1.0d0) is at least as much as float-digits(1.0s0)",
             float-digits(1.0d0) >= float-digits(1.0s0));
end test;

define test test-float-precision ()
  check-true("float-precision(0.0s0) is zero", zero?(float-precision(0.0s0)));
  check-true("float-precision(0.0d0) is zero", zero?(float-precision(0.0d0)));
  check-equal("float-precision and float-digits are the same"
                " for normalized single floats",
              float-precision(1.0s0), float-digits(1.0s0));
  check-equal("float-precision and float-digits are the same"
                " for normalized double floats",
              float-precision(1.0d0), float-digits(1.0d0));
end test;

define test test-decode-single-float-radix ()
  let single-beta :: <single-float> = as(<single-float>, float-radix(1.0s0));
  let (significand :: <single-float>,
       exponent :: <integer>,
       sign :: <single-float>) = decode-float(single-beta);
  check-equal("significand for <single-float> radix = 1 / radix",
              1.0s0 / single-beta, significand);
  check-equal("exponent for <single-float> radix = 2",
              2, exponent);
  check-equal("sign for <single-float> radix = 1.0s0",
              1.0s0, sign);
end test;

define test test-decode-single-float-subnormal ()
  let single-beta :: <single-float> = as(<single-float>, float-radix(1.0s0));
  let single-subnormal :: <single-float> = encode-single-float(as(<machine-word>, #x1));
  let (significand :: <single-float>,
       exponent :: <integer>,
       sign :: <single-float>) = decode-float(single-subnormal);
  check-equal("significand for <single-float> subnormal = 1 / radix",
              1.0s0 / single-beta, significand);
  check-equal("exponent for <single-float> subnormal = -148",
              -148, exponent);
  check-equal("sign for <single-float> subnormal = 1.0s0",
              1.0s0, sign);
end test;

define test test-decode-double-float-radix ()
  let double-beta :: <double-float> = as(<double-float>, float-radix(1.0d0));
  let (significand :: <double-float>,
       exponent :: <integer>,
       sign :: <double-float>) = decode-float(double-beta);
  check-equal("significand for <double-float> radix = 1 / radix",
              1.0d0 / double-beta, significand);
  check-equal("exponent for <double-float> radix = 2",
              2, exponent);
  check-equal("sign for <double-float> radix = 1.0d0",
              1.0d0, sign);
end test;

define test test-decode-double-float-subnormal ()
  let double-beta :: <double-float> = as(<double-float>, float-radix(1.0d0));
  let double-subnormal :: <double-float>
    = encode-double-float(as(<machine-word>, #x1),
                          as(<machine-word>, #x0));
  let (significand :: <double-float>,
       exponent :: <integer>,
       sign :: <double-float>) = decode-float(double-subnormal);
  check-equal("significand for <double-float> subnormal = 1 / radix",
              1.0d0 / double-beta, significand);
  check-equal("exponent for <double-float> subnormal = -1073",
              -1073, exponent);
  check-equal("sign for <double-float> subnormal = 1.0d0",
              1.0d0, sign);
end test;

define test test-decode-double-float-negative-subnormal ()
  let double-beta :: <double-float> = as(<double-float>, float-radix(1.0d0));
  let double-subnormal :: <double-float>
    = encode-double-float(as(<machine-word>, #x0),
                          as(<machine-word>, #x80000001));
  let (significand :: <double-float>,
       exponent :: <integer>,
       sign :: <double-float>) = decode-float(double-subnormal);
  check-equal("significand for <double-float> negative subnormal = 1 / radix",
              1.0d0 / double-beta, significand);
  check-equal("exponent for <double-float> negative subnormal = -1041",
              -1041, exponent);
  check-equal("sign for <double-float> negative subnormal = -1.0d0",
              -1.0d0, sign);
end test;

define test test-scale-float ()
  check-equal("scale-float(1.0s0, 1) is float-radix(1.0s0)",
              as(<single-float>, float-radix(1.0s0)),
              scale-float(1.0s0, 1));
  check-equal("scale-float(-1.0s0, 1) is -float-radix(1.0s0)",
              as(<single-float>, -float-radix(1.0s0)),
              scale-float(-1.0s0, 1));
  check-equal("scale-float(1.0d0, 1) is float-radix(1.0d0)",
              as(<double-float>, float-radix(1.0d0)),
              scale-float(1.0d0, 1));
  check-equal("scale-float(-1.0d0, 1) is -float-radix(1.0d0)",
              as(<double-float>, -float-radix(1.0d0)),
              scale-float(-1.0d0, 1));
end test;

define inline function classify-single-float (bits)
 => (classification :: <float-classification>)
  classify-float(encode-single-float(as(<machine-word>, bits)))
end function classify-single-float;

define inline function classify-double-float (low-bits, high-bits)
 => (classification :: <float-classification>)
  classify-float(encode-double-float(as(<machine-word>, low-bits),
                                     as(<machine-word>, high-bits)))
end function classify-double-float;

// These values came from http://www.astro.umass.edu/~weinberg/a732/notes07_01.pdf
define test test-classify-float ()
  assert-equal(classify-single-float(#x00000000), #"zero");
  assert-equal(classify-single-float(#x80000000), #"zero");
  assert-equal(classify-single-float(#x7f800000), #"infinite");
  assert-equal(classify-single-float(#xff800000), #"infinite");
  assert-equal(classify-single-float(#x00000001), #"subnormal");
  assert-equal(classify-single-float(#x007fffff), #"subnormal");
  assert-equal(classify-single-float(#x00800000), #"normal");
  assert-equal(classify-single-float(#x7f7fffff), #"normal");
  assert-equal(classify-single-float(#x7fc00000), #"nan");

  assert-equal(classify-double-float(#x00000000, #x00000000), #"zero");
  assert-equal(classify-double-float(#x00000000, #x80000000), #"zero");
  assert-equal(classify-double-float(#x00000000, #x7ff00000), #"infinite");
  assert-equal(classify-double-float(#x00000000, #xfff00000), #"infinite");
  assert-equal(classify-double-float(#x00000001, #x00000000), #"subnormal");
  assert-equal(classify-double-float(#xffffffff, #x000fffff), #"subnormal");
  assert-equal(classify-double-float(#x00000000, #x00100000), #"normal");
  assert-equal(classify-double-float(#xffffffff, #x7fefffff), #"normal");
  assert-equal(classify-double-float(#x00000000, #x7ff80000), #"nan");
end test;

define suite common-dylan-numerics-test-suite ()
  test test-$double-float-epsilon;
  test test-$maximum-double-float-exponent;
  test test-$maximum-single-float-exponent;
  test test-$minimum-double-float-exponent;
  test test-$minimum-single-float-exponent;
  test test-$single-float-epsilon;
  test test-<arithmetic-domain-error>;
  test test-<arithmetic-error>;
  test test-<arithmetic-overflow-error>;
  test test-<arithmetic-underflow-error>;
  test test-<division-by-zero-error>;
  test test-classify-float;
  test test-decode-single-float-radix;
  test test-decode-single-float-subnormal;
  test test-decode-double-float-radix;
  test test-decode-double-float-subnormal;
  test test-decode-double-float-negative-subnormal;
  test test-float-digits;
  test test-float-precision;
  test test-float-radix-single-float;
  test test-float-radix-double-float;
  test test-integer-length;
  test test-scale-float;
end suite;
