Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong, Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Trigonometric identities:
//
// sin^2(x) + cos^2(x) = 1
// sin(2x) = 2sin(x)cos(x)
// tan^2(x) + 1 = sec^2(x)
// sin(x+y) = sin(x)cos(y) + sin(y)cos(x)
// cos(x+y) = cos(x)cos(y) - sin(x)sin(y)

// define constant sq2/2s0 = as(<single-float>, sqrt(2) / 2.0s0);
// define constant sq3/2s0 = as(<single-float>, sqrt(3) / 2.0s0);

define constant sq2/2d0 = as(<double-float>, sqrt(2) / 2.0d0);
define constant sq3/2d0 = as(<double-float>, sqrt(3) / 2.0d0);

define constant arg-values-s0 
  = vector( 0.0s0,                          // 0
	   $single-pi / 6.0s0,              // pi/6
	   $single-pi / 4.0s0,              // pi/4
	   $single-pi / 3.0s0,              // pi/3
	   $single-pi / 2.0s0);             // pi/2


define constant arg-values-d0 
  = vector( 0.d0,
	   $double-pi / 6.0d0,
	   $double-pi / 4.0d0,
	   $double-pi / 3.0d0, 
	   $double-pi / 2.0d0);


//     0   pi/6   pi/4     pi/3    pi/2
define constant sin-values
  = vector( 0d0, .5d0, sq2/2d0, sq3/2d0, 1.0d0 );

//     0   pi/6   pi/4     pi/3    pi/2
define constant cos-values 
  = vector( 1.0d0, sq3/2d0, sq2/2d0, .5d0, 0.d0 );

define constant $epsilon = 0.000000001;

define method almost-equal
    ( x :: <real>, y :: <real> ) => (result :: <boolean>)
  abs( x - y ) <  $epsilon
end method almost-equal;


/// Function tests

define transcendentals function-test sin ()
  // Standard values.
  check-equivalents( "sin", sin, arg-values-s0, sin-values );
  check-equivalents( "sin", sin, arg-values-d0, sin-values );

  for(x from -500000.333 below 100 by 1997,
      y = 3 then modulo( y * 17, 101 ))
    let cx = cos(x);
    let cy = cos(y);
    let sx = sin(x);
    let sy = sin(y);
    let sxy = sx * cy + sy * cx;
    
    check-true
      (format-to-string( "sin(%=) = %=", x + y, sxy),
       almost-equal( sin( as(<single-float>, x + y)), sxy )
	 & almost-equal( sin( as(<double-float>, x + y)), sxy )
	 );

  end for;
end function-test sin;

define transcendentals function-test cos ()
  // Standard values.
  check-equivalents( "cos", cos, arg-values-s0, cos-values );
  check-equivalents( "cos", cos, arg-values-d0, cos-values );

  for(x from -500000.333 below 100 by 1997,
      y = 3 then modulo( y * 17, 101 ))
    let cx = cos(x);
    let cy = cos(y);
    let sx = sin(x);
    let sy = sin(y);
    let cxy = cx * cy - sx * sy;
    
    check-true
      (format-to-string( "cos(%=) = %=", x + y, cxy),
       almost-equal( cos( as(<single-float>, x + y)), cxy )
	 & almost-equal( cos( as(<double-float>, x + y)), cxy )
	 );

  end for;
end function-test cos;

define transcendentals function-test tan ()
  for( i from -12345.33 below 10000 by 1001 )
    let s = sin(i);
    let c = cos(i);
    let tn = s / c;
    unless (c = 0.0)
      check-true
	(format-to-string( "tan(%=) = %=", i, tn),
	 almost-equal( tan(as(<single-float>, i)), tn )
	   & almost-equal( tan(as(<double-float>, i)), tn )
	   );
    end unless;
  end for;
end function-test tan;


define transcendentals function-test asin ()
  check-condition("asin(1.01s0) errors",
	      <error>,
	      asin(1.01s0));
  check-condition("asin(1.01d0) errors",
	      <error>,
	      asin(1.01d0));

  for (arg from - $double-pi / 2 below $double-pi / 2 by .3)
    check-true
      (format-to-string("asin(sin(%=)) = %=", arg, arg),
       almost-equal(asin(sin(arg)), arg))
  end
end function-test asin;

define transcendentals function-test acos ()
  check-condition("acos(1.01s0) errors",
	      <error>,
	      acos(1.01s0));
  check-condition("asin(1.01d0) errors",
	      <error>,
	      acos(1.01d0));

  for( arg from 0.0d0 below $double-pi by .3 )
    check-true
      (format-to-string( "acos(cos(%=)) = %=", arg, arg),
       almost-equal( acos(cos(arg)), arg )
	 );
  end for;
end function-test acos;

define transcendentals function-test atan ()
  for( arg from -200.01 below 200 by 7 )
    check-true
      (format-to-string( "tan(atan(%=)) = %=", arg, arg),
       almost-equal( tan(atan(arg)), arg ));
  end for;
end function-test atan;

define transcendentals function-test atan2 ()
  // y = 0, x > 0
  check-true("atan2( 0.0s0, .5s0) = pi/2",
             almost-equal( atan2(0.0s0, .5s0), $double-pi / 2 ));
  check-true("atan2( 0.0d0, .5d0) = pi/2",
             almost-equal( atan2(0.0d0, .5d0), $double-pi / 2 ));

  // y = 0, x < 0
  // y > 0, x = 0
  // y < 0, x = 0
  
  // y = 0, x = 0
  check-condition("atan2( 0,0s0, 0,0s0 ) errors",
                  <error>,
                  atan2( 0.s0, 0.s0 ));

  check-condition("atan2( 0,0d0, 0,0d0 ) errors",
                  <error>,
                  atan2( 0.d0, 0.d0 ));

  // JWL left the following two lines unfinished.  Commented out by carlg.
  //for( arg from .01 to 
  //atan2( 

  // y > 0, x > 0
  // y > 0, x < 0
  // y < 0, x < 0
  // y < 0, x > 0
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
  check-condition( "log(-1) errors",
  <error>,
	      log(-1));

  check-condition( "log(- .5) errors",
	      <error>,
	      log(- .5));

  for( arg from -200.333 below 200 by 7 )
    let arg-expd = exp(arg);
    check-true(format-to-string( "log(%=) = %=", arg-expd, arg),
               almost-equal( log(arg-expd), arg ));
  end for;
end function-test log;

define transcendentals function-test exp ()

  check-true( "exp(0) = 1",
	     almost-equal( exp(0), 1.0 )
	       & almost-equal( exp(0.0s0), 1.0 )
	       & almost-equal( exp(0.0d0), 1.0 )
	       );

  check-true( "exp(-1) = 1/e",
	     almost-equal( exp(-1), 1 / $double-e )
	       & almost-equal( exp(-1.0s0), 1 / $double-e )
	       & almost-equal( exp(-1.0d0), 1 / $double-e )
	       );

// JWL left the following for loop unfinished.  Commented out by carlg.
/*
  for(i from -200.333 below 100 by 7,
      j = 3 then modulo( j * 17, 101 ))
    check-true
      (format-to-string( "exp(%= + %=) = exp(%=)*exp(%=)",
			i, j, i, j ),
       almost-equal( exp(i + j), exp(i) * exp(j) )
	 );
*/
end function-test exp;

define transcendentals function-test logn ()

  for( b = 1.414 then b * 3, while: b < 100 )
    for(val = sqrt(b) then val * sqrt(b),
	res = .5 then res + .5,
	while: ( val < 50 ) )
      check-true
	(format-to-string( "logn(%=, %=) = %=", val, b, res),
	 almost-equal( logn(val, b), res )
	 );
    end for;
  end for;
end function-test logn;

define transcendentals function-test \^ ()
/* Commented out by carlg.  Apparently unfinished.
  for( i from 0.0 below 100.0 by 2.33 )
    for( j from 0.0 below 22.0 by 1.75 )
      check-true
	(format-to-string( "logn(%=, %=) = %=", val, b, res),
	 almost-equal( log( i ^ j ) = (j * log( i )) )
	 );
      check-true
	(format-to-string( "logn(%=, %=) = %=", val, b, res),
	 almost-equal(logn( as(<double-float>, i) ^ as(<double-float>, j)),
		      as(<double-float>, j) * logn( as(<double-float>, i) ) )
	 );
    end for;
  end for;
*/
end function-test \^;


define transcendentals function-test sqrt ()
  check-condition( "sqrt(-1) errors",
	      <error>,
	      sqrt(-1));

  check-condition( "sqrt(-1.0) errors",
	      <error>,
	      sqrt(-1.0));

  check-condition( "sqrt(-1.d0) errors",
	      <error>,
	      sqrt(-1.d0));
/* Commented out by carlg.  Apparently unfinished.
  for(arg = $double-e then arg * 3 + 5,
      while: (arg < $double-e ^ 64) )
    let arg-squared = arg * arg;
    check-true
      (format-to-string( "sqrt(%=) = %=", arg-squared, arg),
       almost-equal( sqrt(arg-squared), arg )
	 );
*/
end function-test sqrt;

define transcendentals function-test isqrt ()
  check-condition( "isqrt(-1) errors",
	      <error>,
	      isqrt(-1));
  // Compare isqrt to floor(sqrt)
  for(arg = 2 then arg * 3 + 5,
      while: (arg < floor/($maximum-integer, 3) ) )
    check-equal
      ( format-to-string( "isqrt(%=) = floor(sqrt(%=))", arg, arg),
       isqrt(arg),
       floor(sqrt(arg))
	 );
  end;
end function-test isqrt;

define function check-equivalents
    (name, function, args, results)
  do(curry(check-equivalent, name, function), args, results)
end function check-equivalents;

define function check-equivalent
    (name, function, arg, result)
  check-true
    (format-to-string("%=(%=) = %=", name, arg, result),
     almost-equal(function(arg), result))
end function check-equivalent;
