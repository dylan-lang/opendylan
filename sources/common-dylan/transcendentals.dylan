Module:       common-dylan-internals
Synopsis:     Transcendentals
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $single-pi :: <single-float> = 3.14159265s0;
define constant $single-e  :: <single-float> = 2.71828182s0;

define constant $double-pi :: <double-float> = 3.141592653589793d0;
define constant $double-e  :: <double-float> = 2.718281828459045d0;

///---*** NOTE: If we ever implement <extended-float>, use these definitions ...
/// define constant $extended-pi :: <extended-float>
///   = 3.14159265358979323846264338327950288419716939937511x0;
/// define constant $extended-e  :: <extended-float>
///   = 2.71828182845904523536028747135266249775724709369996x0;

///---*** NOTE: We should signal a more specific condition for operations
///---*** that would produce complex results.  Further, we might want to
///---*** provide a restart for said cases to return NaN.


define macro unary-transcendental-definer
  { define unary-transcendental ?:name
				(?x:name)
      ?prefix:body
    end }
    => { define open generic ?name (x :: <number>) => (result :: <number>);
	 define sealed domain ?name (<real>);
	 define sealed may-inline method ?name (x :: <real>) => (result :: <float>)
	   ?name(as(<float>, x))
	 end method ?name;
	 define sealed may-inline method ?name (?x :: <single-float>)
	  => (result :: <single-float>)
	   ?prefix;
	   primitive-raw-as-single-float
	     ("primitive-single-float-" ## ?name(primitive-single-float-as-raw(?x)))
	 end method ?name;
	 define sealed may-inline method ?name (?x :: <double-float>)
	  => (result :: <double-float>)
	   ?prefix;
	   primitive-raw-as-double-float
	     ("primitive-double-float-" ## ?name(primitive-double-float-as-raw(?x)))
	 end method ?name }
end macro unary-transcendental-definer;

define unary-transcendental exp (x) end;

define unary-transcendental log (x) end;

define may-inline function logn (x :: <number>, base :: <number>) => (result :: <number>)
  log(x) / log(base)
end function logn;

define unary-transcendental sqrt (x)
  if (negative?(x))
    error("SQRT would produce complex number")
  end
end unary-transcendental sqrt;

define may-inline function isqrt (x :: <integer>) => (result :: <integer>)
  round(sqrt(x))
end function isqrt;

define unary-transcendental sin (x) end;
define unary-transcendental cos (x) end;
define unary-transcendental tan (x) end;

define unary-transcendental asin (x)
  if (abs(x) > 1)
    error("ASIN would produce complex number")
  end
end unary-transcendental asin;

define unary-transcendental acos (x)
  if (abs(x) > 1)
    error("ACOS would produce complex number")
  end
end unary-transcendental acos;

define unary-transcendental atan (x) end;


///---*** NOTE: These definitions used here are from the Common Lisp HyperSpec.
///---*** As written, these definitions will have some accuracy problems and
///---*** should be revisited at a future date.  (Also, is the CLHS correct?)

define macro unary-hyperbolic-definer
  { define unary-hyperbolic ?:name (?x:name)
      ?single:expression, ?double:expression
    end }
    => { define open generic ?name (x :: <number>) => (result :: <number>);
	 define sealed domain ?name (<real>);
	 define sealed may-inline method ?name (x :: <real>) => (result :: <float>)
	   ?name(as(<float>, x))
	 end method ?name;
	 define sealed may-inline method ?name (?x :: <single-float>)
	  => (result :: <single-float>)
	   ?single
	 end method ?name;
	 define sealed may-inline method ?name (?x :: <double-float>)
	  => (result :: <double-float>)
	   ?double
	 end method ?name }
end macro unary-hyperbolic-definer;

define unary-hyperbolic sinh (x)
  (exp(x) - exp(-x)) / 2.0s0,
  (exp(x) - exp(-x)) / 2.0d0
end unary-hyperbolic sinh;

define unary-hyperbolic cosh (x)
  (exp(x) + exp(-x)) / 2.0s0,
  (exp(x) + exp(-x)) / 2.0d0
end unary-hyperbolic cosh;

define unary-hyperbolic tanh (x)
  (exp(x) - exp(-x)) / (exp(x) + exp(-x)),
  (exp(x) - exp(-x)) / (exp(x) + exp(-x))
end unary-hyperbolic tanh;

define unary-hyperbolic asinh (x)
  log(x + sqrt(1.0s0 + x ^ 2)),
  log(x + sqrt(1.0d0 + x ^ 2))
end unary-hyperbolic asinh;

define unary-hyperbolic acosh (x)
  2.0s0 * log(sqrt((x + 1.0s0) / 2.0s0) + sqrt((x - 1.0s0) / 2.0s0)),
  2.0d0 * log(sqrt((x + 1.0d0) / 2.0d0) + sqrt((x - 1.0d0) / 2.0d0))
end unary-hyperbolic acosh;

define unary-hyperbolic atanh (x)
  (log(1.0s0 + x) - log(1.0s0 - x)) / 2.0s0,
  (log(1.0d0 + x) - log(1.0d0 - x)) / 2.0d0
end unary-hyperbolic atanh;


/// NOTE: Always seals the (<real>, <real>) domain:  The domains, if given, are used
/// to define the "default" method to avoid sealing violations w.r.t. the Dylan library.
define macro binary-transcendental-domain-definer
  { define binary-transcendental-domain ?:name (?x:name, ?y:name)
					       (?domain1:name, ?domain2:name)}
    => { define sideways sealed domain ?name (<real>, <real>);
	 define sideways sealed may-inline method ?name (?x :: ?domain1, ?y :: ?domain2)
	  => (result :: <float>)
	   ?name(as(<float>, ?x), as(<float>, ?y))
	 end method ?name;
	 define sideways sealed may-inline method ?name
	     (?x :: <single-float>, ?y :: <double-float>) => (result :: <double-float>)
	   ?name(as(<double-float>, ?x), ?y)
	 end method ?name;
	 define sideways sealed may-inline method ?name 
	     (?x :: <double-float>, ?y :: <single-float>) => (result :: <double-float>)
	   ?name(?x, as(<double-float>, ?y))
	 end method ?name }
  { define binary-transcendental-domain ?:name (?x:name, ?y:name) }
    => {define binary-transcendental-domain ?name (?x, ?y) (<real>, <real>) }
end macro binary-transcendental-domain-definer;

define macro binary-transcendental-definer
  { define binary-transcendental ?:name (?x:name, ?y:name) }
    => { define open generic ?name (?x :: <number>, ?y :: <number>) => (result :: <number>);
	 define binary-transcendental-domain ?name (?x, ?y) }
end macro binary-transcendental-definer;

define binary-transcendental-domain \^ (base, power) (<real>, <float>);

define sideways sealed may-inline method \^ (base :: <single-float>, power :: <single-float>)
 => (result :: <single-float>)
  if (negative?(base))
    if (integral?(power))
      base ^ truncate(power)
    else
      error("\\^ would produce complex number")
    end
  else
    primitive-raw-as-single-float(primitive-single-float-expt
				    (primitive-single-float-as-raw(base),
				     primitive-single-float-as-raw(power)))
  end
end method \^;

define sideways sealed may-inline method \^ (base :: <double-float>, power :: <double-float>)
 => (result :: <double-float>)
  if (negative?(base))
    if (integral?(power))
      base ^ truncate(power)
    else
      error("\\^ would produce complex number")
    end
  else
    primitive-raw-as-double-float(primitive-double-float-expt
				    (primitive-double-float-as-raw(base),
				     primitive-double-float-as-raw(power)))
  end
end method \^;

define binary-transcendental atan2 (y, x);

define macro atan2-method-definer
  { define atan2-method (?type:name, ?pi:name, ?zero:expression) end }
    => { define sealed may-inline method atan2 (y :: ?type, x :: ?type) => (result :: ?type)
	   if (zero?(x))
	     if (zero?(y))
	       error("atan2(0, 0) is undefined")
	     else
	       if (positive?(y))
		 ?pi / 2.0
	       else
		 -?pi / 2.0
	       end
	     end
	   elseif (zero?(y))
	     if (positive?(x))
	       ?zero
	     else
	       ?pi
	     end
	   else
	     let atan = atan(y / x);
	     if (positive?(x))
	       atan
	     elseif (positive?(y))
	       atan + ?pi
	     else
	       atan - ?pi
	     end
	   end
	 end method atan2 }
end macro atan2-method-definer;

define atan2-method (<single-float>, $single-pi, 0.0s0) end;
define atan2-method (<double-float>, $double-pi, 0.0d0) end;
