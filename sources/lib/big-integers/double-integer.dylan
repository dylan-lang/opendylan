Module:    big-integers-internal
Authors:   Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BOOTED: define ... class <double-integer> ... end;

///---*** NOTE: Update the calls to machine-word-overflow to signal
///---*** the proper condition once we have separate <integer> conditions!

///---*** NOTE: KLUDGE: This is a copy of the make(<double-integer> method
///---*** which, for some reason, isn't getting properly inlined by the
///---*** compiler.
define inline-only function make-<double-integer>
    (low :: <machine-word>, high :: <machine-word>) => (di :: <double-integer>)
  let di :: <double-integer> = system-allocate-simple-instance(<double-integer>);
  %double-integer-low(di) := low;
  %double-integer-high(di) := high;
  di
end function make-<double-integer>;

define inline-only function sign-word (x :: <machine-word>) => (sign :: <machine-word>)
  if (machine-word-less-than?(x, coerce-integer-to-machine-word(0)))
    coerce-integer-to-machine-word(-1)
  else
    coerce-integer-to-machine-word(0)
  end
end function sign-word;

define inline-only function interpret-result-as-abstract-integer
    (low :: <machine-word>, high :: <machine-word>) => (value :: <integer>)
  if (double-integer-is-integer?(low, high))
    coerce-machine-word-to-integer(low)
  else
    make-<double-integer>(low, high)
  end
end function interpret-result-as-abstract-integer;


/// Update the values of these variable from the Generic-Arithmetic library
/// to reflect the extension of the range of acceptable <integer>s.

$minimum-integer := make-<double-integer>($minimum-unsigned-machine-word,
					  $minimum-signed-machine-word);

$maximum-integer := make-<double-integer>($maximum-unsigned-machine-word,
					  $maximum-signed-machine-word);


/// <float>/<double-integer> Conversions
define sealed sideways inline method as (class == <single-float>, x :: <double-integer>)
 => (result :: <single-float>)
  primitive-raw-as-single-float
    (primitive-double-integer-as-single-float
       (primitive-unwrap-machine-word(%double-integer-low(x)),
	primitive-unwrap-machine-word(%double-integer-high(x))))
end method as;

define sealed sideways inline method as (class == <double-float>, x :: <double-integer>)
 => (result :: <double-float>)
  primitive-raw-as-double-float
    (primitive-double-integer-as-double-float
       (primitive-unwrap-machine-word(%double-integer-low(x)),
	primitive-unwrap-machine-word(%double-integer-high(x))))
end method as;


/// Defines six methods for a binary arithmetic generic function.
/// Methods for (dylan/<integer>, dylan/<integer>) and (<double-integer>, <double-integer>)
/// are hand-written.  The methods for (dylan/<integer>, <double-integer>) and
/// (<double-integer>, dylan/<integer>) simply "promote" the dylan/<integer> to a
/// <double-integer> and then execute the (<double-integer>, <double-integer>) body.
/// The methods for (<integer>, <float>) and (<float>, <integer>) convert the <integer>
/// to a <float> and invoke the appropriate (<float>, <float>) method.
define macro binary-arithmetic-methods-definer
  { define binary-arithmetic-methods ?:name (?x:name, ?y:name)
      <integer> =>
	?integer:expression;
      <double-integer> (?x-low:name, ?x-high:name, ?y-low:name, ?y-high:name) =>
	?double-integer:expression;
    end }
    => { define sideways sealed inline method ?name 
	     (?x :: dylan/<integer>, ?y :: dylan/<integer>)
	  => (value :: <integer>)
	   ?integer
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?x :: dylan/<integer>, ?y :: <double-integer>)
	  => (value :: <integer>)
	   let ?x-low :: <machine-word> = coerce-integer-to-machine-word(?x);
	   let ?x-high :: <machine-word> = sign-word(?x-low);
	   let ?y-low :: <machine-word> = %double-integer-low(?y);
	   let ?y-high :: <machine-word> = %double-integer-high(?y);
	   ?double-integer
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?x :: <double-integer>, ?y :: dylan/<integer>)
	  => (value :: <integer>)
	   let ?x-low :: <machine-word> = %double-integer-low(?x);
	   let ?x-high :: <machine-word> = %double-integer-high(?x);
	   let ?y-low :: <machine-word> = coerce-integer-to-machine-word(?y);
	   let ?y-high :: <machine-word> = sign-word(?y-low);
	   ?double-integer
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?x :: <double-integer>, ?y :: <double-integer>)
	  => (value :: <integer>)
	   let ?x-low :: <machine-word> = %double-integer-low(?x);
	   let ?x-high :: <machine-word> = %double-integer-high(?x);
	   let ?y-low :: <machine-word> = %double-integer-low(?y);
	   let ?y-high :: <machine-word> = %double-integer-high(?y);
	   ?double-integer
	 end method ?name; 
	 define sideways sealed inline method ?name
	     (?x :: <float>, ?y :: <integer>) => (value :: <float>)
	   ?name(?x, as(<float>, ?y))
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?x :: <integer>, ?y :: <float>) => (value :: <float>)
	   ?name(as(<float>, ?x), ?y)
	 end method ?name }
end macro binary-arithmetic-methods-definer;

define binary-arithmetic-methods \+ (x, y)
  <integer> =>
    begin
      let mx = interpret-integer-as-machine-word(x);
      let my = strip-integer-tag(interpret-integer-as-machine-word(y));
      let (result, overflow?) = machine-word-add-with-overflow(mx, my);
      if (overflow?)
	let mx :: <machine-word> = coerce-integer-to-machine-word(x);
	let my :: <machine-word> = coerce-integer-to-machine-word(y);
	let (low :: <machine-word>, carry :: <machine-word>)
	  = machine-word-unsigned-add-with-carry(mx, my);
	let high :: <machine-word>
	  = machine-word-add-signal-overflow(sign-word(mx), sign-word(my));
	let high :: <machine-word> = machine-word-add-signal-overflow(high, carry);
	make-<double-integer>(low, high)
      else
	interpret-machine-word-as-integer(result)
      end
    end;
  <double-integer> (x-low, x-high, y-low, y-high) =>
    begin
      let (low :: <machine-word>, carry :: <machine-word>)
	= machine-word-unsigned-add-with-carry(x-low, y-low);
      let high :: <machine-word> = machine-word-add-signal-overflow(x-high, y-high);
      let high :: <machine-word> = machine-word-add-signal-overflow(high, carry);
      interpret-result-as-abstract-integer(low, high)
    end;
end binary-arithmetic-methods \+;

define binary-arithmetic-methods \- (x, y)
  <integer> =>
    begin
      let mx = interpret-integer-as-machine-word(x);
      let my = strip-integer-tag(interpret-integer-as-machine-word(y));
      let (result, overflow?) = machine-word-subtract-with-overflow(mx, my);
      if (overflow?)
	let mx :: <machine-word> = coerce-integer-to-machine-word(x);
	let my :: <machine-word> = coerce-integer-to-machine-word(y);
	let (low :: <machine-word>, borrow :: <machine-word>)
	  = machine-word-unsigned-subtract-with-borrow(mx, my);
	let high :: <machine-word>
	  = machine-word-subtract-signal-overflow(sign-word(mx), sign-word(my));
	let high :: <machine-word> = machine-word-subtract-signal-overflow(high, borrow);
	make-<double-integer>(low, high)
      else
	interpret-machine-word-as-integer(result)
      end
    end;
  <double-integer> (x-low, x-high, y-low, y-high) =>
    begin
      let (low :: <machine-word>, borrow :: <machine-word>)
	= machine-word-unsigned-subtract-with-borrow(x-low, y-low);
      let high :: <machine-word> = machine-word-subtract-signal-overflow(x-high, y-high);
      let high :: <machine-word> = machine-word-subtract-signal-overflow(high, borrow);
      interpret-result-as-abstract-integer(low, high)
    end;
end binary-arithmetic-methods \-;

define binary-arithmetic-methods \* (x, y)
  <integer> =>
    begin
      let mx = strip-integer-tag(interpret-integer-as-machine-word(x));
      let my = coerce-integer-to-machine-word(y);
      let (raw-result :: <raw-machine-word>, overflow? :: <boolean>)
	= primitive-machine-word-multiply-low-with-overflow
	    (primitive-unwrap-machine-word(mx), primitive-unwrap-machine-word(my));
      let result :: <machine-word> = primitive-wrap-machine-word(raw-result);
      if (overflow?)
	let (raw-low :: <raw-machine-word>, raw-high :: <raw-machine-word>)
	  = primitive-machine-word-multiply-low/high(integer-as-raw(x), integer-as-raw(y));
	make-<double-integer>(primitive-wrap-machine-word(raw-low),
			      primitive-wrap-machine-word(raw-high))
      else
	interpret-machine-word-as-integer(insert-integer-tag(result))
      end
    end;
  <double-integer> (x-low, x-high, y-low, y-high) =>
    begin
      let result-negative? = negative?(x) ~= negative?(y);
      let (low :: <machine-word>, xlyl-high :: <machine-word>)
	= machine-word-unsigned-multiply(x-low, y-low);
      let (xlyh-low :: <machine-word>, xlyh-high :: <machine-word>)
	= machine-word-unsigned-multiply(x-low, y-high);
      let (xhyl-low :: <machine-word>, xhyl-high :: <machine-word>)
	= machine-word-unsigned-multiply(x-high, y-low);
      let high :: <machine-word> = machine-word-unsigned-add-with-carry(xlyh-low, xhyl-low);
      let high :: <machine-word> = machine-word-unsigned-add-with-carry(xlyl-high, high);
      let result = interpret-result-as-abstract-integer(low, high);
      if (negative?(result) ~= result-negative?)
	machine-word-overflow()
      //---*** NEED MORE TESTS!
      else
	result
      end
    end;
end binary-arithmetic-methods \*;


/// While there are no \/ methods defined on (<integer>, <integer>) arguments, we'll
/// define methods for (<integer>, <float>) and (<float>, <integer>) to perform the
/// division using floating point arithmetic ...
define sideways sealed inline method \/ (dividend :: <float>, divisor :: <integer>)
 => (result :: <float>)
  dividend / as(<float>, divisor)
end method \/;

define sideways sealed inline method \/ (dividend :: <integer>, divisor :: <float>)
 => (result :: <float>)
  as(<float>, dividend) / divisor
end method \/;

/// Defines a single method for a unary division generic function which 
/// simply returns its argument and a remainder of zero.
define macro unary-division-methods-definer
  { define unary-division-methods ?:name }
    => { define sideways sealed inline method ?name (x :: <integer>)
	  => (quotient :: <integer>, remainder :: <integer>)
	   values(x, 0)
	 end method ?name }
end macro unary-division-methods-definer;

define unary-division-methods floor;
define unary-division-methods ceiling;
define unary-division-methods round;
define unary-division-methods truncate;

define constant $machine-half-word-size :: dylan/<integer> 
  = dylan/ash($machine-word-size, -1);

///---*** FINISH ME!  (I.e., <double-integer> by <double-integer>, round/)

/// Defines six methods for a binary division function.
/// The method for (dylan/<integer>, dylan/<integer>) simply invokes the corresponding
/// method in the Dylan library as it can't overflow or produce non-dylan/<integer> results.
/// The remaining three purely "integer" methods are are hand-written.
/// The methods for (<integer>, <float>) and (<float>, <integer>) convert the <integer>
/// to a <float> and invoke the appropriate (<float>, <float>) method.
define macro division-methods-definer
  { define division-methods ?:name (?dividend:name, ?divisor:name)
				   (?dividend-low:name, ?dividend-high:name,
				    ?divisor-low:name, ?divisor-high:name)
      integer/double-integer:
	?integer-double-integer:expression;
      double-integer/integer:
	?double-integer-integer:expression;
      double-integer/double-integer:
	?double-integer-double-integer:expression;
    end }
    => { define sideways sealed inline method ?name 
	     (?dividend :: dylan/<integer>, ?divisor :: dylan/<integer>)
	  => (quotient :: dylan/<integer>, remainder :: dylan/<integer>)
	   "dylan/" ## ?name(?dividend, ?divisor)
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?dividend :: dylan/<integer>, ?divisor :: <double-integer>)
	  => (quotient :: dylan/<integer>, remainder :: <integer>)
	   ?integer-double-integer
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?dividend :: <double-integer>, ?divisor :: dylan/<integer>)
	  => (quotient :: <integer>, remainder :: dylan/<integer>)
	   let ?dividend-low :: <machine-word> = %double-integer-low(?dividend);
	   let ?dividend-high :: <machine-word> = %double-integer-high(?dividend);
	   let ?divisor-low :: <machine-word> = coerce-integer-to-machine-word(?divisor);
	   ?double-integer-integer
	 end method ?name;
	 define sideways sealed inline method ?name
	     (?dividend :: <double-integer>, ?divisor :: <double-integer>)
	  => (quotient :: <integer>, remainder :: <integer>)
	   let ?dividend-low :: <machine-word> = %double-integer-low(?dividend);
	   let ?dividend-high :: <machine-word> = %double-integer-high(?dividend);
	   let ?divisor-low :: <machine-word> = %double-integer-low(?divisor);
	   let ?divisor-high :: <machine-word> = %double-integer-high(?divisor);
	   ?double-integer-double-integer
	 end method ?name;
	 define sideways sealed inline method ?name
	     (dividend :: <float>, divisor :: <integer>)
	  => (quotient :: <integer>, remainder :: <float>)
	   ?name(dividend, as(<float>, divisor))
	 end method ?name;
	 define sideways sealed inline method ?name
	     (dividend :: <integer>, divisor :: <float>)
	  => (quotient :: <integer>, remainder :: <float>)
	   ?name(as(<float>, dividend), divisor)
       	 end method ?name }
end macro division-methods-definer;

/// The "guts" of truncate/(<double-integer>, dylan/<integer>) is split out into
/// this function so that it can be used by the floor/, ceiling/, and round/ implementations.
/// It does its job by treating the <double-integer> as a three digit value and doing
/// good old fashioned long division.  (Treating the <double-integer> as a two digit value
/// can result in intermediate divisions whose result is too large to fit in a single word
/// which would cause the primitive to signal overflow.)
define inline-only function double-integer-by-integer-truncate/
    (dividend :: <double-integer>, dividend-low :: <machine-word>, 
     dividend-high :: <machine-word>,
     divisor :: dylan/<integer>, divisor-low :: <machine-word>) 
 => (quotient-low :: <machine-word>, quotient-high :: <machine-word>,
     remainder :: dylan/<integer>)
  let quotient-sign :: <machine-word>
    = if (negative?(dividend) ~= negative?(divisor))
	coerce-integer-to-machine-word(-1)
      else
	coerce-integer-to-machine-word(0)
      end;
  let (quotient-high :: <machine-word>, carry-high :: <machine-word>)
    = machine-word-double-truncate/(dividend-high, sign-word(dividend-high), divisor-low);
  //
  //---*** NOTE: This code can fail if either "carry" is too large.  Is there a better way?
  let dividend-middle :: <machine-word>
    = machine-word-logior
        (machine-word-shift-left-signal-overflow(carry-high, $machine-half-word-size),
	 primitive-wrap-machine-word
	   (primitive-machine-word-unsigned-double-shift-left-high
	      (primitive-unwrap-machine-word(dividend-low), integer-as-raw(0),
	       integer-as-raw($machine-half-word-size))));
  let (quotient-middle :: <machine-word>, carry-middle :: <machine-word>)
    = machine-word-double-truncate/(dividend-middle, sign-word(carry-high), divisor-low);
  let dividend-low :: <machine-word>
    = machine-word-logior
        (machine-word-shift-left-signal-overflow(carry-middle, $machine-half-word-size),
	 machine-word-unsigned-shift-right
	   (machine-word-unsigned-shift-left(dividend-low, $machine-half-word-size),
	    $machine-half-word-size));
  let (quotient-low :: <machine-word>, remainder :: <machine-word>)
    = machine-word-double-truncate/(dividend-low, sign-word(carry-middle), divisor-low);
  //
  //---*** NOTE: I'm not certain that this adjustment is correct although I haven't
  //---*** run into a case yet where it produces the wrong result.  (My worry is that
  //---*** if the middle "carry" was zero but the high "carry" wasn't, we may incorrectly
  //---*** adjust the middle of the quotient.)
  let quotient-adjust :: <machine-word>
    = if (machine-word-equal?(carry-high, coerce-integer-to-machine-word(0)))
	coerce-integer-to-machine-word(0)
      else
	quotient-sign
      end;
  let quotient-high :: <machine-word>
    = machine-word-add-with-overflow(quotient-high, quotient-adjust);
  let quotient-middle :: <machine-word>
    = machine-word-add-with-overflow(quotient-middle, quotient-adjust);
  let quotient-low :: <machine-word>
    = machine-word-logior
        (machine-word-unsigned-shift-left(quotient-middle, $machine-half-word-size),
	 machine-word-unsigned-shift-right
	   (machine-word-unsigned-shift-left(quotient-low, $machine-half-word-size),
	    $machine-half-word-size));
  let remainder :: dylan/<integer> = coerce-machine-word-to-integer(remainder);
  //
  // If the remainder is non-zero and the quotient is negative, the above code will
  // produce a quotient that's off by one so we must adjust both it and the remainder...
  let adjust-result? :: <boolean>
    = ~zero?(remainder) & negative?(remainder) ~= negative?(dividend);
  let quotient-low-adjust :: <machine-word>
    = if (adjust-result?)
	if (negative?(divisor))
	  coerce-integer-to-machine-word(-1)
	else
	  coerce-integer-to-machine-word(1)
	end
      else
	coerce-integer-to-machine-word(0)
      end;
  let remainder-adjust :: dylan/<integer>
    = if (adjust-result?)
	divisor
      else
	0
      end;
  let (quotient-low :: <machine-word>, carry :: <machine-word>)
    = machine-word-unsigned-add-with-carry(quotient-low, quotient-low-adjust);
  let quotient-high :: <machine-word>
    = machine-word-add-signal-overflow(quotient-high, sign-word(quotient-low-adjust));
  let quotient-high :: <machine-word>
    = machine-word-add-signal-overflow(quotient-high, carry);
  let remainder :: dylan/<integer> = dylan/-(remainder, remainder-adjust);
  values(quotient-low, quotient-high, remainder)
end function double-integer-by-integer-truncate/;

define division-methods floor/ (dividend, divisor)
			       (dividend-low, dividend-high, divisor-low, divisor-high)
  integer/double-integer:
    begin
      if (negative?(dividend) == negative?(divisor))
	values(0, dividend)
      else
	values(-1, divisor + dividend)
      end
    end;
  double-integer/integer:
    begin
      let (quotient-low :: <machine-word>, quotient-high :: <machine-word>, 
	   remainder :: dylan/<integer>)
	= double-integer-by-integer-truncate/(dividend, dividend-low, dividend-high,
					      divisor, divisor-low);
      if (~zero?(remainder) & if (negative?(divisor))
				positive?(dividend)
			      else
				negative?(dividend)
			      end)
	let (quotient-low :: <machine-word>, borrow :: <machine-word>)
	  = machine-word-unsigned-subtract-with-borrow(quotient-low, 
						       coerce-integer-to-machine-word(1));
	let quotient-high :: <machine-word>
	  = machine-word-subtract-signal-overflow(quotient-high, borrow);
	values(interpret-result-as-abstract-integer(quotient-low, quotient-high),
	       remainder + divisor)
      else
	values(interpret-result-as-abstract-integer(quotient-low, quotient-high),
	       remainder)
      end
    end;
  double-integer/double-integer:
    begin
      error("floor/(%d, %d) is not yet implemented", dividend, divisor)
    end;
end division-methods floor/;

define division-methods ceiling/ (dividend, divisor)
				 (dividend-low, dividend-high, divisor-low, divisor-high)
  integer/double-integer:
    begin
      if (negative?(dividend) == negative?(divisor))
	values(1, dividend - divisor)
      else
	values(0, dividend)
      end
    end;
  double-integer/integer:
    begin
      let (quotient-low :: <machine-word>, quotient-high :: <machine-word>, 
	   remainder :: dylan/<integer>)
	= double-integer-by-integer-truncate/(dividend, dividend-low, dividend-high,
					      divisor, divisor-low);
      if (~zero?(remainder) & if (negative?(divisor))
				negative?(dividend)
			      else
				positive?(dividend)
			      end)
	let (quotient-low :: <machine-word>, carry :: <machine-word>)
	  = machine-word-unsigned-add-with-carry(quotient-low, 
						 coerce-integer-to-machine-word(1));
	let quotient-high :: <machine-word>
	  = machine-word-add-signal-overflow(quotient-high, carry);
	values(interpret-result-as-abstract-integer(quotient-low, quotient-high),
	       remainder - divisor)
      else
	values(interpret-result-as-abstract-integer(quotient-low, quotient-high),
	       remainder)
      end
    end;
  double-integer/double-integer:
    begin
      error("ceiling/(%d, %d) is not yet implemented", dividend, divisor)
    end;
end division-methods ceiling/;

define division-methods round/ (dividend, divisor)
			       (dividend-low, dividend-high, divisor-low, divisor-high)
  integer/double-integer:
    begin
      error("round/(%d, %d) is not yet implemented", dividend, divisor)
    end;
  double-integer/integer:
    begin
      error("round/(%d, %d) is not yet implemented", dividend, divisor)
    end;
  double-integer/double-integer:
    begin
      error("round/(%d, %d) is not yet implemented", dividend, divisor)
    end;
end division-methods round/;

define division-methods truncate/ (dividend, divisor)
				  (dividend-low, dividend-high, divisor-low, divisor-high)
  integer/double-integer:
    begin
      values(0, dividend)
    end;
  double-integer/integer:
    begin
      let (quotient-low :: <machine-word>, quotient-high :: <machine-word>, 
	   remainder :: dylan/<integer>)
	= double-integer-by-integer-truncate/(dividend, dividend-low, dividend-high,
					      divisor, divisor-low);
      values(interpret-result-as-abstract-integer(quotient-low, quotient-high),
	     remainder)
    end;
  double-integer/double-integer:
    begin
      error("truncate/(%d, %d) is not yet implemented", dividend, divisor)
    end;
end division-methods truncate/;


/// Defines three methods for the modulo and remainder generic functions.
/// The (<integer>, <integer>) method invokes the appropriate binary division function
/// and returns its second value (i.e., the remainder).
/// The methods for (<integer>, <float>) and (<float>, <integer>) convert the <integer>
/// to a <float> and invoke the appropriate (<float>, <float>) method.
define macro division-remainder-methods-definer
  { define division-remainder-methods ?:name ?function:name }
    => { define sideways sealed inline method ?name
	     (dividend :: <integer>, divisor :: <integer>) => (remainder :: <integer>)
	   let (quotient :: <integer>, remainder :: <integer>) = ?function(dividend, divisor);
	   remainder
	 end method ?name;
	 define sideways sealed inline method ?name
	     (dividend :: <float>, divisor :: <integer>) => (remainder :: <float>)
	   ?name(dividend, as(<float>, divisor))
	 end method ?name;
	 define sideways sealed inline method ?name
	     (dividend :: <integer>, divisor :: <float>) => (remainder :: <float>)
	   ?name(as(<float>, dividend), divisor)
       	 end method ?name }
end macro division-remainder-methods-definer;

define division-remainder-methods modulo floor/;
define division-remainder-methods remainder truncate/;


/// Defines two methods for a unary sign changing generic function (i.e., abs or negative).
/// Both methods are hand written.
define macro unary-arithmetic-methods-definer
  { define unary-arithmetic-methods ?:name (?x:name)
      <integer> =>
	?integer:expression;
      <double-integer> (?x-low:name, ?x-high:name) =>
	?double-integer:expression;
    end }
    => { define sideways sealed inline method ?name (?x :: dylan/<integer>)
	  => (value :: <integer>)
	   ?integer
	 end method ?name;
	 define sideways sealed inline method ?name (?x :: <double-integer>)
	  => (value :: <integer>)
	   let ?x-low :: <machine-word> = %double-integer-low(?x);
	   let ?x-high :: <machine-word> = %double-integer-high(?x);
	   ?double-integer
	 end method ?name }
end macro unary-arithmetic-methods-definer;

/// NOTE: Presumes twos-complement arithmetic!
define unary-arithmetic-methods negative (x)
  <integer> =>
    begin
      let mx = strip-integer-tag(interpret-integer-as-machine-word(x));
      let (mresult, overflow?) = machine-word-negative-with-overflow(mx);
      if (overflow?)
	let low-1 :: <machine-word> = machine-word-lognot(coerce-integer-to-machine-word(x));
	let high-1 :: <machine-word> = sign-word(low-1);
	let (low :: <machine-word>, carry :: <machine-word>)
	  = machine-word-unsigned-add-with-carry(low-1, coerce-integer-to-machine-word(1));
	let high :: <machine-word> = machine-word-add-signal-overflow(high-1, carry);
	make-<double-integer>(low, high)
      else
	interpret-machine-word-as-integer(insert-integer-tag(mresult))
      end
    end;
  <double-integer> (x-low, x-high) =>
    begin
      let low-1 :: <machine-word> = machine-word-lognot(x-low);
      let high-1 :: <machine-word> = machine-word-lognot(x-high);
      let (low :: <machine-word>, carry :: <machine-word>)
	= machine-word-unsigned-add-with-carry(low-1, coerce-integer-to-machine-word(1));
      let high :: <machine-word> = machine-word-add-signal-overflow(high-1, carry);
      interpret-result-as-abstract-integer(low, high)
    end;
end unary-arithmetic-methods negative;

/// NOTE: Presumes twos-complement arithmetic!
define unary-arithmetic-methods abs (x)
  <integer> =>
    begin
      let mx = strip-integer-tag(interpret-integer-as-machine-word(x));
      let (mresult, overflow?) = machine-word-abs-with-overflow(mx);
      if (overflow?)
	// We could only overflow if we actually tried to negate the input value ...
	let low-1 :: <machine-word> = machine-word-lognot(coerce-integer-to-machine-word(x));
	let high-1 :: <machine-word> = sign-word(low-1);
	let (low :: <machine-word>, carry :: <machine-word>)
	  = machine-word-unsigned-add-with-carry(low-1, coerce-integer-to-machine-word(1));
	let high :: <machine-word> = machine-word-add-signal-overflow(high-1, carry);
	make-<double-integer>(low, high)
      else
	interpret-machine-word-as-integer(insert-integer-tag(mresult))
      end
    end;
  <double-integer> (x-low, x-high) =>
    begin
      if (negative?(x))
	let low-1 :: <machine-word> = machine-word-lognot(x-low);
	let high-1 :: <machine-word> = machine-word-lognot(x-high);
	let (low :: <machine-word>, carry :: <machine-word>)
	  = machine-word-unsigned-add-with-carry(low-1, coerce-integer-to-machine-word(1));
	let high :: <machine-word> = machine-word-add-signal-overflow(high-1, carry);
	interpret-result-as-abstract-integer(low, high);
      else
	x
      end
    end;
end unary-arithmetic-methods abs;


define sideways sealed method \^ (base :: <integer>, power :: <integer>)
 => (res :: <rational>)
  if (negative?(power))
    //---*** THIS IS WRONG AS / ISN'T DEFINED FOR <integer>!
    1 / (base ^ -power)
  elseif (base = 2)
    ash(1, power)
  elseif (negative?(base))
    if (odd?(power)) -(-base ^ power) else (-base ^ power) end
  else
    // Avoids squaring on last iteration to prevent premature overflow ...
    iterate loop (base :: <integer> = if (power > 1) base * base else base end,
		  p :: <integer> = ash(power, -1),
		  x :: <integer> = if (odd?(power)) base else 1 end)
      if (zero?(p))
	x
      else
	loop(if (p > 1) base * base else base end, 
	     ash(p, -1),
	     if (odd?(p)) base * x else x end)
      end if
    end iterate
  end if
end method \^;


/// Defines two methods for a shift generic function as well as four helper methods.
/// Two of the helper methods shift left while the other two shift right.  All helper
/// methods are hand-written.  At present, the helper methods to shift dylan/<integer>s
/// simply "promote" the value to a <double-integer> and execute that method.  At some
/// point, we should implement dylan/<integer> specific methods but they're tricky when
/// shifting left due to potential overflows.
define macro shift-methods-definer
  { define shift-methods ?:name (?x:name, ?shift:name) (?x-low:name, ?x-high:name)
      left:  ?left:expression;
      right: ?right:expression;
    end }
    => { define sideways sealed inline method ?name 
	     (?x :: dylan/<integer>, ?shift :: dylan/<integer>) => (value :: <integer>)
	   if (negative?(?shift))
	     let ?shift :: dylan/<integer> = min(dylan/negative(?shift),
						 dylan/*(2, $machine-word-size));
	     ?name ## "-right"(?x, ?shift)
	   else
	     let ?shift :: dylan/<integer> = min(?shift, dylan/*(2, $machine-word-size));
	     ?name ## "-left"(?x, ?shift)
	   end
	 end method ?name;
         define sideways sealed inline method ?name 
	     (?x :: <double-integer>, ?shift :: dylan/<integer>) => (value :: <integer>)
	   if (negative?(?shift))
	     let ?shift :: dylan/<integer> = min(dylan/negative(?shift),
						 dylan/*(2, $machine-word-size));
	     ?name ## "-right"(?x, ?shift)
	   else
	     let ?shift :: dylan/<integer> = min(?shift, dylan/*(2, $machine-word-size));
	     ?name ## "-left"(?x, ?shift)
	   end
	 end method ?name;
         define sealed inline method ?name ## "-left"
	     (?x :: dylan/<integer>, ?shift :: dylan/<integer>) => (value :: <integer>)
	   let ?x-low :: <machine-word> = coerce-integer-to-machine-word(?x);
	   let ?x-high :: <machine-word> = sign-word(?x-low);
	   ?left
	 end method ?name ## "-left";
         define sealed inline method ?name ## "-right"
	     (?x :: dylan/<integer>, ?shift :: dylan/<integer>) => (value :: <integer>)
	   let ?x-low :: <machine-word> = coerce-integer-to-machine-word(?x);
	   let ?x-high :: <machine-word> = sign-word(?x-low);
	   ?right
	 end method ?name ## "-right";
         define sealed inline method ?name ## "-left"
	     (?x :: <double-integer>, ?shift :: dylan/<integer>) => (value :: <integer>)
	   let ?x-low :: <machine-word> = %double-integer-low(?x);
	   let ?x-high :: <machine-word> = %double-integer-high(?x);
	   ?left
	 end method ?name ## "-left";
         define sealed inline method ?name ## "-right"
	     (?x :: <double-integer>, ?shift :: dylan/<integer>) => (value :: <integer>)
	   let ?x-low :: <machine-word> = %double-integer-low(?x);
	   let ?x-high :: <machine-word> = %double-integer-high(?x);
	   ?right
	 end method ?name ## "-right" }
end macro shift-methods-definer;

define shift-methods ash (x, shift) (x-low, x-high)
  left:
    begin
      if (shift < $machine-word-size)
	let (result-low :: <machine-word>, high-low :: <machine-word>)
	   = machine-word-unsigned-double-shift-left(x-low,
						     coerce-integer-to-machine-word(0),
						     shift);
	let high-high :: <machine-word>
	  = machine-word-shift-left-signal-overflow(x-high, shift);
	let result-high :: <machine-word> = machine-word-logior(high-low, high-high);
	interpret-result-as-abstract-integer(result-low, result-high)
      else
	// Shifting left by more than a word --
	//   Only low part of the <double-integer> contributes to the result.
	if (~machine-word-equal?(x-high, sign-word(x-high))
	      | ~machine-word-equal?(x-high, sign-word(x-low)))
	  machine-word-overflow()
	end;
	let shift :: dylan/<integer> = dylan/-(shift, $machine-word-size);
	if (shift = $machine-word-size)
	  // Primitives can't shift by the full size of a machine word so
	  // we'll perform two half-word shifts instead.  (Note that this
	  // code presumes the word size is even.)
	  shift := $machine-half-word-size;
	  x-low := machine-word-shift-left-signal-overflow(x-low, shift);
	end;
	let result-high :: <machine-word>
	  = machine-word-shift-left-signal-overflow(x-low, shift);
	interpret-result-as-abstract-integer(coerce-integer-to-machine-word(0), result-high)
      end
    end;
  right:
    begin
      if (shift < $machine-word-size)
	let result-high :: <machine-word> = machine-word-shift-right(x-high, shift);
	let low-high :: <machine-word> = machine-word-unsigned-rotate-right(x-high, shift);
	let low-low :: <machine-word> = machine-word-unsigned-shift-right(x-low, shift);
	let result-low :: <machine-word>
	   = machine-word-logior(low-low,
				 machine-word-logand(low-high,
						     machine-word-unsigned-shift-left
						       (coerce-integer-to-machine-word(-1),
							dylan/-($machine-word-size, shift))));
	interpret-result-as-abstract-integer(result-low, result-high)
      else
	// Shifting right by more than a word --
	//   Only high part of the <double-integer> contributes to the result.
	let shift :: dylan/<integer> = dylan/-(shift, $machine-word-size);
	if (shift < $machine-word-size)
	  let result-high :: <machine-word> = sign-word(x-high);
	  let result-low :: <machine-word> = machine-word-shift-right(x-high, shift);
	  interpret-result-as-abstract-integer(result-low, result-high)
	else
	  // Shifting right by 2 words (or more) just propogates the sign bit ...
	  if (negative?(x)) -1 else 0 end
	end
      end
    end;
end shift-methods ash;

define shift-methods lsh (x, shift) (x-low, x-high)
  left:
    begin
      if (shift < $machine-word-size)
	let (result-low :: <machine-word>, result-high :: <machine-word>)
	  = machine-word-unsigned-double-shift-left(x-low, x-high, shift);
	interpret-result-as-abstract-integer(result-low, result-high)
      else
	// Shifting left by more than a word --
	//    Only low part of the <double-integer> contributes to the result
	let shift :: dylan/<integer> = dylan/-(shift, $machine-word-size);
	if (shift < $machine-word-size)
	  let result-high :: <machine-word> = machine-word-unsigned-shift-left(x-low, shift);
	  interpret-result-as-abstract-integer(coerce-integer-to-machine-word(0), result-high)
	else
	  // Logical shifts by 2 words (or more) always return 0 ...
	  0
	end
      end
    end;
  right:
    begin
      if (shift < $machine-word-size)
	let (result-low :: <machine-word>, result-high :: <machine-word>)
	  = machine-word-unsigned-double-shift-right(x-low, x-high, shift);
	interpret-result-as-abstract-integer(result-low, result-high)
      else
	// Shifting right by more than a word --
	//   Only high part of the <double-integer> contributes to the result.
	let shift :: dylan/<integer> = dylan/-(shift, $machine-word-size);
	if (shift < $machine-word-size)
	  let result-low :: <machine-word> = machine-word-unsigned-shift-right(x-high, shift);
	  interpret-result-as-abstract-integer(result-low, coerce-integer-to-machine-word(0))
	else
	  // Logical shifts by 2 words (or more) always return 0 ...
	  0
	end
      end
    end;
end shift-methods lsh;


/// Defines four methods for a binary logical generic function.
/// The method for (dylan/<integer>, dylan/<integer>) simply invokes the corresponding
/// method in the Dylan library as it can't overflow or produce non-dylan/<integer> results.
/// The remaining three methods are fairly straightforward.
define macro binary-logical-methods-definer
  { define binary-logical-methods ?:name ?binary:name ?lowlevel:name }
    => { define sideways inline method ?binary (x :: dylan/<integer>, y :: dylan/<integer>)
          => (result :: <integer>)
	   "dylan/" ## ?name(x, y)
	 end method ?binary;
	 define sideways inline method ?binary (x :: dylan/<integer>, y :: <double-integer>)
          => (result :: <integer>)
	   let x-low :: <machine-word> = coerce-integer-to-machine-word(x);
	   let low :: <machine-word> = ?lowlevel(x-low, %double-integer-low(y));
	   let high :: <machine-word> = ?lowlevel(sign-word(x-low), %double-integer-high(y));
	   interpret-result-as-abstract-integer(low, high)
	 end method ?binary;
	 define sideways inline method ?binary (x :: <double-integer>, y :: dylan/<integer>)
          => (result :: <integer>)
	   let y-low :: <machine-word> = coerce-integer-to-machine-word(y);
	   let low :: <machine-word> = ?lowlevel(%double-integer-low(x), y-low);
	   let high :: <machine-word> = ?lowlevel(%double-integer-high(x), sign-word(y-low));
	   interpret-result-as-abstract-integer(low, high)
	 end method ?binary;
	 define sideways inline method ?binary (x :: <double-integer>, y :: <double-integer>)
          => (result :: <integer>)
	   let low :: <machine-word> = ?lowlevel(%double-integer-low(x),
						 %double-integer-low(y));
	   let high :: <machine-word> = ?lowlevel(%double-integer-high(x),
						  %double-integer-high(y));
	   interpret-result-as-abstract-integer(low, high)
	 end method ?binary }
  { define binary-logical-methods ?:name }
    => { define binary-logical-methods ?name "binary-" ## ?name "machine-word-" ## ?name }
end macro binary-logical-methods-definer;

define binary-logical-methods logior;
define binary-logical-methods logxor;
define binary-logical-methods logand;

define sideways inline method lognot (x :: dylan/<integer>) => (result :: <integer>)
  dylan/lognot(x)
end method lognot;

define sideways inline method lognot (x :: <double-integer>) => (result :: <integer>)
  let low :: <machine-word> = machine-word-lognot(%double-integer-low(x));
  let high :: <machine-word> = machine-word-lognot(%double-integer-high(x));
  interpret-result-as-abstract-integer(low, high)
end method lognot;

define sideways inline method logbit? (bitno :: dylan/<integer>, x :: dylan/<integer>)
 => (set? :: <boolean>)
  dylan/logbit?(bitno, x)
end method logbit?;

define sideways inline method logbit? (bitno :: dylan/<integer>, x :: <double-integer>)
 => (set? :: <boolean>)
  case
    bitno < 0 =>
      //---*** Should signal an error here?
      #f;
    bitno < $machine-word-size =>
      machine-word-logbit?(bitno, %double-integer-low(x));
    bitno < 2 * $machine-word-size =>
      machine-word-logbit?(dylan/-(bitno, $machine-word-size), %double-integer-high(x));
    otherwise =>
      negative?(x)
  end
end method logbit?;


/// Defines four methods on a binary algebraic generic function.
/// All four methods use the same hand-written body, relying on the compiler to optimize
/// the individual cases based on its knowledge of the parameter types.
define macro algebraic-methods-definer
  { define algebraic-methods ?:name (?n:name, ?m:name) ?:body end }
    => { define sideways sealed method ?name (?n :: dylan/<integer>, ?m :: dylan/<integer>)
	  => (result :: <integer>)
	   ?body
	 end method ?name; 
	 define sideways sealed method ?name (?n :: dylan/<integer>, ?m :: <double-integer>)
	  => (result :: <integer>)
	   ?body
	 end method ?name;
	 define sideways sealed method ?name (?n :: <double-integer>, ?m :: dylan/<integer>)
	  => (result :: <integer>)
	   ?body
	 end method ?name;
	 define sideways sealed method ?name (?n :: <double-integer>, ?m :: <double-integer>)
	  => (result :: <integer>)
	   ?body
	 end method ?name }
end macro algebraic-methods-definer;

///---*** NOTE: These methods won't work until <double-integer> division is fully implemented.
define algebraic-methods lcm (n, m)
  truncate/(max(n, m), gcd(n, m)) * min(n, m)
end algebraic-methods lcm;

///---*** NOTE: These methods won't work until <double-integer> division is fully implemented.
define algebraic-methods gcd (n, m)
  case
    n = 0 =>
      m;
    m = 0 =>
      n;
    n = m =>
      n;
    otherwise =>
      for (k :: <integer> from 0,
           u :: <integer> = abs(n) then ash(u, -1),
           v :: <integer> = abs(m) then ash(v, -1),
           until: odd?(logior(u, v)))
      finally
        block (return)
          for (tmp :: <integer>
                 = if (u.odd?)
                     v.negative
                   else
                     ash(u, -1)
                   end if
                 then ash(tmp, -1))
            if (tmp.odd?)
              if (tmp.positive?)
                u := tmp
              else
                v := tmp.negative
              end if;
              tmp := u - v;
              if (tmp.zero?)
                return(ash(u, k))
              end if
            end if;
          end for
        end block;
      end for
  end case
end algebraic-methods gcd;


define constant $number-characters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

///---*** NOTE: This really wants to be a method on integer-to-string but that's a function!
define sideways method number-to-string (n :: <double-integer>) => (s :: <string>)
  let buffer = make(<stretchy-vector>);
  local method process-integer (arg :: <integer>) => ()
          let (quotient, remainder) = truncate/(arg, 10);
          unless (zero?(quotient)) 
	    process-integer(quotient)
	  end;
          add!(buffer, $number-characters[remainder])
	end method process-integer;
  if (negative?(n))
    add!(buffer, '-');
    // Do the first digit by hand to avoid overflows ...
    let (quotient, remainder) = truncate/(n, 10);
    unless (zero?(quotient)) 
      process-integer(-quotient)
    end;
    add!(buffer, $number-characters[-remainder]);
  else
    process-integer(n);
  end;
  as(<string>, buffer)
end method number-to-string;
