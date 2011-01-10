Module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// Some useful type constants.

// A raw machine word which is non-negative and less than the word size.
// This range doesn't actually need to be checked; the caller is expected
// to do the range checking.
define inline constant <raw-bit-number> = <raw-machine-word>;

//////////////////////////////////////////////////////////////////////////////
// Conversions between integers and machine integers
//
// Most of the conversion definitions are located in files specific to to
// particular integer representation scheme.  Only the common definitions
// are located here.

define inline-only function coerce-abstract-integer-to-machine-word
    (x :: <abstract-integer>) => (result :: <machine-word>)
  primitive-wrap-machine-word(primitive-unwrap-abstract-integer(x))
end function coerce-abstract-integer-to-machine-word;

define inline-only function coerce-machine-word-to-unsigned-abstract-integer
     (x :: <machine-word>) => (result :: <abstract-integer>)
  primitive-wrap-unsigned-abstract-integer(primitive-unwrap-machine-word(x))
end function coerce-machine-word-to-unsigned-abstract-integer;

define inline function as-unsigned (t :: <type>, m :: <machine-word>)
  as(t, coerce-machine-word-to-unsigned-abstract-integer(m))
end function as-unsigned;

/// These "convenience" functions are exported for external use.

///---*** Compiler "bug" causes bogus warnings/code if return type is declared!
define inline-only function integer-as-raw (x :: <integer>)
 /*=> (raw-x :: <raw-machine-word>)*/
  primitive-unwrap-machine-word(coerce-integer-to-machine-word(x))
end function integer-as-raw;

///---*** Compiler "bug" causes bogus warnings/code if return type is declared!
define inline-only function interpret-integer-as-raw (x :: <integer>)
 /*=> (raw-x :: <raw-machine-word>)*/
  primitive-unwrap-machine-word(interpret-integer-as-machine-word(x))
end function interpret-integer-as-raw;

interpret-integer-as-raw;  // USED IN COMPILER

define inline-only function raw-as-integer (raw-x :: <raw-machine-word>)
 => (x :: <integer>)
  coerce-machine-word-to-integer(primitive-wrap-machine-word(raw-x))
end function raw-as-integer;

///---*** Compiler "bug" causes bogus warnings/code if return type is declared!
define inline-only function abstract-integer-as-raw (x :: <abstract-integer>)
 /*=> (raw-x :: <raw-machine-word>)*/
  primitive-unwrap-machine-word(coerce-abstract-integer-to-machine-word(x))
end function abstract-integer-as-raw;

define inline-only function raw-as-abstract-integer (raw-x :: <raw-machine-word>)
 => (x :: <abstract-integer>)
  coerce-machine-word-to-abstract-integer(primitive-wrap-machine-word(raw-x))
end function raw-as-abstract-integer;

/// A "<hash-index>" is always an <integer> so, if a <machine-word> is too large
/// to be converted into an <integer>, we'll just take its most significant bits
/// for use as the <hash-index>.
define inline-only function machine-word-as-hash-index (x :: <machine-word>)
 => (hi :: <integer>)
  if (machine-word-is-integer?(x))
    coerce-machine-word-to-integer(x)
  else
    interpret-machine-word-as-integer(force-integer-tag(x))
  end
end function machine-word-as-hash-index;

/// TODO: SHOULD BE OPTIMIZER THAT MAKES THIS IRRELEVANT
define sealed inline method as 
    (class == <machine-word>, x :: <machine-word>) => (res :: <machine-word>)
  x
end method;

//////////////////////////////////////////////////////////////////////////////
// Machine word size

define inline-only function word-size () => (result :: <integer>)
  // !@#$ this may not ultimately be the correct primitive for this, 
  // In kab's spec for machine words there never was a primitive for
  // turning a raw machine word directly into an integer.  A primitive
  // is needed here to prevent infinite recursion, and this value is
  // guaranteed to be in the fixnum range.
  raw-as-integer
    (primitive-machine-word-multiply-low
       (integer-as-raw(8), primitive-word-size()))
end function word-size;

//////////////////////////////////////////////////////////////////////////////
// Comparison

define macro comparison-definer
  { define comparison ?:name }
    => { define inline-only function ?name (x :: <machine-word>, y :: <machine-word>)
	  => (result :: <boolean>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
	   "primitive-" ## ?name(raw-x, raw-y)
	 end function ?name
       }
end macro;

define comparison machine-word-equal?;
define comparison machine-word-less-than?;

//////////////////////////////////////////////////////////////////////////////
// Comparison predicates

///---*** NOTE: As a temporary fix for bug 628, the comparison operators for
///---*** <machine-word> vs. <abstract-integer> will coerce the <abstract-integer>
///---*** into a <machine-word> which can potentially overflow.  Once the necessary
///---*** changes are made to the FFI to allow primitive-wrap-abstract-integer
///---*** to actually return <abstract-integer>s instead of <machine-word>s,
///---*** restore the commented-out versions of these predicates!
define macro comparison-predicate-definer
  { define comparison-predicate ?:name ?lowlevel:name }
  => { define sealed inline method ?name
           (x :: <machine-word>, y :: <machine-word>)
        => (result :: <boolean>)
         ?lowlevel(x, y)
       end method ?name;
       define sealed inline method ?name
           (x :: <machine-word>, y :: <abstract-integer>)
        => (result :: <boolean>)
         ?name(x, coerce-abstract-integer-to-machine-word(y))
         //---*** ?name(coerce-machine-word-to-abstract-integer(x), y)
       end method ?name;
       define sealed inline method ?name
           (x :: <abstract-integer>, y :: <machine-word>)
        => (result :: <boolean>)
         ?name(coerce-abstract-integer-to-machine-word(x), y)
         //---*** ?name(x, coerce-machine-word-to-abstract-integer(y))
       end method ?name;
       }
end macro;

define comparison-predicate \= machine-word-equal?;
define comparison-predicate \< machine-word-less-than?;

//////////////////////////////////////////////////////////////////////////////
// Logical

define macro logical-definer
  { define logical ?:name }
    => { define inline-only function ?name
             (x :: <machine-word>, y :: <machine-word>)
          => (result :: <machine-word>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
	   primitive-wrap-machine-word("primitive-" ## ?name(raw-x, raw-y))
         end function ?name
       }
end macro;

define logical machine-word-logior;
define logical machine-word-logxor;
define logical machine-word-logand;

define inline-only function machine-word-lognot (x :: <machine-word>)
 => (result :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-machine-word-lognot(primitive-unwrap-machine-word(x)))
end function machine-word-lognot;

define inline-only function machine-word-logbit?
    (index :: <integer>, x :: <machine-word>)
 => (result :: <boolean>)
  let raw-index :: <raw-bit-number> = integer-as-raw(index);
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  primitive-machine-word-logbit?(raw-index, raw-x)
end function machine-word-logbit?;

define inline-only function machine-word-logbit-deposit
    (z :: <boolean>, index :: <integer>, x :: <machine-word>)
 => (result :: <machine-word>)
  let raw-index :: <raw-bit-number> = integer-as-raw(index);
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  if (z)
    primitive-wrap-machine-word
      (primitive-machine-word-logbit-set(raw-index, raw-x))
  else
    primitive-wrap-machine-word
      (primitive-machine-word-logbit-clear(raw-index, raw-x))
  end if
end function machine-word-logbit-deposit;

define inline-only function machine-word-bit-field-extract
    (offset :: <integer>, size :: <integer>, x :: <machine-word>)
 => (result :: <machine-word>)
  let raw-offset :: <raw-bit-number> = integer-as-raw(offset);
  let raw-size :: <raw-bit-number> = integer-as-raw(size);
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  primitive-wrap-machine-word
    (primitive-machine-word-bit-field-extract
       (raw-offset, raw-size, raw-x))
end function machine-word-bit-field-extract;

define inline-only function machine-word-bit-field-deposit
    (field :: <machine-word>, offset :: <integer>, size :: <integer>, x :: <machine-word>)
 => (result :: <machine-word>)
  let raw-field :: <raw-machine-word> = primitive-unwrap-machine-word(field);
  let raw-offset :: <raw-bit-number> = integer-as-raw(offset);
  let raw-size :: <raw-bit-number> = integer-as-raw(size);
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  primitive-wrap-machine-word
    (primitive-machine-word-bit-field-deposit
       (raw-field, raw-offset, raw-size, raw-x))
end function machine-word-bit-field-deposit;

define macro counter-definer
  { define counter ?:name }
    => { define inline-only function ?name (x :: <machine-word>)
	  => (result :: <integer>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   raw-as-integer("primitive-" ## ?name(raw-x))
	 end function ?name
       }
end macro;

define counter machine-word-count-low-zeros;
define counter machine-word-count-high-zeros;

//////////////////////////////////////////////////////////////////////////////
// Arithmetic

define macro simple-arithmetic-definer
  { define simple-arithmetic ?:name }
    => { define inline-only function ?name
             (x :: <machine-word>, y :: <machine-word>)
          => (result :: <machine-word>, overflow? :: <boolean>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
	   let (result :: <raw-machine-word>, overflow? :: <boolean>)
		 = "primitive-" ## ?name(raw-x, raw-y);
	   values(primitive-wrap-machine-word(result), overflow?)
	 end function ?name
       }
end macro;

define simple-arithmetic machine-word-add-with-overflow;
define simple-arithmetic machine-word-subtract-with-overflow;

define inline-only function machine-word-multiply-with-overflow
    (x :: <machine-word>, y :: <machine-word>)
 => (result-low :: <machine-word>, result-high :: <machine-word>, overflow? :: <boolean>)
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
  let (low :: <raw-machine-word>,
       high :: <raw-machine-word>,
       overflow? :: <boolean>)
        = primitive-machine-word-multiply-with-overflow(raw-x, raw-y);
  values(primitive-wrap-machine-word(low),
         primitive-wrap-machine-word(high),
         overflow?)
end function machine-word-multiply-with-overflow;

define macro sign-adjust-definer
  { define sign-adjust ?:name }
    => { define inline-only function ?name (x :: <machine-word>)
	  => (result :: <machine-word>, overflow? :: <boolean>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let (result :: <raw-machine-word>, overflow? :: <boolean>)
		 = "primitive-" ## ?name(raw-x);
	   values(primitive-wrap-machine-word(result), overflow?)
	 end function ?name
       }
end macro;

define sign-adjust machine-word-negative-with-overflow;
define sign-adjust machine-word-abs-with-overflow;

//////////////////////////////////////////////////////////////////////////////
// Division

define macro signed-division-definer
  { define signed-division ?:name }
    => { define inline-only function ?name
             (dividend :: <machine-word>, divisor :: <machine-word>)
          => (quotient :: <machine-word>, remainder :: <machine-word>)
	   let raw-dividend :: <raw-machine-word>
		 = primitive-unwrap-machine-word(dividend);
	   let raw-divisor :: <raw-machine-word>
		 = primitive-unwrap-machine-word(divisor);
	   let (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>)
		 = "primitive-" ## ?name(raw-dividend, raw-divisor);
	   values(primitive-wrap-machine-word(quotient),
		  primitive-wrap-machine-word(remainder))
	 end function ?name
       }
end macro;

define signed-division machine-word-floor/;
define signed-division machine-word-ceiling/;
define signed-division machine-word-round/;
define signed-division machine-word-truncate/;
define signed-division machine-word-divide;

//////////////////////////////////////////////////////////////////////////////
// Shift

define inline-only function machine-word-shift-left-with-overflow
    (x :: <machine-word>, count :: <integer>)
 => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>)
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  let raw-count :: <raw-bit-number> = integer-as-raw(count);
  let (low :: <raw-machine-word>,
       high :: <raw-machine-word>,
       overflow? :: <boolean>)
        = primitive-machine-word-shift-left-with-overflow(raw-x, raw-count);
  values(primitive-wrap-machine-word(low),
         primitive-wrap-machine-word(high),
         overflow?)
end function machine-word-shift-left-with-overflow;

define inline-only function machine-word-shift-right
    (x :: <machine-word>, count :: <integer>)
 => (result :: <machine-word>)
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  // !@#$ this may not ultimately be the correct primitive for this, 
  // In kab's spec for machine words there never was a primitive for
  // turning an integer directly into a raw machine word.  A primitive
  // is needed here to prevent infinite recursion, and this value is
  // guaranteed to be in the fixnum range.
  let raw-count :: <raw-bit-number> = integer-as-raw(count);
  primitive-wrap-machine-word
    (primitive-machine-word-shift-right(raw-x, raw-count))
end function machine-word-shift-right;

//////////////////////////////////////////////////////////////////////////////
// Unsigned

define macro unsigned-arithmetic-definer
  { define unsigned-arithmetic ?:name }
    => { define inline-only function ?name
             (x :: <machine-word>, y :: <machine-word>)
          => (v1 :: <machine-word>, v2 :: <machine-word>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
	   let (v1 :: <raw-machine-word>, v2 :: <raw-machine-word>)
		 = "primitive-" ## ?name(raw-x, raw-y);
	   values(primitive-wrap-machine-word(v1),
		  primitive-wrap-machine-word(v2))
	 end function ?name
       }
end macro;

define unsigned-arithmetic machine-word-unsigned-add-with-carry;
define unsigned-arithmetic machine-word-unsigned-subtract-with-borrow;
define unsigned-arithmetic machine-word-unsigned-multiply;
define unsigned-arithmetic machine-word-unsigned-divide;

define macro unsigned-shift-definer
  { define unsigned-shift ?:name }
    => { define inline-only function ?name
             (x :: <machine-word>, count :: <integer>)
          => (result :: <machine-word>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let raw-count :: <raw-bit-number> = integer-as-raw(count);
	   primitive-wrap-machine-word
	     ("primitive-" ## ?name(raw-x, raw-count))
	 end function ?name
       }
end macro;

define unsigned-shift machine-word-unsigned-rotate-left;
define unsigned-shift machine-word-unsigned-rotate-right;
define unsigned-shift machine-word-unsigned-shift-right;

define inline-only function machine-word-unsigned-shift-left
    (x :: <machine-word>, count :: <integer>)
 => (result :: <machine-word>)
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  let raw-count :: <raw-bit-number> = integer-as-raw(count);
  primitive-wrap-machine-word
    (primitive-machine-word-unsigned-double-shift-left-high
       (integer-as-raw(0), raw-x, raw-count))
end function machine-word-unsigned-shift-left;

define inline-only function machine-word-unsigned-less-than?
    (x :: <machine-word>, y :: <machine-word>)
 => (result :: <boolean>)
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
  primitive-machine-word-unsigned-less-than?(raw-x, raw-y)
end function machine-word-unsigned-less-than?;

//////////////////////////////////////////////////////////////////////////////
// Signal overflow
//   We use <arithmetic-overflow-error> to signal this condition but override
//   the default message to supply a more detailed explanation of the cause.

/// This function is also invoked by the low-level runtime in response to a hardware exception
define function machine-word-overflow ()
  error(make(<arithmetic-overflow-error>,
	     format-string: "Integer overflow: The result of the last operation is "
	                      "too large for this integer representation."))
end function machine-word-overflow;

define macro signal-overflow-definer
  { define signal-overflow ?:name }
    => { define inline-only function ?name
	     (x :: <machine-word>, y :: <machine-word>)
	  => (result :: <machine-word>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   let raw-y :: <raw-machine-word> = primitive-unwrap-machine-word(y);
	   primitive-wrap-machine-word("primitive-" ## ?name(raw-x, raw-y))
	 end function ?name
       }
end macro;

define signal-overflow machine-word-add-signal-overflow;
define signal-overflow machine-word-subtract-signal-overflow;
define signal-overflow machine-word-multiply-signal-overflow;

define macro signal-overflow-sign-adjust-definer
  { define signal-overflow-sign-adjust ?:name }
    => { define inline-only function ?name (x :: <machine-word>)
	  => (result :: <machine-word>)
	   let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
	   primitive-wrap-machine-word("primitive-" ## ?name(raw-x))
	 end function ?name
       }
end macro;

define signal-overflow-sign-adjust machine-word-negative-signal-overflow;
define signal-overflow-sign-adjust machine-word-abs-signal-overflow;

define inline-only function machine-word-shift-left-signal-overflow
    (x :: <machine-word>, count :: <integer>)
 => (result :: <machine-word>)
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  let raw-count :: <raw-bit-number> = integer-as-raw(count);
  primitive-wrap-machine-word
    (primitive-machine-word-shift-left-signal-overflow(raw-x, raw-count))
end function machine-word-shift-left-signal-overflow;

//////////////////////////////////////////////////////////////////////////////
// Double division

define macro double-division-definer
  { define double-division ?:name }
    => { define inline-only function ?name
             (dividend-low :: <machine-word>,
              dividend-high :: <machine-word>,
	      divisor :: <machine-word>)
	  => (quotient :: <machine-word>, remainder :: <machine-word>)
	   let raw-low :: <raw-machine-word>
		 = primitive-unwrap-machine-word(dividend-low);
	   let raw-high :: <raw-machine-word>
		 = primitive-unwrap-machine-word(dividend-high);
	   let raw-div :: <raw-machine-word>
		 = primitive-unwrap-machine-word(divisor);
	   let (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>)
		 = "primitive-" ## ?name(raw-low, raw-high, raw-div);
	   values(primitive-wrap-machine-word(quotient),
		  primitive-wrap-machine-word(remainder))
	 end function ?name
       }
end macro;

define double-division machine-word-double-floor/;
define double-division machine-word-double-ceiling/;
define double-division machine-word-double-round/;
define double-division machine-word-double-truncate/;
define double-division machine-word-double-divide;

//////////////////////////////////////////////////////////////////////////////
// Unsigned double

define inline-only function machine-word-unsigned-double-divide
   (dividend-low :: <machine-word>,
    dividend-high :: <machine-word>,
    divisor :: <machine-word>)
 => (quotient :: <machine-word>, remainder :: <machine-word>)
  let raw-low :: <raw-machine-word>
        = primitive-unwrap-machine-word(dividend-low);
  let raw-high :: <raw-machine-word>
        = primitive-unwrap-machine-word(dividend-high);
  let raw-div :: <raw-machine-word>
        = primitive-unwrap-machine-word(divisor);
  let (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>)
        = primitive-machine-word-unsigned-double-divide(raw-low, raw-high, raw-div);
  values(primitive-wrap-machine-word(quotient),
         primitive-wrap-machine-word(remainder))
end function machine-word-unsigned-double-divide;

define macro unsigned-double-shift-definer
  { define unsigned-double-shift ?:name }
    => { define inline-only function ?name
             (low :: <machine-word>, high :: <machine-word>, count :: <integer>)
	  => (low :: <machine-word>, high :: <machine-word>)
	   let raw-low :: <raw-machine-word> = primitive-unwrap-machine-word(low);
	   let raw-high :: <raw-machine-word> = primitive-unwrap-machine-word(high);
	   let raw-count :: <raw-bit-number> = integer-as-raw(count);
	   let (low :: <raw-machine-word>, high :: <raw-machine-word>)
		 = "primitive-" ## ?name(raw-low, raw-high, raw-count);
	   values(primitive-wrap-machine-word(low),
		  primitive-wrap-machine-word(high))
	 end function ?name
       }
end macro;

define unsigned-double-shift machine-word-unsigned-double-shift-left;
define unsigned-double-shift machine-word-unsigned-double-shift-right;
