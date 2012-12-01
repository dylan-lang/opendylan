Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <float> ... end;
// BOOTED: define ... class <single-float> ... end;
// BOOTED: define ... class <double-float> ... end;

define sealed method make (class == <single-float>, #rest all-keys, #key) => (res)
  uninstantiable-error(class);
end method;

define sealed method make (class == <double-float>, #rest all-keys, #key) => (res)
  uninstantiable-error(class);
end method;

///---*** NOTE: Is there a way to eliminate contagious-type???

// The methods more specific than <float> are just here because the compiler
// can't fold object-class({type estimate: <some-leaf-class>}) yet.

define inline method contagious-type
    (x :: <abstract-integer>, y :: <float>) => (result :: <class>)
  object-class(y)
end method contagious-type;

define inline method contagious-type
    (x :: <abstract-integer>, y :: <single-float>) => (result == <single-float>)
  <single-float>
end method contagious-type;

define inline method contagious-type
    (x :: <abstract-integer>, y :: <double-float>) => (result == <double-float>)
  <double-float>
end method contagious-type;

define inline method contagious-type
    (x :: <float>, y :: <abstract-integer>) => (result :: <class>)
  object-class(x)
end method contagious-type;

define inline method contagious-type
    (x :: <single-float>, y :: <abstract-integer>) => (result == <single-float>)
  <single-float>
end method contagious-type;

define inline method contagious-type
    (x :: <double-float>, y :: <abstract-integer>) => (result == <double-float>)
  <double-float>
end method contagious-type;

define inline method contagious-type
    (x :: <single-float>, y :: <single-float>) => (result == <single-float>)
  <single-float>
end method contagious-type;

define inline method contagious-type
    (x :: <double-float>, y :: <double-float>) => (result == <double-float>)
  <double-float>
end method contagious-type;

define inline method contagious-type
    (x :: <double-float>, y :: <single-float>) => (result == <double-float>)
  <double-float>
end method contagious-type;

define inline method contagious-type
    (x :: <single-float>, y :: <double-float>) => (result == <double-float>)
  <double-float>
end method contagious-type;

define sealed inline method as (class == <float>, x :: <float>) => (result :: <float>)
  x
end method as;

// These are needed because the more specific single/double combos seem to
// prevent the above method from being selected.

define sealed inline method as
    (class == <single-float>, x :: <single-float>)
 => (result :: <single-float>)
  x
end method as;

define sealed inline method as
    (class == <double-float>, x :: <double-float>)
 => (result :: <double-float>)
  x
end method as;

/// NOTE: Should we implement <extended-float>, convert to it instead of
/// <double-float> if the <abstract-integer> has more than 53 significant
/// bits, the size of a <double-float>'s mantissa
define sealed inline method as (class == <float>, x :: <abstract-integer>)
 => (result :: <double-float>)
  as(<double-float>, x)
end method as;

define constant $maximum-single-float-mantissa = ash(1, 24) - 2;
define constant $minimum-single-float-mantissa = -(ash(1, 24) - 2);

/// If the <integer> has more bits than can be represented by a <single-float>'s
/// mantissa (i.e., 24), we'll convert it to a <double-float> instead
define sealed inline method as (class == <float>, x :: <integer>)
 => (result :: <float>)
  if (x < $minimum-single-float-mantissa | x > $maximum-single-float-mantissa)
    as(<double-float>, x)
  else
    as(<single-float>, x)
  end
end method as;


//// CONDITIONS -- These functions are invoked by the low-level runtime
////               when the indicated hardware exception is raised.

define function float-divide-by-0 ()
  error(make(<division-by-zero-error>))
end function float-divide-by-0;

define function float-invalid ()
  error(make(<arithmetic-domain-error>))
end function float-invalid;

define function float-overflow ()
  error(make(<arithmetic-overflow-error>))
end function float-overflow;

define function float-underflow ()
  error(make(<arithmetic-underflow-error>))
end function float-underflow;


/// SINGLE FLOAT

///---*** Is this really necessary?
define method shallow-copy (x :: <single-float>) => (copy :: <single-float>)
  primitive-raw-as-single-float(primitive-single-float-as-raw(x))
end method shallow-copy;

define sealed inline method as (class == <single-float>, x :: <integer>)
 => (result :: <single-float>)
  primitive-raw-as-single-float(primitive-integer-as-single-float(integer-as-raw(x)))
end method as;

define sealed inline method as (class == <single-float>, x :: <double-float>)
 => (result :: <single-float>)
  primitive-raw-as-single-float
    (primitive-double-float-as-single(primitive-double-float-as-raw(x)))
end method as;

define inline-only function decode-single-float (x :: <single-float>)
 => (decoded :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-single-float-as-machine-word(primitive-single-float-as-raw(x)))
end function decode-single-float;

define inline-only function encode-single-float (x :: <machine-word>)
 => (encoded :: <single-float>)
  primitive-raw-as-single-float
    (primitive-cast-machine-word-as-single-float(primitive-unwrap-machine-word(x)))
end function encode-single-float;

define sealed inline method \=
    (x :: <single-float>, y :: <single-float>) => (result :: <boolean>)
  primitive-single-float-equals?
    (primitive-single-float-as-raw(x),
     primitive-single-float-as-raw(y))
end method \=;

define sealed inline method \<
    (x :: <single-float>, y :: <single-float>) => (result :: <boolean>)
  primitive-single-float-less-than?
    (primitive-single-float-as-raw(x),
     primitive-single-float-as-raw(y))
end method \<;

define sealed inline method zero? (x :: <single-float>) => (result :: <boolean>)
  primitive-single-float-equals?
    (primitive-single-float-as-raw(x),
     primitive-single-float-as-raw(0.0))
end method zero?;

define sealed inline method positive? (x :: <single-float>) => (result :: <boolean>)
  primitive-single-float-less-than?
    (primitive-single-float-as-raw(0.0),
     primitive-single-float-as-raw(x))
end method positive?;

define sealed inline method negative? (x :: <single-float>) => (result :: <boolean>)
  primitive-single-float-less-than?
    (primitive-single-float-as-raw(x),
     primitive-single-float-as-raw(0.0))
end method negative?;

define sealed inline method integral? (x :: <single-float>) => (result :: <boolean>)
  let (_integer, remainder :: <single-float>) = truncate/(x, 1.0);
  remainder = 0.0
end method integral?;

define sealed inline method \+
    (x :: <single-float>, y :: <single-float>) => (z :: <single-float>)
  primitive-raw-as-single-float
    (primitive-single-float-add
       (primitive-single-float-as-raw(x),
        primitive-single-float-as-raw(y)))
end method \+;

define sealed inline method \-
    (x :: <single-float>, y :: <single-float>) => (z :: <single-float>)
  primitive-raw-as-single-float
    (primitive-single-float-subtract
       (primitive-single-float-as-raw(x),
        primitive-single-float-as-raw(y)))
end method \-;

define sealed inline method \*
    (x :: <single-float>, y :: <single-float>) => (z :: <single-float>)
  primitive-raw-as-single-float
    (primitive-single-float-multiply
       (primitive-single-float-as-raw(x),
        primitive-single-float-as-raw(y)))
end method \*;

define sealed inline method \/
    (x :: <single-float>, y :: <single-float>) => (z :: <single-float>)
  primitive-raw-as-single-float
    (primitive-single-float-divide
       (primitive-single-float-as-raw(x),
        primitive-single-float-as-raw(y)))
end method \/;

define sealed inline method negative
    (x :: <single-float>) => (z :: <single-float>)
  primitive-raw-as-single-float
    (primitive-single-float-negate(primitive-single-float-as-raw(x)))
end method negative;

define sealed inline method truncate/ (real :: <single-float>, divisor :: <single-float>)
 => (result :: <integer>, remainder :: <single-float>)
  let divided = real / divisor;
  let result = raw-as-integer
                 (primitive-single-float-as-integer
                    (primitive-single-float-as-raw(divided)));
  values(result, divisor * (divided - as(<single-float>, result)))
end method truncate/;

define sealed inline method \^ (base :: <single-float>, power :: <integer>)
 => (result :: <single-float>)
  let negative-result? = negative?(base) & odd?(power);
  let result = primitive-raw-as-single-float
                 (primitive-single-float-expt
                    (primitive-single-float-as-raw(abs(base)),
                     primitive-integer-as-single-float(integer-as-raw(power))));
  if (negative-result?) negative(result) else result end
end method \^;


/// DOUBLE FLOAT

///---*** Is this really necessary?
define method shallow-copy (x :: <double-float>) => (copy :: <double-float>)
  primitive-raw-as-double-float(primitive-double-float-as-raw(x))
end method shallow-copy;

define sealed inline method as (class == <double-float>, x :: <integer>)
 => (result :: <double-float>)
  primitive-raw-as-double-float(primitive-integer-as-double-float(integer-as-raw(x)))
end method as;

define sealed inline method as (class == <double-float>, x :: <single-float>)
 => (result :: <double-float>)
  primitive-raw-as-double-float
    (primitive-single-float-as-double(primitive-single-float-as-raw(x)))
end method as;

define inline-only function decode-double-float (x :: <double-float>)
 => (low :: <machine-word>, high :: <machine-word>)
  let (low :: <raw-machine-word>, high :: <raw-machine-word>)
    = primitive-cast-double-float-as-machine-words(primitive-double-float-as-raw(x));
  values(primitive-wrap-machine-word(low), primitive-wrap-machine-word(high))
end function decode-double-float;

define inline-only function encode-double-float
    (low :: <machine-word>, high :: <machine-word>) => (encoded :: <double-float>)
  primitive-raw-as-double-float
    (primitive-cast-machine-words-as-double-float(primitive-unwrap-machine-word(low),
                                                  primitive-unwrap-machine-word(high)))
end function encode-double-float;

define sealed inline method \=
    (x :: <double-float>, y :: <double-float>) => (result :: <boolean>)
  primitive-double-float-equals?
    (primitive-double-float-as-raw(x),
     primitive-double-float-as-raw(y))
end method \=;

define sealed inline method \<
    (x :: <double-float>, y :: <double-float>) => (result :: <boolean>)
  primitive-double-float-less-than?
    (primitive-double-float-as-raw(x),
     primitive-double-float-as-raw(y))
end method \<;

define sealed inline method zero? (x :: <double-float>) => (result :: <boolean>)
  primitive-double-float-equals?
    (primitive-double-float-as-raw(x),
     primitive-double-float-as-raw(0.0d0))
end method zero?;

define sealed inline method positive? (x :: <double-float>) => (result :: <boolean>)
  primitive-double-float-less-than?
    (primitive-double-float-as-raw(0.0d0),
     primitive-double-float-as-raw(x))
end method positive?;

define sealed inline method negative? (x :: <double-float>) => (result :: <boolean>)
  primitive-double-float-less-than?
    (primitive-double-float-as-raw(x),
     primitive-double-float-as-raw(0.0d0))
end method negative?;

define sealed inline method integral? (x :: <double-float>) => (result :: <boolean>)
  let (_integer, remainder :: <double-float>) = truncate/(x, 1.0d0);
  remainder = 0.0d0
end method integral?;

define sealed inline method \+
    (x :: <double-float>, y :: <double-float>) => (z :: <double-float>)
  primitive-raw-as-double-float
    (primitive-double-float-add
       (primitive-double-float-as-raw(x),
        primitive-double-float-as-raw(y)))
end method \+;

define sealed inline method \-
    (x :: <double-float>, y :: <double-float>) => (z :: <double-float>)
  primitive-raw-as-double-float
    (primitive-double-float-subtract
       (primitive-double-float-as-raw(x),
        primitive-double-float-as-raw(y)))
end method \-;

define sealed inline method \*
    (x :: <double-float>, y :: <double-float>) => (z :: <double-float>)
  primitive-raw-as-double-float
    (primitive-double-float-multiply
       (primitive-double-float-as-raw(x),
        primitive-double-float-as-raw(y)))
end method \*;

define sealed inline method \/
    (x :: <double-float>, y :: <double-float>) => (z :: <double-float>)
  primitive-raw-as-double-float
    (primitive-double-float-divide
       (primitive-double-float-as-raw(x),
        primitive-double-float-as-raw(y)))
end method \/;

define sealed inline method negative
    (x :: <double-float>) => (z :: <double-float>)
  primitive-raw-as-double-float
    (primitive-double-float-negate(primitive-double-float-as-raw(x)))
end method negative;

define sealed inline method truncate/ (real :: <double-float>, divisor :: <double-float>)
 => (result :: <integer>, remainder :: <double-float>)
  let divided = real / divisor;
  let result = raw-as-integer
                 (primitive-double-float-as-integer
                    (primitive-double-float-as-raw(divided)));
  values(result, divisor * (divided - as(<double-float>, result)))
end method truncate/;

define sealed inline method \^ (base :: <double-float>, power :: <integer>)
 => (result :: <double-float>)
  let negative-result? = negative?(base) & odd?(power);
  let result = primitive-raw-as-double-float
                 (primitive-double-float-expt
                    (primitive-double-float-as-raw(abs(base)),
                     primitive-integer-as-double-float(integer-as-raw(power))));
  if (negative-result?) negative(result) else result end
end method \^;


/// EXTENDED FLOAT

///--- NOTE: In our implementation, <extended-float> == <double-float> ...

define constant <extended-float> = <double-float>;
