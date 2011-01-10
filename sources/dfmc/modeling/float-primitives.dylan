module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// SINGLE-FLOAT

define function raw-sf-op 
    (op :: <function>, rx :: <&raw-single-float>, ry :: <&raw-single-float>)
 => (res)
  let x :: <single-float> = ^raw-object-value(rx);
  let y :: <single-float> = ^raw-object-value(ry);
  make-raw-literal(op(x, y))
end function raw-sf-op;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-as-raw
    (x :: <single-float>) => (r :: <raw-single-float>)
  ^%single-float-data(x) 
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-raw-as-single-float
    (r :: <raw-single-float>) => (x :: <single-float>)
  make-compile-time-literal(^raw-object-value(r)) 
end;
/// NOTE: The Dylan library expects this primitive to round towards zero (i.e., truncate)
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-as-integer
    (f :: <raw-single-float>) => (i :: <raw-integer>)
  make-raw-literal(round(^raw-object-value(f)))
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-integer-as-single-float
    (x :: <raw-integer>) => (z :: <raw-single-float>)
  make-raw-literal(as(<single-float>, ^raw-object-value(x)))
end;
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-as-double-integer
    (f :: <raw-single-float>) => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-integer-as-single-float
    (low :: <raw-machine-word>, high :: <raw-machine-word>) => (f :: <raw-single-float>);

define side-effect-free stateless dynamic-extent &primitive primitive-cast-single-float-as-machine-word
    (f :: <raw-single-float>) => (b :: <raw-machine-word>);
define side-effect-free stateless dynamic-extent &primitive primitive-cast-machine-word-as-single-float
    (b :: <raw-machine-word>) => (f :: <raw-single-float>);

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-negate
    (x :: <raw-single-float>) => (negated :: <raw-single-float>)
  let x :: <single-float> = ^raw-object-value(x);
  make-raw-literal(0.0 - x)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-add
    (x :: <raw-single-float>, y :: <raw-single-float>) => (sum :: <raw-single-float>)
  raw-sf-op(\+, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-subtract
    (x :: <raw-single-float>, y :: <raw-single-float>) => (difference :: <raw-single-float>)
  raw-sf-op(\-, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-multiply
    (x :: <raw-single-float>, y :: <raw-single-float>) => (product :: <raw-single-float>)
  raw-sf-op(\*, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-divide
    (x :: <raw-single-float>, y :: <raw-single-float>) => (ratio :: <raw-single-float>)
  raw-sf-op(\/, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-equals?
    (x :: <raw-single-float>, y :: <raw-single-float>) => (equal? :: <boolean>)
  raw-sf-op(\=, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-less-than?
    (x :: <raw-single-float>, y :: <raw-single-float>) => (less? :: <boolean>)
  raw-sf-op(\<, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive primitive-single-float-sqrt
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-log
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-exp
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-expt
    (base :: <raw-single-float>, power :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-sin
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-cos
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-tan
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-asin
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-acos
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-single-float-atan
    (x :: <raw-single-float>) => (z :: <raw-single-float>);

/// DOUBLE-FLOAT

define function raw-df-op 
    (op :: <function>, rx :: <&raw-double-float>, ry :: <&raw-double-float>)
 => (z :: <&raw-double-float>)
  let x :: <double-float> = ^raw-object-value(rx);
  let y :: <double-float> = ^raw-object-value(ry);
  make-raw-literal(op(x, y))
end function raw-df-op;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-as-raw
    (x :: <double-float>) => (r :: <raw-double-float>)
  ^%double-float-data(x) 
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-raw-as-double-float
    (r :: <raw-double-float>) => (x :: <double-float>)
  make-compile-time-literal(^raw-object-value(r))
end;

/// NOTE: The Dylan library expects this primitive to round towards zero (i.e., truncate)
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-as-integer
    (f :: <raw-double-float>) => (i :: <raw-integer>)
  make-raw-literal(round(^raw-object-value(f)))
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-integer-as-double-float
    (x :: <raw-integer>) => (z :: <raw-double-float>)
  make-raw-literal(as(<double-float>, ^raw-object-value(x)))
end;
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-as-double-integer
    (f :: <raw-double-float>) => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-integer-as-double-float
    (low :: <raw-machine-word>, high :: <raw-machine-word>) => (f :: <raw-double-float>);

define side-effect-free stateless dynamic-extent &primitive primitive-cast-double-float-as-machine-words
    (f :: <raw-double-float>) => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define side-effect-free stateless dynamic-extent &primitive primitive-cast-machine-words-as-double-float
    (low :: <raw-machine-word>, high :: <raw-machine-word>) => (f :: <raw-double-float>);

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-negate
    (x :: <raw-double-float>) => (negated :: <raw-double-float>)
  let x :: <double-float> = ^raw-object-value(x);
  make-raw-literal(0.0d0 - x)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-add
    (x :: <raw-double-float>, y :: <raw-double-float>) => (sum :: <raw-double-float>)
  raw-df-op(\+, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-subtract
    (x :: <raw-double-float>, y :: <raw-double-float>) => (difference :: <raw-double-float>)
  raw-df-op(\-, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-multiply
    (x :: <raw-double-float>, y :: <raw-double-float>) => (product :: <raw-double-float>)
  raw-df-op(\*, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-divide
    (x :: <raw-double-float>, y :: <raw-double-float>) => (ratio :: <raw-double-float>)
  raw-df-op(\/, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-equals?
    (x :: <raw-double-float>, y :: <raw-double-float>) => (equal? :: <boolean>)
  raw-df-op(\=, x, y)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-less-than?
    (x :: <raw-double-float>, y :: <raw-double-float>) => (less? :: <boolean>)
  raw-df-op(\<, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive primitive-double-float-sqrt
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-log
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-exp
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-expt
    (base :: <raw-double-float>, power :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-sin
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-cos
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-tan
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-asin
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-acos
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
define side-effect-free stateless dynamic-extent &primitive primitive-double-float-atan
    (x :: <raw-double-float>) => (z :: <raw-double-float>);

/// FLOAT CONVERSIONS

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-single-float-as-double
    (s :: <raw-single-float>) => (d :: <raw-double-float>)
  make-raw-literal(as(<double-float>, ^raw-object-value(s)))
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-double-float-as-single
    (d :: <raw-double-float>) => (s :: <raw-single-float>)
  make-raw-literal(as(<single-float>, ^raw-object-value(d)))
end;

/// FLOAT CLASSIFICATION (e.g. nan, infinity, zero, normal)

define side-effect-free stateless dynamic-extent &primitive primitive-single-float-class
    (x :: <raw-single-float>) => (class :: <string>);

define side-effect-free stateless dynamic-extent &primitive primitive-double-float-class
    (x :: <raw-double-float>) => (class :: <string>);
