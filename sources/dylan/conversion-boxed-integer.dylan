Module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Conversion between boxed integers and machine words

define inline-only function as-offset-for-tagged-integer
    (x :: <integer>) => (result :: <integer>)
  x
end function as-offset-for-tagged-integer;

define inline-only function as-field-size-for-tagged-integer
    (x :: <integer>) => (result :: <integer>)
  x
end function as-field-size-for-tagged-integer;

define inline-only constant interpret-machine-word-as-integer
  = coerce-machine-word-to-integer;
define inline-only constant interpret-integer-as-machine-word
  = coerce-integer-to-machine-word;

// just a type check
define inline-only function machine-word-identity (x :: <machine-word>)
 => (result :: <machine-word>)
  x
end function machine-word-identity;

define inline-only constant strip-integer-tag = machine-word-identity;
define inline-only constant insert-integer-tag = machine-word-identity;
define inline-only constant force-integer-tag = machine-word-identity;

define inline-only function coerce-machine-word-to-integer (x :: <machine-word>)
 => (result :: <integer>)
  primitive-box-integer(primitive-unwrap-machine-word(x))
end function coerce-machine-word-to-integer;

define inline-only function coerce-integer-to-machine-word (x :: <integer>)
 => (result :: <machine-word>)
  primitive-wrap-machine-word(primitive-unbox-integer(x))
end function coerce-integer-to-machine-word;

define inline-only constant coerce-machine-word-to-abstract-integer
  = coerce-machine-word-to-integer;

define inline-only function machine-word-is-integer? (x :: <machine-word>)
 => (integer? :: <boolean>)
  #t
end function machine-word-is-integer?;

///---*** Will need a generalized version for arbitrary precision integers...
define inline-only function double-integer-is-integer?
    (low :: <machine-word>, high :: <machine-word>) => (integer? :: <boolean>)
  if (machine-word-less-than?(low, coerce-integer-to-machine-word(0)))
    machine-word-equal?(high, coerce-integer-to-machine-word(-1))
  else
    machine-word-equal?(high, coerce-integer-to-machine-word(0))
  end if
end function double-integer-is-integer?;


