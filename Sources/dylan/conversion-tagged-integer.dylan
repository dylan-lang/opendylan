Module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Conversion between tagged integers and machine words

///---*** Shouldn't these be defined elsewhere? (Maybe they already are?)
define inline-only constant $integer-tag-value = as(<machine-word>, 1);
define inline-only constant $integer-tag-width = 2;
define inline-only constant $integer-tag-mask  = as(<machine-word>,
                                                    ash(-1, $integer-tag-width));

define inline-only function as-offset-for-tagged-integer
    (x :: <integer>) => (result :: <integer>)
  x + $integer-tag-width
end function as-offset-for-tagged-integer;

define inline-only function as-field-size-for-tagged-integer
    (x :: <integer>) => (result :: <integer>)
  x + $integer-tag-width
end function as-field-size-for-tagged-integer;

define inline-only function interpret-machine-word-as-integer
    (x :: <machine-word>) => (result :: <integer>)
  primitive-cast-raw-as-integer(primitive-unwrap-machine-word(x))
end function interpret-machine-word-as-integer;

define inline-only function interpret-integer-as-machine-word
    (x :: <integer>) => (result :: <machine-word>)
  primitive-wrap-machine-word(primitive-cast-integer-as-raw(x))
end function interpret-integer-as-machine-word;

define inline-only function strip-integer-tag 
    (x :: <machine-word>) => (result :: <machine-word>)
  machine-word-logxor(x, $integer-tag-value)
end function strip-integer-tag;

define inline-only function insert-integer-tag 
    (x :: <machine-word>) => (result :: <machine-word>)
  machine-word-logior(x, $integer-tag-value)
end function insert-integer-tag;

define inline-only function force-integer-tag
    (x :: <machine-word>) => (result :: <machine-word>)
  insert-integer-tag(machine-word-logand(x, $integer-tag-mask))
end function force-integer-tag;

///---*** A problem, probably in the ordering of optimizations, in the compiler
///---*** causes infinite recursions unless we include the expansion of
///---*** machine-word-shift-left-signal-overflow in this function's body.
define inline function coerce-machine-word-to-integer
    (x :: <machine-word>) => (result :: <integer>)
  // let bits = machine-word-shift-left-signal-overflow(x, $integer-tag-width);
  // interpret-machine-word-as-integer(insert-integer-tag(bits))
  let raw-x :: <raw-machine-word> = primitive-unwrap-machine-word(x);
  let bits :: <machine-word>
        = primitive-wrap-machine-word
            (primitive-machine-word-shift-left-signal-overflow
               (raw-x, integer-as-raw($integer-tag-width)));
  interpret-machine-word-as-integer(insert-integer-tag(bits))
end function coerce-machine-word-to-integer;

///---*** A problem, probably in the ordering of optimizations, in the compiler
///---*** causes infinite recursions unless we include the expansion of
///---*** machine-word-shift-right in this function's body.
define inline-only function coerce-integer-to-machine-word
    (x :: <integer>) => (result :: <machine-word>)
  // machine-word-shift-right(interpret-integer-as-machine-word(x), $integer-tag-width)
  let raw-bits :: <raw-machine-word>
        = primitive-unwrap-machine-word(interpret-integer-as-machine-word(x));
  primitive-wrap-machine-word
    (primitive-machine-word-shift-right(raw-bits, integer-as-raw($integer-tag-width)))
end function coerce-integer-to-machine-word;

define inline-only function coerce-machine-word-to-abstract-integer
    (x :: <machine-word>) => (result :: <abstract-integer>)
  primitive-wrap-abstract-integer(primitive-unwrap-machine-word(x))
end function coerce-machine-word-to-abstract-integer;

define inline-only function machine-word-is-integer? (x :: <machine-word>)
 => (integer? :: <boolean>)
  let x-shifted :: <machine-word>
	= machine-word-shift-right(x, word-size() - $integer-tag-width - 1);
  machine-word-equal?(x-shifted, coerce-integer-to-machine-word(0))
  | machine-word-equal?(x-shifted, coerce-integer-to-machine-word(-1))
end function machine-word-is-integer?;

///---*** Will need a generalized version for arbitrary precision integers...
define inline-only function double-integer-is-integer?
    (low :: <machine-word>, high :: <machine-word>) => (integer? :: <boolean>)
  machine-word-equal?(high,
		      machine-word-shift-right(low, word-size() - $integer-tag-width - 1))
  & (machine-word-equal?(high, coerce-integer-to-machine-word(0))
     | machine-word-equal?(high, coerce-integer-to-machine-word(-1)))
end function double-integer-is-integer?;
