module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define custom &machine-word-primitive primitive-integer?
    (x :: <object>) => (result :: <boolean>)
  make-raw-literal(instance?(x, <integer>))
end;
define sign-extend &machine-word-primitive primitive-machine-word-equal?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x = y
end;
define sign-extend &machine-word-primitive primitive-machine-word-not-equal?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x ~= y
end;
define sign-extend &machine-word-primitive primitive-machine-word-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x < y
end;
define sign-extend &machine-word-primitive primitive-machine-word-not-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x >= y
end;
define sign-extend &machine-word-primitive primitive-machine-word-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x > y
end;
define sign-extend &machine-word-primitive primitive-machine-word-not-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x <= y
end;

define custom &machine-word-primitive primitive-wrap-machine-word
    (x :: <raw-machine-word>) => (result :: <machine-word>)
  // TODO: CORRECTNESS: This gets iep's when compiling C-callables. Why?
  select (x by instance?)
    <&raw-integer> =>
      make(<&machine-word>, data: make(<&raw-machine-word>, value: ^raw-object-value(x)));
    <&raw-byte-character> =>
      make(<&machine-word>, data: make(<&raw-machine-word>,
                                       value: as(<integer>, ^raw-object-value(x))));
    <&raw-machine-word> =>
      let raw-value = ^raw-object-value(x);
      if (instance?(raw-value, <&machine-word>))
        raw-value
      else
        make(<&machine-word>, data: make(<&raw-machine-word>, value: raw-value))
      end;
  end
end;

define custom &machine-word-primitive primitive-unwrap-machine-word
    (x :: <machine-word>) => (result :: <raw-machine-word>)
  select (x by instance?)
    <abstract-integer> => make-raw-literal(x);
    <byte-character> => make-raw-literal(as(<integer>, x));
    <&machine-word> => ^%machine-word-data(x);
    otherwise => error("PUNT THIS FOLDER");
  end
end;

/// NOTE: Not used with our current <integer> representation
define &simple-machine-word-primitive primitive-box-integer
    (x :: <raw-machine-word>) => (result :: <integer>);

/// NOTE: Not used with our current <integer> representation
define &simple-machine-word-primitive primitive-unbox-integer
    (x :: <integer>) => (result :: <raw-machine-word>);

/// NOTE: The folder for this primitive contains intimate knowledge of the representation
/// of <integer>s and will have to be updated if we ever change representations.  Also,
/// it can't fold all <integer>s values as the call to ash will signal overflow under
/// some conditions.  (Sigh)
define custom &machine-word-primitive primitive-cast-integer-as-raw
    (x :: <integer>) => (result :: <raw-machine-word>)
  make-raw-literal(generic/logior(generic/ash(x, 2), 1))
end;

/// NOTE: The folder for this primitive contains intimate knowledge of the representation
/// of <integer>s and will have to be updated if we ever change representations.
define custom &machine-word-primitive primitive-cast-raw-as-integer
    (x :: <raw-machine-word>) => (result :: <integer>)
  // Signals an error if the result is too big to be an <integer> ...
  let x :: <abstract-integer> = extract-mw-operand-unsigned(x);
  as(<integer>, generic/lsh(x, -2))
end;

define custom &machine-word-primitive primitive-wrap-abstract-integer
    (x :: <raw-machine-word>) => (result :: <abstract-integer>)
  ^raw-object-value(x)
end;
define &simple-machine-word-primitive primitive-wrap-unsigned-abstract-integer
    (x :: <raw-machine-word>) => (result :: <abstract-integer>);
define custom &machine-word-primitive primitive-unwrap-abstract-integer
    (x :: <abstract-integer>) => (result :: <raw-machine-word>)
  make-raw-literal(x)
end;

define &machine-word-primitive primitive-machine-word-logand
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/logand(x, y)
end;
define &machine-word-primitive primitive-machine-word-logior
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/logior(x, y)
end;
define &machine-word-primitive primitive-machine-word-logxor
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/logxor(x, y)
end;
define &machine-word-primitive primitive-machine-word-lognot
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/lognot(x)
end;
define &machine-word-primitive primitive-machine-word-logbit?
    (index :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  generic/logbit?(index, y)
end;

/// TODO: THESE ARE ACTUALLY IN DYLAN LIBRARY

define inline function logbit-set (index :: <abstract-integer>, y :: <abstract-integer>)
 => (r :: <abstract-integer>)
  generic/logior(y, generic/ash(1, index))
end function;

define inline function logbit-clear (index :: <abstract-integer>, y :: <abstract-integer>)
 => (r :: <abstract-integer>)
  generic/logand(y, generic/lognot(generic/ash(1, index)))
end function;

define &machine-word-primitive primitive-machine-word-logbit-set
    (index :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <raw-machine-word>)
  logbit-set(index, y)
end;
define &machine-word-primitive primitive-machine-word-logbit-clear
    (index :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <raw-machine-word>)
  logbit-clear(index, y)
end;

/// TODO: THESE ARE ACTUALLY IN DYLAN LIBRARY

define inline function bit-field-extract
    (offset :: <abstract-integer>, size :: <abstract-integer>, x :: <abstract-integer>)
 => (res :: <abstract-integer>)
  generic/ash(generic/logand(x, generic/ash(generic/-(generic/ash(1, size), 1), offset)),
              generic/negative(offset))
end function bit-field-extract;

define inline function bit-field-deposit
    (field :: <abstract-integer>,
     offset :: <abstract-integer>, size :: <abstract-integer>, x :: <abstract-integer>)
 => (res :: <abstract-integer>)
  generic/logior(generic/logand(x, generic/lognot(generic/ash(generic/-(generic/ash(1, size),
                                                                        1),
                                                              offset))),
                 generic/ash(field, offset))
end function bit-field-deposit;

define &machine-word-primitive primitive-machine-word-bit-field-deposit
    (field :: <raw-machine-word>, offset :: <raw-machine-word>, size :: <raw-machine-word>, x :: <raw-machine-word>)
 => (result :: <raw-machine-word>)
  bit-field-deposit(field, offset, size, x)
end;

define &machine-word-primitive primitive-machine-word-bit-field-extract
    (offset :: <raw-machine-word>, size :: <raw-machine-word>, x :: <raw-machine-word>)
 => (result :: <raw-machine-word>)
  bit-field-extract(offset, size, x)
end;

define &simple-machine-word-primitive primitive-machine-word-count-low-zeros
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-count-high-zeros
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);

define &machine-word-primitive primitive-machine-word-add
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (sum :: <raw-machine-word>)
  generic/+(x, y)
end;
define &simple-machine-word-primitive primitive-machine-word-add-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (sum :: <raw-machine-word>, overflow? :: <boolean>);
define &machine-word-primitive primitive-machine-word-subtract
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (difference :: <raw-machine-word>)
  generic/-(x, y)
end;
define &simple-machine-word-primitive primitive-machine-word-subtract-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (difference :: <raw-machine-word>, overflow? :: <boolean>);

define &machine-word-primitive primitive-machine-word-multiply-low
   (x :: <raw-machine-word>, y :: <raw-machine-word>)=> (low :: <raw-machine-word>)
  generic/*(x, y)
end;
define &simple-machine-word-primitive primitive-machine-word-multiply-high
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-multiply-low/high
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-multiply-low-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, overflow? :: <boolean>);
define &simple-machine-word-primitive primitive-machine-word-multiply-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>, overflow? :: <boolean>);

define sign-extend &machine-word-primitive primitive-machine-word-negative
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/negative(x)
end;
define &simple-machine-word-primitive primitive-machine-word-negative-with-overflow
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>, overflow? :: <boolean>);
define sign-extend &machine-word-primitive primitive-machine-word-abs
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/abs(x)
end;
define &simple-machine-word-primitive primitive-machine-word-abs-with-overflow
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>, overflow? :: <boolean>);

define &machine-word-primitive primitive-machine-word-floor/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>)
  generic/floor/(dividend, divisor)
end;
define &machine-word-primitive primitive-machine-word-floor/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>)
  let (quotient, remainder) = generic/floor/(dividend, divisor);
  remainder
end;
define &simple-machine-word-primitive primitive-machine-word-floor/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &machine-word-primitive primitive-machine-word-ceiling/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>)
  generic/ceiling/(dividend, divisor)
end;
define &machine-word-primitive primitive-machine-word-ceiling/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>)
  let (quotient, remainder) = generic/ceiling/(dividend, divisor);
  remainder;
end;
define &simple-machine-word-primitive primitive-machine-word-ceiling/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &machine-word-primitive primitive-machine-word-round/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>)
  generic/round/(dividend, divisor)
end;
define &machine-word-primitive primitive-machine-word-round/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>)
  let (quotient, remainder) = generic/round/(dividend, divisor);
  remainder
end;
define &simple-machine-word-primitive primitive-machine-word-round/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &machine-word-primitive primitive-machine-word-truncate/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>)
  generic/truncate/(dividend, divisor)
end;
define &machine-word-primitive primitive-machine-word-truncate/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>)
  let (quotient, remainder) = generic/truncate/(dividend, divisor);
  remainder
end;
define &simple-machine-word-primitive primitive-machine-word-truncate/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-divide-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-divide-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-divide
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);

define sign-extend &machine-word-primitive primitive-machine-word-shift-left-low
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (low :: <raw-machine-word>)
  generic/ash(x, shift)
end;
define &simple-machine-word-primitive primitive-machine-word-shift-left-high
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-shift-left-low/high
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-shift-left-low-with-overflow
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, overflow? :: <boolean>);
define &simple-machine-word-primitive primitive-machine-word-shift-left-with-overflow
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>, overflow? :: <boolean>);

define sign-extend &machine-word-primitive primitive-machine-word-shift-right
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/ash(x, generic/negative(shift))
end;

/// NOTE: We can still try to fold the overflow signalling primtives because, if the
/// computation overflows at compile time, the folding will be abandoned, the primitive
/// will be called at run-time, and the overflow will be signalled.
define overflow &machine-word-primitive primitive-machine-word-add-signal-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (sum :: <raw-machine-word>)
  generic/+(x, y)
end;
define overflow &machine-word-primitive primitive-machine-word-subtract-signal-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (difference :: <raw-machine-word>)
  generic/-(x, y)
end;
define overflow &machine-word-primitive primitive-machine-word-multiply-signal-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (low :: <raw-machine-word>)
  generic/*(x, y)
end;
define sign-extend overflow &machine-word-primitive primitive-machine-word-negative-signal-overflow
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/negative(x)
end;
define sign-extend overflow &machine-word-primitive primitive-machine-word-abs-signal-overflow
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/abs(x)
end;
define sign-extend overflow &machine-word-primitive primitive-machine-word-shift-left-signal-overflow
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/ash(x, shift)
end;

define &simple-machine-word-primitive primitive-machine-word-double-floor/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-floor/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-floor/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-ceiling/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-ceiling/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-ceiling/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-round/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-round/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-round/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-truncate/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-truncate/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-truncate/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-divide-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-divide-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-double-divide
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);

define &machine-word-primitive primitive-machine-word-unsigned-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x < y
end;
define &machine-word-primitive primitive-machine-word-unsigned-not-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x >= y
end;
define &machine-word-primitive primitive-machine-word-unsigned-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x > y
end;
define &machine-word-primitive primitive-machine-word-unsigned-not-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>)
  x <= y
end;

define &simple-machine-word-primitive primitive-machine-word-unsigned-add-with-carry
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (sum :: <raw-machine-word>, carry :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-subtract-with-borrow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (difference :: <raw-machine-word>, borrow :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-multiply-high
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-multiply
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-divide-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-divide-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-divide
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);

define &simple-machine-word-primitive primitive-machine-word-unsigned-rotate-left
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (result :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-rotate-right
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (result :: <raw-machine-word>);

define &machine-word-primitive primitive-machine-word-unsigned-shift-right
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (result :: <raw-machine-word>)
  generic/lsh(x, generic/negative(shift))
end;
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-divide-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-divide-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-divide
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);

define &simple-machine-word-primitive primitive-machine-word-unsigned-shift-left-high
    (x :: <raw-machine-word>, shift :: <raw-machine-word>) => (result :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-shift-left-high
    (x-low :: <raw-machine-word>, x-high :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-shift-left
    (x-low :: <raw-machine-word>, x-high :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-shift-right-low
    (x-low :: <raw-machine-word>, x-high :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-shift-right-high
    (x-low :: <raw-machine-word>, x-high :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
define &simple-machine-word-primitive primitive-machine-word-unsigned-double-shift-right
    (x-low :: <raw-machine-word>, x-high :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
