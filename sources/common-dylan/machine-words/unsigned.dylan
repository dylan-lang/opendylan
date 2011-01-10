Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// u%<

define sealed generic u%< (x :: <object>, y :: <object>)
  => result :: <boolean>;

define inline method u%< (x :: <machine-word>, y :: <machine-word>)
  => result :: <boolean>;
  machine-word-unsigned-less-than?(x, y);
end method;

define inline method u%< (x :: <machine-word>, y :: <abstract-integer>)
  => result :: <boolean>;
  machine-word-unsigned-less-than?
      (x, coerce-abstract-integer-to-machine-word(y));
end method;

define inline method u%< (x :: <abstract-integer>, y :: <machine-word>)
  => result :: <boolean>;
  machine-word-unsigned-less-than?
      (coerce-abstract-integer-to-machine-word(x), y);
end method;

define inline method u%< (x :: <abstract-integer>, y :: <abstract-integer>)
  => result :: <boolean>;
  machine-word-unsigned-less-than?
      (coerce-abstract-integer-to-machine-word(x),
       coerce-abstract-integer-to-machine-word(y));
end method;

//////////////////////////////////////////////////////////////////////////////
// u%+ (x :: <machine-word>, y :: <machine-word>)
//   => (sum :: <machine-word>, carry :: <machine-word>)
//
// u%- (x :: <machine-word>, y :: <machine-word>)
//   => (diff :: <machine-word>, borrow :: <machine-word>)
//
// u%* (x :: <machine-word>, y :: <machine-word>)
//   => (low :: <machine-word>, high :: <machine-word>)
//
// u%divide (dividend :: <machine-word>, divisor :: <machine-word>)
//   => (quotient :: <machine-word>, remainder :: <machine-word>)

define macro unsigned-arithmetic-definer
  { define unsigned-arithmetic ?:name ?lowlevel:name }
  => { define sealed generic ?name (x :: <object>, y :: <object>)
         => (v1 :: <machine-word>, v2 :: <machine-word>);
       define inline method ?name
            (x :: <machine-word>, y :: <machine-word>)
         => (v1 :: <machine-word>, v2 :: <machine-word>);
         ?lowlevel(x, y);
       end method;
       define inline method ?name
            (x :: <machine-word>, y :: <abstract-integer>)
         => (v1 :: <machine-word>, v2 :: <machine-word>);
         ?lowlevel(x, coerce-abstract-integer-to-machine-word(y));
       end method;
       define inline method ?name
            (x :: <abstract-integer>, y :: <machine-word>)
         => (v1 :: <machine-word>, v2 :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(x), y)
       end method;
       define inline method ?name
            (x :: <abstract-integer>, y :: <abstract-integer>)
         => (v1 :: <machine-word>, v2 :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(x),
                   coerce-abstract-integer-to-machine-word(y));
       end method;
       }
end;

define unsigned-arithmetic u%+ machine-word-unsigned-add-with-carry;
define unsigned-arithmetic u%- machine-word-unsigned-subtract-with-borrow;
define unsigned-arithmetic u%* machine-word-unsigned-multiply;
define unsigned-arithmetic u%divide machine-word-unsigned-divide;

//////////////////////////////////////////////////////////////////////////////
// u%rotate-left (m :: <machine-word>, count :: <integer>)
//   => result :: <machine-word>
//
// u%rotate-right (m :: <machine-word>, count :: <integer>)
//   => result :: <machine-word>
//
// u%shift-left (m :: <machine-word>, count :: <integer>)
//   => result :: <machine-word>
//
// u%shift-right (m :: <machine-word>, count :: <integer>)
//   => result :: <machine-word>

define macro unsigned-shift-definer
  { define unsigned-shift ?:name ?lowlevel:name }
  => { define sealed generic ?name (m :: <object>, count :: <integer>)
         => result :: <machine-word>;
       define inline method ?name (m :: <machine-word>, count :: <integer>)
         => result :: <machine-word>;
         check-shift-quantity(count);
         ?lowlevel(m, count);
       end method;
       define inline method ?name (m :: <abstract-integer>, count :: <integer>)
         => result :: <machine-word>;
         check-shift-quantity(count);
         ?lowlevel(coerce-abstract-integer-to-machine-word(m), count);
       end method;
       }
end macro;

define unsigned-shift u%rotate-left machine-word-unsigned-rotate-left;
define unsigned-shift u%rotate-right machine-word-unsigned-rotate-right;
define unsigned-shift u%shift-left machine-word-unsigned-shift-left;
define unsigned-shift u%shift-right machine-word-unsigned-shift-right;
