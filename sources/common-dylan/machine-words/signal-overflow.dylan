Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// so%+ (x :: <machine-word>, y :: <machine-word>)
//   => result :: <machine-word>
//
// so%- (x :: <machine-word>, y :: <machine-word>)
//   => result :: <machine-word>
//
// so%* (x :: <machine-word>, y :: <machine-word>)
//   => result :: <machine-word>

define macro signal-overflow-arithmetic-definer
  { define signal-overflow-arithmetic ?:name ?lowlevel:name }
  => { define sealed generic ?name (x :: <object>, y :: <object>)
         => result :: <machine-word>;
       define inline method ?name (x :: <machine-word>, y :: <machine-word>)
         => result :: <machine-word>;
         ?lowlevel(x, y);
       end method;
       define inline method ?name
            (x :: <machine-word>, y :: <abstract-integer>)
         => result :: <machine-word>;
         ?lowlevel(x, coerce-abstract-integer-to-machine-word(y));
       end method;
       define inline method ?name
            (x :: <abstract-integer>, y :: <machine-word>)
         => result :: <machine-word>;
         ?lowlevel(coerce-abstract-integer-to-machine-word(x), y);
       end method;
       define inline method ?name
            (x :: <abstract-integer>, y :: <abstract-integer>)
         => result :: <machine-word>;
         ?lowlevel(coerce-abstract-integer-to-machine-word(x),
                   coerce-abstract-integer-to-machine-word(y));
       end method;
       }
end macro;

define signal-overflow-arithmetic so%+ machine-word-add-signal-overflow;
define signal-overflow-arithmetic so%- machine-word-subtract-signal-overflow;
define signal-overflow-arithmetic so%* machine-word-multiply-signal-overflow;

//////////////////////////////////////////////////////////////////////////////
// so%negative(x :: <machine-word>)
//   => result :: <machine-word>
//
// so%abs(x :: <machine-word>)
//   => result :: <machine-word>

define macro signal-overflow-sign-modifier-definer
  { define signal-overflow-sign-modifier ?:name ?lowlevel:name }
  => { define sealed generic ?name (x :: <object>)
         => result :: <machine-word>;
       define inline method ?name (x :: <machine-word>)
         => result :: <machine-word>;
         ?lowlevel(x);
       end method;
       define inline method ?name (x :: <abstract-integer>)
         => result :: <machine-word>;
         ?lowlevel(coerce-abstract-integer-to-machine-word(x));
       end method;
       }
end macro;

define signal-overflow-sign-modifier so%negative machine-word-negative-signal-overflow;
define signal-overflow-sign-modifier so%abs machine-word-abs-signal-overflow;

//////////////////////////////////////////////////////////////////////////////
// so%shift-left (x :: <machine-word>, count :: <integer>)
//   => result :: <machine-word>

define sealed generic so%shift-left (x :: <object>, count :: <integer>)
  => result :: <machine-word>;

define method so%shift-left (x :: <machine-word>, count :: <integer>)
  => result :: <machine-word>;
  check-shift-quantity(count);
  machine-word-shift-left-signal-overflow(x, count);
end method;

define method so%shift-left (x :: <abstract-integer>, count :: <integer>)
  => result :: <machine-word>;
  check-shift-quantity(count);
  machine-word-shift-left-signal-overflow
    (coerce-abstract-integer-to-machine-word(x), count);
end method;
