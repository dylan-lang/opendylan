Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// %logior (#rest machine-words) => result :: <machine-word>
// %logxor (#rest machine-words) => result :: <machine-word>
// %logand (#rest machine-words) => result :: <machine-word>

// --- TBD: figure out how to get the compiler to transform the n-ary
// --- versions into chained calls to binary versions.

define macro logical-definer
  { define logical ?:name,
                   ?binary:name,
                   ?lowlevel:name,
                   ?init:expression }
  => { define function ?name (#rest machine-words)
         => result :: <machine-word>;
         for (x in machine-words,
              result = as(<machine-word>, ?init) then ?binary(result, x))
         finally result;
         end for;
       end;
       define sealed generic ?binary (x :: <object>, y :: <object>)
         => result :: <machine-word>;
       define inline method ?binary
            (x :: <machine-word>, y :: <machine-word>)
         => result :: <machine-word>;
         ?lowlevel(x, y);
       end method;
       define inline method ?binary
            (x :: <machine-word>, y :: <abstract-integer>)
         => result :: <machine-word>;
         ?lowlevel(x, coerce-abstract-integer-to-machine-word(y));
       end method;
       define inline method ?binary
            (x :: <abstract-integer>, y :: <machine-word>)
         => result :: <machine-word>;
         ?lowlevel(coerce-abstract-integer-to-machine-word(x), y);
       end method;
       define inline method ?binary
            (x :: <abstract-integer>, y :: <abstract-integer>)
         => result :: <machine-word>;
         ?lowlevel(coerce-abstract-integer-to-machine-word(x),
                   coerce-abstract-integer-to-machine-word(y));
       end method;
       }
end macro;

define logical %logior, binary-%logior, machine-word-logior, 0;
define logical %logxor, binary-%logxor, machine-word-logxor, 0;
define logical %logand, binary-%logand, machine-word-logand, -1;

//////////////////////////////////////////////////////////////////////////////
// %lognot (m :: <machine-word>)
//   => result :: <machine-word>

define sealed generic %lognot (x :: <object>)
  => result :: <machine-word>;

define inline method %lognot (x :: <machine-word>)
  => result :: <machine-word>;
  machine-word-lognot(x);
end method;

define inline method %lognot (x :: <abstract-integer>)
  => result :: <machine-word>;
  machine-word-lognot(coerce-abstract-integer-to-machine-word(x));
end method;

//////////////////////////////////////////////////////////////////////////////
// %logbit? (index :: <integer>, m :: <machine-word>) => result :: <boolean>

define sealed generic %logbit? (index :: <integer>, m :: <object>)
  => result :: <boolean>;

define inline method %logbit? (index :: <integer>, m :: <machine-word>)
  => result :: <boolean>;
  check-bit-index(index);
  machine-word-logbit?(index, m);
end method %logbit?;

define method %logbit? (index :: <integer>, m :: <abstract-integer>)
  => result :: <boolean>;
  check-bit-index(index);
  machine-word-logbit?(index, coerce-abstract-integer-to-machine-word(m));
end method %logbit?;

//////////////////////////////////////////////////////////////////////////////
// %count-low-zeros (x :: <machine-word>) => count :: <integer>
// %count-high-zeros (x :: <machine-word>) => count :: <integer>

define macro count-zeros-definer
  { define count-zeros ?:name ?counter:name }
  => { define sealed generic ?name (x :: <object>)
         => count :: <integer>;
       define inline method ?name (x :: <machine-word>)
         => count :: <integer>;
         ?counter(x);
       end method;
       define inline method ?name (x :: <abstract-integer>)
         => count :: <integer>;
         ?counter(coerce-abstract-integer-to-machine-word(x));
       end method;
       }
end macro;

define count-zeros %count-low-zeros machine-word-count-low-zeros;
define count-zeros %count-high-zeros machine-word-count-high-zeros;
