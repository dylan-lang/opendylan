Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// %floor/ (dividend :: <machine-word>, divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// %ceiling/ (dividend :: <machine-word>, divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// %round/ (dividend :: <machine-word>, divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// %truncate/ (dividend :: <machine-word>, divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// %divide (dividend :: <machine-word>, divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>

define macro division-definer
  { define division ?:name ?lowlevel:name }
  => { define sealed generic ?name (dividend :: <object>, divisor :: <object>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
       define inline method ?name
            (dividend :: <machine-word>, divisor :: <machine-word>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(dividend, divisor);
       end method;
       define inline method ?name
            (dividend :: <machine-word>, divisor :: <abstract-integer>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(dividend, coerce-abstract-integer-to-machine-word(divisor));
       end method;
       define inline method ?name
            (dividend :: <abstract-integer>, divisor :: <machine-word>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(dividend), divisor);
       end method;
       define inline method ?name
            (dividend :: <abstract-integer>, divisor :: <abstract-integer>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(dividend),
                   coerce-abstract-integer-to-machine-word(divisor));
       end method;
       }
end;

define division %floor/ machine-word-floor/;
define division %ceiling/ machine-word-ceiling/;
define division %round/ machine-word-round/;
define division %truncate/ machine-word-truncate/;
define division %divide machine-word-divide;

