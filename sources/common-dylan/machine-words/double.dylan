Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// d%floor/ (dividend-low :: <machine-word>,
//           dividend-high :: <machine-word>,
//           divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// d%ceiling/ (dividend-low :: <machine-word>,
//             dividend-high :: <machine-word>,
//             divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// d%round/ (dividend-low :: <machine-word>,
//           dividend-high :: <machine-word>,
//           divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// d%truncate/ (dividend-low :: <machine-word>,
//              dividend-high :: <machine-word>,
//              divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>
//
// d%divide (dividend-low :: <machine-word>,
//           dividend-high :: <machine-word>,
//           divisor :: <machine-word>)
//   => quotient :: <machine-word>, remainder :: <machine-word>

define macro double-division-definer
  { define double-division ?:name ?lowlevel:name }
  => { define sealed generic ?name (dividend-low :: <object>,
                                    dividend-high :: <object>,
                                    divisor :: <object>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
       define inline method ?name (dividend-low :: <machine-word>,
                                   dividend-high :: <machine-word>,
                                   divisor :: <machine-word>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(dividend-low,
                   dividend-high,
                   divisor);
       end method;
       define inline method ?name (dividend-low :: <machine-word>,
                                   dividend-high :: <machine-word>,
                                   divisor :: <abstract-integer>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(dividend-low,
                   dividend-high,
                   coerce-abstract-integer-to-machine-word(divisor));
       end method;
       define inline method ?name (dividend-low :: <machine-word>,
                                   dividend-high :: <abstract-integer>,
                                   divisor :: <machine-word>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(dividend-low,
                   coerce-abstract-integer-to-machine-word(dividend-high),
                   divisor);
       end method;
       define inline method ?name (dividend-low :: <machine-word>,
                                   dividend-high :: <abstract-integer>,
                                   divisor :: <abstract-integer>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(dividend-low,
                   coerce-abstract-integer-to-machine-word(dividend-high),
                   coerce-abstract-integer-to-machine-word(divisor));
       end method;
       define inline method ?name (dividend-low :: <abstract-integer>,
                                   dividend-high :: <machine-word>,
                                   divisor :: <machine-word>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(dividend-low),
                   dividend-high,
                   divisor);
       end method;
       define inline method ?name (dividend-low :: <abstract-integer>,
                                   dividend-high :: <machine-word>,
                                   divisor :: <abstract-integer>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(dividend-low),
                   dividend-high,
                   coerce-abstract-integer-to-machine-word(divisor));
       end method;
       define inline method ?name (dividend-low :: <abstract-integer>,
                                   dividend-high :: <abstract-integer>,
                                   divisor :: <machine-word>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(dividend-low),
                   coerce-abstract-integer-to-machine-word(dividend-high),
                   divisor);
       end method;
       define inline method ?name (dividend-low :: <abstract-integer>,
                                   dividend-high :: <abstract-integer>,
                                   divisor :: <abstract-integer>)
         => (quotient :: <machine-word>, remainder :: <machine-word>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(dividend-low),
                   coerce-abstract-integer-to-machine-word(dividend-high),
                   coerce-abstract-integer-to-machine-word(divisor));
       end method;
       }
end macro;

define double-division d%floor/ machine-word-double-floor/;
define double-division d%ceiling/ machine-word-double-ceiling/;
define double-division d%round/ machine-word-double-round/;
define double-division d%truncate/ machine-word-double-truncate/;
define double-division d%divide machine-word-double-divide;
