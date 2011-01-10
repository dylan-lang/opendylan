Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// ud%divide (dividend-low :: <machine-word>,
//            dividend-high :: <machine-word>,
//            divisor :: <machine-word>)
//   => (quotient :: <machine-word>, remainder :: <machine-word>)

define sealed generic ud%divide (dividend-low :: <object>,
                                 dividend-high :: <object>,
                                 divisor :: <object>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);

define inline method ud%divide (dividend-low :: <machine-word>,
                                dividend-high :: <machine-word>,
                                divisor :: <machine-word>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (dividend-low,
       dividend-high,
       divisor);
end method;

define inline method ud%divide (dividend-low :: <machine-word>,
                                dividend-high :: <machine-word>,
                                divisor :: <abstract-integer>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (dividend-low,
       dividend-high,
       coerce-abstract-integer-to-machine-word(divisor));
end method;

define inline method ud%divide (dividend-low :: <machine-word>,
                                dividend-high :: <abstract-integer>,
                                divisor :: <machine-word>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (dividend-low,
       coerce-abstract-integer-to-machine-word(dividend-high),
       divisor);
end method;

define inline method ud%divide (dividend-low :: <machine-word>,
                                dividend-high :: <abstract-integer>,
                                divisor :: <abstract-integer>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (dividend-low,
       coerce-abstract-integer-to-machine-word(dividend-high),
       coerce-abstract-integer-to-machine-word(divisor));
end method;

define inline method ud%divide (dividend-low :: <abstract-integer>,
                                dividend-high :: <machine-word>,
                                divisor :: <machine-word>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (coerce-abstract-integer-to-machine-word(dividend-low),
       dividend-high,
       divisor);
end method;

define inline method ud%divide (dividend-low :: <abstract-integer>,
                                dividend-high :: <machine-word>,
                                divisor :: <abstract-integer>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (coerce-abstract-integer-to-machine-word(dividend-low),
       dividend-high,
       coerce-abstract-integer-to-machine-word(divisor));
end method;

define inline method ud%divide (dividend-low :: <abstract-integer>,
                                dividend-high :: <abstract-integer>,
                                divisor :: <machine-word>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (coerce-abstract-integer-to-machine-word(dividend-low),
       coerce-abstract-integer-to-machine-word(dividend-high),
       divisor);
end method;

define inline method ud%divide (dividend-low :: <abstract-integer>,
                                dividend-high :: <abstract-integer>,
                                divisor :: <abstract-integer>)
  => (quotient :: <machine-word>, remainder :: <machine-word>);
  machine-word-unsigned-double-divide
      (coerce-abstract-integer-to-machine-word(dividend-low),
       coerce-abstract-integer-to-machine-word(dividend-high),
       coerce-abstract-integer-to-machine-word(divisor));
end method;

//////////////////////////////////////////////////////////////////////////////
// ud%shift-left (low :: <machine-word>,
//                high :: <machine-word>,
//                count :: <integer>)
//   => low :: <machine-word>, high :: <machine-word>
//
// ud%shift-right (low :: <machine-word>,
//                 high :: <machine-word>,
//                 count :: <integer>)
//   => low :: <machine-word>, high :: <machine-word>

define macro unsigned-double-shift-definer
  { define unsigned-double-shift ?:name ?lowlevel:name }
  => { define sealed generic ?name (low :: <object>,
                                    high :: <object>,
                                    count :: <integer>)
         => (low :: <machine-word>, high :: <machine-word>);
       define inline method ?name (low :: <machine-word>,
                                   high :: <machine-word>,
                                   count :: <integer>)
         => (low :: <machine-word>, high :: <machine-word>);
         check-shift-quantity(count);
         ?lowlevel(low,
                   high,
                   count);
       end method;
       define inline method ?name (low :: <machine-word>,
                                   high :: <abstract-integer>,
                                   count :: <integer>)
         => (low :: <machine-word>, high :: <machine-word>);
         check-shift-quantity(count);
         ?lowlevel(low,
                   coerce-abstract-integer-to-machine-word(high),
                   count);
       end method;
       define inline method ?name (low :: <abstract-integer>,
                                   high :: <machine-word>,
                                   count :: <integer>)
         => (low :: <machine-word>, high :: <machine-word>);
         check-shift-quantity(count);
         ?lowlevel(coerce-abstract-integer-to-machine-word(low),
                   high,
                   count);
       end method;
       define inline method ?name (low :: <abstract-integer>,
                                   high :: <abstract-integer>,
                                   count :: <integer>)
         => (low :: <machine-word>, high :: <machine-word>);
         check-shift-quantity(count);
         ?lowlevel(coerce-abstract-integer-to-machine-word(low),
                   coerce-abstract-integer-to-machine-word(high),
                   count);
       end method;
       }
end macro;

define unsigned-double-shift ud%shift-left machine-word-unsigned-double-shift-left;
define unsigned-double-shift ud%shift-right machine-word-unsigned-double-shift-right;
