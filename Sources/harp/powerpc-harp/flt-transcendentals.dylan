module:    powerpc-harp
Synopsis:  PowerPC float transcendentals
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Implement floating-point transcendentals templates by calling out to
// software library routines (no PowerPC hardware implementations).
// This requires that the maths library "libm" is linked into Dylan
// executables.

with-ops-in powerpc-instructions
  (facos, fasin, fatan, fatanh, fcos, fcosh, fetox, fetoxm1, fgetexp,
   fgetman, fint, flog10, flog2, floge, flogep1, fsin, fsinh,
   ftan, ftanh, f10tox, f2tox,
   dacos, dasin, datan, datanh, dcos, dcosh, detox, detoxm1, dgetexp,
   dgetman, dint, dlog10, dlog2, dloge, dlogep1, dsin, dsinh,
   dtan, dtanh, d10tox, d2tox)
  destroys-fn := constant-fn(powerpc-allocatable-registers);
end;


/// 6 words of space must be reserved at the top of the stack 
/// for 6 Table-Of-Contents, Link-register .... by caller.

define constant $callee-reserved-space = 6;

define macro lazy-constant-ref-definer
  { define lazy-constant-ref ?:name }
    =>
  {
    define variable "*" ## ?name ## "-ref*" = #f;

    define function ?name ## "-ref"(be :: <powerpc-back-end>)
      => (ref :: <constant-reference>)
      "*" ## ?name ## "-ref*" |
      ("*" ## ?name ## "-ref*" :=
        ins--constant-ref(be, raw-mangle(be, ?"name")));
    end;
  }
end macro;

define macro powerpc-transcendental-template-definer
  { define powerpc-transcendental-template ?:name ?fun:name}
    =>
  {
    define powerpc-template ?name

      options (self);

      pattern (be, i, d, s)

      harp-out(be) fmove(be, f1, s) end;

      let ref = ?fun ## "-ref"(be);
      let reg = emit-make-reg(be, ref, reg--tmp1);

      // Must be externally registered by PowerPC Harp clients; not here.
      // ins--register-external(be, ref);

      harp-out(be) sub(be, reg--stack, reg--stack, 4 * $callee-reserved-space) end;

      emit-x(be, mtspr-op, reg, lr, r0);  // Prepare call address in the link register
      emit-br-reg(be, bclrl-op, bra-cc);  // Jump to link register

      harp-out(be) add(be, reg--stack, reg--stack, 4 * $callee-reserved-space) end;

      harp-out(be) fmove(be, d, f1) end;

    end powerpc-template;
  }

  { define powerpc-transcendental-template (?:name ?fun:name, ?names:*) }
    =>
  {
    define lazy-constant-ref ?fun;
    define powerpc-transcendental-template ?name ?fun;
    define powerpc-transcendental-template (?names)
  }

  { define powerpc-transcendental-template (?f-op:name ?d-op:name ?fun:name, ?names:*) }
    =>
  { 
    define lazy-constant-ref ?fun;
    define powerpc-transcendental-template ?f-op ?fun;
    define powerpc-transcendental-template ?d-op ?fun;
    define powerpc-transcendental-template (?names)
  }

  { define powerpc-transcendental-template () }  => {}
end macro;

define powerpc-transcendental-template
  (fsin dsin sin,
   fsinh dsinh sinh,
   fasin dasin asin,
   fcos dcos cos,
   fcosh dcosh cosh,
   facos dacos acos,
   ftan dtan tan,
   ftanh dtanh tanh,
   fatan datan atan,
   fatanh datanh atanh,
   floge dloge log,
   flog10 dlog10 log10,
   flog2 dlog2 log2,
   flogep1 dlogep1 log1p,
   fetox detox exp,
   fetoxm1 detoxm1 expm1,
   f2tox d2tox exp2
   );
