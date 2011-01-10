module:    native-instructions
Synopsis:  A more complex HARP instruction set 
           (intended as a placeholder for the instructions).
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A more complex instruction set - just to keep a place holder
// on those complex HARP instructions which are not needed
// for the first backends


define abstract class <harp-complex-back-end> (<harp-native-back-end>)
end;

define instruction-set <complex-instruction-set>
    (<instruction-set>, <harp-complex-back-end>)

 create default-complex-instructions, inheriting: default-instructions;

/// catcher is a pseudo instruction put at the beginning of all basic
/// blocks which can be thrown to. Its sole purpose is to disallow the use
/// of real registers (even ones normally preserved by a call) for any
/// virtuals live over it.

  none op catcher, code-gen-fn: method () #f end;
  t    op stack-check;
  u    op fake-use, spread: spread-tu;
  dduu op bigit32-add, bigit32-sub, bigit32-mul, eliminatable: #t;
  dduu op bigit16-div;
  dduuu op bigit32-div;
       op load-code-offset, spread: spread-tdu;
       op block-call, block-jump, block-call-with-count, 
           block-jump-with-count, spread: spread-uu;
       op bsr, spread: spread-t;
//     op jsr-alien, s-jsr-alien, spread: spread-tuu;
       op trap, spread: spread-duu;
       op scl, spread: spread-t;
       op address-of, spread: spread-du;

end;


with-ops-in default-complex-instructions (bsr)
  external-transfer := #t;
end with-ops-in;


define instruction-function load-code-offset 
    (backend :: <harp-complex-back-end>, dest, tag :: <tag>, offset, #key op)
  load-some-address(backend, op, dest, tag, offset);
end;


define method implicit-uses-for-nregs
   (regs :: <register-model>, nregs :: <integer>, #key leaf-case = #f)
  if (nregs >= 0)
    let masks = 
      if (leaf-case) regs.reg-arg-masks else reg.reg-arg-masks-out end if;
    masks[nregs];
  else
    - nregs;
  end if;
end;

define instruction-function block-call 
    (backend :: <harp-complex-back-end>, thing, nregs :: <integer>, #key op)
  let regs = backend.registers;
  output-instruction(backend, op, thing, regs.reg-result-out,
                     implicit-uses-for-nregs(regs, nregs));
  make-fall-thru-bb(backend);
end;

define instruction-function block-jump
    (backend :: <harp-complex-back-end>, thing, nregs :: <integer>, #key op)
  let regs = backend.registers;
  make-fall-thru-bb(backend);
  output-instruction(backend, op, thing, regs.reg-result-out,
                     implicit-uses-for-nregs(regs, nregs));
  make-current-bb(backend);
end;


define instruction-function block-call-with-count
    (backend :: <harp-complex-back-end>, thing, nregs :: <integer>, #key op)
  let regs = backend.registers;
  output-instruction(backend, op, thing, regs.reg-result-out,
                     implicit-uses-for-nregs(regs, nregs));
  make-fall-thru-bb(backend);
end;

define instruction-function block-jump-with-count
    (backend :: <harp-complex-back-end>, thing, nregs :: <integer>, #key op)
  let regs = backend.registers;
  make-fall-thru-bb(backend);
  output-instruction(backend, op, thing, regs.reg-result-out,
                     implicit-uses-for-nregs(regs, nregs));
  make-current-bb(backend);
end;


define instruction-function bsr
    (backend :: <harp-complex-back-end>,
     tag :: <tag>, nregs :: <integer>, #key op)
  let regs = backend.registers;
  output-instruction(backend, op, tag, 
                     vector(regs.reg-result-out, regs.reg-mv-count),
                     implicit-uses-for-nregs(regs, nregs));
  let dest-bb = find-bb(backend, tag);
  let current = backend.variables.current-bb;
  pushnew!(dest-bb, current.bb-other-set);
  pushnew!(current, dest-bb.bb-prev-set);
  make-fall-thru-bb(backend);
end;


with-ops-in default-complex-instructions (bsr)
  implicit-uses :=
  method (backend, ins)
    let sv-ins = backend.variables.sv-instructions;
    let regs = backend.registers;
    with-du (sv-ins at ins)
      uze(1) + real-register-mask(regs.reg-arg-count) +
	       real-register-mask(regs.reg-function);
    end with-du;
  end method;
end with-ops-in;

define instruction-function trap 
    (backend :: <harp-complex-back-end>, d, n, arg, #key op)
  output-instruction(backend, op, #f, d, n, arg);
end;



/// ins--address-of(be, def, use) loads def with the address of use and ensures
/// that use is uniquely spilled. Note that though this may end the live range
/// of use, def will still point to the spill slot and there is no chance of
/// anything else ever being put there. This fact is crucial for the use this
/// op is designed for ie the Fortran interface.

define instruction-function address-of 
    (backend :: <harp-complex-back-end>, def, uze, #key op)
  uniquely-spill(backend, uze);
  output-du(backend, op, def, uze);
end;

define instruction-function scl 
    (backend :: <harp-complex-back-end>, thing, #key op)
  output-instruction(backend, op, thing);
end;



