module:    powerpc-harp
Synopsis:  PowerPC save/restore instructions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// In order to use the store-multiple instruction, we organise register
/// preservation differently from other processors. We push the return address
/// first (as normal), then the constants and frame registers. Only then do we
/// save the preserved registers.  We can do this with a single instruction, 
/// so it is worth the incompatible change.

/// Since registers must be stored as a contiguous chunk, it is efficient
/// to allocate the highest numbered preservable register first, so as to
/// avoid unnecessary stores.

/// Push-Regs will push registers "from" to 31 onto the stack in reverse order,
/// followed optionally by a push of floating-point registers in "from-flt" 
/// list. Note that, for now at least, we have no preservable floating point
/// registers.

/// On entry, from is an integer, but from-flt is a list of FP registers.

// MJS 17/04/91: GAP-BELOW is the # bytes to be allocated for local data


define method push-regs
    (be :: <powerpc-back-end>, from, #key gap-below = 0, from-flt = #())
  let num = 32 - from;
  let gpr-offset = -4 * num;
  let from-reg = register-from-number(from);
  let fnum = size(from-flt);
  let fp-bytes = 8 * fnum;
  let stm-offset = fp-bytes + gap-below;
  let before-store-offset = gpr-offset - stm-offset;
  let after-store-offset = 0;
  if (stm-offset > #x7fff)
    before-store-offset := gpr-offset - fp-bytes;
    after-store-offset := gap-below;
    stm-offset := fp-bytes
  end;
  emit-d(be, addic-op, reg--stack, reg--stack, before-store-offset);
  emit-d(be, stmw-op, from-reg, reg--stack, stm-offset);
  for (i from 0 below fnum)
    emit-dri(be, stfd-op, head(from-flt), reg--stack, i * 8);
    from-flt := tail(from-flt)
  end;
  unless (zero?(after-store-offset))
    emit-d(be, addic-op, reg--stack, reg--stack, after-store-offset)
  end
end method push-regs;


/// Pop-Regs has the inverse effect to Push-Regs.


define method pop-regs
    (be :: <powerpc-back-end>, from, #key frame-size = #f, from-flt = #())
  let num = 32 - from;
  let from-reg = register-from-number(from);
  let fnum = size(from-flt);
  let fp-bytes = 8 * fnum;
  let bytes = 4 * num + fp-bytes;
  let x-frame-size = frame-size | 0;
  let lm-offset = x-frame-size + fp-bytes;
  if (lm-offset > #xffff)
    emit-d(be, addic-op, reg--stack, reg--frame, - bytes);
    x-frame-size := 0;
    lm-offset := fp-bytes
  end;
  for (i from 0 below fnum)
    emit-dri(be, lfd-op, head(from-flt), reg--stack,
             x-frame-size + i * 8);
    from-flt := tail(from-flt)
  end;
  emit-d(be, lmw-op, from-reg, reg--stack, lm-offset);
  // // don't pop the return address yet -- do this later
  emit-d(be, addic-op, reg--stack, reg--stack, x-frame-size + bytes)
end method pop-regs;


/// save-regs and restor-regs should save/restore all registers apart from
/// the argument return register (register 3). However, we don't care about
/// r2 as LISP never uses it & we expect r1 (stack pointer) to be OK anyway
/// else we really have problems. We can therefore do a store multiple from
/// r4 and an explicit push of r0 (arg count).


define powerpc-template save-regs

  pattern (be)
    harp-out(be) push(be, r0) end;
    push-regs(be, 4);

end powerpc-template;


define powerpc-template restor-regs

  pattern (be)
    pop-regs(be, 4);
    harp-out(be) pop(be, r0) end;

end powerpc-template;


/*
define variable *debug-preserved-registers* = #f;

define method show-preserved (be :: <powerpc-back-end>) => ()
  if (*debug-preserved-registers*)
    let state = be.variables.vreg-state;
    let preserved = state.allocated-preserved;
    let num = state.number-preserved;
    format-out("\n\n+++++++++ Number preserved = %=.\n", num);
    for (reg in list-from-prset(be, preserved))
      format-out("+++++++++    Preserving %=.\n", reg);
    end for;
  end if;
end method;
*/


/// For this processor, the preserve-register routines will be
/// responsible for updating the frame from the stack and vice-versa.

define powerpc-template preserve-registers-entry

  pattern (be)

    let state = be.variables.vreg-state;
    let num-preserved = state.number-preserved;
    let lowest = lowest-preserved-register(be);
    // number of preserved registers
    let number = state.number-preserved;
    let size = number * 4;         // size of preserved registers on stack

    if (be.variables.compiling-call-in)
      // Do what C does in Prologs...
      // Get the return address into r0, and store in SP+4
      emit-x(be, mfspr-op, reg--c-link, lr, r0);
      emit-d(be, stw-op, reg--c-link, reg--stack, 4);

      push-regs(be, lowest);
      // TO DO: Preserve C floating-point registers!!!
      /*
      push-regs(be, 13,
		from-flt: powerpc-c-float-preserved-registers-list,
		from-int: list(r1, r2));
      */
    else
      // get the return address to push
      emit-x(be, mfspr-op, reg--link, lr, r0);

      push-regs(be, lowest);
    end if;

    harp-out(be) add(be, reg--frame, reg--stack, size) end;

    let marker = make(<start-frame-marker>, size: 0);
    emit-labelled-constant(be, marker, 0);

end powerpc-template;


define powerpc-template preserve-registers-exit

  pattern (be)
    let marker = make(<end-frame-marker>, size: 0);
    emit-labelled-constant(be, marker, 0);

    let state = be.variables.vreg-state;
    let num-preserved = state.number-preserved;
    let lowest = lowest-preserved-register(be);
    // number of preserved registers
    let number = state.number-preserved;
    let size = number * 4;     // size of preserved registers on stack

    // pop regs, using sp as base if offset from fp is known
    harp-out(be) sub(be, reg--stack, reg--frame, size) end;
    if (be.variables.compiling-call-in)
      pop-regs(be, lowest);

      // Do what C does in Epilogs...
      // Load return address from SP+4 into r0, and put back into LR
      emit-d(be, lwz-op, reg--c-link, reg--stack, 4);
      emit-x(be, mtspr-op, reg--c-link, lr, r0)
    else
      pop-regs(be, lowest);

      // put the return address back into LR
      emit-x(be, mtspr-op, reg--link, lr, r0)
    end if;

end powerpc-template;


define powerpc load-address-of-stack-arg-template;

define powerpc load-address-of-stack-arg-n-template;


define powerpc load-stack-arg-n-template;

define powerpc load-frame-offset-template;

define powerpc store-frame-offset-template;


with-ops-in powerpc-instructions (store-stack-arg-n)
  disallow-fn := tmp3-fn;
end;

define powerpc store-stack-arg-n-template;


define powerpc-template add-into-stack-pointer

  pattern (be, r, s)
    harp-out(be) add(be, reg--stack, r, s) end;

end powerpc-template;


with-ops-in powerpc-instructions (remove-optionals, move-return-address, 
                                  adjust-stack, load-count-adjusting-stack)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;

define powerpc remove-optionals-template (reg--tmp1, reg--tmp2, reg--tmp1);

define method call-remove-optionals
    (be :: <powerpc-back-end>) => ()
  let remove-opts =
    emit-make-reg(be, remove-optionals-runtime, reg--tmp2);

  // Preserve link register in reg--link if no frame
  unless (be.variables.with-stack)
    read-return-address(be)
  end;

  // Prepare call address in the link register
  emit-x(be, mtspr-op, remove-opts, lr, r0);

  emit-br-reg(be, bclrl-op, bra-cc);  // Jump to link register

  // Restore link register from reg--link (remove-optionals won't modify reg--link)
  write-return-address(be);

end method;


define constant powerpc-allocatable-registers-list 
  = as(<list>, powerpc-allocatable-registers);

with-move-return-address-ops powerpc end;

define powerpc move-return-address-template;

define powerpc adjust-stack-template;


with-load-count-adjusting-stack-ops powerpc end;

define powerpc load-count-adjusting-stack-template;

