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

    // get the return address to push
    emit-x(be, mfspr-op, reg--link, lr, r0);

    if (be.variables.compiling-call-in)
      push-regs(be, lowest);
      /*
      push-regs(be, 13,
		from-flt: powerpc-c-float-preserved-registers-list,
		from-int: list(r1, r2));
      */
    else
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
    else
      pop-regs(be, lowest)
    end if;

    // put the return address back into LR
    emit-x(be, mtspr-op, reg--link, lr, r0)

end powerpc-template;


// LOAD-ADDRESS-OF-STACK-ARG is provided for historical reasons.
// Its use is deprecated. Use LOAD-ADDRESS-OF-STACK-ARG-N instead.

define powerpc-template load-address-of-stack-arg
  pattern (be, stack? :: any, d)
    harp-out (be)
      load-address-of-stack-arg-n(be, stack?, d, 0)
    end;
end powerpc-template;


// LOAD-ADDRESS-OF-STACK-ARG-N
// Get the address of an arg-spill by its index.

define powerpc-template load-address-of-stack-arg-n
  pattern (be, stack? :: any, d, n :: <integer>)
    let add-op = op-element(be.instructions, add);
    operate-on-stack-arg(be, add-op, d, n, stack?);
end powerpc-template;


// LOAD-STACK-ARG-N takes N as an offset in words, as for LW. N must
// be constant.

define powerpc-template load-stack-arg-n 

  pattern (be, stack? :: any, d, n :: <integer>)
    unless (d.arg-spill?)
      let ld-op = op-element(be.instructions, ld);
      operate-on-stack-arg(be, ld-op, d, n, stack?);
    end unless;

  pattern (be, stack? :: any, d :: any, n)
    #f;

end powerpc-template;


define method operate-on-stack-arg 
    (be :: <powerpc-back-end>, op :: <op>, dest, n :: <integer>, stack?) => ()
  let wstack = be.variables.with-stack;
  unless (stack? == wstack | be.variables.compiling-defasm)
    harp-warning(be, "with-stack wrong");
  end unless;
  if (wstack)
    harp-reapply(be, op, dest, reg--frame, 4 * (2 + n));
  else
    harp-reapply(be, op, dest, reg--stack, 4 * (0 + n));
  end if;
end method;




with-ops-in powerpc-instructions (store-stack-arg-n)
  disallow-fn := tmp3-fn;
end;

// STORE-STACK-ARG-N takes N as an offset in words, as for LOAD-STACK-ARG-N.

define powerpc-template store-stack-arg-n 
  pattern (be, d, n :: <integer>)
    unless (arg-spill?(d) & n == arg-spill-offset-to-arg-number(d.spill-offset))
      let st-op = op-element(be.instructions, st);
      operate-on-stack-arg(be, st-op, d, n, be.variables.with-stack);
    end unless;
end powerpc-template;


define powerpc-template add-into-stack-pointer

  pattern (be, r, s)
    harp-out(be) add(be, reg--stack, r, s) end;

end powerpc-template;


// REMOVE-OPTIONALS takes an arg-spill index which gives the 
// location of the count of bytes on the stack. All the optional
// arguments from the count backwards are popped from the stack.
// The second arguments is either #f, or a pointer into the stack
// which must be updated because of the remove optionals. This pointer
// may be specified either as a register or as a stack index.

define constant remove-optionals-runtime = 
    runtime-reference("primitive_remove_optionals");


with-ops-in powerpc-instructions (remove-optionals, move-return-address, 
                                  adjust-stack, load-count-adjusting-stack)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;

define powerpc-template remove-optionals

  // Stack pointer specified as an arg-spill index
  pattern (be, offset :: <integer>, pointer-index :: <integer>)
    let adjust-size = reg--tmp1;
    let pointer-reg = reg--tmp2;
    find-size-for-stack-pointer-adjust(be, adjust-size, offset);
    harp-out (be) 
      // First find the original pointer
      load-stack-arg-n(be, #f, pointer-reg, pointer-index);
      // Add in the adjustment
      add(be, pointer-reg, pointer-reg, adjust-size);
      // and store back
      store-stack-arg-n(be, pointer-reg, pointer-index);
      remove-optionals(be, offset, #f);
    end;

  // Stack pointer specified as a register
  pattern (be, offset :: <integer>, stack-pointer)
    let adjust-size = reg--tmp1;
    find-size-for-stack-pointer-adjust(be, adjust-size, offset);
    harp-out (be) 
      // now add this to stack pointer
      add(be, stack-pointer, stack-pointer, adjust-size);
      remove-optionals(be, offset, #f);
    end;

  pattern (be, offset :: <integer>, stack-pointer :: any)
    check-for-valid-stack-adjust(be, offset);
    harp-out (be)
      move(be, reg--tmp1,  4 * (0 + offset)); // no return address
    end;
    call-remove-optionals(be);
end powerpc-template;

define method call-remove-optionals
    (be :: <powerpc-back-end>) => ()
  let remove-opts =
    emit-make-reg(be, remove-optionals-runtime, reg--tmp2);

  // Prepare call address in the link register
  emit-x(be, mtspr-op, remove-opts, lr, r0);

  emit-br-reg(be, bclrl-op, bra-cc);  // Jump to link register

  // remove-optionals won't modify reg--link
  write-return-address(be);

end method;

define method find-size-for-stack-pointer-adjust
    (be :: <powerpc-back-end>, dest :: <register>, count-offset :: <integer>)
  // Get the size of the required arguments on the stack
  let bytes-of-required = count-offset * 4;
  // Get the total size of all the arguments on the stack
  harp-out (be) load-stack-arg-n(be, #f, dest, count-offset) end;
  // the size of the adjustment is  total-size - required-size
  unless (bytes-of-required == 0)
    harp-out (be) sub(be, dest, dest, bytes-of-required) end;
  end unless;
end method;




// MOVE-RETURN-ADDRESS takes an offset, which is the amount in bytes
// by which to move the return address to pop the stack.
// The first argument represents the count register. This may either be #f
// (to indicate that nothing needs to be done to the count) or a virtual
// register (the register is the count as an arg-spill which is updated 
// to reflect the reduced arguments on the stack) or an integer (to 
// indicate that the count and optionals should be removed from the stack.
// in this case the integer is the arg-spill index of the count register).
// The last argument is a register which points into the stack at a location
// which will be effected by the removal of the optionals. This register will
// be updated to point to the same place after the shuffle on the stack. The 
// register may be specified either as an arg-spill index or a real register.


define constant powerpc-allocatable-registers-list 
  = as(<list>, powerpc-allocatable-registers);

with-ops-in powerpc-instructions (move-return-address)
  // ensure that the count register is arg-spilled.
  clash-fn := powerpc-method (duu)
                if (duu-def(1) == #f | instance?(duu-def(1), <integer>))
                  #();
                else
                  list(pair(duu-def(1), powerpc-allocatable-registers-list));
                end if;
              end powerpc-method; 		  
end;


define powerpc-template move-return-address

  pattern (be, count-reg :: <ispill> by colour, offset :: <integer>, update :: any)
    harp-out (be)
      sub(be, count-reg, count-reg, offset);
      move-return-address(be, #f, offset, update);
    end harp-out;

  pattern (be, count-loc :: <integer>, offset :: <integer>, update :: any)
    harp-out (be)
      remove-optionals(be, count-loc, update);
      move-return-address(be, #f, offset, update);
    end harp-out;

  pattern (be, count-reg, offset :: <integer>, update :: any)
    harp-error("MOVE-RETURN-ADDRESS found count in %=.", count-reg);

  pattern (be, count-reg :: any, offset :: <integer>, update :: any)
    unless (offset == 0)
      harp-out (be) add(be, reg--stack, reg--stack, offset) end;
    end unless;

    /*
    check-for-valid-stack-adjust(be, offset);
    unless (offset == 0)
      let adjust = offset - 4;
      // Pop the return address to the new location on the stack:
      harp-out (be) pop-mem(be, reg--stack, adjust) end;
      // finally adjust the stack pointer
      unless (adjust == 0)
	harp-out (be) add(be, reg--stack, reg--stack, adjust) end;
      end unless;
    end unless;
    */

end powerpc-template;



// ADJUST-STACK is the opposite of MOVE-RETURN-ADDRESS. It  takes an 
// offset, which is the amount in bytes to push onto the stack before
// the return address.

define powerpc-template adjust-stack
  pattern (be, offset :: <integer>)
    check-for-valid-stack-adjust(be, offset);
    unless (offset == 0)
      let adjust = offset - 4;
      // first adjust the stack pointer prior to the final push
      unless (adjust == 0)
	harp-out (be) sub(be, reg--stack, reg--stack, adjust) end;
      end unless;
      // Now push the return address to the new location on the stack:
      harp-out (be) push-mem(be, reg--stack, adjust) end;
    end unless;
end powerpc-template;



// LOAD-COUNT-ADJUSTING-STACK is a combination of ADJUST-STACK and 
// LOAD-STACK-ARG-N. Conceptually, the stack is first adjusted, then 
// the count is updated on the stack, the and the count virtual register
// is loaded. This is merged into a single instruction to simplify the 
// need to calculate stack indices in the prolog.
//
// Note that the arg-spill location of the count register (and all other 
// arg-spills) are defined to be valid _after_ this instruction, not before.


with-ops-in powerpc-instructions (load-count-adjusting-stack)
  // ensure that the count register is arg-spilled.
  clash-fn := powerpc-method (duu)
                list(pair(duu-def(1), powerpc-allocatable-registers-list));
              end powerpc-method; 		  
end;


define powerpc-template load-count-adjusting-stack
  pattern (be, stack? :: any, count-reg :: <ispill> by colour, 
           offset :: <integer>, n :: <integer>)
    unless (offset == 0)
      harp-out (be)
        adjust-stack(be, offset);
	// load-stack-arg-n(be, stack?, count-reg, n); // this is a NOP
        add(be, count-reg, count-reg, offset);
      end harp-out;
    end unless;

  pattern (be, stack? :: any, count-reg :: any,
           offset :: <integer>, n :: <integer>)
    unless (offset == 0)
      let tmp = be.registers.reg-tmp1;
      harp-out (be)
        adjust-stack(be, offset);
	load-stack-arg-n(be, stack?, tmp, n);
        add(be, tmp, tmp, offset);
	store-stack-arg-n(be, tmp, n);
      end harp-out;
    end unless;

end powerpc-template;



define method check-for-valid-stack-adjust 
    (be :: <powerpc-back-end>, offset :: <integer>)
  if ((offset ~== 0) & ((offset < 0) | be.variables.with-stack))
    harp-error("Invalid attempt to adjust stack by %= when stack state ~s.",
               offset, be.variables.with-stack);
  end if;
end method;
