module:    pentium-harp
Synopsis:  Pentium save and restore instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Although the 386 does posses a 'push all' instruction, there's no way to
/// prevent it saving/restoring ALL registers. This is not very useful.
/// However, with only 5 of the things, why worry - we don't save eax, esp, ebp
   

// The Dylan calling convention has no preserved registers on the Pentium,
// but the C convention does - so we have to allow for them for call-ins.

define variable *debug-preserved-registers* = #f;

define method show-preserved (be :: <pentium-back-end>) => ()
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


define pentium-template preserve-registers-entry
  pattern (be)
    harp-out (be)
      push(be, reg--frame);
      move(be, reg--frame, reg--stack);
    end;
    let state = be.variables.vreg-state;
    let num-preserved = state.number-preserved;
    if (num-preserved ~== 0)
      let preserved = be.variables.vreg-state.allocated-preserved;
      show-preserved(be);
      for (reg in list-from-prset(be, preserved))
        harp-out (be) push(be, reg) end;
      end for;
    end if;
    let marker = make(<start-frame-marker>, size: 0);
    emit-labelled-constant(be, marker, 0);
end pentium-template;


define pentium-template preserve-registers-exit
  pattern (be)
    let state = be.variables.vreg-state;
    let num-preserved = state.number-preserved;
    let marker = make(<end-frame-marker>, size: 0);
    emit-labelled-constant(be, marker, 0);
    if (num-preserved == 0)
      emit(be, leave);
    else
      let preserved = state.allocated-preserved;
      harp-out (be) add(be, reg--stack, reg--frame, num-preserved * -4) 
      end harp-out;
      for (reg in reverse(list-from-prset(be, preserved)))
        harp-out (be) pop(be, reg) end;
      end for;
      harp-out (be) pop(be, reg--frame) end;
    end if;
end pentium-template;



// LOAD-ADDRESS-OF-STACK-ARG is provided for historical reasons.
// Its use is deprecated. Use LOAD-ADDRESS-OF-STACK-ARG-N instead.

define pentium-template load-address-of-stack-arg
  pattern (be, stack? :: any, d)
    harp-out (be)
      load-address-of-stack-arg-n(be, stack?, d, 0)
    end;
end pentium-template;


// LOAD-ADDRESS-OF-STACK-ARG-N
// Get the address of an arg-spill by its index.

define pentium-template load-address-of-stack-arg-n
  pattern (be, stack? :: any, d, n :: <integer>)
    let add-op = op-element(be.instructions, add);
    operate-on-stack-arg(be, add-op, d, n, stack?);
end pentium-template;


// LOAD-STACK-ARG-N takes N as an offset in words, as for LW. N must
// be constant.

define pentium-template load-stack-arg-n 

  pattern (be, stack? :: any, d, n :: <integer>)
    unless (d.arg-spill?)
      let ld-op = op-element(be.instructions, ld);
      operate-on-stack-arg(be, ld-op, d, n, stack?);
    end unless;

  pattern (be, stack? :: any, d :: any, n)
    #f;

end pentium-template;


with-ops-in pentium-instructions  (load-stack-arg-byte-signed)
  info := op-element(pentium-instructions, ldb-signed);
end;  

with-ops-in pentium-instructions  (load-stack-arg-half-signed)
  info := op-element(pentium-instructions, ldh-signed);
end;  

with-ops-in pentium-instructions  (load-stack-arg-byte-unsigned)
  info := op-element(pentium-instructions, ldb);
end;  

with-ops-in pentium-instructions  (load-stack-arg-half-unsigned)
  info := op-element(pentium-instructions, ldh);
end;  


define pentium-template (load-stack-arg-byte-signed, load-stack-arg-byte-unsigned, 
                         load-stack-arg-half-signed, load-stack-arg-half-unsigned)

  options (self);

  pattern (be, i :: <op> by op-info, stack? :: any, d, n :: <integer>)
    operate-on-stack-arg(be, i, d, n, stack?);

  pattern (be, i, stack? :: any, d :: any, n)
    #f;

end pentium-template;



define method operate-on-stack-arg 
    (be :: <pentium-back-end>, op :: <op>, dest, n :: <integer>, stack?) => ()
  let wstack = be.variables.with-stack;
  unless (stack? == wstack | be.variables.compiling-defasm)
    harp-warning(be, "with-stack wrong");
  end unless;
  if (wstack)
    harp-reapply(be, op, dest, reg--frame, 4 * (2 + n));
  else
    harp-reapply(be, op, dest, reg--stack, 4 * (1 + n));
  end if;
end method;


// LOAD-STACK-INDEX takes N as an offset in bytes. N may be a constant
// or a register. This is used to support the PC's &stack primitive.


define pentium-template load-stack-index

  pattern (be, d, n :: <integer>)
    if (be.variables.with-stack)
      harp-out (be) ld(be, d, reg--frame, n + (4 * 2)) end;
    else
      harp-out (be) ld(be, d, reg--stack, n + 4) end;
    end if;

  pattern (be, d, n)
    if (be.variables.with-stack)
      harp-out (be) ld-index(be, d, reg--frame, n, 4 * 2) end;
    else
      harp-out (be) ld-index(be, d, reg--stack, n, 4) end;
    end if;

  pattern (be, d :: any, n)
    #f;

end pentium-template;


// STORE-STACK-ARG-N takes N as an offset in words, as for LOAD-STACK-ARG-N.

define pentium-template store-stack-arg-n 
  pattern (be, d, n :: <integer>)
    unless (arg-spill?(d) & n == arg-spill-offset-to-arg-number(d.spill-offset))
      let st-op = op-element(be.instructions, st);
      operate-on-stack-arg(be, st-op, d, n, be.variables.with-stack);
    end unless;
end pentium-template;



// REMOVE-OPTIONALS takes an arg-spill index which gives the 
// location of the count of bytes on the stack. All the optional
// arguments from the count backwards are popped from the stack.
// The second arguments is either #f, or a pointer into the stack
// which must be updated because of the remove optionals. This pointer
// may be specified either as a register or as a stack index.

define constant remove-optionals-runtime = 
    runtime-reference("primitive_remove_optionals");


with-ops-in pentium-instructions (remove-optionals, move-return-address, 
                                  adjust-stack, load-count-adjusting-stack)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;

define pentium-template remove-optionals

  // Stack pointer specified as an arg-spill index
  pattern (be, offset :: <integer>, pointer-index :: <integer>)
    let adjust-size = reg--tmp1;
    let pointer-reg = edx;  // we destroy this - so it's a good temporary
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
      move(be, esi,  4 * (1 + offset));
      call(be, remove-optionals-runtime, 0);
    end;

end pentium-template;

define method find-size-for-stack-pointer-adjust
    (be :: <pentium-back-end>, dest :: <register>, count-offset :: <integer>)
  // Get the size of the required arguments on the stack
  let bytes-of-required = count-offset * 4;
  // Get the total size of all the arguments on the stack
  harp-out (be) load-stack-arg-n(be, #f, dest, count-offset) end;
  // the size of the adjustment is  total-size - required-size
  unless (bytes-of-required == 0)
    harp-out (be) sub(be, dest, dest, bytes-of-required) end;
  end unless;
end method;


with-ops-in pentium-instructions (remove-optionals)
  destroys-fn := constant-fn(list(edx));
end;



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


define constant pentium-allocatable-registers-list 
  = as(<list>, pentium-allocatable-registers);

with-ops-in pentium-instructions (move-return-address)
  // ensure that the count register is arg-spilled.
  clash-fn := pentium-method (duu)
                if (duu-def(1) == #f | instance?(duu-def(1), <integer>))
                  #();
                else
                  list(pair(duu-def(1), pentium-allocatable-registers-list));
                end if;
              end pentium-method; 		  
end;


define pentium-template move-return-address

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

end pentium-template;



// ADJUST-STACK is the opposite of MOVE-RETURN-ADDRESS. It  takes an 
// offset, which is the amount in bytes to push onto the stack before
// the return address.

define pentium-template adjust-stack
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
end pentium-template;



// LOAD-COUNT-ADJUSTING-STACK is a combination of ADJUST-STACK and 
// LOAD-STACK-ARG-N. Conceptually, the stack is first adjusted, then 
// the count is updated on the stack, the and the count virtual register
// is loaded. This is merged into a single instruction to simplify the 
// need to calculate stack indices in the prolog.
//
// Note that the arg-spill location of the count register (and all other 
// arg-spills) are defined to be valid _after_ this instruction, not before.


with-ops-in pentium-instructions (load-count-adjusting-stack)
  // ensure that the count register is arg-spilled.
  clash-fn := pentium-method (duu)
                list(pair(duu-def(1), pentium-allocatable-registers-list));
              end pentium-method; 		  
end;


define pentium-template load-count-adjusting-stack
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

end pentium-template;



define method check-for-valid-stack-adjust 
    (be :: <pentium-back-end>, offset :: <integer>)
  if ((offset ~== 0) & ((offset < 0) | be.variables.with-stack))
    harp-error("Invalid attempt to adjust stack by %= when stack state ~s.",
               offset, be.variables.with-stack);
  end if;
end method;
