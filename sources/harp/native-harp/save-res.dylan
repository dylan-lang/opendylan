module:    native-main-harp
Synopsis:  Native save/restore instructions
Author:    Tony Mann, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// LOAD-ADDRESS-OF-STACK-ARG is provided for historical reasons.
// Its use is deprecated. Use LOAD-ADDRESS-OF-STACK-ARG-N instead.

define macro load-address-of-stack-arg-template-definer
  { define ?back-end:name load-address-of-stack-arg-template }
    =>
  { define ?back-end ## "-template" load-address-of-stack-arg
      pattern (?=be, stack? :: any, d)
        harp-out (?=be)
          load-address-of-stack-arg-n(?=be, stack?, d, 0)
        end;
    end ?back-end ## "-template" }
end macro;


// LOAD-ADDRESS-OF-STACK-ARG-N
// Get the address of an arg-spill by its index.

define macro load-address-of-stack-arg-n-template-definer
  { define ?back-end:name load-address-of-stack-arg-n-template }
    =>
  { define ?back-end ## "-template" load-address-of-stack-arg-n
      pattern (?=be, stack? :: any, d, n :: <integer>)
        let add-op = op-element(?=be.instructions, add);
        operate-on-stack-arg(?=be, add-op, d, n, stack?);
    end ?back-end ## "-template" }
end macro;


// LOAD-STACK-ARG-N takes N as an offset in words, as for LW. N must
// be constant.

define macro load-stack-arg-n-template-definer
  { define ?back-end:name load-stack-arg-n-template }
    =>
  { define ?back-end ## "-template" load-stack-arg-n 
    
      pattern (?=be, stack? :: any, d, n :: <integer>)
        unless (d.arg-spill?)
          let ld-op = op-element(?=be.instructions, ld);
          operate-on-stack-arg(?=be, ld-op, d, n, stack?);
        end unless;
    
      pattern (?=be, stack? :: any, d :: any, n)
        #f;
    
    end ?back-end ## "-template" }
end macro;

define macro load-frame-offset-template-definer
  { define ?back-end:name load-frame-offset-template }
    =>
  { define ?back-end ## "-template" load-frame-offset
    
      pattern (?=be, stack? :: any, d, n :: <integer>)
        if (n < 0)
          harp-out(?=be)
    	    ld(?=be, d, ?=reg--frame, 4 * n - size-of-preserved-registers(?=be));
          end;
        else
          harp-out(?=be)
            ld(?=be, d, ?=reg--frame, 4 * (2 + n));
          end;
        end;
    
    end ?back-end ## "-template" }
end macro;

define macro store-frame-offset-template-definer
  { define ?back-end:name store-frame-offset-template }
    =>
  { define ?back-end ## "-template" store-frame-offset
    
      pattern (?=be, s, n :: <integer>)
        if (n < 0)
          harp-out(?=be)
    	    st(?=be, s, ?=reg--frame, 4 * n - size-of-preserved-registers(?=be));
          end;
        else
          harp-out(?=be)
            st(?=be, s, ?=reg--frame, 4 * (2 + n));
          end;
        end;
    
    end ?back-end ## "-template" }
end macro;


// STORE-STACK-ARG-N takes N as an offset in words, as for LOAD-STACK-ARG-N.

define macro store-stack-arg-n-template-definer
  { define ?back-end:name store-stack-arg-n-template }
    =>
  { define ?back-end ## "-template" store-stack-arg-n
      pattern (?=be, d, n :: <integer>)
        unless (arg-spill?(d) & n == arg-spill-offset-to-arg-number(d.spill-offset))
          let st-op = op-element(?=be.instructions, st);
          operate-on-stack-arg(?=be, st-op, d, n, ?=be.variables.with-stack);
        end unless;
    end ?back-end ## "-template" }
end macro;


define method operate-on-stack-arg 
    (be :: <harp-native-back-end>, op :: <op>, dest, n :: <integer>, stack?) => ()
  let wstack = be.variables.with-stack;
  unless (stack? == wstack | be.variables.compiling-defasm)
    harp-warning(be, "with-stack wrong");
  end unless;
  if (wstack)
    harp-reapply(be, op, dest, be.registers.reg-frame, 4 * (2 + n));
  else
    harp-reapply(be, op, dest, be.registers.reg-stack, 4 * (return-address-size(be) + n));
  end if;
end method;


// REMOVE-OPTIONALS takes an arg-spill index which gives the 
// location of the count of bytes on the stack. All the optional
// arguments from the count backwards are popped from the stack.
// The second arguments is either #f, or a pointer into the stack
// which must be updated because of the remove optionals. This pointer
// may be specified either as a register or as a stack index.

define constant remove-optionals-runtime = 
    runtime-reference("primitive_remove_optionals");


define macro remove-optionals-template-definer
  { define ?back-end:name remove-optionals-template (?tmp1:name, ?tmp2:name, ?tmp3:name) }
    =>
  { define ?back-end ## "-template" remove-optionals
    
      // Stack pointer specified as an arg-spill index
      pattern (?=be, offset :: <integer>, pointer-index :: <integer>)
        let adjust-size = ?tmp1;
        let pointer-reg = ?tmp2;
        find-size-for-stack-pointer-adjust(?=be, adjust-size, offset);
        harp-out (?=be) 
          // First find the original pointer
          load-stack-arg-n(?=be, #f, pointer-reg, pointer-index);
          // Add in the adjustment
          add(?=be, pointer-reg, pointer-reg, adjust-size);
          // and store back
          store-stack-arg-n(?=be, pointer-reg, pointer-index);
          remove-optionals(?=be, offset, #f);
        end;
    
      // Stack pointer specified as a register
      pattern (?=be, offset :: <integer>, stack-pointer)
        let adjust-size = ?tmp1;
        find-size-for-stack-pointer-adjust(?=be, adjust-size, offset);
        harp-out (?=be) 
          // now add this to stack pointer
          add(?=be, stack-pointer, stack-pointer, adjust-size);
          remove-optionals(?=be, offset, #f);
        end;
    
      pattern (?=be, offset :: <integer>, stack-pointer :: any)
        check-for-valid-stack-adjust(?=be, offset);
        harp-out (?=be)
          move(?=be, ?tmp3,  4 * (return-address-size(?=be) + offset));
        end;
        call-remove-optionals(?=be);
    end ?back-end ## "-template" }
end macro;

define open generic call-remove-optionals
    (be :: <harp-native-back-end>) => ();

define method call-remove-optionals
    (be :: <harp-native-back-end>) => ()
  harp-out (be)
    call(be, remove-optionals-runtime, 0);
  end;
end method;


define method find-size-for-stack-pointer-adjust
    (be :: <harp-native-back-end>, dest :: <register>, count-offset :: <integer>)
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



define macro with-move-return-address-ops
  { with-move-return-address-ops ?back-end:name end }
    => { with-ops-in ?back-end ## "-instructions" (move-return-address)
	   // ensure that the count register is arg-spilled.
	   clash-fn := ?back-end ## "-method" (duu)
	     if (duu-def(1) == #f | instance?(duu-def(1), <integer>))
	       #();
	     else
	       list(pair(duu-def(1), ?back-end ## "-allocatable-registers-list"));
	     end if;
	   end ?back-end ## "-method";
         end }
end macro;

define macro move-return-address-template-definer
  { define ?back-end:name move-return-address-template }
    =>
  { define ?back-end ## "-template" move-return-address
      pattern (?=be, count-reg :: <ispill> by colour, offset :: <integer>, update :: any)
        harp-out (?=be)
          sub(?=be, count-reg, count-reg, offset);
          move-return-address(?=be, #f, offset, update);
        end harp-out;

      pattern (?=be, count-loc :: <integer>, offset :: <integer>, update :: any)
        harp-out (?=be)
          remove-optionals(?=be, count-loc, update);
          move-return-address(?=be, #f, offset, update);
        end harp-out;

      pattern (?=be, count-reg, offset :: <integer>, update :: any)
        harp-error("MOVE-RETURN-ADDRESS found count in %=.", count-reg);

      pattern (?=be, count-reg :: any, offset :: <integer>, update :: any)
        check-for-valid-stack-adjust(?=be, offset);
        unless (offset == 0)
          if-return-address ()
            let adjust = offset - 4;
            // Pop the return address to the new location on the stack:
            harp-out (?=be) pop-mem(?=be, ?=reg--stack, adjust) end;
            // finally adjust the stack pointer
            unless (adjust == 0)
	      harp-out (?=be) add(?=be, ?=reg--stack, ?=reg--stack, adjust) end;
            end unless;
          else
            harp-out (?=be) add(?=be, ?=reg--stack, ?=reg--stack, offset) end;
          end;
        end unless;

    end ?back-end ## "-template" }
end macro;


// ADJUST-STACK is the opposite of MOVE-RETURN-ADDRESS. It  takes an 
// offset, which is the amount in bytes to push onto the stack before
// the return address.

define macro adjust-stack-template-definer
  { define ?back-end:name adjust-stack-template }
    =>
  { define ?back-end ## "-template" adjust-stack
      pattern (?=be, offset :: <integer>)
        check-for-valid-stack-adjust(?=be, offset);
        unless (offset == 0)
          if-return-address ()
            let adjust = offset - 4;
            // first adjust the stack pointer prior to the final push
            unless (adjust == 0)
	      harp-out (?=be) sub(?=be, ?=reg--stack, ?=reg--stack, adjust) end;
            end unless;
            // Now push the return address to the new location on the stack:
            harp-out (?=be) push-mem(?=be, ?=reg--stack, adjust) end;
          else
            harp-out (?=be) sub(?=be, ?=reg--stack, ?=reg--stack, offset) end;
          end;
        end unless;
    end ?back-end ## "-template" }
end macro;


// LOAD-COUNT-ADJUSTING-STACK is a combination of ADJUST-STACK and 
// LOAD-STACK-ARG-N. Conceptually, the stack is first adjusted, then 
// the count is updated on the stack, the and the count virtual register
// is loaded. This is merged into a single instruction to simplify the 
// need to calculate stack indices in the prolog.
//
// Note that the arg-spill location of the count register (and all other 
// arg-spills) are defined to be valid _after_ this instruction, not before.



define macro with-load-count-adjusting-stack-ops
  { with-load-count-adjusting-stack-ops ?back-end:name end }
    => { with-ops-in ?back-end ## "-instructions" (load-count-adjusting-stack)
           // ensure that the count register is arg-spilled.
           clash-fn := ?back-end ## "-method" (duu)
                         list(pair(duu-def(1), ?back-end ## "-allocatable-registers-list"));
                       end ?back-end ## "-method"; 		  
         end }
end macro;


define macro load-count-adjusting-stack-template-definer
  { define ?back-end:name load-count-adjusting-stack-template }
    =>
  { define ?back-end ## "-template" load-count-adjusting-stack
      pattern (?=be, stack? :: any, count-reg :: <ispill> by colour, 
               offset :: <integer>, n :: <integer>)
        unless (offset == 0)
          harp-out (?=be)
            adjust-stack(?=be, offset);
	    // load-stack-arg-n(?=be, stack?, count-reg, n); // this is a NOP
            add(?=be, count-reg, count-reg, offset);
          end harp-out;
        end unless;

      pattern (?=be, stack? :: any, count-reg :: any,
               offset :: <integer>, n :: <integer>)
        unless (offset == 0)
          let tmp = ?=be.registers.reg-tmp1;
          harp-out (?=be)
            adjust-stack(?=be, offset);
	    load-stack-arg-n(?=be, stack?, tmp, n);
            add(?=be, tmp, tmp, offset);
	    store-stack-arg-n(?=be, tmp, n);
          end harp-out;
        end unless;

    end ?back-end ## "-template" }
end macro;


define method check-for-valid-stack-adjust 
    (be :: <harp-native-back-end>, offset :: <integer>)
  if ((offset ~== 0) & ((offset < 0) | be.variables.with-stack))
    harp-error("Invalid attempt to adjust stack by %= when stack state ~s.",
               offset, be.variables.with-stack);
  end if;
end method;
