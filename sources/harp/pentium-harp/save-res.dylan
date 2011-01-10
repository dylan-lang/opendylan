module:    pentium-harp
Synopsis:  Pentium save and restore instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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



define pentium load-address-of-stack-arg-template;

define pentium load-address-of-stack-arg-n-template;


define pentium load-stack-arg-n-template;

define pentium load-frame-offset-template;

define pentium store-frame-offset-template;


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


define pentium store-stack-arg-n-template;


with-ops-in pentium-instructions (remove-optionals, move-return-address, 
                                  adjust-stack, load-count-adjusting-stack)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;

define pentium remove-optionals-template (reg--tmp1, edx, esi);


with-ops-in pentium-instructions (remove-optionals)
  destroys-fn := constant-fn(list(edx));
end;



define constant pentium-allocatable-registers-list 
  = as(<list>, pentium-allocatable-registers);

with-move-return-address-ops pentium end;

define pentium move-return-address-template;

define pentium adjust-stack-template;


with-load-count-adjusting-stack-ops pentium end;

define pentium load-count-adjusting-stack-template;

