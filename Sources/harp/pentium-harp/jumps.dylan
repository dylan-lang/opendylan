module:    pentium-harp
Synopsis:  Pentium jump and call instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// When this function returns true, we use the assembler to generate code
// from our symbolic information.

define open generic emit-jumps-symbolically?(be :: <pentium-back-end>) => (b :: <boolean>);

define method emit-jumps-symbolically?(be :: <pentium-back-end>) => (b :: <boolean>)
  #t;
end;

define method emit-jumps-symbolically?(be :: <pentium-linux-back-end>) => (b :: <boolean>)
  #f;
end;

define method jmp-indirect-code (return) => (i :: <integer>)
  if (return) #b010000 else #b100000 end;
end method;

define method jmp-direct-code (return) => (i :: <integer>)
  if (return) call else jmp end;
end method;



define local-pentium-template (jump-indirect)
  
  pattern (be, return :: any, d :: <real-register> by colour, o :: <integer>)
    emit(be, grp5);
    emit-reg-offset(be, d, o, jmp-indirect-code(return));

  pattern (be, return :: any, d :: <ic/spill-ref> by colour, o :: <integer>)
    harp-out (be) move(be, reg--tmp1, d) end;
    emit(be, grp5);
    emit-reg-offset(be, reg--tmp1, o, jmp-indirect-code(return));

  pattern (be, return :: any, d :: <i-address-constant-reference>, o :: <integer>)
    harp-out (be) ld(be, reg--tmp1, d, o) end;
    emit(be, grp5);
    emit-reg-offset(be, reg--tmp1, 0, jmp-indirect-code(return));

end local-pentium-template;


define local-pentium-template (jump)

  pattern (be, return :: any, d :: <i-address-constant-reference>)
    if (be.emit-jumps-symbolically?)
      let opcode = if (return) #"CALL" else #"JMP" end;
      let value  = if (return) call else jmp end;
      let instruction-size = 5;
      emit-constant-ref-with-opcode(be, opcode, value, d, instruction-size);
    else
      emit(be, jmp-direct-code(return));
      emit-constant-ref-relative(be, d);
    end if;

  pattern (be, return :: any, d :: <ic/m/spill-ref> by colour)
    emit(be, grp5);
    emit-m-c-spill-dest(be, d, jmp-indirect-code(return));

end local-pentium-template;


/// Foreign calls must be made with the direction flag clear.
/// For simplicity's sake, we do that in the jmp-alien and call-alien
/// templates.

with-ops-in pentium-instructions (jmp-alien, call-alien) info := #t end;
with-ops-in pentium-instructions (jmp,       call)       info := #f end;

with-ops-in pentium-instructions (call, call-indirect, jmp-indirect)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;

define pentium-template (call, call-alien)
  options (self);

  pattern (be, alien? :: <boolean> by op-info, dest, nregs)
    if (alien?) harp-out (be) clear-direction-flag(be) end end;
    call-local(jump, be, #t, dest);
end pentium-template;


define pentium-template (jmp, jmp-alien)
  options (self);

  pattern (be, i, dest :: <ispill> by colour, nregs, 
           ret-addr-shift, count :: any, adjust :: any)
    harp-error("Attempt to tail call with a spill.");

  pattern (be, alien? :: <boolean> by op-info, dest, nregs, 
           ret-addr-shift, count :: any, adjust :: any)
    harp-out (be) move-return-address(be, count, ret-addr-shift, adjust) end;
    if (alien?) harp-out (be) clear-direction-flag(be) end end;
    call-local(jump, be, #f, dest);
end pentium-template;


define pentium-template (call-indirect)
  pattern (be, dest, offset, nregs)
    call-local(jump-indirect, be, #t, dest, offset);
end pentium-template;


define pentium-template (jmp-indirect)
  pattern (be, dest :: <ispill> by colour, offset, nregs, ret-addr-shift, count :: any, adjust :: any)
    harp-error("Attempt to tail call with a spill.");

  pattern (be, dest, offset, nregs, ret-addr-shift, count :: any, adjust :: any)
    harp-out (be) move-return-address(be, count, ret-addr-shift, adjust) end;
    call-local(jump-indirect, be, #f, dest, offset);
end pentium-template;

