module:    powerpc-harp
Synopsis:  PowerPC call/jump code
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




with-ops-in powerpc-instructions (call, call-indirect)
  destroys-fn := constant-fn(powerpc-not-preserved);
end;

with-ops-in powerpc-instructions (call-alien)
  destroys-fn := constant-fn(powerpc-allocatable-registers);
end;


define method do-fast-jump
    (be :: <powerpc-back-end>, base, offset, branch-op,
     #key indirect?)
  let dest-reg =
    if (indirect?)
      harp-out(be) ld(be, reg--tmp1, base, offset) end;
      reg--tmp1
    elseif (offset == 0 & instance?(base, <real-register>))
      base;
    else
      harp-out(be) add(be, reg--tmp1, base, offset) end;
      reg--tmp1
    end;
  emit-x(be, mtspr-op, dest-reg, ctr, r0);  // move address to count register
  emit-br-reg(be, branch-op, bra-cc)
end method do-fast-jump;


with-ops-in powerpc-instructions (jmp-alien, call-alien) info := #t end;
with-ops-in powerpc-instructions (jmp,       call)       info := #f end;

with-ops-in powerpc-instructions (call, call-indirect, jmp-indirect)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;


define powerpc-template (call, call-alien)
  options (self);

  pattern (be, alien? :: <boolean> by op-info, dest, nregs)

    unless (be.variables.with-stack)
      harp-out(be) save-return-address(be) end;
    end;

    // tmp1 - tmp4 are inadmissible for alien calls because
    // they are argument registers
    let tmp = if (alien?) reg--tmp5 else reg--tmp1 end;
    let dd = emit-make-reg(be, dest, tmp);

    emit-x(be, mtspr-op, dd, lr, r0);  // Prepare call address in the link register

    emit-br-reg(be, bclrl-op, bra-cc);  // Jump to link register

    unless (be.variables.with-stack)
      harp-out(be) restore-return-address(be) end;
    end;

end powerpc-template;

define powerpc-template save-return-address

  pattern (be)

    read-return-address(be);
    harp-out(be) push(be, reg--link) end;

end powerpc-template;

define powerpc-template restore-return-address

  pattern (be)

    harp-out(be) pop(be, reg--link) end;
    write-return-address(be);

end powerpc-template;


/*
define powerpc-template leaf-call

  pattern (be, dest, nregs)
    // get the return address to push
    emit-x(be, mfspr-op, reg--link, lr, r0);

    harp-out(be)
      push(be, reg--link);
      call(be, dest, nregs);
      pop(be, reg--link);
    end;

    // put the return address back into LR
    emit-x(be, mtspr-op, reg--link, lr, r0)

end powerpc-template;
*/

define method read-return-address(be :: <powerpc-back-end>) => ()
  // get the return address to push
  emit-x(be, mfspr-op, reg--link, lr, r0);
end method;

define method write-return-address(be :: <powerpc-back-end>) => ()
  // put the return address back into LR
  emit-x(be, mtspr-op, reg--link, lr, r0)
end method;


define powerpc-template (jmp, jmp-alien)
  options (self);

  pattern (be, i, dest :: <ispill> by colour, nregs, 
           ret-addr-shift, count :: any, adjust :: any)
    harp-error("Attempt to tail call with a spill.");

  pattern (be, alien? :: <boolean> by op-info, dest, nregs, 
           ret-addr-shift, count :: any, adjust :: any)
    harp-out (be) move-return-address(be, count, ret-addr-shift, adjust) end;
    // tmp1 - tmp4 are inadmissible for alien calls because
    // they are argument registers
    let tmp = if (alien?) reg--tmp5 else reg--tmp1 end;
    let dd = emit-make-reg(be, dest, tmp);

    // Important: use count register *not* link register
    // so as to preserve the link register for future returns
    // to parent frames
    emit-x(be, mtspr-op, dd, ctr, r0);  // Prepare call address in the count register
    emit-br-reg(be, bcctr-op, bra-cc);   // Jump to count register

end powerpc-template;


define powerpc-template call-indirect

  pattern (be, dest, offset, nregs)
    do-fast-jump(be, dest, offset, bcctrl-op, indirect?: #t);

end powerpc-template;

define powerpc-template (jmp-indirect)

  pattern (be, dest :: <ispill> by colour, offset, nregs, ret-addr-shift, count :: any, adjust :: any)
    harp-error("Attempt to tail call with a spill.");

  pattern (be, dest, offset, nregs, ret-addr-shift, count :: any, adjust :: any)
    harp-out (be) move-return-address(be, count, ret-addr-shift, adjust) end;
    do-fast-jump(be, dest, offset, bcctr-op, indirect?: #t);

end powerpc-template;

/*

define method scl-method (self :: <new-sdi>, span, code?)
  if (code?)
    push!(pair(new-sdi-code-fragment(self), - span), *scl-list*);
    #f;
  else
    0
  end
end method scl-method;

define constant scl-method-vector = vector(scl-method);

define powerpc-template scl

  pattern (be, thing)
   emit-sdi(be,
	    make(<new-sdi>,
		 dest-tag: *start-tag*, dest-offset: 0,
		 cached-size: 0, method-index: 0,
		 method-vector: scl-method-vector,
		 code-fragment: thing));

end powerpc-template;


*/


define powerpc-template bsr

  pattern (be, tag)
    emit-call-sdi(be, tag);

end powerpc-template;


define method ppc-call-sdi
    (self :: <new-sdi>, span :: <integer>, code?)
  if (code?)
    sdi-l(bl-op, span);
  else 4
  end
end method ppc-call-sdi;

define constant call-sdi-vector = vector(ppc-call-sdi);

define method emit-call-sdi
    (be :: <powerpc-back-end>, tag :: <tag>, #key offset = 0)
  let sdi =
    make(<new-sdi>,
	 dest-tag: tag,
	 dest-offset: offset,
	 cached-size: 4,      // all instrs 4 bytes
	 method-index: 0,
	 method-vector: call-sdi-vector);
  emit-sdi(be, sdi)
end method emit-call-sdi;




/// RTS is just a jump to the address in the link register.  It is up to
/// the preserve registers code to ensure the link register is maintained
/// at exit.


define powerpc-template rts

  pattern (be)
    // pop the return address
    // harp-out(be) pop(be, reg--link) end;

    // jump to the address in LR
    emit-br-reg(be, bclr-op, bra-cc);

end powerpc-template;

