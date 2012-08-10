module:    x86-harp
Synopsis:  Pentium pop/push instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define pentium-template pop-mem

  // pop a real register ...
  pattern (be, d :: <real-register> by colour, o :: <integer>)
    emit(be, #x8f);
    emit-reg-offset(be, d, o, 0);

  pattern (be, d :: <real-register> by colour, o :: <real-register> by colour)
    emit(be, #x8f);
    emit-double-indexed(be, d, o, 0, 0);

end pentium-template;


define pentium-template pop

  // pop the flags register
  pattern (be, d by direction-flag-ref)
    emit(be, popfd);

  // pop a real register ...
  pattern (be, d :: <real-register> by colour)
    emit(be, #x58 + d.real-register-number);

  // or a spill ...
  pattern (be, d :: <ic/spill-ref> by colour)
    emit(be, #x8f);
    emit-m-c-spill-dest(be, d, 0);

end pentium-template;


/// Pushes ...

with-ops-in pentium-instructions (push-mem)
  c-preserved-destroys-fn
    :=  pentium-method (uu)
          destroys-tmp1-if(ic/spill-ref(uu-uze(1)) 
                           | ic/spill-ref(uu-uze(2)))
        end pentium-method;
end with-ops-in;

define pentium-template push-mem

  // push a real register ...
  pattern (be, d :: <real-register> by colour, o :: <integer>)
    emit(be, #xff);
    emit-reg-offset(be, d, o, #b00110000);

  pattern (be, d :: <real-register> by colour, o :: <real-register> by colour)
    emit(be, #xff);
    emit-double-indexed(be, d, o, 0, #b00110000);

  pattern (be, d :: <ic/spill-ref> by colour, o)
    harp-out (be)
      move(be, reg--tmp1, d);
      push-mem(be, reg--tmp1, o);
    end harp-out;

  pattern (be, d, o :: <ic/spill-ref> by colour)
    harp-out (be)
      move(be, reg--tmp2, d);
      push-mem(be, reg--tmp2, o);
    end harp-out;

end pentium-template;


define pentium-template push

  // push the flags register
  pattern (be, d by direction-flag-ref)
    emit(be, pushfd);

  // push a real register ...
  pattern (be, d :: <real-register> by colour)
    emit(be, #x50 + d.real-register-number);

  // or a spill ...
  
  pattern (be, d :: <sf-ic/spill-ref> by colour)
    emit(be, #xff);
    emit-f-c-spill-operand(be, d, #b110000);

  pattern (be, d :: <df-ic/spill-ref> by colour)
    emit(be, #xff);
    emit-f-c-spill-operand(be, d, #b110000, hilo: 4); // first push the high word
    emit(be, #xff);
    emit-f-c-spill-operand(be, d, #b110000, hilo: 0); // then the low

  pattern (be, d :: <ic/spill-ref> by colour)
    emit(be, #xff);
    emit-m-c-spill-dest(be, d, #b110000);

  // or a short constant ...
  pattern (be, d :: <integer> of eight-bit-const-ref)
    emit(be, #x6a);
    emit-one-byte(be, d);

  // or a long one ...
  pattern (be, d :: <ac/const-ref> by colour)
    emit(be, #x68);
    emit-immediate-constant(be, d);

end pentium-template;


define pentium-template t-push
  pattern (be, d, stack)
    if (stack == reg--stack)
      harp-out (be) push(be, d) end;
    else
      harp-out (be)
        sub(be, stack, stack, 4);
	st(be, d, stack, 0);
      end harp-out;
    end if;
end pentium-template;

   

define pentium-template t-pop
  pattern (be, d, stack)
    if (stack == reg--stack)
      harp-out (be) pop(be, d) end;
    else
      harp-out (be) ld(be, d, stack, 0) end;
      unless (d == stack)
        harp-out (be)  add(be, stack, stack, 4) end;
      end unless;
    end if;
end pentium-template;


