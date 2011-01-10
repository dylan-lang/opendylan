module:    pentium-harp
Synopsis:  General support for Pentium FP
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// 387 opcodes and operands -- a mess
//
//  memory addressing mode is:
//
//  11011XXX mmXXXrrr <optional displacement> <optional immediate value>
// where XXX XXX are opcodes
// mm is an addressing mode, rrr specifies a register (and segment):
// mm  rrr  :  segment   register    mode
// 00  000      data      eax        (reg)
// 00  001      data      ecx        (reg)
// 00  010      data      edx        (reg)
// 00  011      data      ebx        (reg)
// 00  100      s-i-b format
// 00  101      data      ---        (<disp32>)   // or is this an immediate?
// 00  110      data      esi        (reg)
// 00  111      data      edi        (reg)

// 01  000      data      eax        (reg+<disp8>)
// 01  001      data      ecx        (reg+<disp8>)
// 01  010      data      edx        (reg+<disp8>)
// 01  011      data      ebx        (reg+<disp8>)
// 01  100      s-i-b format
// 01  101      stack     ebp        (reg+<disp8>)
// 01  110      data      esi        (reg+<disp8>)
// 01  111      data      edi        (reg+<disp8>)

// 10  000      data      eax        (reg+<disp32>)
// 10  001      data      ecx        (reg+<disp32>)
// 10  010      data      edx        (reg+<disp32>)
// 10  011      data      ebx        (reg+<disp32>)
// 10  100      s-i-b format
// 10  101      stack     ebp        (reg+<disp32>)
// 10  110      data      esi        (reg+<disp32>)
// 10  111      data      edi        (reg+<disp32>)

// 11  000      ST(0)      // 387 registers, note
// 11  001      ST(1)
// 11  010      ST(2)
// 11  011      ST(3)
// 11  100      ST(4)
// 11  101      ST(5)
// 11  110      ST(6)
// 11  111      ST(7)

// for s-i-b format modes, there is a following s-i-b byte:
// mm       ss ind bas  :  segment   reg   mode
// 00       xx ind 000      data     eax    (bas + xx*ind)    // xx represents power of 2
// 00       xx ind 001      data     ecx    (bas + xx*ind)
// 00       xx ind 010      data     edx    (bas + xx*ind)
// 00       xx ind 011      data     ebx    (bas + xx*ind)
// 00       xx ind 100      stack    esp    (bas + xx*ind)
// 00       xx ind 101      data   -------  (<disp32> + xx*ind)
// 00       xx ind 110      data     esi    (bas + xx*ind)
// 00       xx ind 111      data     edi    (bas + xx*ind)

// 01       xx ind 10y      stack    bas    (bas + xx*ind + <disp8>)
// 01       xx ind yyy      data     bas    (bas + xx*ind + <disp8>)

// 11       xx ind 10y      stack    bas    (bas + xx*ind + <disp32>)
// 11       xx ind yyy      data     bas    (bas + xx*ind + <disp32>)

// note that for ind:  000  eax
//                     001  ecx
//                     010  edx
//                     011  ebx
//                     100  use zero  (scale field must be 00)
//                     101  ebp
//                     110  esi
//                     111  edi
      


define constant flt-esc = #b11011000;  // low 3 bits are opcode

define constant mc-fld =  #b000000;
define constant mc-fldd = #b101000;
define constant mc-fstp = #b011000;
define constant mc-fstpd = #b111000;

define constant mf-word   =   #b010;
define constant mf-dbword =   #b110;
define constant mf-single =   #b000;
define constant mf-double =   #b100;


define method emit-fspill-operand 
    (be :: <pentium-back-end>, spill :: <fspill>, ex :: <integer>, 
     #key hilo = 0)
  let offset :: <integer> = hilo + signed-frame-pointer-offset(be, spill);
  if (signed-eight-bits?(offset))
    emit(be, mod01 + ex + 5);		// stack seg, (ebp+<disp8>)
    emit-one-byte(be, offset);
  else
    emit(be, mod10 + ex + 5);		// stack seg, (ebp+<disp32>)
    emit-four-bytes(be, offset);
  end if;
end method;



define method emit-f-constant-operand 
    (be :: <pentium-back-end>, 
     const-ref :: <indirect-constant-reference>, 
     ex :: <integer>, 
     #key hilo = 0)
  let shifted-const-ref = 
    if (hilo.zero?)
      const-ref
    else
      // If we have to worry about the offset, then make a new
      // constant reference, without worrying about its type
      call-instruction(constant-ref, be,  const-ref.cr-refers-to-object,
  		       offset: hilo + const-ref.cr-const-offset,
                       mode: #"indirect");

    end if;
  emit-constant-operand(be, shifted-const-ref, ex);
end method;


define method emit-f-c-spill-operand 
    (be :: <pentium-back-end>, float :: <fspill>, ex :: <integer>, 
     #key hilo = 0)
  emit-fspill-operand(be, float, ex, hilo: hilo);
end method;


define method emit-f-c-spill-operand 
    (be :: <pentium-back-end>, float :: <indirect-constant-reference>, ex :: <integer>, 
     #key hilo = 0)
  emit-f-constant-operand(be, float, ex, hilo: hilo);
end method;


define constant float-push-single = mf-single;

define constant float-push-double = mf-double;

define local-pentium-template (float-push-single, float-push-double)
  options (self);

  pattern (be, mf, val :: <fspill> by colour)
    emit(be, mf + flt-esc +  1);
    emit-fspill-operand(be, val, mc-fld);

  pattern (be, mf, val by f-indirect-constant-ref)
    emit(be, mf + flt-esc +  1);
    emit-constant-operand(be, val, mc-fld);

/*
  pattern (be, mf, val by f-address-constant-ref) /// THIS IS WRONG
    emit(be, mf + flt-esc +  1);
    emit-constant-operand(be, val, mc-fld);
*/

end local-pentium-template;

define method push-single (be :: <pentium-back-end>, val)
  call-local(float-push-single, be, val)
end method;

define method push-double (be :: <pentium-back-end>, val)
  call-local(float-push-double, be, val)
end method;

define method push-integer (be :: <pentium-back-end>, s :: <ispill>)
  push-integer-internal(be, reg--frame, signed-frame-pointer-offset(be, s));
end method;

define method push-integer (be :: <pentium-back-end>, s)
  harp-out (be) push(be, s) end;
  push-integer-internal(be, reg--stack, 0);
  harp-out (be) add(be, reg--stack, reg--stack, 4) end;
end method;

define method push-integer-internal
    (be :: <pentium-back-end>, base :: <register>, offset :: <integer>)
  emit(be, flt-esc + mf-word + 1);
  emit-reg-offset(be, base, offset, mc-fld);
end method;

define method push-double-integer (be :: <pentium-back-end>, low, high)
  harp-out (be) 
    push(be, high);
    push(be, low);
  end;
  emit(be, flt-esc + mf-dbword + 1);
  emit-reg-offset(be, reg--stack, 0, mc-fldd);
  harp-out (be) add(be, reg--stack, reg--stack, 8) end;
end method;


define method pop-single (be :: <pentium-back-end>, spill-ref :: <sfspill>)
  emit(be, flt-esc + mf-single + 1);
  emit-fspill-operand(be, spill-ref, mc-fstp);
end method;

define method pop-single
    (be :: <pentium-back-end>, dest :: <pentium-float-register>)
  emit(be, flt-esc + mf-single + 1);
  emit-reg-direct(be, dest, mc-fstp);
end method;

define method pop-double (be :: <pentium-back-end>, spill-ref :: <dfspill>)
  emit(be, flt-esc + mf-double + 1);
  emit-fspill-operand(be, spill-ref, mc-fstp);
end method;

define method pop-double 
    (be :: <pentium-back-end>, dest :: <pentium-float-register>)
  emit(be, flt-esc + mf-double + 1);
  emit-reg-direct(be, dest, mc-fstp);
end method;

define method pop-integer (be :: <pentium-back-end>, d :: <ispill>)
  pop-integer-internal(be, reg--frame, signed-frame-pointer-offset(be, d));
end method;

define method pop-integer (be :: <pentium-back-end>, d)
  harp-out (be) sub(be, reg--stack, reg--stack, 4) end;
  pop-integer-internal(be, reg--stack, 0);
  harp-out (be) pop(be, d) end;
end method;

define method pop-integer-internal
    (be :: <pentium-back-end>, base :: <register>, offset :: <integer>)
  emit(be, flt-esc + mf-word + 1);
  emit-reg-offset(be, base, offset, mc-fstp);
end method;



define method pop-double-integer (be :: <pentium-back-end>, low, high)
  harp-out (be) sub(be, reg--stack, reg--stack, 8) end;
  emit(be, flt-esc + mf-dbword + 1);
  emit-reg-offset(be, reg--stack, 0, mc-fstpd);
  harp-out (be)
    pop(be, low);
    pop(be, high);
  end;
end method;



define variable fpu-stack-cautious? = #t;

define method emit-fpu-reset (be :: <pentium-back-end>)
  if (fpu-stack-cautious?)
    emit(be, flt-esc + #b011, #b11100011);
  end if;
end method;

