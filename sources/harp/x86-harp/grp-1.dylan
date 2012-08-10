module:    x86-harp
Synopsis:  The Pentium group-1 instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// (c) Copyright Functional Objects, Inc. 1988 - 1994
///
/// AJW 22/7/88, Tony 1/6/94 (amongst other edits)

/// The group 1 instructions are adc, add, and, cmp, or, sbb, sub.

define constant add2 = #b0;
define constant or2 =  #b001000;
define constant adc2 = #b010000;
define constant sbb2 = #b011000;
define constant and2 = #b100000;
define constant sub2 = #b101000;
define constant xor2 = #b110000;
define constant cmp2 = #b111000;
define constant lea =  #x8d;

with-ops-in pentium-instructions (add2-mem) info := add2 end;
with-ops-in pentium-instructions (sub2-mem) info := sub2 end;
with-ops-in pentium-instructions (and2-mem) info := and2 end;
with-ops-in pentium-instructions (or2-mem)  info :=  or2 end;
with-ops-in pentium-instructions (eor2-mem) info := xor2 end;
with-ops-in pentium-instructions (and2-byte-mem) info := and2 end;
with-ops-in pentium-instructions (or2-byte-mem)  info := or2  end;


// Encode the locked versions of add2-mem and sub2-mem by setting a 
// high bit


define constant locked-ins = #x1000;

with-ops-in pentium-instructions (add2-mem-locked) info := add2 + locked-ins end;
with-ops-in pentium-instructions (sub2-mem-locked) info := sub2 + locked-ins end;

define method opcode-mask (opcode :: <integer>) => (masked-opcode :: <integer>)
  logand(opcode, #xff);
end method;

define method locked-ins? (opcode :: <integer>) => (locked? :: <boolean>)
  logand(opcode, locked-ins) ~== 0;
end method;

define method emit-any-lock-prefix (be :: <x86-back-end>, opcode :: <integer>)
  if (opcode.locked-ins?)
    emit(be, lock);
  end if;
end method;


define local-pentium-template add-setting-flags
  pattern (be, d, s1, s2)
    three-2(be, local-fn(add2), d, s1, s2);
end local-pentium-template;

define pentium-template add
  // We might be able to optimize ADD with the LEA instruction - saving
  // a move into a temporary if the destination is another register. It
  // is not worth doing this unless we have 3 separate regs

  pattern (be, d, s1 is d, s2)
    three-2(be, local-fn(add2), d, d, s2);

  pattern (be, d, s1, s2 is d)
    three-2(be, local-fn(add2), d, s1, d);

  pattern (be, d, s1 :: <m/const-ref> by colour, s2 :: <m/const-ref> by colour)
    canon(be, local-fn(fast-add), d, s1, s2);

  pattern (be, d, s1, s2)
    three-2(be, local-fn(add2), d, s1, s2);

end pentium-template;

define pentium-template sub
  pattern (be, d, s1, s2)
    three-2(be, local-fn(sub2), d, s1, s2);
end pentium-template;

define pentium-template and
  pattern (be, d, s1, s2)
    three-2(be, local-fn(and2), d, s1, s2);
end pentium-template;

define pentium-template or
  pattern (be, d, s1, s2)
    three-2(be, local-fn(or2), d, s1, s2);
end pentium-template;

define pentium-template eor
  pattern (be, d, s1, s2)
    three-2(be, local-fn(xor2), d, s1, s2);
end pentium-template;


define method cmp2-ref (x) => (b :: <boolean>)
 x == cmp2;
end;

define local-pentium-template (adc2, add2, and2, cmp2, or2, sbb2, sub2, xor2)
  options (self);
  
  // Do all eight bit constant refs
  pattern (be, i, d :: <ic/m/spill-ref> by colour, s :: <integer> of eight-bit-const-ref) 
   emit(be, #x83);
   emit-m-c-spill-dest(be, d, i);
   emit-one-byte(be, s);
      
  // And now when the destination is EAX, source 32 bit constant
  pattern (be, i, d by eax-ref, s :: <ac/const-ref> by colour)
    emit(be, 5 + i);
    emit-immediate-constant(be, s);

  // And now any destination, 32 bit constant source
  pattern (be, i, d :: <ic/m/spill-ref> by colour, s :: <ac/const-ref> by colour)
    emit(be, #x81);
    emit-m-c-spill-dest(be, d, i);
    emit-immediate-constant(be, s);

  // anything into register
  pattern (be, i, d :: <real-register> by colour, s :: <ic/m/spill-ref> by colour)
    emit(be, 3 + i);
    emit-m-c-spill-dest(be, s, ex-reg(d));
      
  // register into anything
  pattern (be, i, d :: <ic/m/spill-ref> by colour, s :: <real-register> by colour)
    emit(be, 1 + i);
    emit-m-c-spill-dest(be, d, ex-reg(s));
  
 // address constant - address constant crops up occasionally
  pattern (be, i by cmp2-ref, d :: <ac/const-ref> by colour, s :: <ac/const-ref> by colour)
    harp-out (be)
      move(be, reg--tmp1, d);
    end harp-out;
    call-local(cmp2, be, reg--tmp1, s);

end local-pentium-template;

define local-pentium-template (fast-add)
  // The 2 operands are not spills, and that the destination is a
  // machine register. The LEA instruction can do this - but it does
  // not set the flags. 

  // rrc at word - assume canonicalisation, of course
  pattern (be, d :: <real-register> by colour, r :: <real-register> by colour, s :: <integer>)
    emit(be, lea);
    emit-reg-offset(be, r, s, ex-reg(d));

  // rrr
  pattern (be, d :: <real-register> by colour, r :: <real-register> by colour, s :: <real-register> by colour)
    emit(be, lea);
    emit-reg-indexed(be, r, s, ex-reg(d));

  pattern (be, d, r :: <integer>, s :: <integer>)
    harp-out (be)
      move(be, d, r + s);
    end harp-out;
  
  pattern (be, d :: <ispill> by colour, r, s)
    call-local(fast-add, be, reg--tmp1, r, s);
    harp-out (be)
      move(be, d, reg--tmp1);
    end harp-out;
  
end local-pentium-template;




/// AND2-MEM et al. is like AND2 etc. but the operation occurs directly
/// into memory. The instructions take 3 operands to specify the
/// address in memory. The last of these must be a constant.
/// This allows these instructions to make full use of the 386
/// addressing modes.

define pentium-template (and2-mem, or2-mem, add2-mem, sub2-mem, eor2-mem,
                         add2-mem-locked, sub2-mem-locked)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, r, s :: <integer>, o :: <integer>, w :: <m/spill-ref> by colour) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let wn = ensure-mreg(be, w, tmps);
    let offset = s + o;
    emit-any-lock-prefix(be, i);
    emit(be, 1 + i.opcode-mask);
    emit-reg-offset(be, rn, offset, ex-reg(wn));

  // Handle the case where 2 of the args are constants as is operand 2
  pattern (be, i :: <integer> by op-info, r, s :: <integer>, o :: <integer>, w :: <integer>) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let w8bit = eight-bit-const-ref(w);
    let offset = s + o;
    emit-any-lock-prefix(be, i);
    emit(be, if (w8bit) #x83 else #x81 end);
    emit-reg-offset(be, rn, offset, i.opcode-mask);
    if (w8bit)
      emit-one-byte(be, w);
    else
      emit-four-bytes(be, w);
    end if;

  // Handle the reverse case where 2 of the args are constants
  pattern (be, i, s :: <integer>, r, o :: <integer>, w) 
    harp-reapply(be, i, r, s, o, w);

  // Operation between memory operand and a constant
  pattern (be, i :: <integer> by op-info, r, s, o :: <integer>, w :: <integer>)
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    let w8bit = eight-bit-const-ref(w);
    emit-any-lock-prefix(be, i);
    emit(be, if (w8bit) #x83 else #x81 end);
    emit-double-indexed(be, rn, sn, o, i.opcode-mask);
    if (w8bit)
      emit-one-byte(be, w);
    else
      emit-four-bytes(be, w);
    end if;

  // If there are 3 spills, add s and r first into tmp2 and
  // then go round the template again, knowing that w goes into tmp1.
  pattern (be, i, r :: <ispill> by colour, s :: <ispill> by colour, o :: <integer>, w :: <ispill> by colour)
    harp-out (be)
      add(be, reg--tmp2, r, s);
    end harp-out;
    harp-reapply(be, i, reg--tmp2, 0, o, w);

  // We can handle up 2 spills with tmp1 and tmp2
  pattern (be, i :: <integer> by op-info, r, s, o :: <integer>, w)
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    let wn = ensure-mreg(be, w, tmps);
    emit-any-lock-prefix(be, i);
    emit(be, 1 + i.opcode-mask);
    emit-double-indexed(be, rn, sn, o, ex-reg(wn));

end pentium-template;




/// XADD-MEM-LOCKED is similar to ADD2-MEM-LOCKED, except that it returns 
/// the result of the addition, and doesn't accept the double index


define pentium-template (xadd-mem-locked)

  pattern (be, d :: <real-register> by colour, r :: <real-register> by colour, s :: <ac/const-ref> by colour, w) 
    harp-out (be) move(be, d, w) end;
    emit(be, lock); emit(be, #x0f); emit(be, #xc1);
    emit-reg-offset(be, r, s, ex-reg(d));
    harp-out (be) add(be, d, d, w) end;

  pattern (be, d :: <real-register> by colour, r :: <real-register> by colour, s :: <real-register> by colour, w) 
    harp-out (be) move(be, d, w) end;
    emit(be, lock); emit(be, #x0f); emit(be, #xc1);
    emit-reg-indexed(be, r, s, ex-reg(d));
    harp-out (be) add(be, d, d, w) end;

  pattern (be, d :: <ic/spill-ref> by colour, r :: <real-register> by colour, s :: <ac/const-ref> by colour, w) 
    harp-out (be) 
      xadd-mem-locked(be, reg--tmp1, r, s, w);
      move(be, d, reg--tmp1);
    end harp-out;

  pattern (be, d, r :: <ic/spill-ref> by colour, s :: <ic/spill-ref> by colour, w) 
    harp-out (be)
      move(be, reg--tmp2, r);
      add(be, reg--tmp2, reg--tmp2, s);
      xadd-mem-locked(be, d, reg--tmp2, 0, w);
    end harp-out;

  pattern (be, d, r, s :: <ic/spill-ref> by colour, w) 
    harp-out (be)
      move(be, reg--tmp2, s);
      xadd-mem-locked(be, d, r, reg--tmp2, w);
    end harp-out;

  pattern (be, d, r, s, w) 
    harp-out (be)
      move(be, reg--tmp2, r);
      xadd-mem-locked(be, d, reg--tmp2, s, w);
    end harp-out;

end pentium-template;



/// AND2-BYTE-MEM et al. is like AND2-MEM etc. but for bytes.

define pentium-template (and2-byte-mem, or2-byte-mem)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, r, s :: <integer>, o :: <integer>, w by byte-addressable) 
    let tmps = list(reg--tmp2, reg--tmp1); // make sure wn is a byte reg
    let wn = ensure-mreg(be, w, tmps);     // make sure wn is a byte reg
    let rn = ensure-mreg(be, r, tmps);
    let offset = s + o;
    emit(be, i);
    emit-reg-offset(be, rn, offset, ex-reg(wn));

  // Handle the case where 2 of the args are constants as is operand 2
  pattern (be, i :: <integer> by op-info, r, s :: <integer>, o :: <integer>, w :: <integer> of eight-bit-const-ref) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let offset = s + o;
    emit(be, #x80);
    emit-reg-offset(be, rn, offset, i);
    emit-one-byte(be, w);

  // Handle the reverse case where 2 of the args are constants
  pattern (be, i, s :: <integer>, r, o :: <integer>, w) 
    harp-reapply(be, i, r, s, o, w);

  // Operation between memory operand and a constant
  pattern (be, i :: <integer> by op-info, r, s, o :: <integer>, w :: <integer> of eight-bit-const-ref)
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    emit(be, #x80);
    emit-double-indexed(be, rn, sn, o, i);
    emit-one-byte(be, w);

  // If there are 3 spills, add s and r first into tmp2 and
  // then go round the template again, knowing that w goes into tmp1.
  pattern (be, i, r :: <ispill> by colour, s :: <ispill> by colour, o :: <integer>, w :: <ispill> by colour)
    harp-out (be)
      add(be, reg--tmp2, r, s);
    end harp-out;
    harp-reapply(be, i, reg--tmp2, 0, o, w);

  // We can handle up 2 spills with tmp1 and tmp2
  pattern (be, i :: <integer> by op-info, r, s, o :: <integer>, w)
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    let wn = ensure-mreg(be, w, tmps);
    emit(be, 1 + i);
    emit-double-indexed(be, rn, sn, o, ex-reg(wn));

end pentium-template;


define pentium-template (not)
  pattern (be, d, r :: <integer>)
    harp-out(be)
      move(be, d, lognot(r));
    end harp-out;

  pattern (be, d, r)
    harp-out (be)
      eor(be, d, r, -1);
    end harp-out; 	//I can't be bothered with the NOT instruction

end pentium-template;
