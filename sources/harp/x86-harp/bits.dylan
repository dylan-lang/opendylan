module:    x86-harp
Synopsis:  Pentium bit field bashing instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// LDBITS to be done in a fashion vaguely reminiscent of Clipper:
///
/// LD   WORD dest base 0
/// MOVE WORD ECX offset
/// LSR2 WORD dest ECX             <- supply offset in C for pref
/// MOVE WORD z 1
/// MOVE WORD ECX field-width
/// ASL2 WORD z ECX
/// DEC  WORD z                    Provide the decrement ins specially here
/// AND2 WORD dest z 

///
/// Overall we need 2 registers (z, offset), of which offset must
/// start off in ecx, field being moved there afterward. ecx is live inside,
/// as is the other register (reg::tmp1). We're not terribly fussy about the
/// other two. If dest ends up in a spill that means about 3 memory cycles.
/// Well, what's cache for?


with-ops-in pentium-instructions (ldbits, extract-bits)
  clash-fn 
  :=  pentium-method (duuu)
        list(list(duuu-def(1), duuu-uze(2)),
	     list(duuu-def(1), duuu-uze(3)));
      end pentium-method;

  destroys-fn := ecx-fn;

  c-preserved-destroys-fn 
    :=  pentium-method (duuu)
          destroys-tmp1-if(const-ref(duuu-uze(3)));
        end pentium-method;
end;


/// Coded assuming tmp1 = ESI and tmp2 = ECX - ie modified for PC

define pentium-template ldbits
  pattern (be, d, b, o, f)
    call-local(ld-word, be, d, b, 0);
    harp-out (be) extract-bits(be, d, d, o, f) end;
end pentium-template;



define pentium-template extract-bits

  // Life is short - just make destination & field clash with ECX
  pattern (be, d, w, o, f :: <integer>)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    do-the-shift(be, lsr2, d, o); // shift it down by offset (in ECX) ???? Y.
    call-local(and2, be, d, lognot(ash(-1, f)));
  
  pattern (be, d, w, o, f)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    do-the-shift(be, lsr2, d, o); // shift it down by offset (in ECX) ???? Y.
    harp-out (be) move(be, reg--tmp1, -1) end;
    do-the-shift(be, asl2, reg--tmp1, f);  // shift up by field (in ECX)
    harp-out (be) not(be, reg--tmp1, reg--tmp1) end;	   // that makes the mask
    call-local(and2, be, d, reg--tmp1);	   // that uses it

end pentium-template;


/// OK, storbits is usually even more of a challenge. Here are two contenders.
/// First, courtesy of the 386 h'book.

/// LD   WORD r1 src 0
/// MOVE WORD ECX offset    // pref CL to offset
/// ROR2 WORD r1 CL
/// MOVE WORD r2 field      // annoyingly, this one must be in a register
/// MOVE WORD ECX length    // don't need length after here
/// SHRD r1 r2 CL           // all 3 registers live at the same time !!!!!
/// ROL2 WORD r1 CL
/// MOVE WORD ECX offset
/// ROL2 r1 CL
/// ST   WORD r1 src 0      // ie: we need r1 r2 & ecx available

/// My attempt -

/// MOVE WORD r1 1
/// MOVE WORD ECX length
/// ASL2 WORD r1 ECX
/// DEC  r1
/// MOVE WORD ECX offset    // pref ECX to offset
/// ASL2 WORD r1 ECX
/// ROR2 WORD field ECX
/// NOT  WORD r1
/// LD   WORD ECX dest 0
/// XOR  WORD ECX field
/// AND  WORD ECX r1
/// XOR  WORD ECX field
/// ST   WORD ECX dest 0
/// MOVE WORD ECX offset
/// ROL2 WORD field ECX      // restore the field

/// On the whole, let's use Intel's idea.


define method pentium-rotate
    (be :: <harp-x86-back-end>, op :: <integer>, place, amount :: <object>)
  harp-out (be) move(be, ecx, amount) end;
  emit(be, #xd3);
  emit-m-spill-dest(be, place, op);
end method;

define method pentium-rotate
    (be :: <harp-x86-back-end>, op :: <integer>, place, amount :: <integer>)
  unless (zero?(amount))
     emit(be, #xc1);
     emit-m-spill-dest(be, place, op);
     emit-one-byte(be, amount);
  end unless;
end method;


define method rol2m (be :: <harp-x86-back-end>, place, amount) 
  pentium-rotate(be, #b000000, place, amount);
end;

define method ror2m (be :: <harp-x86-back-end>, place, amount) 
  pentium-rotate(be, #b001000, place, amount);
end;


/// stbits could be better - if s is a register, the move* to reg--tmp2 is
/// superfluous.


with-ops-in pentium-instructions (stbits, set-bits)
  disallow-fn := constant-fn(vector(ecx, reg--tmp3));
  destroys-fn := constant-fn(vector(ecx, esi, reg--tmp3));
  c-preserved-destroys-fn := all-c-preserved-fn; 
end;


define pentium-template stbits
  pattern (be,  b, o, f, s)
    harp-out (be)
      ld(be, reg--tmp3, b, 0);
      set-bits(be, reg--tmp3, reg--tmp3, o, f, s);
      st(be, reg--tmp3, b, 0);
    end harp-out;
end pentium-template;



with-ops-in pentium-instructions (set-bits)
  clash-fn  :=  pentium-method (duuuu)
                  list(list(duuuu-def(1), duuuu-uze(2)),
		       list(duuuu-def(1), duuuu-uze(3)),
		       list(duuuu-def(1), duuuu-uze(4)));
                end pentium-method;
end;



define pentium-template set-bits

  // Firstly detect the easier all-constant-shift case

  pattern (be, d :: <ispill> by colour, w, o, f, s)
    harp-out (be)
      set-bits(be, reg--tmp3, w, o, f, s);
      move(be, d, reg--tmp3);
    end harp-out;

  pattern (be,  d :: <real-register> by colour, w, o :: <integer>, f :: <integer>, s)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    ror2m(be, d, o);
    harp-out (be) move(be, reg--tmp1, s) end;
    emit(be, #x0f, #xac);			// SHRD tmp1 tmp2 constant
    emit-reg-direct(be, d, reg--tmp1.ex-reg);
    emit-one-byte(be, f);
    rol2m(be, d, f);
    rol2m(be, d, o);

  // Let's say that only f is constant

  pattern (be,  d :: <real-register> by colour, w, o, f :: <integer>, s)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    harp-out (be)
      move(be, reg--tmp1, s);
      move(be, ecx, o);		// explicitly set up ecx
    end;
    ror2m(be, d, ecx);
    emit(be, #x0f, #xac);	// SHRD tmp1 tmp2 constant
    emit-reg-direct(be, d, reg--tmp1.ex-reg);
    emit-one-byte(be, f);
    rol2m(be, d, f);
    rol2m(be, d, ecx);

  // Or perhaps only o is constant (less plausible ...)

  pattern (be, d :: <real-register> by colour, w, o :: <integer>, f, s)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    ror2m(be, d, o);
    harp-out (be) 
      move(be, reg--tmp1, s);
      move(be, ecx, f);			// setup ecx
    end;
    emit(be, #x0f, #xad);		// SHRD tmp3 tmp1 ecx
    emit-reg-direct(be, d, reg--tmp1.ex-reg);
    rol2m(be, d, ecx);
    rol2m(be, d, o);

  // Probably both of them are non-constant

  pattern (be, d :: <real-register> by colour, w, o, f, s)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    harp-out (be) 
      move(be, reg--tmp1, s);
      move(be, ecx, o);			// setup ecx
    end;
    ror2m(be, d, ecx);
    harp-out (be) move(be, ecx, f) end;	// setup ecx (try to pref)
    emit(be, #x0f, #xad);		// SHRD tmp3 tmp1 ecx
    emit-reg-direct(be, d, reg--tmp1.ex-reg);
    rol2m(be, d, ecx);
    harp-out (be) move(be, ecx, o) end;
    rol2m(be, d, ecx);

end pentium-template;


/// BITS-MEM and BITC-MEM set and clear bits in memory respectively.
/// They are useful for set-sbits, and are more efficient that st-bits.
/// We use the BTS and BTR 386 instructions, ignoring the effect on
/// the carry flag.

define constant bits-mem = #xab;
define constant bitc-mem = #xb3;

with-ops-in pentium-instructions (bits-mem) info := bits-mem end;
with-ops-in pentium-instructions (bitc-mem) info := bitc-mem end;

with-ops-in pentium-instructions (bitc-mem, bits-mem)
    c-preserved-destroys-fn  := tmp1-fn;
end;


define pentium-template (bits-mem, bitc-mem)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, r, s :: <integer>, 
           o :: <integer>, bit :: <m/spill-ref> by colour) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let bn = ensure-mreg(be, bit, tmps);
    let offset = s + o;
    emit(be, #x0f, i);
    emit-reg-offset(be, rn, offset, bn.ex-reg);

  // Handle the case where 2 of the args are constants as is operand 2
  pattern (be, i :: <integer> by op-info, r, s :: <integer>, 
           o :: <integer>, bit :: <integer> of eight-bit-const-ref) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let offset = s + o;
    let code = if (i == bits-mem) #x28 else #x30 end;
    emit(be, #x0f, #xba);
    emit-reg-offset(be, rn, offset, code);
    emit-one-byte(be, bit);

  // Handle the reverse case where 2 of the args are constants
  pattern (be, i, s :: <integer>, r, o :: <integer>, bit) 
    harp-reapply(be, i, r, s, o, bit);

  // Operation between memory operand and a constant
  pattern (be, i :: <integer> by op-info, r, s, o :: <integer>, bit :: <integer> of eight-bit-const-ref) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    let code = if (i == bits-mem) #x28 else #x30 end;
    emit(be, #x0f, #xba);
    emit-double-indexed(be, rn, sn, o, code);
    emit-one-byte(be, bit);

  // If there are 3 spills, add s and r first into tmp2 and
  // then go round the template again, knowing that bit goes into tmp1.
  pattern (be, i, r :: <ispill> by colour, s :: <ispill> by colour, o :: <integer>, bit :: <ispill> by colour) 
    harp-out (be) add(be, reg--tmp2, r, s) end;
    harp-reapply(be, i, reg--tmp2, 0, o, bit);

  // We can handle up 2 spills with tmp1 and tmp2
  pattern (be, i :: <integer> by op-info, r, s, o :: <integer>, bit) 
    let tmps = temps-list;
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    let bn = ensure-mreg(be, bit, tmps);
    emit(be, #x0f, i);
    emit-double-indexed(be, rn, sn, o, bn.ex-reg);

  // If O is not a constant, then rearrange if poss
  pattern (be, i, r, s :: <integer>, o, bit) 
    harp-reapply(be, i, r, o, s, bit);

  // As a last resort, perform all address calculation in tmp2
  // then go round the template again, knowing that bit goes into tmp1.
  pattern (be, i, r, s, o, bit) 
    harp-out (be) add(be, reg--tmp2, r, s) end;
    harp-out (be) add(be, reg--tmp2, reg--tmp2, o) end;
    harp-reapply(be, i, reg--tmp2, 0, 0, bit);

end pentium-template;



with-ops-in pentium-instructions (set-bit, unset-bit)
    c-preserved-destroys-fn  := tmp1-fn;

    clash-fn := pentium-method (duu)
                if (const-ref(duu-uze(2)))
                  #();
                else
		  list(list(duu-def(1), duu-uze(2)));
                end if;
              end pentium-method;
end with-ops-in;


define constant c-set-bit   = #b101;
define constant c-unset-bit = #b110;

with-ops-in pentium-instructions (set-bit)   info := c-set-bit   end;
with-ops-in pentium-instructions (unset-bit) info := c-unset-bit end;


define pentium-template (set-bit, unset-bit)
  options (self);
  
  // Do all eight bit constant refs
  pattern (be, i :: <integer> by op-info, d, s, bit :: <integer> of eight-bit-const-ref) 
   let mask = ash(1, bit);
   select (i)
     c-set-bit   => harp-out (be) or(be, d, s, mask) end;
     c-unset-bit => harp-out (be) and(be, d, s, lognot(mask)) end;
   end select;
        
  // bit in register
  pattern (be, i :: <integer> by op-info, d :: <ic/m/spill-ref> by colour, s, bit :: <real-register> by colour)
    harp-out (be) move(be, d, s) end;
    emit(be, #x0f, #x83 + ash(i, 3));
    emit-m-c-spill-dest(be, d, ex-reg(bit));
  
  // bit anywhere else
  pattern (be, i, d, s, bit :: <ic/m/spill-ref> by colour)
    harp-out (be) move(be, reg--tmp1, bit) end;
    harp-reapply(be, i, d, s, reg--tmp1);
    
end pentium-template;
