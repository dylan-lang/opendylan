module:    pentium-harp
Synopsis:  Pentium Move instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Plain move comes in 32-bit versions only. We use mov. The zero and sign
/// extend versions aren't worth looking at. Only the 8 -> 32 bit version is
/// worth worrying about, and that saves one byte at the cost of one cycle.



/// Additions for PC ...
/// We must allow for constant references. Address constants are
/// analogous to const-refs. Indirect constants are analogous to
/// spill-refs. Hence we can share templates to avoid too many extra
/// cases. EMIT-M-SPILL-DEST has been modified to allow for indirect
/// constant refs too.


with-ops-in pentium-instructions (move)
  c-preserved-destroys-fn
    :=  pentium-method (du)
          destroys-tmp1-if(ic/spill-ref(du-def(1)) & ic/spill-ref(du-uze(1)))
        end pentium-method;
end with-ops-in;



define pentium-template move

  // first check for eliminatable case
  pattern (be, d, s is d)
    #f;

  // Register destination and zero source
  pattern (be, d :: <real-register> by colour, s :: <integer> of zero-number?)
    harp-out (be) eor(be, d, d, d) end;

  // Register destination and source i32
  pattern (be, d :: <real-register> by colour, s :: <ac/const-ref> by colour)
    emit(be, #xb8 + d.real-register-number);
    emit-immediate-constant(be, s); //!!!! DO THE 8 BIT CASE !!!!

  // Spill/constant destination and source i32/constant
  pattern (be, d :: <ic/spill-ref> by colour, s :: <ac/const-ref> by colour)
    emit(be, #xc7);
    emit-m-c-spill-dest(be, d, 0);
    emit-immediate-constant(be, s);

  // Register/register
  pattern (be, d :: <real-register> by colour, s :: <real-register> by colour)
    emit(be, #x89);
    emit-reg-direct(be, d, s.ex-reg);

  // Register destination and spill/constant source
  pattern (be, d :: <real-register> by colour, s :: <ic/spill-ref> by colour)
    emit(be, #x8b);
    emit-m-c-spill-dest(be, s, d.ex-reg);

  // Register into spill
  pattern (be, d :: <ic/spill-ref> by colour, s :: <real-register> by colour)
    emit(be, #x89);
    emit-m-c-spill-dest(be, d, s.ex-reg);

  // [Indirect constant|Spill]/spill needs a temporary
  pattern (be, d :: <ic/spill-ref> by colour, s :: <ic/spill-ref> by colour)
    harp-out (be)
      move(be, reg--tmp1, s);
      move(be, d, reg--tmp1);
    end harp-out;

end pentium-template;



/// MOVE-LOWER-BYTE moves the 3 most significant bytes of the operand1
/// and the least significant byte of operand 2 into the destination.
/// This is useful for retagging of shorts.


with-ops-in pentium-instructions (move-lower-byte)
  c-preserved-destroys-fn := tmp1-fn;  // not worth being smarter

  prefer-fn := pentium-method (duu)
		 prefer(duu-def(1), byte-addressable-regs);
               end pentium-method;
end;


define pentium-template move-lower-byte

  pattern (be, d by byte-reg-ref, s1, s2 :: <integer> of unsigned-eight-bits?)
    harp-out (be) move(be, d, s1) end;
    emit(be, #xb0 + d.real-register-number);
    emit-one-byte(be, s2);

  pattern (be, d :: <real-register> by colour, s1, s2 :: <integer> of unsigned-eight-bits?)
    harp-out (be) and(be, d, s1, ash(-1, 8)) end;
    unless (zero?(s2))
      harp-out (be) or(be, d, d, s2) end;
    end unless;

  pattern (be, d :: <ic/spill-ref> by colour, s1, s2 :: <integer> of unsigned-eight-bits?)
    harp-out (be) move(be, d, s1) end;
    emit(be, #xc6);
    emit-m-c-spill-dest(be, d, 0);
    emit-one-byte(be, s2);

  pattern (be, d by byte-reg-ref, s1, s2 by byte-reg-ref)
    harp-out (be) move(be, d, s1) end;
    emit(be, #x88);
    emit-reg-direct(be, d, s2.ex-reg);

  // catch-all case
  pattern (be, d, s1, s2)
    harp-out (be)
      and(be, reg--tmp1, s2, #xff);
      and(be, d, s1, ash(-1, 8));
      or(be, d, d, reg--tmp1);
    end harp-out;

end pentium-template;


/// MOVE-ARG-COUNT-BYTE loads the arg-count register with only the low byte
/// of a fixnum. This optimisation saves on code size, as a 4 byte move is big.

define pentium-template move-arg-count-byte

  // the expected case - const into ecx
  pattern (be, d by ecx-ref, s :: <integer> of unsigned-eight-bits?)
    emit(be, #xb1);
    emit-one-byte(be, s);

  // catch-all case
  pattern (be, d, s)
    harp-out (be) move(be, d, s) end;

end pentium-template;

       


/// Now for load - this may use the reg--tmp1 internally, but there's no
/// problem about it returning its result there. The operation is either mov
/// or movzx.


define constant ld-byte = #xb6;
define constant ld-half = #xb7;
define constant ld-word = #x8b;
define constant ld-byte-signed = #xbe;
define constant ld-half-signed = #xbf;

define method emit-ld-operation 
    (be :: <pentium-back-end>, type :: <integer>)
  unless (type == ld-word)
    emit(be, #x0f);
  end unless;
  emit(be, type);
end method;

define pentium-template ld
  pattern (be, d, r, s)
    canon(be, local-fn(ld-word), d, r, s);
end pentium-template;

define pentium-template ldh
  pattern (be, d, r, s)
    canon(be, local-fn(ld-half), d, r, s);
end pentium-template;

define pentium-template ldb
  pattern (be, d, r, s)
    canon(be, local-fn(ld-byte), d, r, s);
end pentium-template;

define pentium-template ldh-signed
  pattern (be, d, r, s)
    canon(be, local-fn(ld-half-signed), d, r, s);
end pentium-template;

define pentium-template ldb-signed
  pattern (be, d, r, s)
    canon(be, local-fn(ld-byte-signed), d, r, s);
end pentium-template;


define method coerce-constant-with-offset 
    (be :: <pentium-back-end>, 
     addr-const :: <constant-reference>, 
     offset :: <integer>) => (c :: <constant-reference>)
  if (offset = 0)
    addr-const;
  else
    call-instruction(constant-ref, be, addr-const.cr-refers-to-object,
                     offset: offset + addr-const.cr-const-offset);
  end if;
end method;

define method coerce-constant-with-offset 
    (be :: <pentium-back-end>, 
     offset1 :: <integer>, 
     offset2 :: <integer>) => (o :: <integer>)
  offset1 + offset2;
end method;


define method ld-op (index-op :: <integer>)
  select (index-op)
    ld-word  => local-fn(ld-word);
    ld-half  => local-fn(ld-half);
    ld-byte  => local-fn(ld-byte);
    ld-half-signed  => local-fn(ld-half-signed);
    ld-byte-signed  => local-fn(ld-byte-signed);
    otherwise => #f;
  end;
end method;


define local-pentium-template (ld-word, ld-half, ld-byte, 
			       ld-half-signed, ld-byte-signed)
  options (self);

  // sxx is a load to the temporary register followed by a move to spill
  pattern (be, i by ld-op, d :: <ic/spill-ref> by colour, r, s) 
    i(be, reg--tmp1, r, s);
    harp-out (be) move(be, d, reg--tmp1) end;

  // rrc at word - assume canonicalisation, of course
  pattern (be, i, d :: <real-register> by colour, r :: <real-register> by colour, s :: <ac/const-ref> by colour)
    emit-ld-operation(be, i);
    emit-reg-offset(be, r, s, d.ex-reg);

  // rrr
  pattern (be, i, d :: <real-register> by colour, r :: <real-register> by colour, s :: <real-register> by colour)
    emit-ld-operation(be, i);
    emit-reg-indexed(be, r, s, d.ex-reg);

  // rrs we load the temporary and load off it
  pattern (be, i by ld-op, d :: <real-register> by colour, r :: <real-register> by colour, s :: <ic/spill-ref> by colour)
    harp-out (be) move(be, reg--tmp1, s) end;
    i(be, d, r, reg--tmp1);

  // rsc we do like rrs
  pattern (be, i by ld-op, d :: <real-register> by colour, r :: <ic/spill-ref> by colour, s :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, r) end;
    i(be, d, reg--tmp1, s);

  // rss we construct the address with an add
  pattern (be, i by ld-op, d :: <real-register> by colour, r :: <ic/spill-ref> by colour, s :: <ispill> by colour)
    harp-out (be)
      move(be, reg--tmp1, r);
      add(be, reg--tmp1, reg--tmp1, s);
    end;
    i(be, d, reg--tmp1, 0);

  // Address constant with fixed constant:
  pattern (be, i, d :: <real-register> by colour, r :: <i-address-constant-reference>, s :: <integer>)
    let new = coerce-constant-with-offset(be, r, s);
    emit-ld-operation(be, i);
    emit-reg-constant-offset(be, new, d.ex-reg);

  // rcc
  pattern (be, i, d :: <real-register> by colour, r :: <integer>, s :: <integer>)
    emit-ld-operation(be, i);
    emit-reg-constant-offset(be, s + r, d.ex-reg);

end local-pentium-template;


// define the emit values for the indexed load ops as constants

define constant ld-index  = ld-word;
define constant ldb-index = ld-byte;
define constant ldh-index = ld-half;
define constant ldb-index-signed = ld-byte-signed;
define constant ldh-index-signed = ld-half-signed;

// and also put the constants in the info field of the ops for easy
// access by the templates.

with-ops-in pentium-instructions (ld-index)  info := ld-index  end;
with-ops-in pentium-instructions (ldb-index) info := ldb-index end;
with-ops-in pentium-instructions (ldh-index) info := ldh-index end;
with-ops-in pentium-instructions (ldb-index-signed) info := ldb-index-signed end;
with-ops-in pentium-instructions (ldh-index-signed) info := ldh-index-signed end;

define method simple-load-op
    (be :: <pentium-back-end>, index-op :: <integer>) => (op :: <op>)
  let instrs = be.instructions;
  select (index-op)
    ld-index  => op-element(instrs, ld);
    ldb-index => op-element(instrs, ldb);
    ldh-index => op-element(instrs, ldh);
    ldb-index-signed => op-element(instrs, ldb-signed);
    ldh-index-signed => op-element(instrs, ldh-signed);
  end select;
end method;



/// LD-INDEX is like LD but takes an extra argument which must be a
/// constant. This allows array indexing to make full use of the 386
/// addressing modes.

define pentium-template (ld-index, ldb-index, ldh-index, 
                         ldb-index-signed, ldh-index-signed)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, r, s :: <ac/const-ref> by colour, o :: <integer>)
    let base = coerce-constant-with-offset(be, s, o);
    harp-reapply(be, simple-load-op(be, i), d, r, base);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, s :: <ac/const-ref> by colour, r, o :: <integer>)
    let base = coerce-constant-with-offset(be, s, o);
    harp-reapply(be, simple-load-op(be, i), d, r, base);

  pattern (be, i, d :: <ispill> by colour, r, s, o :: <integer>)
    harp-reapply(be, i, reg--tmp1, r, s, o);
    harp-out (be) move(be, d, reg--tmp1) end;

  // The main case where we can make use of the 386 addressing modes.
  pattern (be, i :: <integer> by op-info, d :: <real-register> by colour, r, s, o :: <integer>)
    let tmps = list(reg--tmp1);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    if (sn)
      emit-ld-operation(be, i);
      emit-double-indexed(be, rn, sn, o,d.ex-reg);
    else
      // Where both base registers are spills, we must add them
      // together first.
      harp-out (be) add(be, rn, rn, s) end;  // rn is tmp1
      emit-ld-operation(be, i);
      emit-reg-offset(be, rn, o, d.ex-reg);
    end if;

end pentium-template;




with-ops-in pentium-instructions (ld-index-scaled)  info := ld-index  end;
with-ops-in pentium-instructions (ldh-index-scaled) info := ldh-index end;

with-ops-in pentium-instructions (ldh-index-scaled-signed) 
  info := ldh-index-signed
end;



/// LD-INDEX-SCALED is like LD-INDEX but the first of the offset parameters
/// is scaled by the size of the data item for the load.  This allows array
/// indexing to make full use of the 386 addressing modes.


define pentium-template (ld-index-scaled, ldh-index-scaled, ldh-index-scaled-signed)
  options (self);

  // Handle the case where the scale arg is constant
  pattern (be, i :: <integer> by op-info, d, r, s :: <integer>, o :: <integer>)
    let scale = i.scale-factor;
    harp-reapply(be, simple-load-op(be, i), d, r, (s * scale) + o);

  pattern (be, i, d :: <ispill> by colour, r, s, o :: <integer>)
    harp-reapply(be, i, reg--tmp1, r, s, o);
    harp-out (be) move(be, d, reg--tmp1) end;

  // Handle the case where the unscaled base arg is constant
  pattern (be, i :: <integer> by op-info, d :: <real-register> by colour, r :: <ac/const-ref> by colour, s, o :: <integer>)
    let scale = i.scale-factor;
    let tmps = list(reg--tmp1);
    let sn = ensure-mreg(be, s, tmps);
    let base = coerce-constant-with-offset(be, r, o);
    emit-ld-operation(be, i);
    emit-reg-offset-scaled(be, sn, scale, base, d.ex-reg);

  // The main case where we can make use of the 386 addressing modes.
  pattern (be, i :: <integer> by op-info, d :: <real-register> by colour, r, s, o :: <integer>)
    let scale = i.scale-factor;
    let tmps = list(reg--tmp1, reg--tmp2);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    emit-ld-operation(be, i);
    emit-double-index-scaled(be, sn, scale, rn, o, d.ex-reg);

end pentium-template;


/// Instructions for loading from the Thread Environment Block
///
/// First, some general support for segment registers:-


define method segment-prefix 
    (segment :: <pentium-segment-register>) => (prefix :: <integer>)
  select (segment)
    es => #x26;
    cs => #x2e;
    ss => #x36;
    ds => #x3e;
    fs => #x64;
    gs => #x65;
  end select;
end method;




define method op--store-thread-local
    (be :: <pentium-back-end>, data, offset :: <integer>) => ()
  emit(be, fs.segment-prefix);
  harp-out(be)
    st(be, data, 0, offset);
  end harp-out;
end method;

define method op--load-thread-local
    (be :: <pentium-back-end>, dest :: <real-register>, offset :: <integer>) => ()
  emit(be, fs.segment-prefix);
  harp-out(be)
    ld(be, dest, 0, offset);
  end harp-out;
end method;

define method op--tlb-base-register 
    (be :: <pentium-windows-back-end>, dest :: <real-register>) => ()
  op--load-thread-local(be, dest, #x14);
end method;


/// Linux TEB support:
/// We use Linux' TLV support here.

define method op--tlb-base-register 
    (be :: <pentium-unix-back-end>, dest :: <real-register>) => ()
  op--load-thread-local(be, dest, /* dummy */ 0);
end method;

define constant $teb = 
    thread-local-runtime-reference("teb");

define method op--store-thread-local
    (be :: <pentium-linux-back-end>, data, offset :: <integer>) => ()
  emit(be, gs.segment-prefix);
  harp-out(be)
    st(be, data, $teb, 0 /*ignore offset for Linux */);
  end harp-out;
end method;

define method op--load-thread-local
    (be :: <pentium-linux-back-end>, dest :: <real-register>, offset :: <integer>) => ()
  emit(be, gs.segment-prefix);
  harp-out(be)
    ld(be, dest, $teb, 0 /*ignore offset for Linux */);
  end harp-out;
end method;

define method op--store-thread-local
    (be :: <pentium-freebsd-back-end>, data, offset :: <integer>) => ()
  harp-out(be)
    push(be, reg--tmp2);
  end harp-out;
  emit(be, gs.segment-prefix);
  harp-out(be)
    ld(be, reg--tmp2, 0, 0);
    st(be, data, $teb, reg--tmp2);
    pop(be, reg--tmp2);
  end harp-out;
end method;

define method op--load-thread-local
    (be :: <pentium-freebsd-back-end>, dest :: <real-register>, offset :: <integer>) => ()
  emit(be, gs.segment-prefix);
  harp-out(be)
    ld(be, dest, 0, 0);
    ld(be, dest, $teb, dest);
  end harp-out;
end method;


/// Now the templates

// Temporary: the TLB is currently stored in the Windows TIB at offset #x14
// (reserved for application use as pvArbitrary).

with-ops-in pentium-instructions (get-teb, set-teb) info := #x14 end;
with-ops-in pentium-instructions (get-seh, set-seh) info := 0 end;


define pentium-template (get-teb, get-seh)
  options (self);

  pattern (be, index :: <integer> by op-info, dest :: <real-register> by colour)
    op--load-thread-local(be, dest, index);

  pattern (be, index :: <integer> by op-info, dest)
    op--load-thread-local(be, reg--tmp1, index);
    harp-out (be) move(be, dest, reg--tmp1) end;
end pentium-template;


define pentium-template (set-teb, set-seh)
  options (self);

  pattern (be, index :: <integer> by op-info, data :: <ic/spill-ref> by colour)
    harp-out (be) move(be, reg--tmp1, data) end;
    op--store-thread-local(be, reg--tmp1, index);  /* may clobber reg--tmp2 */

  pattern (be, index :: <integer> by op-info, data)
    op--store-thread-local(be, data, index);       /* may clobber reg--tmp2 */

end pentium-template;



define pentium-template (ld-teb)

  pattern (be, dest :: <ic/spill-ref> by colour, index)
    harp-out (be)
      ld-teb(be, reg--tmp1, index);
      move(be, dest, reg--tmp1);
    end harp-out;

  pattern (be, dest :: <real-register> by colour, index :: <integer>)
    op--tlb-base-register(be, dest);
    harp-out (be)
      ld(be, dest, dest, index);
    end harp-out;
end pentium-template;


define pentium-template (ld-teb-address)

  pattern (be, dest :: <ic/spill-ref> by colour, index)
    harp-out (be)
      ld-teb-address(be, reg--tmp1, index);
      move(be, dest, reg--tmp1);
    end harp-out;

  pattern (be, dest :: <real-register> by colour, index :: <integer>)
    op--tlb-base-register(be, dest);
    unless (index == 0)
      harp-out (be)
        add(be, dest, dest, index);
      end harp-out;
    end unless;
end pentium-template;


define pentium-template (st-teb)

  pattern (be, data :: <ic/spill-ref> by colour, index :: <integer>)
    harp-out (be)
      move(be, reg--tmp2, data);
      st-teb(be, reg--tmp2, index);
    end;

  pattern (be, data, index :: <integer>)
    op--tlb-base-register(be, reg--tmp1);
    harp-out (be)
      st(be, data, reg--tmp1, index);
    end harp-out;

end pentium-template;


define pentium-template (get-stack-bottom)
  // The stack bottom (i.e., the highest address) is stored at offxet #x04
  // in the TEB
  pattern (be, dest :: <real-register> by colour)
    op--load-thread-local(be, dest, #x04);
end pentium-template;


/// Store on this godforsaken processor with only 8 registers presents
/// problems. We will potentially need 2 temporary registers when doing the
/// spill/spill/spill case. Ugh! Furthermore, we can store at byte only
/// registers A C D B. Storing at half requires a prefix of the operand size
/// prefix $66.

// This code differs from the CL version because of the losss of first
// class variable objects. Instead we give constant values to each of 
// the names for later identity testing. We choose as constant values, 
// something which will be directly emitted in at least oine circumstance.

define constant st-byte = #xc6;
define constant st-half = #x89;
define constant st-word = #xc7;

define method emit-st-operation 
    (be :: <pentium-back-end>, type :: <integer>, src)
  if (ac/const-ref(src))
    if (type == st-half)
      emit(be, #x66, #xc7);
    else
      emit(be, type);
    end if;
  else
    select (type)
      st-byte => if (byte-reg-ref(src))
                   emit(be, #x88);
                 else
                   harp-error("Attempt to ST register %s at BYTE fails", src);
                 end if;
      st-half => emit(be, #x66, #x89);
      st-word => emit(be, #x89);
    end select;
  end if;
end method;


define method ex-whatsit (x)
  if (ac/const-ref(x)) 0 else x.ex-reg end;
end method;



define method emit-possible-immediate-arg 
    (be :: <harp-back-end>, x, l :: <integer>)
  if (ac/const-ref(x))
    if (instance?(x, <abstract-integer>))
      select (l)
        st-byte => emit-one-byte(be, x);
        st-half => emit-two-bytes(be, x);
        st-word => emit-four-bytes(be, x);
      end select;
    else
      select (l)
        st-word => emit-constant-ref(be, x);
      end select;
    end if;
  end if;
end method;
	
define pentium-template st
  pattern (be, d, r, s)
    canon(be, local-fn(st-word), d, r, s);
end pentium-template;

define pentium-template sth
  pattern (be, d, r, s)
    canon(be, local-fn(st-half), d, r, s);
end pentium-template;

define pentium-template stb
  pattern (be, d, r, s)
    canon(be, local-fn(st-byte), d, r, s);
end pentium-template;


define method word-store (x)
  if (x == st-word) st-op(x) else #f end;
end method;


define method st-op (index-op :: <integer>)
  select (index-op)
    st-word  => local-fn(st-word);
    st-half  => local-fn(st-half);
    st-byte  => local-fn(st-byte);
    otherwise => #f;
  end;
end method;


define local-pentium-template (st-word, st-half, st-byte)
  options (self);

  // srx case for st-word must not clobber TMP2 as this is the
  // *safe-reg-for-st*. But for st-byte we MUST use tmp2.
  pattern (be, i by word-store, d :: <ic/spill-ref> by colour, r :: <real-register> by colour, s :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, d) end;
    i(be, reg--tmp1, r, s);

  // sxx case is load spill to second temp than run rxx.
  pattern (be, i by st-op, d :: <ic/spill-ref> by colour, r, s)
    harp-out (be) move(be, reg--tmp2, d) end;
    i(be, reg--tmp2, r, s);

  // rrc
  pattern (be, i, d, r :: <real-register> by colour, s :: <ac/const-ref> by colour)
    emit-st-operation(be, i, d);
    emit-reg-offset(be, r, s, d.ex-whatsit);
    emit-possible-immediate-arg(be, d, i);
   
  /// rrr
  pattern (be, i, d, r :: <real-register> by colour, s :: <real-register> by colour)
    emit-st-operation(be, i, d);
    emit-reg-indexed(be, r, s, d.ex-whatsit);
    emit-possible-immediate-arg(be, d, i);

  // special case - rrs with the addressing register as esi (= tmp1)
  // we make esi live inside the instruction, and destructively modify it.
  pattern (be, i by st-op, d, r by esi-ref, s :: <ic/spill-ref> by colour)
    harp-out (be) add(be, esi, esi, s) end;
    i(be, d, esi, 0);

  // rrs goes via one temporary
  pattern (be, i by st-op, d, r :: <real-register> by colour, s :: <ic/spill-ref> by colour)
    harp-out (be) move(be, reg--tmp1, s) end;
    i(be, d, r, reg--tmp1);

  // rsc also goes via one temporary
  pattern (be, i by st-op, d, r :: <ic/spill-ref> by colour, s :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, r) end;
    i(be, d, reg--tmp1, s);

  // rss involves constructing the address with an add
  pattern (be, i by st-op, d, r :: <ic/spill-ref> by colour, s :: <ic/spill-ref> by colour)
    harp-out (be)
      move(be, reg--tmp1, r);
      add(be, reg--tmp1, reg--tmp1, s);
    end;
    i(be, d, reg--tmp1, 0);

  // Address constant with fixed constant:
  pattern (be, i, d, r :: <i-address-constant-reference>, s :: <integer>)
    let new = coerce-constant-with-offset(be, r, s);
    emit-st-operation(be, i, d);
    emit-reg-constant-offset(be, new, d.ex-whatsit);
    emit-possible-immediate-arg(be, d, i);

  // Address constant with fixed constant:
  pattern (be, i, d, r :: <integer>, s :: <integer>)
    emit-st-operation(be, i, d);
    emit-reg-constant-offset(be, r + s, d.ex-whatsit);
    emit-possible-immediate-arg(be, d, i);

end local-pentium-template;


// define the emit values for the indexed store ops as constants

define constant st-index  = st-word;
define constant stb-index = st-byte;
define constant sth-index = st-half;

// and also put the constants in the info field of the ops for easy
// access by the templates.

with-ops-in pentium-instructions (st-index)  info := st-index  end;
with-ops-in pentium-instructions (stb-index) info := stb-index end;
with-ops-in pentium-instructions (sth-index) info := sth-index end;


define method simple-store-op
    (be :: <pentium-back-end>, index-op :: <integer>) => (op :: <op>)
  let instrs = be.instructions;
  select (index-op)
    st-index  => op-element(instrs, st);
    stb-index => op-element(instrs, stb);
    sth-index => op-element(instrs, sth);
  end select;
end method;


/// ST-INDEX is like ST but takes an extra argument which must be a
/// constant. This allows array indexing to make full use of the 386
/// addressing modes. 


define pentium-template (st-index, stb-index, sth-index)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, r, s :: <ac/const-ref> by colour, o :: <integer>)
    let base = coerce-constant-with-offset(be, s, o);
    harp-reapply(be, simple-store-op(be, i), d, r, base);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, s :: <ac/const-ref> by colour, r, o :: <integer>)
    let base = coerce-constant-with-offset(be, s, o);
    harp-reapply(be, simple-store-op(be, i), d, r, base);

  // The main case.
  // If 2 of the 3 arguments are spills, we can use the temporaries we
  // have reserved. If all 3 are spills, we add two of them into a
  // temporary, and still get by with only 2 temps.
  pattern (be, i :: <integer> by op-info, d, r, s, o :: <integer>)
   let tmps = list(reg--tmp2, reg--tmp1);
   // NB tmp2 is 1st in tmp list because it's byte-adressable (necessary for d)
   let dn = ac/const-ref(d) | ensure-mreg(be, d, tmps);
   let rn = ensure-mreg(be, r, tmps);
   let sn = ensure-mreg(be, s, tmps);
   if (sn)
     emit-st-operation(be, i, dn);
     emit-double-indexed(be, rn, sn, o, dn.ex-whatsit);
     emit-possible-immediate-arg(be, dn, i);
   else
     // This is the pathological case where d r s are all spills.
     harp-out (be) add(be, rn, rn, s) end;  // rn is tmp1
     emit-st-operation(be, i, dn);
     emit-reg-offset(be, rn, o, dn.ex-whatsit);
     emit-possible-immediate-arg(be, dn, i);
   end if;

end pentium-template;





with-ops-in pentium-instructions (st-index-scaled)  info := st-index  end;
with-ops-in pentium-instructions (sth-index-scaled) info := sth-index end;


define method scale-factor
    (index-op :: <integer>) => (scale :: <integer>)
  select (index-op)
    st-index  => 4; //  words are 4 bytes
    ld-index  => 4; //  words are 4 bytes
    sth-index => 2; //  halves are 2
    ldh-index => 2; //  halves are 2
    ldh-index-signed => 2; //  halves are 2
  end select;
end method;


/// ST-INDEX-SCALED is like ST-INDEX but the first of the offset parameters
/// is scaled by the size of the data item for the store. This allows array 
/// indexing to make full use of the 386 addressing modes. 


define pentium-template (st-index-scaled, sth-index-scaled)
  options (self);

  // Handle the case where the scale arg is constant
  pattern (be, i :: <integer> by op-info, d, r, s :: <integer>, o :: <integer>)
    let scale = i.scale-factor;
    harp-reapply(be, simple-store-op(be, i), d, r, (s * scale) + o);

  // Handle the case where the unscaled base arg is constant
  pattern (be, i :: <integer> by op-info, d, r :: <ac/const-ref> by colour, s, o :: <integer>)
    let scale = i.scale-factor;
    let tmps = list(reg--tmp1, reg--tmp2);
    let dn = ac/const-ref(d) | ensure-mreg(be, d, tmps);
    let sn = ensure-mreg(be, s, tmps);
    let base = coerce-constant-with-offset(be, r, o);
    emit-st-operation(be, i, dn);
    emit-reg-offset-scaled(be, sn, scale, base, dn.ex-whatsit);
    emit-possible-immediate-arg(be, dn, i);

  // The main case.
  // If all 3 args are spills, we need 3 temporary registers. Hence tmp3
  // is disallowed, as well as tmp2.
  pattern (be, i :: <integer> by op-info, d, r, s, o :: <integer>)
    let scale = i.scale-factor;
    let tmps = list(reg--tmp1, reg--tmp2, reg--tmp3);
    let dn = ac/const-ref(d) | ensure-mreg(be, d, tmps);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    emit-st-operation(be, i, dn);
    emit-double-index-scaled(be, sn, scale, rn, o, dn.ex-whatsit);
    emit-possible-immediate-arg(be, dn, i);

end pentium-template;



/// CONDITIONAL-MOVE
/// Use the CMPXCHG instruction, with the LOCK prefix to guarantee atomicity
/// even with multiprocessors


define pentium-template (conditional-move)

  /// For this instruction, constant references are treated as direct
  /// (address) constant references even if they are actually 
  /// indirect. This is designed to make life easier for harp-cg
  /// to avoid having to allocate a new direct constant.

  pattern (be, tag, var :: <real-register> by colour, new-val :: <real-register> by colour, comp)
    harp-out (be) move(be, eax, comp) end;
    emit(be, lock);
    emit(be, #x0f);
    emit(be, #xb1);
    emit-reg-offset(be, var, 0, new-val.ex-reg);
    emit-branch-sdi(be, bne-x, tag);

  pattern (be, tag, var :: <i-constant-reference>, new-val :: <real-register> by colour, comp)
    harp-out (be) move(be, eax, comp) end;
    emit(be, lock);
    emit(be, #x0f);
    emit(be, #xb1);
    emit-constant-operand(be, var, new-val.ex-reg);
    emit-branch-sdi(be, bne-x, tag);

  pattern (be, tag, var :: <ispill> by colour, new-val :: <real-register> by colour, comp)
    harp-out (be) 
      move(be, reg--tmp2, var);
      conditional-move(be, tag, reg--tmp2, new-val, comp);
    end;

  pattern (be, tag, var, new-val, comp)
    harp-out (be) 
      move(be, reg--tmp1, new-val);
      conditional-move(be, tag, var, reg--tmp1, comp);
    end;

end pentium-template;



define constant $vector-cond-move-destroys = vector(eax, reg--tmp1, reg--tmp2);

with-ops-in pentium-instructions (conditional-move)

  c-preserved-destroys-fn :=  pentium-method (uuu)
                                destroys-tmp1-if(ic/spill-ref(uuu-uze(2))) 
                              end pentium-method;

  destroys-fn := constant-fn($vector-cond-move-destroys);

  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(3), $vector-eax);
               end pentium-method; 

  clash-fn := pentium-method (uuu)
                list(list(uuu-uze(1), eax),
                     list(uuu-uze(2), eax));
              end pentium-method;

end with-ops-in;
