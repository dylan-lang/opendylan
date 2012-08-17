module:    x86-harp
Synopsis:  Pentium branch instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


with-ops-in pentium-instructions (bmvset, bmvunset, bz-bytes, bnz-bytes)
  disallow-fn := ecx-fn;
end with-ops-in;


with-ops-in pentium-instructions 
    (bz-bytes, bnz-bytes, beq-byte, bne-byte, 
     bge-byte, bgt-byte, ble-byte, blt-byte)
  prefer-fn := pentium-method (uu)
		 prefer(uu-uze(1), byte-addressable-regs);
                 prefer(uu-uze(2), byte-addressable-regs);
               end pentium-method;
end with-ops-in;

with-ops-in pentium-instructions (bit, nbit)
  prefer-fn := pentium-method (duuu)
		 prefer(duuu-uze(1), byte-addressable-regs);
               end pentium-method;
end with-ops-in;




///                        386 op
/// BEQ length dest s1 s2   JZ    if s1 & s2 (sized length) equal, goto dest
/// BNE length dest s1 s2   JNZ   same, but not equal
/// BIT length dest x y z         branch if (x & y) /= z to w

/// BGT length dest s1 s2   JG    branch if s1 signed greater than s2
/// BGE length dest s1 s2   JGE   branch if s1 signed greater or equal s2
/// BLT length dest s1 s2   JL    branch if s1 signed less than s2
/// BLE length dest s1 s2   JLE   branch if s1 signed less than or equal s2

/// BHI length dest s1 s2   JA    branch if s1 unsigned greater than s2
/// BHS length dest s1 s2   JAE   branch if s1 unsigned greater or equal s2
/// BLO length dest s1 s2   JB    branch if s1 unsigned less than s2
/// BLS length dest s1 s2   JBE   branch if s1 unsigned less than or equal s2

define constant beq-x = #x74;
define constant bne-x = #x75;
define constant bgt-x = #x7f;
define constant bge-x = #x7d;
define constant blt-x = #x7c;
define constant ble-x = #x7e;
define constant bhi-x = #x77;
define constant bhs-x = #x73;
define constant bcc-x = #x73;    // carry clear
define constant blo-x = #x72;
define constant bcs-x = #x72;    // carry set
define constant bls-x = #x76;
// define constant bp-x =  #x7a;    // parity flag test for doing 
//                                  // float unordered comparisons
// define constant bpo-x = #x7b;


/// General SDI emitter

define method emit-general-sdi 
    (be :: <harp-x86-back-end>, tag :: <tag>, 
     method-vector :: <simple-object-vector>, 
     cached-size :: <integer>,
     #key dest-offset = 0, code = #f)

  emit-sdi(be, make(<new-sdi>, dest-tag: tag,
		               dest-offset: dest-offset,
		               cached-size: cached-size,
                               method-index: 0,
		               method-vector: method-vector,
                               code-fragment: code));
end method;


/// sdis for conditional branches

define method emit-branch-sdi 
    (be :: <harp-x86-back-end>, opcode :: <integer>, tag :: <tag>)
  emit-general-sdi(be, tag, vector(branch-8, branch-32), 2, code: opcode);
end method;


/// Check the length here !!

define method branch-8 
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if ((-126 <= span) & (span <= 129))
    if (codep)
      list(self.new-sdi-code-fragment, one-byte(span - 2));
    else
      2;
    end if;
  else 
    #f;
  end if;
end method;

define method branch-32
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if (codep)
    pair(#x0f, pair(#x10 + self.new-sdi-code-fragment, four-bytes(span - 6)))
  else
    6;
  end if;
end method;


/// Branch. Can we work out what these are called on the 386?

/// the sdi for unconditional branch (we use the 386 naming here (ie jmp)
/// because those wonderful intel designers decided to make conditional and
/// unconditional branches about as similar as a bowl of petunias is to a sperm
/// whale - different things --> different names)

define method emit-jmp-sdi (be :: <harp-x86-back-end>, tag :: <tag>)
  emit-general-sdi(be, tag, vector(jmp-8, jmp-32), 2);
end method;

define method jmp-8 (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if ((-126 <= span) & (span <= 129))
    if (codep)
      list(#xeb, one-byte(span - 2));
    else
      2;
    end if;
  else 
    #f;
  end if;
end method;


define method jmp-32 (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if (codep)
    pair(jmp, four-bytes(span - 5));
  else
    5;
  end if;
end method;



with-ops-in pentium-instructions (beq) info := beq-x end;
with-ops-in pentium-instructions (bne) info := bne-x end;
with-ops-in pentium-instructions (bgt) info := bgt-x end;
with-ops-in pentium-instructions (bge) info := bge-x end;
with-ops-in pentium-instructions (blt) info := blt-x end;
with-ops-in pentium-instructions (ble) info := ble-x end;
with-ops-in pentium-instructions (bhi) info := bhi-x end;
with-ops-in pentium-instructions (bhs) info := bhs-x end;
with-ops-in pentium-instructions (blo) info := blo-x end;
with-ops-in pentium-instructions (bls) info := bls-x end;


define method logical-positive (x :: <abstract-integer>) => (res :: <abstract-integer>)
  if (x > 0) 
    x
  else  // Limited integer precision means we can't do this yet !"$%
    harp-error("Can't perform unsigned comparison on negative number");
  end if;
end method;

define method unsigned-test (test :: <function>) => (test :: <method>)
  method (left :: <abstract-integer>, right :: <abstract-integer>) => (b :: <boolean>)
    let unsigned-left  = logical-positive(left);
    let unsigned-right = logical-positive(right);
    test(unsigned-left, unsigned-right);
  end method;
end method;


// Modified handle-constant-comparison for PC to avoid raw values
// (TonyM 15/10/91)

define method handle-constant-comparison 
    (be :: <harp-x86-back-end>, 
     info :: <integer>, 
     dest :: <tag>,
     left :: <abstract-integer>, right :: <abstract-integer>)

  let take-the-branch? :: <function> =
    select (info)
      beq-x => \= ;
      bne-x => \~=;
      bgt-x => \> ;
      bge-x => \>=;
      blt-x => \< ;
      ble-x => \<=;
      bhi-x => unsigned-test(\>) ;
      bhs-x => unsigned-test(\>=);
      blo-x => unsigned-test(\<) ;
      bls-x => unsigned-test(\<=);
    end select;

  if (take-the-branch?(left, right))
    emit-jmp-sdi(be, dest);
  end if;
end method;



define constant straight-test = op-info;

define method opposite-test (x :: <op>)
  select (x.op-info)
    beq-x => beq-x;
    bne-x => bne-x;

    bgt-x => blt-x;
    bge-x => ble-x;
    blt-x => bgt-x;
    ble-x => bge-x;

    bhi-x => blo-x;
    bhs-x => bls-x;
    blo-x => bhi-x;
    bls-x => bhs-x;

    otherwise => #f;
  end select;
end method;


/// If we detect the cr & cs cases and reverse them, all the others go OK
/// forwards. The ss case needs a register.

with-ops-in pentium-instructions (beq, bne, bgt, bge, blt, ble,
                                  bhi, bhs, blo, bls, band, bnand)
  c-preserved-destroys-fn
    :=  pentium-method (tuu)
          destroys-tmp1-if(ic/spill-ref(tuu-uze(1)) & ic/spill-ref(tuu-uze(2)));
        end pentium-method;
end with-ops-in;

define pentium-template (beq, bne, bgt, bge, blt, ble, bhi, bhs, blo, bls)
  options (self);

  // Kick off with the infamous constant comparison case
  pattern (be, pred :: <integer> by op-info, tag, c1 :: <integer>, c2 :: <integer>)
    handle-constant-comparison(be, pred, tag, c1, c2);

  // The constant register/spill cases need reversing ...
  pattern (be, b by opposite-test, tag, r :: <ac/const-ref> by colour, s :: <ic/m/spill-ref> by colour)
    call-local(cmp2, be, s, r);
    emit-branch-sdi(be, b, tag);

  // The spill/spill case we must do here by steam
  pattern (be, b by straight-test, tag, r :: <ic/spill-ref> by colour, s :: <ic/spill-ref> by colour)
    harp-out (be) move(be, reg--tmp1, r) end;
    call-local(cmp2, be, reg--tmp1, s);
    emit-branch-sdi(be, b, tag);

  // All other cases just go straight through
  pattern (be, b by straight-test, tag, r, s)
    call-local(cmp2, be, r, s);
    emit-branch-sdi(be, b, tag);

end pentium-template;


with-ops-in pentium-instructions (band)  info := bne-x end;
with-ops-in pentium-instructions (bnand) info := beq-x end;


define pentium-template (band, bnand)
  options (self);

  pattern (be, b :: <integer> by op-info, tag, r, s)
    let call-test2 = method (be, ignore, r, s) call-local(test2, be, r, s) end;
    canon(be, call-test2, tag, r, s);
    emit-branch-sdi(be, b, tag);

end pentium-template;



define local-pentium-template (test2)

  // When the destination is EAX, source 8 bit constant
  pattern (be, d by eax-ref, s :: <integer> of unsigned-eight-bits?)
    emit(be, #xa8);
    emit-one-byte(be, s);

  // And now when the destination is EAX, source 32 bit constant
  pattern (be, d by eax-ref, s :: <ac/const-ref> by colour)
    emit(be, #xa9);
    emit-immediate-constant(be, s);

  // And now any destination, 32 bit constant source
  pattern (be, d :: <ic/m/spill-ref> by colour, s :: <ac/const-ref> by colour)
    emit(be, #xf7);
    emit-m-c-spill-dest(be, d, 0);
    emit-immediate-constant(be, s);

  // anything into register
  pattern (be, d :: <real-register> by colour, s :: <ic/m/spill-ref> by colour)
    emit(be, #x85);
    emit-m-c-spill-dest(be, s, ex-reg(d));

  // Default case
  pattern (be,  d, s)
    harp-out (be) move(be, reg--tmp1, d) end;
    call-local(test2, be, reg--tmp1, s);

end local-pentium-template;


/// BMVSET and BMVUNSET
/// These branch when the multiple value count is set / unset
/// respectively. 

with-ops-in pentium-instructions (bmvset)   info := beq-x end;
with-ops-in pentium-instructions (bmvunset) info := bne-x end;


define pentium-template (bmvset, bmvunset)
  options (self);

  // Move the flags into a register, and test if the direction bit set
  pattern (be, b :: <integer> by op-info, tag)
    emit(be, pushfd);               // push the flags register
    harp-out (be) pop(be, ecx) end; // pop it into ecx
    emit(be, #x80, #xe5);           // AND CH with 4
    emit-one-byte(be, 4);
    emit-branch-sdi(be, b,tag);

end pentium-template;


/// INS::OFFSET-TO-TAG
/// This simply emits 4 bytes of the relative distance to the tag. 

define pentium-template (offset-to-tag)
  pattern (be, tag)
    emit-offset-sdi(be, tag);
end pentium-template;


define method emit-offset-sdi (be :: <harp-x86-back-end>, tag :: <tag>)
  emit-general-sdi(be, tag, vector(offset-32), 4);
end method;



define method offset-32 
     (self :: <new-sdi>, span :: <abstract-integer>, codep :: <boolean>)
  if (codep)
    four-bytes(generic--(span, 4));
  else
    4;
  end if;
end method;



/// bne-byte-mem and beq-byte-mem are what LW calls 
/// bne-mem and beq-mem [confusingly added 11/10/90 (tony)]
/// they make some predicates faster and more compact
/// N.B. they are currently BYTE comparisons in memory, with an
/// immediate operand

with-ops-in pentium-instructions (beq-byte-mem) info := beq-x end;
with-ops-in pentium-instructions (bne-byte-mem) info := bne-x end;

with-ops-in pentium-instructions (beq-byte-mem, bne-byte-mem, bne-mem, beq-mem)
  c-preserved-destroys-fn
    :=  pentium-method (tuuu)
          destroys-tmp1-if(ic/spill-ref(tuuu-uze(1)));
        end pentium-method;
end with-ops-in;

define method cmp-byte-mem 
    (be :: <harp-x86-back-end>, 
     info :: <integer>, dest :: <tag>, 
     ad-reg :: <real-register>, 
     disp :: <integer>, cmp :: <integer>)
  emit(be, #x80);
  emit-reg-offset(be, ad-reg, disp, cmp2);
  emit-one-byte(be, cmp);
  emit-branch-sdi(be, info, dest);
end method;


define pentium-template (bne-byte-mem, beq-byte-mem)
  options (self);

  pattern (be, i, d, r :: <ic/spill-ref> by colour, o :: <integer> of sixteen-bit-const-ref, s)
    harp-out (be) move(be, reg--tmp1, r) end;
    harp-reapply(be, i, d, reg--tmp1, o, s);

  pattern (be, i :: <integer> by op-info, d, r :: <real-register> by colour,
           o :: <integer> of sixteen-bit-const-ref, s :: <integer> of unsigned-eight-bits?)
    cmp-byte-mem(be, i, d, r, o, s);

  pattern (be, i :: <integer> by op-info, d, r :: <real-register> by colour,
           o :: <integer> of sixteen-bit-const-ref, s by byte-reg-ref)
    emit(be, #x3a);
    emit-reg-offset(be, r, o, s.ex-reg);
    emit-branch-sdi(be, i, d);

end pentium-template;



/// bne-mem and beq-mem are word comparisons - unlike
/// their LW counterparts.

with-ops-in pentium-instructions (beq-mem) info := beq-x end;
with-ops-in pentium-instructions (bne-mem) info := bne-x end;

define method cmp-mem 
    (be :: <harp-x86-back-end>, 
     info :: <integer>, dest :: <tag>, 
     ad-reg :: <real-register>, 
     disp :: <integer>, cmp)
  emit(be, #x81);
  emit-reg-offset(be, ad-reg, disp, cmp2);
  emit-immediate-constant(be, cmp);
  emit-branch-sdi(be, info, dest);
end method;


define pentium-template (bne-mem, beq-mem)
  options (self);

  pattern (be, i, d, r :: <ic/spill-ref> by colour, o :: <integer> of sixteen-bit-const-ref, s)
    harp-out (be) move(be, reg--tmp1, r) end;
    harp-reapply(be, i, d, reg--tmp1, o, s);

  pattern (be, i :: <integer> by op-info, d, r :: <real-register> by colour,
           o :: <integer> of sixteen-bit-const-ref, s :: <ac/const-ref> by colour)
    cmp-mem(be, i, d, r, o, s);

  pattern (be, i :: <integer> by op-info, d, r :: <real-register> by colour,
           o :: <integer> of sixteen-bit-const-ref, s :: <real-register> by colour)
    emit(be, #x3b);
    emit-reg-offset(be, r, o, s.ex-reg);
    emit-branch-sdi(be, i, d);

end pentium-template;







/// BNE-BYTE and BEQ-BYTE et al.
/// These are useful for the PC for predicate tests.


with-ops-in pentium-instructions (beq-byte) info := beq-x end;
with-ops-in pentium-instructions (bne-byte) info := bne-x end;
with-ops-in pentium-instructions (bgt-byte) info := bgt-x end;
with-ops-in pentium-instructions (bge-byte) info := bge-x end;
with-ops-in pentium-instructions (blt-byte) info := blt-x end;
with-ops-in pentium-instructions (ble-byte) info := ble-x end;


define method byte-equals-test (i :: <op>)
  let info = i.op-info;
  if (info == beq-x | info == bne-x)
    info;
  else
    #f;
  end if;
end method;

define pentium-template (bne-byte, beq-byte, bge-byte, bgt-byte, 
                         ble-byte, blt-byte)
  options (self);

  // For EAX, use a byte CMP of AL
  pattern (be, i :: <integer> by op-info, d, r by eax-ref, s :: <integer> of unsigned-eight-bits?)
    emit(be, #x3c);
    emit-one-byte(be, s);
    emit-branch-sdi(be, i, d);

  // Test a byte register is zero by ORing it with itself
  pattern (be, i by byte-equals-test, d, r by byte-reg-ref, s :: <integer> of zero-number?)
    emit(be, #x0A);                // Or the byte register with itself
    emit-reg-direct(be, r, r.ex-reg);
    emit-branch-sdi(be, i, d);     // And we are equal if zero

  // Test a byte register for any other value with a byte CMP
  pattern (be, i :: <integer> by op-info, d, r by byte-reg-ref, s :: <integer> of unsigned-eight-bits?)
    emit(be, #x80);
    emit-reg-direct(be, r, #b111000);
    emit-one-byte(be, s);
    emit-branch-sdi(be, i, d);

  // Test a 32 bit register is zero by TESTing with #xFF
  // This is inefficient, but is prefered against.
  pattern (be, i by byte-equals-test, d, r :: <real-register> by colour, s :: <integer> of zero-number?)
    emit(be, #xf7);
    emit-reg-direct(be, r, 0);
    emit-four-bytes(be, #xff);
    emit-branch-sdi(be, i, d);

  // For a spill, use a byte CMP
  pattern (be, i :: <integer> by op-info, d, r :: <ic/spill-ref> by colour, s :: <integer> of unsigned-eight-bits?)
    emit(be, #x80);
    emit-m-c-spill-dest(be, r, #b111000);
    emit-one-byte(be, s);
    emit-branch-sdi(be, i, d);

  // The general case for a 32 bit register should be rare so it
  // need not be very efficient
  pattern (be, i by byte-equals-test, d, r, s :: <integer> of unsigned-eight-bits?)
    harp-out (be) and(be, reg--tmp1, r, #xff) end;
    if (zero?(s))
      emit-branch-sdi(be, i, d);
    else
      harp-reapply(be, full-test(be, i), d, reg--tmp1, s);
    end if;

  // Test a byte register against anything byte-addressable.
  // This is "abnormal" HARP usage - but gets used by the runtime
  pattern (be, i :: <integer> by op-info, d, r by byte-reg-ref, s :: <ic/spill-ref> by colour)
    emit(be, #x38);
    emit-m-c-spill-dest(be, s, r.ex-reg);
    emit-branch-sdi(be, i, d);

  // native code emission of this is actually in the reverse order
  // tony -- nosa 25/08
  pattern (be, i :: <integer> by op-info, d, r by byte-reg-ref, s by byte-reg-ref)
    emit(be, #x38);
    emit-m-c-spill-dest(be, r, s.ex-reg);
    emit-branch-sdi(be, i, d);

end pentium-template;


define method full-test 
      (be :: <harp-x86-back-end>, info :: <integer>) => (op :: <op>)
  let instrs = be.instructions;
  select (info)
    beq-x => op-element(instrs, beq);
    bne-x => op-element(instrs, bne);
  end select;
end method;


/// BZ-BYTES and BNZ-BYTES
/// Branch depending on whether 2 bytes are both zero.
/// This is useful for checking if 2 numbers are fixnums very
/// efficiently. 


with-ops-in pentium-instructions (bz-bytes)  info := beq-x end;
with-ops-in pentium-instructions (bnz-bytes) info := bne-x end;

define pentium-template (bz-bytes, bnz-bytes)
  options (self);

  // If one arg is byte addressable, move the other into ECX, and then
  // OR CL with the byte addressable arg.
  pattern (be, i :: <integer> by op-info, d, r by byte-addressable, s)
    harp-out (be) move(be, ecx, s) end;
    emit(be, #x0A);                     // Or the byte register with CL
    emit-m-c-spill-dest(be, r, ecx.ex-reg);
    emit-branch-sdi(be, i, d);          // And we are equal if zero

  pattern (be, i, d, s, r by byte-addressable)
    harp-reapply(be, i, d, r, s);

  /// General case: move one arg into ECX, OR with the second arg, and
  /// compare CL.
  pattern (be, i :: <integer> by op-info, d, r, s)
    harp-out (be)
      move(be, ecx, r);
      or(be, ecx, ecx, s);               // OR into ECX
    end harp-out;
    emit(be, #x80, #xFD);                // and CMP CL with 0
    emit-one-byte(be, 0);
    emit-branch-sdi(be, i, d);

end pentium-template;



/// Let's have a go at BIT too. Two useful instruction are BT (save one bit
/// in carry flag) and TEST. Unfortunately, the modes to TEST are slightly
/// restrictive whan it comes to short operands.

/// So, the strategy is: when both mask and comparison are one bit constants,
/// use BT to address any register or spill. When comparison is zero, use
/// TEST to address any register or spill, subject to certain special cases
/// on operand length (shame!). Otherwise load the temporary register

define method one-bit-mask (x :: <object>)
  #f;
end;

define method one-bit-mask (x :: <integer>)
  select (x)
    1 => 0;      2 =>  1;      4 =>  2;      8 =>  3;
    16 => 4;	 32 => 5;      64 => 6;      128 => 7;
    256 => 8;    512 => 9;     1024 => 10;   2048 => 11;
    4096 => 12;  8192 => 13;   16384 => 14;  32768 => 15;
    65536 => 16; 131072 => 17; 262144 => 18; 524288 => 19;
    1048576 => 20;   2097152 => 21;    4194304 => 22;
    8388608 => 23;   16777216 => 24;   33554432 => 25;
    67108864 => 26;  134217728 => 27;  268435456 => 28;
//  536870912 => 29; 1073741824 => 30; 2147483648 => 31;  // !"$ Limited precision integers
    otherwise => #f;
  end select;
end method;


define constant bit =  bne-x;
define constant nbit = beq-x;

with-ops-in pentium-instructions (bit)  info := bit  end;
with-ops-in pentium-instructions (nbit) info := nbit end;

with-ops-in pentium-instructions (bit, nbit)
  c-preserved-destroys-fn := tmp1-fn;  // not worth being smarter
end;


/// Catch all-constants bit to 'where' of 'what' anded with 'with' is 'is'.

// catching-constant-bit has been modified not to use raw values
// (TonyM 15/10/91)

define method catching-constant-bit
    (be :: <harp-x86-back-end>, 
     info :: <integer>, where :: <tag>,
     what :: <integer>, with :: <integer>, is :: <integer>)
  let take-the-branch? = logand(what, with) ~= is;
  if (info == nbit)
    take-the-branch? := ~ take-the-branch?;
  end if;
  if (take-the-branch?)
    emit-jmp-sdi(be, where);
  end if;
end method;


define pentium-template (bit, nbit)
  options (self);

  // Zeroth case is once again to *fold* the wretched things. Te-d-ious.
  pattern (be, i :: <integer> by op-info, d, p :: <integer>, q :: <integer>, r :: <integer>)
    catching-constant-bit(be, i, d, p, q, r);

  // TEST on A. This case could be quite common. Go for byte or
  // full word - no need to consider half.

  pattern (be, i :: <integer> by op-info, d, p by eax-ref, r :: <integer>, s :: <integer> of zero-number?)
    if (unsigned-eight-bits?(r))
      emit(be, #xa8);
      emit-one-byte(be, r);
    else
      emit(be, #xa9);
      emit-four-bytes(be, r);
    end if;
    emit-branch-sdi(be, i, d);


  // byte TEST on one of the four magic registers or a spill
  pattern (be, i :: <integer> by op-info, d, p by byte-addressable, 
           r :: <integer> of eight-bit-const-ref, s :: <integer> of zero-number?)
    emit(be, #xf6);
    emit-m-c-spill-dest(be, p, 0);
    emit-one-byte(be, r);
    emit-branch-sdi(be, i, d);

  pattern (be, i, d, p :: <i-address-constant-reference>, r, s)
    harp-out (be) move(be, reg--tmp1, p) end;
    harp-reapply(be, i, d, reg--tmp1, r, s);

  // if it's a one-bit mask and either same or zero test, use BT
  pattern (be, i :: <integer> by op-info, d, p, r by one-bit-mask, s is r)
    let code = if (i == bit) bcc-x else bcs-x end;
    emit(be, #x0f, #xba);
    emit-m-c-spill-dest(be, p, #b100000);
    emit-one-byte(be, r);
    emit-branch-sdi(be, code, d);

  pattern (be, i :: <integer> by op-info, d, p, r by one-bit-mask, s :: <integer> of zero-number?)
    let code = if (i == bit) bcs-x else bcc-x end;
    emit(be, #x0f, #xba);
    emit-m-c-spill-dest(be, p, #b100000);
    emit-one-byte(be, r);
    emit-branch-sdi(be, code, d);

  // 4 byte TEST
  pattern (be, i :: <integer> by op-info, d, p, r :: <integer>, s :: <integer> of zero-number?)
    emit(be, #xf7);
    emit-m-c-spill-dest(be, p, 0);
    emit-four-bytes(be, r);
    emit-branch-sdi(be, i, d);

  // Normalize
  pattern (be, i, d, p :: <integer>, r, s :: <integer> of zero-number?)
    harp-reapply(be, i, d, r, p, 0);

  // Those were the special cases, now let's do the move-and-cmp case.

  pattern (be, i :: <integer> by op-info, d, p, r, s)
    harp-out (be) move(be, reg--tmp1, p) end;
    call-local(and2, be, reg--tmp1, r);
    call-local(cmp2, be,  reg--tmp1, s);
    emit-branch-sdi(be, i, d);

end pentium-template;


/// The dynamic-bit instruction is just BT. Wot fun.

with-ops-in pentium-instructions (dynamic-bit, dynamic-nbit)

  disallow-fn
    :=  pentium-method (uu)
          if (const-ref(uu-uze(1)))
            $vector-tmp2;
          else $ev;
          end if;
        end pentium-method;

  c-preserved-destroys-fn := tmp1-fn;  // not worth being smarter

end;

with-ops-in pentium-instructions (dynamic-bit) info := bcs-x end;
with-ops-in pentium-instructions (dynamic-nbit) info := bcc-x end;

define pentium-template (dynamic-bit, dynamic-nbit)
  options (self);

  // Bit-base constant
  pattern (be, i, tag, d :: <integer>, s)
    harp-out (be) move(be, reg--tmp2, d) end;
    harp-reapply(be, i, tag, reg--tmp2, s);

  // Index out of register comes naturally
  pattern (be, i :: <integer> by op-info, tag, d :: <ic/m/spill-ref> by colour, s :: <real-register> by colour)
    emit(be, #x0f, #xa3);
    emit-m-c-spill-dest(be, d, s.ex-reg);
    emit-branch-sdi(be, i, tag);


  // Index constant also comes naturally
  pattern (be, i :: <integer> by op-info, tag, d :: <ic/m/spill-ref> by colour, s :: <integer> of eight-bit-const-ref)
    emit(be, #x0f, #xba);
    emit-m-c-spill-dest(be, d, and2);
    emit-one-byte(be, s);
    emit-branch-sdi(be, i, tag);

  // Index out of spill means using a temporary
  pattern (be, i :: <integer> by op-info, tag, d :: <ic/m/spill-ref> by colour, s :: <ispill> by colour)
    harp-out (be) move(be, reg--tmp1, s) end;
    emit(be, #x0f, #xa3);
    emit-m-c-spill-dest(be, d, reg--tmp1.ex-reg);
    emit-branch-sdi(be, i, tag);

end pentium-template;


define pentium-template bra
  pattern (be, d)
    emit-jmp-sdi(be, d);
end pentium-template;


// From the point of view of code generation, END-CLEANUP is just an RTS

define pentium-template end-cleanup
  pattern (be, tag, implicit-defs-vec)
    emit(be, ret);
end pentium-template;



define pentium-template rts
  pattern (be)
    emit(be, ret);
end pentium-template;



/// RTS-AND-DROP takes one argument - the number of arguments to
/// remove from the stack after the return address. This must be
/// a constant number which is the number of bytes of arguments to drop. 


define constant $rts-and-drop-disallows = vector(ebx, ecx);
  // Temporary: the only reason for disallowing EBX here
  // is that the runtime generator currently depends on this
  // for correct code for primitive-remove-optionals.

define constant $rts-and-drop-foreign-disallows = vector(edx, ecx);

with-ops-in pentium-instructions (rts-and-drop)
  disallow-fn := pentium-method ()
                   if (backend.variables.compiling-call-in)
                     $rts-and-drop-foreign-disallows
                   else $rts-and-drop-disallows;
                   end if;
                 end pentium-method;

  c-preserved-destroys-fn :=  constant-fn($ev);
end with-ops-in;



define pentium-template rts-and-drop

  // Dynamically sized case.
  pattern (be, u :: <real-register> by colour)
    // Choose a temporary which is not a preserved register
    let tmp = if (be.variables.compiling-call-in)
                edx
              else reg--tmp1 
              end if;
    harp-out (be)
      pop(be, tmp);  // pop the return address
      add(be, reg--stack, reg--stack, u);
    end harp-out;
    emit(be, grp5);
    emit-reg-direct(be, tmp, #b100000);

  // Dynamically sized case with the size on the stack.
  pattern (be, u :: <ispill> by colour)
    harp-out (be) 
      move(be, ecx, u);
      rts-and-drop(be, ecx);
    end harp-out;

  // Zero to drop:
  pattern (be, u :: <integer> of zero-number?)
    emit(be, ret);

  // Constant to drop: (actually only support up to 256 arguments)
  pattern (be, u :: <integer> of sixteen-bit-const-ref)
    emit(be, ret-drop);
    emit-two-bytes(be, u);

end pentium-template;


/// pea is a complete bind, since there seems to be no idea of a PC-relative
/// address on the 386. Hence the following bit of very grubby code.
/// The call 0 instruction pushes the address of the pop onto the stack.
/// The sdi at this address consists of a pop and add, a total of 7 bytes,
/// containing a 4 byte signed offset, adjusted in the normal way. The
/// following push puts the adjusted address back onto the stack.
///
/// NB This could be completely reworked for Dylan to use labelled relative
/// constants. See LEA.

define method emit-pea-sdi 
    (be :: <harp-x86-back-end>, tag :: <tag>, offset :: <integer>)
  emit-general-sdi(be, tag, vector(pea-8, pea-32), 5, dest-offset: offset);
end method;

define method pea-8
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if ((-128 <= span) & (span <= 127))
    if (codep)
      list(#x83,			// add2 [esp] span8
           #x04,
           #x24,
	   one-byte(span));
    else
      4;
    end if;
  else 
    #f;
  end if;
end method;


define method pea-32
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if (codep)
    concatenate(#(#x81, #x04,#x24),           // add2 [esp] span32
	        four-bytes(span));
  else
    7;
  end if;
end method;


define pentium-template pea
  pattern (be, tag, offset)
    emit(be, #xe8, 0, 0, 0, 0);			// call $+0
    emit-pea-sdi(be, tag, offset);
end pentium-template;
   


/// LEA and the related LOAD-NLX-ADDRESS

with-ops-in pentium-instructions (lea)
  c-preserved-destroys-fn
    :=  pentium-method (du)
          destroys-tmp1-if(~ m-ref(du-def(1)))
        end pentium-method;
end;

define pentium-template (lea)
  pattern (be, tag, dest :: <real-register> by colour, offset :: <integer>)
    emit(be, #xb8 + dest.real-register-number);
    emit-effective-address-sdi(be, tag, offset);

  pattern (be, tag, dest, offset :: <integer>)
    harp-out (be)
      lea(be, tag, reg--tmp1, offset);
      move(be, dest, reg--tmp1);
    end harp-out;

end pentium-template;


with-load-nlx-address-ops pentium end;

define pentium load-nlx-address-template;


define method emit-effective-address-sdi
     (be :: <harp-x86-back-end>, tag :: <tag>, offset :: <integer>)
  emit-general-sdi(be, tag, vector(effective-address-32), 4, 
                   dest-offset: offset);
end method;



define method effective-address-32 
     (self :: <new-sdi>, span :: <integer>, codep :: <boolean>)
  if (codep)
    list(make(<relative-address-constant>, offset: span, size: 4));
  else
    4;
  end if;
end method;



define pentium strong-scl-template;

define pentium scl-template;

