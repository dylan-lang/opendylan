module:    powerpc-harp
Synopsis:  PowerPC branching code
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// The constants are in rs6k-opcodes

/// The branches and such all live here. We have -
///
/// BEQ dest s1 s2         if s1 & s2 equal, goto dest
/// BNE dest s1 s2         same, but not equal
/// BIT dest x y z         branch if (x & y) /= z to w

/// BGT dest s1 s2         branch if s1 signed greater than s2
/// BGE dest s1 s2         branch if s1 signed greater or equal s2
/// BLT dest s1 s2         branch if s1 signed less than s2
/// BLE dest s1 s2         branch if s1 signed less than or equal s2

/// BHI dest s1 s2         branch if s1 unsigned greater than s2
/// BHS dest s1 s2         branch if s1 unsigned greater or equal s2
/// BLO dest s1 s2         branch if s1 unsigned less than s2
/// BLS dest s1 s2         branch if s1 unsigned less than or equal s2

/// The BEQ and BNE are built from CMP and Bcc.



with-ops-in powerpc-instructions (beq) info := beq-cc end;
with-ops-in powerpc-instructions (bne) info := bne-cc end;
with-ops-in powerpc-instructions (bgt) info := bgt-cc end;
with-ops-in powerpc-instructions (bge) info := bge-cc end;
with-ops-in powerpc-instructions (blt) info := blt-cc end;
with-ops-in powerpc-instructions (ble) info := ble-cc end;
with-ops-in powerpc-instructions (bhi) info := bgt-cc end;
with-ops-in powerpc-instructions (bhs) info := bge-cc end;
with-ops-in powerpc-instructions (blo) info := blt-cc end;
with-ops-in powerpc-instructions (bls) info := ble-cc end;


define constant straight-test = op-info;


/// if we wish to reverse the order of the operands for the branch, then
/// we must use the opposite test

define method opposite-test (i :: <op>)
  select (i.op-info)
    beq-cc => beq-cc;
    bne-cc => bne-cc;

    bgt-cc => blt-cc;
    bge-cc => ble-cc;

    blt-cc => bgt-cc;
    ble-cc => bge-cc;

    otherwise => #f;
  end
end method opposite-test;


define method branch-immediate
    (be :: <powerpc-back-end>,
     cmp-code,
     br-code,
     tag,
     left,
     right-const) => ()
  let rr = emit-make-reg(be, left, reg--tmp1);
  emit-dri(be, cmp-code, 0, rr, right-const);
  emit-branch-sdi(be, br-code, tag)
end method branch-immediate;


define method branch-register
    (be :: <powerpc-back-end>,
     cmp-code,
     br-code,
     tag,
     left,
     right) => ()
  let ll = emit-make-reg(be, left, reg--tmp1);
  let rr = emit-make-reg(be, right, reg--tmp2);
  emit-drr(be, cmp-code, 0, ll, rr);
  emit-branch-sdi(be, br-code, tag)
end method branch-register;

define method handle-constant-comparison 
    (be :: <powerpc-back-end>, 
     info :: <integer>, 
     dest :: <tag>,
     left :: <abstract-integer>, right :: <abstract-integer>)

  let take-the-branch? :: <function> =
    select (info)
      beq-cc => \= ;
      bne-cc => \~=;
      bgt-cc => \> ;
      bge-cc => \>=;
      blt-cc => \< ;
      ble-cc => \<=;
    end select;

  if (take-the-branch?(left, right))
    harp-out (be) bra(be, dest) end
  end if;
end method;


/// Signed conditional branches

define powerpc-template (bgt, bge, blt, ble, beq, bne)
  options (self);

  // First handle the constant case
  pattern (be, b :: <integer> by op-info, tag, c1 :: <integer>, c2 :: <integer>)
    handle-constant-comparison(be, b, tag, c1, c2);

  // Use compare immediate if we have a 16 bit operand
  pattern (be, b :: <integer> by straight-test, dest,
	   r, s :: <integer> of signed-16bit-const-ref)
    branch-immediate(be, cmpi-op, b, dest, r, s);

  // reverse the test if the constant is the left argument
  pattern (be, b-opposite :: <integer> by opposite-test, dest,
	   r :: <integer> of signed-16bit-const-ref, s)
    branch-immediate(be, cmpi-op, b-opposite, dest, s, r);

  // general case
  pattern (be, b :: <integer> by straight-test, dest, r, s)
    branch-register(be, cmp-op, b, dest, r, s);

end powerpc-template;
  

/// Unsigned conditional branches

define powerpc-template (bhi, bhs, blo, bls)
  options (self);

  // First handle the constant case
  pattern (be, b :: <integer> by op-info, tag, c1 :: <integer>, c2 :: <integer>)
    handle-constant-comparison(be, b, tag, c1, c2);

  // Use compare immediate if we have a 16 bit operand
  pattern (be, b :: <integer> by straight-test, dest,
	   r, s :: <integer> of unsigned-16bit-const-ref)
    branch-immediate(be, cmpli-op, b, dest, r, s);

  // reverse the test if the constant is the left argument
  pattern (be, b-opposite :: <integer> by opposite-test, dest,
	   r :: <integer> of unsigned-16bit-const-ref, s)
    branch-immediate(be, cmpli-op, b-opposite, dest, s, r);

  // general case
  pattern (be, b :: <integer> by straight-test, dest, r, s)
    branch-register(be, cmpl-op, b, dest, r, s);

end powerpc-template;


with-ops-in powerpc-instructions (band)  info := bne-cc end;
with-ops-in powerpc-instructions (bnand) info := beq-cc end;

define powerpc-template (band, bnand)
  options (self);

  pattern (be, b :: <integer> by op-info, tag, r, s)
    harp-out(be) and(be, reg--tmp1, r, s) end;
    emit-branch-sdi(be, b, tag);

end powerpc-template;


with-ops-in powerpc-instructions (beq-mem) info := beq-cc end;
with-ops-in powerpc-instructions (bne-mem) info := bne-cc end;

define powerpc-template (bne-mem, beq-mem)
  options (self);

  pattern (be, i :: <integer> by op-info, tag, r,
           o :: <integer> of sixteen-bit-const-ref, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    emit-d(be, lwz-op, reg--tmp1, rr, o);
    branch-register(be, cmp-op, i, tag, reg--tmp1, s);

end powerpc-template;



with-ops-in powerpc-instructions (beq-byte) info := beq-cc end;
with-ops-in powerpc-instructions (bne-byte) info := bne-cc end;
with-ops-in powerpc-instructions (bgt-byte) info := bgt-cc end;
with-ops-in powerpc-instructions (bge-byte) info := bge-cc end;
with-ops-in powerpc-instructions (blt-byte) info := blt-cc end;
with-ops-in powerpc-instructions (ble-byte) info := ble-cc end;


define powerpc-template (bne-byte, beq-byte, bge-byte, bgt-byte, 
                         ble-byte, blt-byte)
  options (self);

  pattern (be, i :: <integer> by op-info, tag, r,
	   s :: <integer> of unsigned-eight-bits?)
    let rr = emit-make-reg(be, r, reg--tmp1);
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp1, rr, #xFF);
    branch-immediate(be, cmpi-op, i, tag, reg--tmp1, s);

end powerpc-template;



define constant bit =  bne-cc;
define constant nbit = beq-cc;

with-ops-in powerpc-instructions (bit)  info := bit  end;
with-ops-in powerpc-instructions (nbit) info := nbit end;


/// BIT - AND the source and mask into tmp1. If non zero value expected, use CMP
/// Finally branch on value of EQ bit in condition register.
/// N.B. AND template will always set the condition codes.  

define powerpc-template (bit, nbit)
  options (self);

  pattern (be, i :: <integer> by op-info, d, p, q, r)
    let branch = i;
    if (instance?(q, <integer>) & q == r)
      let pp = emit-make-reg(be, p, reg--tmp2);
      harp-out(be) or(be, reg--tmp1, pp, q) end;
      branch-register(be, cmp-op, branch, d, reg--tmp1, pp);
    else
      harp-out(be) and(be, reg--tmp1, p, q) end;
      case
        r == 0 => emit-branch-sdi(be, branch, d);

	signed-16bit-const-ref(r) =>
	  branch-immediate(be,
			   cmpi-op,
			   branch,
			   d,
			   reg--tmp1,
			   r);

	otherwise => branch-register(be, cmp-op, branch, d, reg--tmp1, r)
      end
    end

end powerpc-template;


/// For DYNAMIC-BIT we can use the Rotate-Left-Then-AND-With-Mask to shift
/// the bit we are interested in to the least-sig-bit (bit 31 in IBMese),
/// and AND it with 1 in the same instruction.  We can then test for EQ
/// in the condition register.

define constant mask-of-1 = ash(31, 1) + ash(31, 6);

with-ops-in powerpc-instructions (dynamic-bit) info := bne-cc end;
with-ops-in powerpc-instructions (dynamic-nbit) info := beq-cc end;

define powerpc-template (dynamic-bit, dynamic-nbit)
  options (self);

  pattern (be, i :: <integer> by op-info, tag, d,
	   s :: <integer> of unsigned-5bit-const-ref)
    let dd = emit-make-reg(be, d, reg--tmp1);
    emit-rrd-via-tmp1-dest2(be, mw-add(rlwinm!-op, mask-of-1), reg--tmp1, dd, 32 - s);
    emit-branch-sdi(be, i, tag)

   // in general case, need an subtract subtraction to calculate RIGHT rotation
   pattern (be, i :: <integer> by op-info, tag, d, s)
    let ss = emit-make-reg(be, s, reg--tmp1);
    let dd = emit-make-reg(be, d, reg--tmp2);
    emit-d(be, subfic-op, reg--tmp1, ss, 32);  // subtract shift from 32 -> tmp1
    emit-x(be, mw-add(rlwnm!-op, mask-of-1), dd, reg--tmp1, reg--tmp1);
    emit-branch-sdi(be, i, tag)

end powerpc-template;


/// BRA

define powerpc-template bra

  pattern (be, tag)
    emit-branch-sdi(be, bra-cc, tag);

end powerpc-template;


/*

/// stack check

define powerpc-template stack-check

  pattern (be, fail-tag)
    harp-out(be)
      ld(be, reg--tmp1, reg--nil,
	 *%stack-limit-offset-in-bytes);
      bls(be, fail-tag, reg--stack, reg--tmp1)
    end;

end powerpc-template;

*/

/*

/// Increment Call Count

define powerpc-template increment-call-count

  pattern (be, s :: <real-register> by colour)
    harp-out(be) ld(be, reg--tmp1, s, *call-counter-offset*) end;
    emit-d(be, addic-op, reg--tmp1, reg--tmp1, immediate-constant(1));
    harp-out(be) st(be, reg--tmp1, s, *call-counter-offset*) end;

end powerpc-template;

*/




/// sdi emission for branches

/// bc-op only gives us a 16 bit offset, so use b-op for long branch.
/// b-op gives us a 26 bit relative offset. I doubt we'll need more than that!

/// If we want a non-conditional branch, use b-op.  That way, we know that
/// bc-op will only be used with BO set to branch-on-true or branch-on-false.
/// We can therefore invert the test by simply inverting a bit in BO.

define method branch-always-sdi
    (self :: <new-sdi>, span :: <integer>, code? :: <boolean>)
  if (-#x2000000 <= span & span < #x2000000)
    if (code?) sdi-l(b-op, span); else 4 end;
  else
    error("Branch offset bigger than 26 bit field")
  end
end method branch-always-sdi;


define method short-cond-sdi
    (self :: <new-sdi>, span :: <integer>, code? :: <boolean>)
  if (-#x8000 <= span & span < #x8000)
    if (code?)
      let condition = self.new-sdi-code-fragment;
      sdi-branch(bc-op, condition, span);
    else
      4
    end
  end
end method short-cond-sdi;


/// for a longer branch, use bc-op on opposite condition to jump past the b-op
/// We do not expect to ever get a branch bigger than 26 bits

define method long-cond-sdi
    (self :: <new-sdi>, span :: <integer>, code? :: <boolean>)
  if (-#x1fffff0 <= span & span < #x2000000)
    if (code?)
      let condition = opposite-condition(self.new-sdi-code-fragment);
      concatenate
	(sdi-branch(bc-op, condition, 8),  // branch forwards 2 instructions
	 sdi-l(b-op, span - 4));
    else
      8
    end;
  else
    error("Branch offset bigger than 26 bit field")
  end
end method long-cond-sdi;

define constant conditional-methods =
  vector(short-cond-sdi, long-cond-sdi);

define constant always-method = vector(branch-always-sdi);

define method emit-branch-sdi
    (be :: <powerpc-back-end>, cc :: <integer>, tag :: <tag>,
     #key offset = 0)
  let sdi =
    make(<new-sdi>,
	 dest-tag: tag,
	 dest-offset: offset,
	 cached-size: 4,           // single instruction form
	 method-index: 0,
	 method-vector:
	   if (cc == bra-cc)
	     always-method;
	   else
	     conditional-methods
	   end,
	 code-fragment: cc);       // use this field for condition code only
  emit-sdi(be, sdi)
end method emit-branch-sdi;


/// General SDI emitter

define method emit-general-sdi 
    (be :: <powerpc-back-end>, tag :: <tag>, 
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




// From the point of view of code generation, END-CLEANUP is just an RTS

define powerpc-template end-cleanup

  pattern (be, tag, implicit-defs-vec)
    harp-out(be) rts(be) end;

end powerpc-template;



/// RTS-AND-DROP takes one argument - the number of arguments to
/// remove from the stack after the return address. This must be
/// a constant number which is the number of bytes of arguments to drop. 


define constant $rts-and-drop-disallows =
  vector(reg--function, reg--mlist);
  // Temporary: the only reason for disallowing reg--function here
  // is that the runtime generator currently depends on this
  // for correct code for primitive-remove-optionals.

with-ops-in powerpc-instructions (rts-and-drop)
  disallow-fn := constant-fn($rts-and-drop-disallows);

  c-preserved-destroys-fn :=  constant-fn($ev);
end with-ops-in;


define powerpc-template rts-and-drop

  // Dynamically sized case.
  pattern (be, u :: <real-register> by colour)
    harp-out (be)
      add(be, reg--stack, reg--stack, u);
      rts(be);
    end;

  // Dynamically sized case with the size on the stack.
  pattern (be, u :: <ispill> by colour)
    harp-out (be) 
      move(be, reg--tmp1, u);
      rts-and-drop(be, reg--tmp1);
    end harp-out;

  // Zero to drop:
  pattern (be, u :: <integer> of zero-number?)
    harp-out(be) rts(be) end;

  // Constant to drop: (actually only support up to 256 arguments)
  pattern (be, u :: <integer> of sixteen-bit-const-ref)
    harp-out (be)
      add(be, reg--stack, reg--stack, u);
      rts(be);
    end;

end powerpc-template;


/// LEA and the related LOAD-NLX-ADDRESS

define method emit-effective-high-address-sdi
     (be :: <powerpc-back-end>, tag :: <tag>, offset :: <integer>)
  emit-general-sdi(be, tag, vector(effective-high-address), 4, 
                   dest-offset: offset);
end method;

define method emit-effective-low-address-sdi
     (be :: <powerpc-back-end>, tag :: <tag>, offset :: <integer>)
  emit-general-sdi(be, tag, vector(effective-low-address), 4, 
                   dest-offset: offset);
end method;

define method effective-high-address 
     (self :: <new-sdi>, span :: <integer>, code? :: <boolean>)
  if (code?)
    list(make(<relative-address-constant-high>, offset: span, size: 2));
  else
    2;
  end if;
end method;

define method effective-low-address 
     (self :: <new-sdi>, span :: <integer>, code? :: <boolean>)
  if (code?)
    list(make(<relative-address-constant-low>, offset: span, size: 2));
  else
    2;
  end if;
end method;

define powerpc-template lea
  pattern (be, tag, dest :: <real-register> by colour, offset :: <integer>)
    // load hi part
    emit-d-high(be, addis-op, reg--tmp1, r0);
    emit-effective-high-address-sdi(be, tag, offset);
    // load lo part
    emit-d-high(be, addi-op, dest, reg--tmp1);
    emit-effective-low-address-sdi(be, tag, offset);

  pattern (be, tag, dest, offset :: <integer>)
    harp-out (be)
      lea(be, tag, reg--tmp1, offset);
      move(be, dest, reg--tmp1);
    end harp-out;

end powerpc-template;


with-load-nlx-address-ops powerpc end;

define powerpc load-nlx-address-template;


define powerpc strong-scl-template;

define powerpc scl-template;


with-ops-in powerpc-instructions (bne-words, bne-bytes)
  disallow-fn := tmp345-fn;
end;


define powerpc-template (bne-words)

  pattern (be, tag, mem1, mem2, how-many)

    let num = unsigned-32bit-const-ref(how-many)
              | emit-make-reg(be, how-many, reg--tmp2);
    let m1 = reg--tmp2;
    let m2 = reg--tmp3;

    // First get the number of words + 1 into the count register
    setup-count-register(be, num, reg--tmp1, #t);

    // Now find addresses just past from & to areas
    if (unsigned-32bit-const-ref(num))
      harp-out(be)
	add(be, m1, mem1, 4 * num);
	add(be, m2, mem2, 4 * num)
      end
    else
      harp-out(be)
	asl(be, reg--tmp4, num, 2);
	add(be, m1, mem1, reg--tmp4);
	add(be, m2, mem2, reg--tmp4)
      end
    end;

    emit-d(be, addi-op, reg--tmp5, r0, 0);
    emit-branch(be, bc-op, bra-cc, 7 * 4);       // branch to TEST

    // LOOP
    emit-d(be, lwzu-op, reg--tmp1, m1, -4);
    emit-d(be, lwzu-op, reg--tmp4, m2, -4);
    emit-drr(be, cmp-op, 0, reg--tmp1, reg--tmp4);
    emit-branch(be, bc-op, beq-cc, 3 * 4);

    emit-d(be, addi-op, reg--tmp5, r0, 1);
    emit-branch(be, bc-op, bra-cc, 2 * 4);

    // TEST
    emit-branch(be, bc-op, dcr-cc, -4 * 4);        // decrement count register and
                                                  // branch to LOOP if CTR not 0

    emit-dri(be, cmpi-op, 0, reg--tmp5, 0);
    emit-branch-sdi(be, bne-cc, tag);

end powerpc-template;


define powerpc-template (bne-bytes)
  pattern (be, tag, mem1, mem2, how-many)

    // compare all full words
    harp-out (be) 
      asr(be, reg--tmp1, how-many, 2);
      bne-words(be, tag, mem1, mem2, reg--tmp1);
    end harp-out;

    // compare any left over bytes

    let num = emit-make-reg(be, how-many, reg--tmp1);
    let m1 = reg--tmp2;
    let m2  = reg--tmp3;

    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp1, num, #b11);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 0);
    emit-branch(be, bc-op, beq-cc, 25 * 4);

    emit-d(be, lwzu-op, reg--tmp2, m1, -4);
    emit-d(be, lwzu-op, reg--tmp3, m2, -4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 1);
    emit-branch(be, bc-op, beq-cc, 6 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 2);
    emit-branch(be, bc-op, beq-cc, 9 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 3);
    emit-branch(be, bc-op, beq-cc, 12 * 4);

    trap-always(be);

    // compare 1 byte
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp2, reg--tmp2, #xFF);
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp3, reg--tmp3, #xFF);
    emit-drr(be, cmp-op, 0, reg--tmp2, reg--tmp3);
    emit-branch(be, bc-op, beq-cc, 12 * 4);
    emit-branch(be, bc-op, bra-cc, 10 * 4);

    // compare 2 bytes
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp2, reg--tmp2, #xFFFF);
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp3, reg--tmp3, #xFFFF);
    emit-drr(be, cmp-op, 0, reg--tmp2, reg--tmp3);
    emit-branch(be, bc-op, beq-cc, 7 * 4);
    emit-branch(be, bc-op, bra-cc, 5 * 4);

    // compare 3 bytes
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp2, reg--tmp2, #xFFFFFF);
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp3, reg--tmp3, #xFFFFFF);
    emit-drr(be, cmp-op, 0, reg--tmp2, reg--tmp3);
    emit-branch(be, bc-op, beq-cc, 2 * 4);

    emit-d(be, addi-op, reg--tmp5, r0, 1);

    emit-dri(be, cmpi-op, 0, reg--tmp5, 0);
    emit-branch-sdi(be, bne-cc, tag);

end powerpc-template;


