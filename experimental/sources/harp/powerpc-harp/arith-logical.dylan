module:    powerpc-harp
Synopsis:  PowerPC add/subtract/logical ops
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// ADD and SUB do not try to set the condition codes. Contrary to the manual,
/// the Summary Overflow flag is NOT set, so we could not use these templates
/// for ADDV or SUBV.

define powerpc-template add

  pattern (be, d, r :: <integer>, s :: <integer>)
    harp-out(be) move(be, d, r + s) end;

  pattern (be, d, r :: <integer>, s)
    harp-out(be) add(be, d, s, r) end;

  pattern (be, d, r, s by canonicalised-const-ref)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let dd = dst-place(d, reg--tmp1);

    if (zero?(s))
      unless (rr == dd)
	emit-d(be, ori-op, rr, dd, 0); // backward args warning
      end;
    else
      let (lo-sum, hi-sum) = split-32-to-signed-16(s);
      unless (zero?(lo-sum))
	emit-d(be, addic-op, dd, rr, lo-sum);
	rr := dd;
      end;
      unless (zero?(hi-sum))
	if (rr == r0)  // r0=0 for cau
	  emit-d(be, ori-op, rr, reg--tmp2, 0); // backward args warning
	  rr := reg--tmp2;
	end;
	emit-d(be, addis-op, dd, rr, hi-sum);
      end;
    end if;
    dst-move(be, d, dd);
      
  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, addc-op, d, ss, rr);

end powerpc-template;


/// For sub, there is no variant of the sfi instruction which sets conditions


define powerpc-template sub

  pattern (be, d, r, s :: <integer>)
    harp-out(be) add(be, d, r, -s) end;

  pattern (be, d, s by canonicalised-const-ref, r)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let dd = dst-place(d, reg--tmp1);
    let (lo-sum, hi-sum) = split-32-to-signed-16(s);

    if (zero?(hi-sum))
      emit-d(be, subfic-op, dd, rr, lo-sum);
    else
      emit-d(be, subfic-op, reg--tmp2, rr, lo-sum);  // r0=0 for cau
      emit-d(be, addis-op, dd, reg--tmp2, hi-sum);
    end if;
    dst-move(be, d, dd);

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, subfc-op, d, ss, rr);

end powerpc-template;


/// Again, for AND OR and EOR set condition codes wherever possible, as
/// they may come in handy  - e.g. for BIT operations

define method emit-or-eor-imm
    (be :: <powerpc-back-end>, harp-op :: <machine-word>, d, r, const)
  if (zero?(const))
    harp-out(be) move(be, d, r) end;
  else

    let rr = emit-make-reg(be, r, reg--tmp1);
    let dd = dst-place(d, reg--tmp1);
    let low = logand(const, #x0000ffff);
    let high = generic-ash(generic-logand(const, #xffff0000), -16);
    let low-op = #f;
    let high-op = #f;

    select (harp-op)
      or!-op =>
	low-op := ori-op;
	high-op := oris-op;
      xor!-op =>
	low-op := xori-op;
	high-op := xoris-op;
    end;

    unless (zero?(low))
      emit-d(be, low-op, rr, dd, low);
      rr := dd;
    end;
    unless (zero?(high))
      emit-d(be, high-op, rr, dd, high);
    end;
    dst-move(be, d, dd);
  end;

end method;


with-ops-in powerpc-instructions (or) info := or!-op end;
with-ops-in powerpc-instructions (eor) info := xor!-op end;

define powerpc-template (or, eor)
  options (self);

  pattern (be, i :: <opcode> by op-info, d, r :: <integer>, s)
    emit-or-eor-imm(be, i, d, s, r);

  pattern (be, i :: <opcode> by op-info, d, r, s :: <integer>)
    emit-or-eor-imm(be, i, d, r, s);

  pattern (be, i :: <opcode> by op-info, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest2(be, i, d, rr, ss);

end powerpc-template;


define constant $bit-masks :: <simple-machine-word-vector> =
  begin
    let smwv :: <simple-machine-word-vector> =
      make(<simple-machine-word-vector>, size: 32);
    for (i :: <integer> from 0 below 32)
      smwv[i] := mw-ash(1, i);
    end for;
    smwv
  end;

// Returns the MBME value if K matches '[0]*[1]*[0]*' or '[1]*[0]*[1]*'
// Otherwise returns #f

define method solid-same-block? (k :: <integer>)
 => (mbme :: false-or(<integer>))
  let k :: <machine-word> = coerce-integer-to-machine-word(k);
  block (return)
    let bit-masks = $bit-masks;
    let starts-with-zero =
      mw-zero?(machine-word-logand(k, coerce-integer-to-machine-word(1)));
    let mb =
      for (index :: <integer> from 30 to 0 by -1,
	   while: starts-with-zero ==
	            mw-zero?(machine-word-logand(k, bit-masks[index])))
      finally index
      end for;
    let me =
      for (index :: <integer> from 0 to mb,
	   while: starts-with-zero ==
	            mw-zero?(machine-word-logand(k, bit-masks[index])))
      finally index
      end for;

    for (index :: <integer> from me + 1 to mb - 1)
      if (starts-with-zero ==
	    mw-zero?(machine-word-logand(k, bit-masks[index])))
	return(#f)
      end;
    end;
    unless (starts-with-zero)
      let me! = mb + 1;
      let mb! = logand(me - 1, 31);
      me := me!;
      mb := mb!;
    end;
    // We have to allow for big-endian numbering
    ash(31 - mb, 6) + ash(31 - me, 1)
  end block;
end method;


define powerpc-template and

  pattern (be, d, r, s :: <integer>)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let mbme = #f;

    case
      unsigned-16bit-const-ref(s) =>
	emit-d-via-tmp1-dest2(be, andi!-op, d, rr, s);
      zero?(logand(s, #xffff)) =>
	emit-d-via-tmp1-dest2(be, andis!-op, d, rr, ash(s, -16));
      otherwise =>
	mbme := solid-same-block?(s);
	if (mbme)
	  emit-rrd-via-tmp1-dest2(be, mw-add(rlwinm!-op, mbme), d, rr, 0);
	else
	  let ss = emit-make-reg(be, s, reg--tmp2);
	  emit-x-via-tmp1-dest2(be, and!-op, d, rr, ss);
	end;
    end;

  pattern (be, d, r :: <integer>, s)
    harp-out(be) and(be, d, s, r) end;

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest2(be, and!-op, d, rr, ss);

end powerpc-template;


define powerpc-template and-not

  pattern (be, d, r, s :: <integer>)
    harp-out(be) and(be, d, r, lognot(s)) end;

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest2(be, andc!-op, d, rr, ss);

end powerpc-template;


/// ADDV must test the SO flag in the condition register. However,
/// contrary to the manual data, the normal add instructions (a. ai.
/// etc) do not set this. The manual does not explain this, but by
/// experiment it seems that ao. does indeed work as described. 


define powerpc-template addv

  pattern (be, tag, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, addco!-op, d, ss, rr);
    emit-branch-sdi(be, bso-cc, tag);

end powerpc-template;


/// Similarly with SUBV, we use the sfo. instruction to set the
/// Summary Overflow flag.


define powerpc-template subv

  pattern (be, tag, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, subfco!-op, d, ss, rr);
    emit-branch-sdi(be, bso-cc, tag);

end powerpc-template;


define powerpc-template not

  pattern (be, d, s)
    let ss = emit-make-reg(be, s, reg--tmp1);

    emit-x-via-tmp1-dest2(be, nor-op, d, ss, ss);

end powerpc-template;



//// Trapping versions


define powerpc-template add-trap

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, addco!-op, d, ss, rr);
    trap-on-overflow(be);

end powerpc-template;


define powerpc-template sub-trap

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, subfco!-op, d, ss, rr);
    trap-on-overflow(be);

end powerpc-template;


define powerpc-template muls-trap

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, mullwo!-op, d, rr, ss);
    trap-on-overflow(be);

end powerpc-template;


define powerpc-template addcx
  pattern (be, sum, carry, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, addc-op, sum, rr, ss);
    // shift in the carry bit
    emit-x(be, mfspr-op, reg--tmp1, xer, r0);
    harp-out(be) lsr(be, reg--tmp1, reg--tmp1, 29) end;
    // mask all else
    emit-d-via-tmp1-dest2(be, andi!-op, carry, reg--tmp1, 1);

  pattern (be, sum, carry :: any, r, s)
    harp-out (be) add(be, sum, r, s) end;
end powerpc-template;


define powerpc-template subcx
  pattern (be, diff, carry, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x-via-tmp1-dest1(be, subfc-op, diff, ss, rr);
    // shift in the carry bit
    emit-x(be, mfspr-op, reg--tmp1, xer, r0);
    harp-out(be) lsr(be, reg--tmp1, reg--tmp1, 29) end;
    // mask all else
    emit-d-via-tmp1-dest2(be, andi!-op, carry, reg--tmp1, 1);

  pattern (be, diff, carry :: any, r, s)
    harp-out (be) sub(be, diff, r, s) end;
end powerpc-template;


/*
define powerpc-template asl-trap

  pattern (be, low, s, count)
    // Does this set the SO bit in Condition Register or not??
    emit-register-shift(be, slw!-op, low, s, count);
    trap-on-overflow(be);

end powerpc-template;
*/

define method trap-on-overflow (be :: <powerpc-back-end>) => ()
  emit-branch(be, bc-op, bns-cc, 8);
  trap-always(be);
/*
  emit-x(be, mfspr-op, reg--tmp1, xer, r0);
  harp-out(be) asr(be, reg--tmp2, reg--tmp1, 31) end;
  emit-dri(be, twi-op, 4, reg--tmp2, 1);
*/
end method;


define method trap-always (be :: <powerpc-back-end>) => ()
  emit-drr(be, tw-op, 31, r0, r0);
end method;


with-ops-in powerpc-instructions (add2-mem) info := #f end;
with-ops-in powerpc-instructions (sub2-mem) info := #t end;


with-ops-in powerpc-instructions (add2-mem, sub2-mem)
  disallow-fn := tmp3-fn;
end;

define powerpc-template (add2-mem, sub2-mem)
  options (self);

  pattern (be, negate? :: <boolean> by op-info,
	   r, s :: <integer>, o :: <integer>, w :: <integer>)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let offset :: <integer> = s + o;

    emit-d(be, lwz-op, reg--tmp2, rr, offset);
    emit-d(be, addic-op, reg--tmp2, reg--tmp2,
	   if (negate?) -w else w end);
    emit-d(be, stw-op, reg--tmp2, rr, offset);

  pattern (be, negate? :: <boolean> by op-info,
	   r, s :: <integer>, o :: <integer>, w)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ww = emit-make-reg(be, w, reg--tmp3);
    let offset :: <integer> = s + o;

    emit-d(be, lwz-op, reg--tmp2, rr, offset);
    if (negate?)
      emit-x(be, subfc-op, reg--tmp2, ww, reg--tmp2);
    else
      emit-x(be, addc-op, reg--tmp2, reg--tmp2, ww);
    end;
    emit-d(be, stw-op, reg--tmp2, rr, offset);

  pattern (be, negate? :: <boolean> by op-info,
	   r, s, o, w)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);
    let ww = emit-make-reg(be, w, reg--tmp3);

    emit-x(be, addc-op, reg--tmp1, rr, ss);

    let oo = emit-make-reg(be, o, reg--tmp2);

    emit-x(be, addc-op, reg--tmp1, reg--tmp1, oo);

    emit-d(be, lwz-op, reg--tmp2, reg--tmp1, 0);
    if (negate?)
      emit-x(be, subfc-op, reg--tmp2, ww, reg--tmp2);
    else
      emit-x(be, addc-op, reg--tmp2, reg--tmp2, ww);
    end;
    emit-d(be, stw-op, reg--tmp2, reg--tmp1, 0);


end powerpc-template;



/// Locking versions


with-ops-in powerpc-instructions (add2-mem-locked) info := #f end;
with-ops-in powerpc-instructions (sub2-mem-locked) info := #t end;


define powerpc-template (add2-mem-locked, sub2-mem-locked)
  options (self);

  pattern (be, negate? :: <boolean> by op-info,
	   r, s :: <integer>, o :: <integer>, w :: <integer>)
    let rr = emit-make-reg(be, r, reg--tmp1);

    emit-d(be, addic-op, reg--tmp1, rr, s + o);

    // LOOP
    emit-x(be, lwarx-op, reg--tmp2, r0, reg--tmp1);
    emit-d(be, addic-op, reg--tmp2, reg--tmp2,
	   if (negate?) -w else w end);
    emit-x(be, stwcx!-op, reg--tmp2, r0, reg--tmp1);

    // TEST
    emit-branch(be, bc-op, bne-cc, -12);

  pattern (be, negate? :: <boolean> by op-info,
	   r, s, o :: <integer>, w :: <integer>)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);

    emit-x(be, addc-op, reg--tmp1, rr, ss);
    emit-d(be, addic-op, reg--tmp1, reg--tmp1, o);

    // LOOP
    emit-x(be, lwarx-op, reg--tmp2, r0, reg--tmp1);
    emit-d(be, addic-op, reg--tmp2, reg--tmp2,
	   if (negate?) -w else w end);
    emit-x(be, stwcx!-op, reg--tmp2, r0, reg--tmp1);

    // TEST
    emit-branch(be, bc-op, bne-cc, -12);

end powerpc-template;


define powerpc-template xadd-mem-locked

  pattern (be, d, r, s :: <integer>, w :: <integer>)
    let rr = emit-make-reg(be, r, reg--tmp1);

    emit-d(be, addic-op, reg--tmp1, rr, s);

    // LOOP
    emit-x(be, lwarx-op, reg--tmp2, r0, reg--tmp1);
    emit-d(be, addic-op, reg--tmp2, reg--tmp2, w);
    emit-x(be, stwcx!-op, reg--tmp2, r0, reg--tmp1);

    // TEST
    emit-branch(be, bc-op, bne-cc, -12);

    harp-out(be) move(be, d, reg--tmp2) end;

end powerpc-template;


