module:    powerpc-harp
Synopsis:  PowerPC multiply/divide
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// Signed and unsigned multiplies are done identically. The result will always
/// be the same for the low 32 bits - the only difference would be detection of 
/// overflow, which we don't care about anyway.  I don't understand why we have
/// the 2 instructions anyway since they are the same. Can anyone help?


define powerpc-template (muls, mulu)
   // some optimisations could be done here to avoid a multiply op 
   // for simple constants - not sure if it's really worth it though.
   
  pattern (be, d, r :: <integer> of signed-16bit-const-ref, s)
    let ss = emit-make-reg(be, s, reg--tmp1);
    emit-d-via-tmp1-dest1(be, mulli-op, d, ss, r)

  pattern (be, d, r, s :: <integer> of signed-16bit-const-ref)
    let rr = emit-make-reg(be, r, reg--tmp1);
    emit-d-via-tmp1-dest1(be, mulli-op, d, rr, s)

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);
    emit-x-via-tmp1-dest1(be, mullw-op, d, rr, ss)

end powerpc-template;


/// For MULV we do care about overflow.  We want to branch when a signed
/// integer multiply overflows. Unfortunately, the multiply instructions
/// do not set overflow in the conditions register, so we have to copy
/// the overflow condition from the fixed point exception register.

define powerpc-template mulv
   // As for muls & mulu, some optimisations could be done here to avoid a
   // multiply op for simple constants, provided it correctly tested overflow

  pattern (be, tag, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);
    emit-x-via-tmp1-dest1(be, mullwo!-op, d, rr, ss);
    // emit-ppc(be, mcrxr-op);
    emit-branch-sdi(be, bso-cc, tag)

end powerpc-template;


with-ops-in powerpc-instructions (mulx)  info := mulhw!-op end;
with-ops-in powerpc-instructions (mulux) info := mulhwu!-op end;

with-ops-in powerpc-instructions (mulx, mulux, mulxv)
  disallow-fn := tmp34-fn;
end;

define powerpc-template (mulx, mulux)
  options (self);

  pattern (be, op :: <opcode> by op-info, low :: any, high :: any, x, y)
    let xx = emit-make-reg(be, x, reg--tmp1);
    let yy = emit-make-reg(be, y, reg--tmp2);
    let lo = dst-place(low, reg--tmp3);
    let hi = dst-place(high, reg--tmp4);

    emit-x(be, mullw-op, lo, xx, yy);
    emit-x(be, op, hi, xx, yy);

    if (low) dst-move(be, low, reg--tmp3) end;
    if (high) dst-move(be, high, reg--tmp4) end

end powerpc-template;


define powerpc-template mulxv
  pattern (be, tag, low :: any, high :: any, r, s)
    harp-out (be) mulx(be, low, high, r, s) end;
    emit-branch-sdi(be, bso-cc, tag);
end powerpc-template;



/// We don't have an unsigned divide on the RS6000 - however, we do have
/// a signed divide with a 64 bit dividend.  Hence all we have to do is fill
/// the top 32 bits with 0 and we can ignore the sign of the dividend.
/// We do have to worry about the sign of the quotient, however.  But if its
/// top bit is set, the required result is bound to be either 0 or 1 depending 
/// on whether the quotient is bigger than the dividend. We treat this as a 
/// special case.


/// Function unsigned-div-fn performs an unsigned divide, calculating
/// either the quotient, the remainder, or both.  Hence it may be used 
/// by both divu and bigit16-div.

define method divide-fn
    (be :: <powerpc-back-end>, unsigned? :: <boolean>, top, bot, quot, rem)
  let tr = emit-make-reg(be, top, reg--tmp1);
  let br = emit-make-reg(be, bot, reg--tmp2);
  let qu = dst-place(quot, reg--tmp3);
  let rm = dst-place(rem, reg--tmp4);
  let div-op =
    if (unsigned?) divwu-op else divw-op end;
  if (~ rem)  // quot only
    emit-x(be, div-op, qu, tr, br);
  else
    emit-d(be, ori-op, tr, reg--tmp5, 0);  // tr may be the same reg as qu
    emit-x(be, div-op, qu, tr, br);
    emit-x(be, mullw-op, rm, qu, br);
    emit-x(be, subfc-op, rm, rm, reg--tmp5)
  end;
  if (quot) dst-move(be, quot, reg--tmp3) end;
  if (rem) dst-move(be, rem, reg--tmp4) end
end method divide-fn;


with-ops-in powerpc-instructions (mods)
  disallow-fn := tmp45-fn;
end;

define powerpc-template mods

  pattern (be, rem, top, bot)
    divide-fn(be, #f, top, bot, #f, rem);

end powerpc-template;


with-ops-in powerpc-instructions (divs, divx, truncatex) info := #f end;
with-ops-in powerpc-instructions (divu, divux) info := #t end;


with-ops-in powerpc-instructions (divu, divs)
  disallow-fn := tmp3-fn;
end;

define powerpc-template (divu, divs)
  options (self);

  pattern (be, unsigned? :: <boolean> by op-info, quot, top, bot)
    divide-fn(be, unsigned?, top, bot, quot, #f);

end powerpc-template;

define powerpc-template divv

  pattern (be, tag, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);
    emit-x-via-tmp1-dest1(be, divwo!-op, d, rr, ss);
    emit-branch-sdi(be, bso-cc, tag)

end powerpc-template;
    

with-ops-in powerpc-instructions (divux, divx, truncatex)
  disallow-fn := tmp345-fn;
end;

define powerpc-template (divux, divx, truncatex)
  options (self);

  pattern (be, unsigned? :: <boolean> by op-info,
	   quot :: any, rem :: any, top, bot)
    divide-fn(be, unsigned?, top, bot, quot, rem);

end powerpc-template;


// 64-bit dividends

define powerpc-template (divuxx, divxx, truncatexx)
  options (self);

  pattern (be, unsigned? :: <boolean> by op-info,
	   quot :: any, rem :: any, low, high, bot)
    divide-fn(be, unsigned?, low, bot, quot, rem);

end powerpc-template;


