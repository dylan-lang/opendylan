module:    powerpc-harp
Synopsis:  PowerPC bit field bashing instructions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// The syntax is LDBITS dest base offset field-width
///               STBITS      base offset field-width source


/// For LDBITS, we can use the rotate-left-then-and-with-mask instruction
/// to find the bits we need provided the field width is known at compile
/// time.  If the field width is not known, we have to resort to the long
/// method of creating a mask for the field width, shifting the data by the
/// offset, followed by an AND of the mask and data.

/// The long method takes up tmp3, and looks as follows:
///   LD  dest base 0
///   LSR  dest dest offset
///   MOVE z -1
///   ASL  z z field-width
///   NOT  z z 
///   AND  dest dest z
/// although we are able to optimise it by avoiding the NOT and 
/// using an AND-with-complement instruction.


with-ops-in powerpc-instructions
  (ldbits, extract-bits, stbits, set-bits)
  disallow-fn := tmp3-fn;
end;


/// mask-for-field returns a mask value suitable for the 
/// rotate-left-with-mask type of instructions. The mask it calculates
/// has "width" 1s starting at bit "offset".  We have to allow for IBM
/// big-endian numbering.

define inline method mask-for-field (width :: <integer>, offset :: <integer>)
 => (mask :: <integer>)
  let left-bit = width + offset - 1;
  let big-endian-left-bit :: <integer> = 31 - left-bit;

  let right-bit = offset;
  let big-endian-right-bit :: <integer> = 31 - right-bit;

  ash(big-endian-left-bit, 6) + ash(big-endian-right-bit, 1)
end method;


define powerpc-template ldbits

  pattern (be, d, b,
	   o :: <integer> of unsigned-5bit-const-ref,
	   f :: <integer> of unsigned-5bit-const-ref)
    let mask = mask-for-field(f, 0);

    harp-out(be) ld(be, reg--tmp1, b, 0) end;
    emit-rrd-via-tmp1-dest2(be, mw-add(rlwinm-op, mask), d, reg--tmp1, 32 - o)

  pattern (be, d, b, o, f :: <integer> of unsigned-5bit-const-ref)
    let oo = emit-make-reg(be, o, reg--tmp2);
    let mask = mask-for-field(f, 0);

    harp-out(be) ld(be, reg--tmp1, b, 0) end;
    emit-d(be, subfic-op, reg--tmp2, oo, 32);  // subtract shift from 32 -> tmp2
    emit-x-via-tmp1-dest2(be, mw-add(rlwnm-op, mask), d, reg--tmp1,
			  reg--tmp2)

  pattern (be, d, b, o, f)
   let ff = emit-make-reg(be, f, reg--tmp1);
   let oo = emit-make-reg(be, o, reg--tmp3);

   // start by making the inverted mask in tmp2
   emit-d(be, addi-op, reg--tmp2, r0, -1);
   emit-x(be, slw-op, reg--tmp2, reg--tmp2, ff);
   // now shift the data into tmp1
   harp-out(be) ld(be, reg--tmp1, b, 0) end;
   emit-x(be, srw-op, reg--tmp1, reg--tmp1, oo);
   // now AND-With-Complement the inverted mask and the data
   emit-x-via-tmp1-dest2(be, andc-op, d, reg--tmp1, reg--tmp2)

end powerpc-template;


define powerpc-template extract-bits

  pattern (be, d, w,
	   o :: <integer> of unsigned-5bit-const-ref,
	   f :: <integer> of unsigned-5bit-const-ref)
    let ww = emit-make-reg(be, w, reg--tmp1);
    let mask = mask-for-field(f, 0);

    emit-rrd-via-tmp1-dest2(be, mw-add(rlwinm-op, mask), d, ww, 32 - o)

  pattern (be, d, w, o, f :: <integer> of unsigned-5bit-const-ref)
    let ww = emit-make-reg(be, w, reg--tmp1);
    let oo = emit-make-reg(be, o, reg--tmp2);
    let mask = mask-for-field(f, 0);

    emit-d(be, subfic-op, reg--tmp2, oo, 32);  // subtract shift from 32 -> tmp2
    emit-x-via-tmp1-dest2(be, mw-add(rlwnm-op, mask), d, ww,
			  reg--tmp2)

  pattern (be, d, w, o, f)
    let ff = emit-make-reg(be, f, reg--tmp1);
    let oo = emit-make-reg(be, o, reg--tmp3);

    // start by making the inverted mask in tmp2
    emit-d(be, addi-op, reg--tmp2, r0, -1);
    emit-x(be, slw-op, reg--tmp2, reg--tmp2, ff);

    let ww = emit-make-reg(be, w, reg--tmp1);
    emit-x-via-tmp1-dest2(be, srw-op, reg--tmp1, ww, oo);
    // now AND-With-Complement the inverted mask and the data
    emit-x-via-tmp1-dest2(be, andc-op, d, reg--tmp1, reg--tmp2)

end powerpc-template;



/// For STBITS, we can use the rotate-left-immediate-then-mask-insert 
/// instruction to shift the source by the required offset, and update
/// the relevant bits in the  destination provided both the offset and field 
/// width are known at compile time. If the field width only were known at 
/// compile time, it would be possible to use the non immediate form of
/// the instruction, but this seems so unlikely, that it will be covered by the
/// general case (for now at least).
///
/// For  the general case, we have to resort to the long method of creating
/// a mask for the field width, shifting the data by the offset, followed by
/// a mask-insert-from-register.  Because of the problems of converting to the 
/// IBM bit ordring, the MASKG mask generation instruction does not save us 
/// anything over the "standard" method of making a mask.


define powerpc-template stbits

  pattern (be, d,
	   o :: <integer> of unsigned-5bit-const-ref,
	   f :: <integer> of unsigned-5bit-const-ref, s)
    let ss = emit-make-reg(be, s, reg--tmp1);
    let mask = mask-for-field(f, o);
    harp-out(be) ld(be, reg--tmp2, d, 0) end;
    emit-rrd-via-tmp1-dest2(be, mw-add(rlwimi-op, mask), reg--tmp2, ss, o);
    harp-out(be) st(be, reg--tmp2, d, 0) end

  pattern (be, d, o, f, s)
    let oo = emit-make-reg(be, o, reg--tmp2);
    let ff = emit-make-reg(be, f, reg--tmp1);

    // create a mask in tmp3
    emit-d(be, addi-op, reg--tmp3, r0, 1);                  // Load 1
    emit-x(be, slw-op, reg--tmp3, reg--tmp3, ff);       // Shift left by width
    emit-d(be, addic-op, reg--tmp3, reg--tmp3, -1);       // Subtract 1 to get mask
    emit-x(be, slw-op, reg--tmp3, reg--tmp3, oo);       // Shift left by offset

    let ss = emit-make-reg(be, s, reg--tmp1);

    // shift the source by the offset, and put in tmp1 ??
    // emit-x(be, slw-op, ss, reg--tmp1, oo);

    // load the data into tmp2, modify & put back

    harp-out(be) ld(be, reg--tmp2, d, 0) end;

    emit-x-via-tmp1-dest2(be, and!-op, reg--tmp1, ss, reg--tmp3);
    emit-x-via-tmp1-dest2(be, andc!-op, reg--tmp2, reg--tmp2, reg--tmp3);
    emit-x-via-tmp1-dest2(be, or!-op, reg--tmp2, reg--tmp1, reg--tmp2);

    harp-out(be) st(be, reg--tmp2, d, 0) end

end powerpc-template;


define powerpc-template set-bits

  pattern (be, d, w,
	   o :: <integer> of unsigned-5bit-const-ref,
	   f :: <integer> of unsigned-5bit-const-ref, s)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    let ss = emit-make-reg(be, s, reg--tmp1);
    let mask = mask-for-field(f, o);
    emit-rrd-via-tmp1-dest2(be, mw-add(rlwimi-op, mask), d, ss, o);

  pattern (be, d, w, o, f, s)
    unless (d == w) harp-out (be) move(be, d, w) end end;
    let oo = emit-make-reg(be, o, reg--tmp2);
    let ff = emit-make-reg(be, f, reg--tmp1);

    // create a mask in tmp3
    emit-d(be, addi-op, reg--tmp3, r0, 1);                  // Load 1
    emit-x(be, slw-op, reg--tmp3, reg--tmp3, ff);       // Shift left by width
    emit-d(be, addic-op, reg--tmp3, reg--tmp3, -1);       // Subtract 1 to get mask
    emit-x(be, slw-op, reg--tmp3, reg--tmp3, oo);       // Shift left by offset

    let ss = emit-make-reg(be, s, reg--tmp1);
    let dd = emit-make-reg(be, d, reg--tmp2);

    emit-x-via-tmp1-dest2(be, and!-op, reg--tmp1, ss, reg--tmp3);
    emit-x-via-tmp1-dest2(be, andc!-op, reg--tmp2, dd, reg--tmp3);
    emit-x-via-tmp1-dest2(be, or!-op, d, reg--tmp1, reg--tmp2);


    // shift the source by the offset, and put in tmp1 ??
    // emit-x(be, slw-op, ss, reg--tmp1, oo);

end powerpc-template;



define powerpc-template set-bit
  
  pattern (be, d, s, bit :: <integer> of unsigned-5bit-const-ref)
    let mask :: <integer> = ash(1, bit);
    harp-out (be) or(be, d, s, mask) end;

  pattern (be, d, s, bit)
    let bit = emit-make-reg(be, bit, reg--tmp1);

    // create a mask in tmp2
    emit-d(be, addi-op, reg--tmp2, r0, 1);
    emit-x(be, slw-op, reg--tmp2, reg--tmp2, bit);

    let ss = emit-make-reg(be, s, reg--tmp1);

    emit-x-via-tmp1-dest2(be, or!-op, d, ss, reg--tmp2);

end powerpc-template;

define powerpc-template unset-bit
  
  pattern (be, d, s, bit :: <integer> of unsigned-5bit-const-ref)
    let mask :: <integer> = ash(1, bit);
    harp-out (be) and(be, d, s, lognot(mask)) end;

  pattern (be, d, s, bit)
    let bit = emit-make-reg(be, bit, reg--tmp1);

    // create a mask in tmp2
    emit-d(be, addi-op, reg--tmp2, r0, 1);
    emit-x(be, slw-op, reg--tmp2, reg--tmp2, bit);

    let ss = emit-make-reg(be, s, reg--tmp1);

    emit-x-via-tmp1-dest2(be, andc!-op, d, ss, reg--tmp2);

end powerpc-template;

