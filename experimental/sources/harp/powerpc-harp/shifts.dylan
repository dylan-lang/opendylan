module:    powerpc-harp
Synopsis:  PowerPC shifts
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// Like every other machine except the clipper, this one does shifts the same
/// way that HARP does - left and right done by separate instructons.

/// MJS 03Apr95: rewrote to avoid use of MQ register (not supported by PowerPC)

define method emit-immediate-rotate
    (be :: <powerpc-back-end>,
     d, r, const, operation :: <symbol>)
  let rr = emit-make-reg(be, r, reg--tmp1);
  let mask :: <integer> =
    select (operation)
      #"shift-left" =>   ash(0, 6) + ash((31 - const), 1);
      #"shift-right" =>  ash(const, 6) + ash(31, 1);
      #"rotate-left" =>  ash(0, 6) + ash(31, 1);
      #"rotate-right" => ash(0, 6) + ash(31, 1);
    end;
  let offset =
    select (operation)
      #"shift-left" =>   const;
      #"shift-right" =>  32 - const;
      #"rotate-left" =>  const;
      #"rotate-right" => 32 - const;
    end;
  emit-rrd-via-tmp1-dest2(be, mw-add(rlwinm-op, mask), d, rr, offset)
end method emit-immediate-rotate;

define method emit-register-shift
    (be :: <powerpc-back-end>, op, d, r, s)
  let rr = emit-make-reg(be, r, reg--tmp1);
  let ss = emit-make-reg(be, s, reg--tmp2);
  emit-x-via-tmp1-dest2(be, op, d, rr, ss)
end method emit-register-shift;


define powerpc-template asl

  pattern (be, d, r, s :: <integer> of unsigned-5bit-const-ref)
    emit-immediate-rotate(be, d, r, s, #"shift-left");

  pattern (be, d, r, s)
    emit-register-shift(be, slw-op, d, r, s);

end powerpc-template;


define powerpc-template asr

  pattern (be, d, r, s :: <integer> of unsigned-5bit-const-ref)
    let rr = emit-make-reg(be, r, reg--tmp1);
    emit-rrd-via-tmp1-dest2(be, srawi-op, d, rr, s)

  pattern (be, d, r, s)
    emit-register-shift(be, sraw-op, d, r, s);

end powerpc-template;


define powerpc-template lsr

  pattern (be, d, r, s :: <integer> of unsigned-5bit-const-ref)
    emit-immediate-rotate(be, d, r, s, #"shift-right");

  pattern (be, d, r, s)
   emit-register-shift(be, srw-op, d, r, s);

end powerpc-template;


define method double-shift-left
    (be :: <powerpc-back-end>, low, high, s-low, s-high, count)
 => ()
  let s-low = emit-make-reg(be, s-low, reg--tmp1);
  let s-high = emit-make-reg(be, s-high, reg--tmp2);
  let count = emit-make-reg(be, count, reg--tmp3);
  let lo = dst-place(low, reg--tmp4);
  let hi = dst-place(high, reg--tmp5);

  emit-d(be, subfic-op, reg--tmp6, count, 32);
  emit-x-via-tmp1-dest2(be, slw-op, hi, s-high, count);
  emit-x-via-tmp1-dest2(be, srw-op, reg--tmp7, s-low, reg--tmp6);
  emit-x(be, or-op, hi, hi, reg--tmp7);
  emit-d(be, addic-op, reg--tmp6, count, -32);
  emit-x-via-tmp1-dest2(be, slw-op, reg--tmp7, s-low, reg--tmp6);
  emit-x(be, or-op, hi, hi, reg--tmp7);
  emit-x-via-tmp1-dest2(be, slw-op, lo, s-low, count);

  if (low) dst-move(be, low, reg--tmp4) end;
  if (high) dst-move(be, high, reg--tmp5) end
end method;

define method double-shift-right
    (be :: <powerpc-back-end>, low, high, s-low, s-high, count)
 => ()
  let s-low = emit-make-reg(be, s-low, reg--tmp1);
  let s-high = emit-make-reg(be, s-high, reg--tmp2);
  let count = emit-make-reg(be, count, reg--tmp3);
  let lo = dst-place(low, reg--tmp4);
  let hi = dst-place(high, reg--tmp5);

  emit-d(be, subfic-op, reg--tmp6, count, 32);
  emit-x-via-tmp1-dest2(be, srw-op, lo, s-low, count);
  emit-x-via-tmp1-dest2(be, slw-op, reg--tmp7, s-high, reg--tmp6);
  emit-x(be, or-op, lo, lo, reg--tmp7);
  emit-d(be, addic-op, reg--tmp6, count, -32);
  emit-x-via-tmp1-dest2(be, srw-op, reg--tmp7, s-high, reg--tmp6);
  emit-x(be, or-op, lo, lo, reg--tmp7);
  emit-x-via-tmp1-dest2(be, srw-op, hi, s-high, count);

  if (low) dst-move(be, low, reg--tmp4) end;
  if (high) dst-move(be, high, reg--tmp5) end
end method;


with-ops-in powerpc-instructions (lslx, lslxx, lsrxx, asl-trap, aslxv)
  disallow-fn := tmp34567-fn;
  c-preserved-destroys-fn  := tmp7-fn;
end;

define powerpc-template lslx
  pattern (be, low :: any, high :: any, s, count)
    double-shift-left(be, low, high, s, 0, count)
end powerpc-template;

define powerpc-template lslxx
  pattern (be, low :: any, high :: any, s-low, s-high, count)
    double-shift-left(be, low, high, s-low, s-high, count)
end powerpc-template;


define powerpc-template lsrxx
  pattern (be, low :: any, high :: any, s-low, s-high, count)
    double-shift-right(be, low, high, s-low, s-high, count)
end powerpc-template;


define powerpc-template asl-trap

  pattern (be, low, s, count)
    double-shift-left(be, low, reg--tmp5, s, 0, count);
    emit-dri(be, cmpi-op, 0, reg--tmp5, 0);
    emit-branch(be, bc-op, beq-cc, 8);
    trap-always(be);

end powerpc-template;

define powerpc-template aslxv
  pattern (be, ov-tag, low, high :: any, s, count)
    let hi = dst-place(high, reg--tmp5);
    double-shift-left(be, low, hi, s, 0, count);
    harp-out (be) bne(be, ov-tag, hi, 0) end;
    if (high) dst-move(be, high, reg--tmp5) end
end powerpc-template;


define powerpc-template rol

  pattern (be, d, r, s :: <integer> of unsigned-5bit-const-ref)
    emit-immediate-rotate(be, d, r, s, #"rotate-left");

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);
    let mask :: <integer> = ash(0, 6) + ash(31, 1);

    emit-x-via-tmp1-dest2(be, mw-add(rlwnm-op, mask), d, rr, ss)

end powerpc-template;

define powerpc-template ror

  pattern (be, d, r, s :: <integer> of unsigned-5bit-const-ref)
    emit-immediate-rotate(be, d, r, s, #"rotate-right");

  pattern (be, d, r, s)
    let rr = emit-make-reg(be, r, reg--tmp1);
    let ss = emit-make-reg(be, s, reg--tmp2);
    let mask :: <integer> = ash(0, 6) + ash(31, 1);

    emit-d(be, subfic-op, reg--tmp2, ss, 32);
    emit-x-via-tmp1-dest2(be, mw-add(rlwnm-op, mask), d, rr, reg--tmp2)

end powerpc-template;

