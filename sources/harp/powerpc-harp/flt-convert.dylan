module:    powerpc-harp
Synopsis:  PowerPC floating point conversion templates
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define powerpc-template move-to-sfreg

  pattern (be, float, int :: <spill> by colour)
    let (base, offset) = spill-base-and-offset(be, int);
    float-emit-d-via-ftmp1(be, lfs-op, float, base, offset)

  pattern (be, float :: <f-ic/spill-ref> by colour, int)
    let src = emit-make-reg(be, int, reg--tmp1);
    move-register-to-memory(be, float, src)

  pattern (be, float, int)
    let src = emit-make-reg(be, int, reg--tmp1);
    emit-d(be, stw-op, src, reg--stack, -4);
    float-emit-d-via-ftmp1(be, lfs-op, float, reg--stack, -4)

end powerpc-template;


define powerpc-template move-from-sfreg

  pattern (be, int, float :: <f-ic/spill-ref> by colour)
    emit-d-via-tmp1-dest1
      (be, lwz-op, int, float.base-register, float-offset(be, float))

  pattern (be, int :: <spill> by colour, float)
    let (base, offset) = spill-base-and-offset(be, int);
    let src = femit-make-reg(be, float, reg--ftmp1);
    emit-d(be, stfs-op, src, base, offset)

  pattern (be, int, float)
    let src = femit-make-reg(be, float, reg--ftmp1);
    emit-d(be, stfs-op, src, reg--stack, -4);
    emit-d-via-tmp1-dest1(be, lwz-op, int, reg--stack, -4)

end powerpc-template;


define powerpc-template move-to-dfreg
  pattern (be, float, low, high)
    let low = emit-make-reg(be, low, reg--tmp1);
    let high = emit-make-reg(be, high, reg--tmp2);

    emit-d(be, stw-op, high, reg--stack, -8);
    emit-d(be, stw-op, low, reg--stack, -4);
    float-emit-d-via-ftmp1(be, lfd-op, float, reg--stack, -8)
end powerpc-template;


define powerpc-template move-from-dfreg
  pattern (be, low :: any, high :: any, float)
    let lo = dst-place(low, reg--tmp1);
    let hi = dst-place(high, reg--tmp2);
    let src = femit-make-reg(be, float, reg--ftmp1);

    emit-d(be, stfd-op, src, reg--stack, -8);
    emit-d-via-tmp1-dest1(be, lwz-op, hi, reg--stack, -8);
    emit-d-via-tmp1-dest1(be, lwz-op, lo, reg--stack, -4);

  if (low) dst-move(be, low, reg--tmp1) end;
  if (high) dst-move(be, high, reg--tmp2) end
end powerpc-template;


define powerpc-template (convert-to-single-float, convert-to-double-float)

  pattern (be, float, int)

/*

    let int = emit-make-reg(be, int, reg--tmp1);

    emit-d(be, std-op, int, reg--stack, -8);
    emit-d(be, lfd-op, reg-ftmp1, reg--stack, -8);
    emit-x(be, fcfid-op, reg--ftmp1, #f, reg--ftmp1);

*/

    harp-out(be)
      move(be, reg--tmp1, #x43300000);
      move(be, reg--tmp2, #x80000000)
    end;
    emit-d(be, stwu-op, reg--tmp1, reg--stack, -8);
    emit-d(be, stw-op, reg--tmp2, reg--stack, 4);

    let src = emit-make-reg(be, int, reg--tmp1);
    let inv = reg--tmp2;
    emit-d(be, lfd-op, reg--ftmp2, reg--stack, 0);
    emit-d(be, xoris-op, src, inv, #x8000);
    emit-d(be, stw-op, inv, reg--stack, 4);
    emit-d(be, lfd-op, reg--ftmp1, reg--stack, 0);
    emit-d(be, addic-op, reg--stack, reg--stack, 8);
    float-emit-x-via-ftmp1(be, fsub-op, float, reg--ftmp1,
                           reg--ftmp2)


end powerpc-template;

define powerpc-template (convert-to-single-float-x, convert-to-double-float-x)

  pattern (be, float, low, high)
    harp-out(be)
      move(be, reg--tmp1, #x43300000);
      move(be, reg--tmp2, #x80000000)
    end;
    emit-d(be, stwu-op, reg--tmp1, reg--stack, -8);
    emit-d(be, stw-op, reg--tmp2, reg--stack, 4);

    let low = emit-make-reg(be, low, reg--tmp1);
    let high = emit-make-reg(be, high, reg--tmp2);

    emit-d(be, lfd-op, reg--ftmp2, reg--stack, 0);

    emit-d(be, xoris-op, low, low, #x8000);
    emit-d(be, stw-op, low, reg--stack, 4);

    emit-d(be, xoris-op, high, high, #x4330);
    emit-d(be, stw-op, high, reg--stack, 0);

    emit-d(be, lfd-op, reg--ftmp1, reg--stack, 0);
    emit-d(be, addic-op, reg--stack, reg--stack, 8);
    float-emit-x-via-ftmp1(be, fsub-op, float, reg--ftmp1,
                           reg--ftmp2)

end powerpc-template;



define powerpc-template (convert-from-single-float, convert-from-double-float)

  pattern (be, int, float)
    
    let float = femit-make-reg(be, float, reg--ftmp1);

    emit-x(be, fctiw-op, reg--ftmp2, #f, float);
    emit-d(be, stfd-op, reg--ftmp2, reg--stack, -8);
    emit-d(be, lwz-op, int, reg--stack, -4);

end powerpc-template;


define powerpc-template (convert-from-single-float-x, convert-from-double-float-x)

// MJS 18/03/91: A munge of the .itrunc library routine. The rounding mode has
// been set up in the FPSCR beforehand. Works by adding the result to a magic
// number to leave the integer in the second word of the the fpreg. To make
// truncation work, must have a magic number with the same sign as the input.

  pattern (be, low, high, float)
    // Store the magic number #x10080000000000.0
    harp-out(be)
      move(be, reg--tmp1, #x43300800);
      move(be, reg--tmp2, #x00000000)
    end;
    emit-d(be, stwu-op, reg--tmp1, reg--stack, -8);
    emit-d(be, stw-op, reg--tmp2, reg--stack, 4);
    
    let float = femit-make-reg(be, float, reg--ftmp1);
    let lo = dst-place(low, reg--tmp1);
    let hi = dst-place(high, reg--tmp2);
    let ft1 = reg--ftmp2;

    emit-d(be, lfs-op, ft1, reg--stack, 4);    // Load 0.0
    emit-drr(be, fcmpu-op, crf0, float, ft1);
    emit-d(be, lfd-op, ft1, reg--stack, 0);    // Load the magic number
    emit-branch(be, bc-op, bge-cc, 8);             // Branch to POSITIVE_IN
    emit-x(be, fneg-op, ft1, r0, ft1);             // Use same sign as input

    // POSITIVE_IN
    emit-x(be, fadd-op, ft1, float, ft1);              // Perform the conversion
    emit-d(be, stfd-op, ft1, reg--stack, 0);

    emit-d(be, lwz-op, lo, reg--stack, 4);
    emit-d(be, lwz-op, hi, reg--stack, 0);

    emit-d(be, addic-op, reg--stack, reg--stack, 8);
    emit-branch(be, bc-op, bge-cc, 8);             // Branch to POSITIVE_OUT

    // Restore sign of input
    emit-x(be, neg-op, hi, hi, r0);

    // POSITIVE_OUT
    dst-move(be, low, reg--tmp1);
    dst-move(be, high, reg--tmp2);

end powerpc-template;


/*

define powerpc-template (convert-from-single-float, convert-from-double-float)

  pattern (be, int, float)
    
    harp-out(be)
      move(be, reg--tmp1, #x41e00000);
      move(be, reg--tmp2, #x00000000)
    end;
    emit-d(be, stwu-op, reg--tmp1, reg--stack, -8);
    emit-d(be, stw-op, reg--tmp2, reg--stack, 4);

    
    let src = femit-make-reg(be, float, reg--ftmp1);
    let dst = dst-place(int, reg--tmp1);
    let t0 = reg--ftmp7;
    let t2 = reg--ftmp2;
    let t3 = reg--ftmp3;
    let t4 = reg--ftmp4;
    let t5 = reg--ftmp5;
    let t6 = reg--ftmp6;

    emit-d(be, lfs-op, t0, reg--stack, 4);   // load zero
    emit-drr(be, fcmpu-op, crf1, src, t0);
    emit-x(be, fabs-op, t5, f0, src);
    emit-d(be, lfd-op, t2, reg--stack, 0);
    emit-x(be, mffs-op, t4, f0, f0);
    harp-out(be) move(be, reg--tmp1, #x43300800) end;
    emit-d(be, stw-op, reg--tmp1, reg--stack, 0);
    emit-drr(be, fcmpu-op, crf0, t5, t2);
    emit-d(be, lfd-op, t3, reg--stack, 0);
    emit-branch(be, bc-op, blt1-cc, 16);         // branch to LAB1

    emit-drr(be, mtfsb1-op, #x1e, f0, f0);
    emit-drr(be, mtfsb1-op, #x1f, f0, f0);
    emit-branch(be, bc-op, blt1-cc, 12);         // branch to LAB2

    // LAB1
    emit-drr(be, mtfsb1-op, #x1e, f0, f0);
    emit-drr(be, mtfsb0-op, #x1f, f0, f0);

    // LAB2
    emit-x(be, fadd-op, t6, src, t3);
    emit-d(be, stfd-op, t6, reg--stack, 0);
    emit-fsf(be, mtfsf-op, 1, t4);
    emit-branch(be, bc-op, bge-cc, 12);          // branch to LAB3
    emit-d(be, lwz-op, dst, reg--stack, 4);
    emit-branch(be, bc-op, bra-cc, 24);          // branch to FINISH

    // LAB3
    emit-fsf(be, mtfsf-op, #xff, t4);
    emit-d(be, addis-op, dst, r0, #x8000);
    emit-branch(be, bc-op, bso-cc, 12);          // branch to LAB4
    emit-branch(be, bc-op, blt1-cc, 8);          // branch to LAB4
    emit-d(be, subfic-op, dst, dst, -1);

    // LAB4
    // harp-out(be) move(be, reg--tmp2, #x20000100) end;
    // FP-SET-FLAG    using   dst for r31   and   tmp2 for r3
    

    // FINISH
    emit-d(be, addic-op, reg--stack, reg--stack, 8);
    dst-move(be, int, dst)

end powerpc-template;

*/


define method rounding-mode (val)
 => (op1, op2)
  select (val)
    #"default"  => values(mtfsb0-op, mtfsb0-op);
    #"round"    => values(mtfsb0-op, mtfsb0-op);
    #"truncate" => values(mtfsb0-op, mtfsb1-op);
    #"floor"    => values(mtfsb1-op, mtfsb1-op);
    #"ceiling"  => values(mtfsb1-op, mtfsb0-op);
    otherwise   => #f;
  end select;
end method;



define powerpc-template set-rounding-mode
  pattern (be, mode)
    let (op1, op2) = rounding-mode(mode);
    emit-xl(be, op1, 30, #f, #f);
    emit-xl(be, op2, 31, #f, #f);
end powerpc-template;

