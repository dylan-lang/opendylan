module:    powerpc-harp
Synopsis:  PowerPC floating point loads/stores and moves
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define powerpc-template fld

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-fld), d, r, s, lfs-op, lfsx-op);

end powerpc-template;


define powerpc-template dld

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-fld), d, r, s, lfd-op, lfdx-op);

end powerpc-template;


define local-powerpc-template ppc-fld

  pattern (be, d, b, o :: <integer> of signed-16bit-const-ref, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp1);
    float-emit-d-via-ftmp1(be, ins, d, bb, o)

  pattern (be, d, b, o, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp1);
    let oo = emit-make-reg(be, o, reg--tmp2);
    float-emit-x-via-ftmp1(be, index-ins, d, bb, oo)

end local-powerpc-template;


define powerpc-template fst

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-fst), d, r, s, stfs-op, stfsx-op);

end powerpc-template;


define powerpc-template dst

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-fst), d, r, s, stfd-op, stfdx-op);

end powerpc-template;


define local-powerpc-template ppc-fst

  pattern (be, s, b, o :: <integer> of signed-16bit-const-ref, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp1);
    let ss = femit-make-reg(be, s, reg--ftmp1);
    emit-d(be, ins, ss, bb, o)

  pattern (be, s, b, o, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp1);
    let oo = emit-make-reg(be, o, reg--tmp2);
    let ss = femit-make-reg(be, s, reg--ftmp1);
    emit-x(be, index-ins, ss, bb, oo)

end local-powerpc-template;


// MJS 05/04/91: Note that fmove is dmove for real registers

define powerpc-template (fmove, dmove)

  pattern (be, d, s is d)
    #f;

  pattern (be, d :: <float-register> by colour, s :: <float-register> by colour)
    emit-x(be, fmr-op, d, r0, s);

  pattern (be, d :: <float-register> by colour, s)
    femit-make-reg(be, s, d)

  pattern (be, d :: <spill> by colour, s)
    let ss = femit-make-reg(be, s, reg--ftmp1);
    float-dst-move(be, d, ss)

end powerpc-template;


// Suitably initialize the FPU control-word

define powerpc-template init-control-word
  pattern (be)
    #f;
end powerpc-template;


// Suitably initialize the FPU

define powerpc-template init-fpu
  pattern (be)
    harp-out (be) init-control-word(be) end;
end powerpc-template;


// Clear exception bits in status-word

define powerpc-template clear-float-exceptions
  pattern (be)
    #f;
end powerpc-template;


/*  Classify floats as follows:

    #x0      Unsupported
    #x100    Nan
    #x400    Normal finite number
    #x500    Infinity
    #x4000   Zero
    #x4100   Empty
    #x4400   Denormal number

*/


define powerpc-template classify-float
  pattern (be, d, s)
    harp-out(be) move(be, d, #x400) end;
end powerpc-template;

