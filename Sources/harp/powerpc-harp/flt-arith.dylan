module:    powerpc-harp
Synopsis:  PowerPC floating arithmetic
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


with-ops-in powerpc-instructions (fadd, fsub, fmul, fdiv) info := #t end;
with-ops-in powerpc-instructions (dadd, dsub, dmul, ddiv) info := #f end;


define powerpc-template (fadd, dadd)
  options (self);

  pattern (be, i :: <boolean> by op-info, d, r, s)
    let rr = femit-make-reg(be, r, reg--ftmp1);
    let ss = femit-make-reg(be, s, reg--ftmp2);
    float-emit-x-via-ftmp1(be, fadd-op, d, rr, ss, single: i)

end powerpc-template;


define powerpc-template (fsub, dsub)
  options (self);

  pattern (be, i :: <boolean> by op-info, d, r :: <integer> of zero-number?, s)
    if (i)
      harp-out(be) fneg(be, d, s) end
    else
      harp-out(be) dneg(be, d, s) end
    end;
    
  pattern (be, i :: <boolean> by op-info, d, r, s)
    let rr = femit-make-reg(be, r, reg--ftmp1);
    let ss = femit-make-reg(be, s, reg--ftmp2);
    float-emit-x-via-ftmp1(be, fsub-op, d, rr, ss, single: i)
    
end powerpc-template;


define powerpc-template (fmul, dmul)
  options (self);

  pattern (be, i :: <boolean> by op-info, d, r, s)
    let rr = femit-make-reg(be, r, reg--ftmp1);
    let ss = femit-make-reg(be, s, reg--ftmp2);
    float-emit-x-f4-via-ftmp1(be, fmul-op, d, rr, ss, single: i)

end powerpc-template;


define powerpc-template (fdiv, ddiv)
  options (self);

  pattern (be, i :: <boolean> by op-info, d, r, s)
    let rr = femit-make-reg(be, r, reg--ftmp1);
    let ss = femit-make-reg(be, s, reg--ftmp2);
    float-emit-x-via-ftmp1(be, fdiv-op, d, rr, ss, single: i)

end powerpc-template;



with-ops-in powerpc-instructions (fneg) info := fneg-op end;
with-ops-in powerpc-instructions (dneg) info := fneg-op end;

with-ops-in powerpc-instructions (fabs) info := fabs-op end;
with-ops-in powerpc-instructions (dabs) info := fabs-op end;

with-ops-in powerpc-instructions (single-to-double-float) info := fmr-op end;
with-ops-in powerpc-instructions (double-to-single-float) info := frsp-op end;


// MJS 28/03/91: I do not think any of these need extra frsp instructions.


define powerpc-template (fneg, dneg, fabs, dabs,
			 single-to-double-float, double-to-single-float)
  options (self);

  pattern (be, i :: <opcode> by op-info, d, s)
    let ss = femit-make-reg(be, s, reg--ftmp1);
    float-emit-x-via-ftmp1(be, i, d, r0, ss)

end powerpc-template;


define powerpc-template (fsqrt)

  pattern (be, d, s)
    let ss = femit-make-reg(be, s, reg--ftmp1);
    float-emit-x-via-ftmp1(be, fsqrts-op, d, r0, ss, single: #t)

end powerpc-template;

define powerpc-template (dsqrt)

  pattern (be, d, s)
    let ss = femit-make-reg(be, s, reg--ftmp1);
    float-emit-x-via-ftmp1(be, fsqrt-op, d, r0, ss)

end powerpc-template;

