module:    powerpc-harp
Synopsis:  PowerPC floating point general
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Should be put in HARP module

define inline method f-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <float-register>)) c end;
end method f-ref;

define method fspill-index
    (be :: <powerpc-back-end>, x :: <sfspill>) => (i :: <integer>)
  let state = be.variables.vreg-state;
  -4 * (1 // one for the spill location itself
        + x.spill-offset
        + state.number-preserved
        + state.next-ng-spill 
       );
end method fspill-index;

define method fspill-index
    (be :: <powerpc-back-end>, x :: <dfspill>) => (i :: <integer>)
  let state = be.variables.vreg-state;
  -4 * (2 // for the spill location itself
        + (2 * x.spill-offset)
        + state.number-preserved
        + state.next-ng-spill 
        + state.next-sf-spill 
       );
end method fspill-index;


define method float-dst-emit-make-addr
    (be :: <powerpc-back-end>, fspill)
  let offset = fspill-index(be, fspill);
  if (signed-16bit-const-ref(offset))
    offset;
  else
    address-distant-spill(offset)
  end
end method float-dst-emit-make-addr;


define method femit-make-reg
    (be :: <powerpc-back-end>, operand :: <float-register>, reg :: <float-register>)
 => (reg :: <float-register>)
  // already in a register, so do nothing             
  operand;
end method femit-make-reg;

define method femit-make-reg
    (be :: <powerpc-back-end>, operand :: <sfspill>, reg :: <float-register>)
 => (reg :: <float-register>)
  // single float load into reg provided               
  emit-d(be, lfs-op, reg, reg--frame, fspill-index(be, operand));
  reg;
end method femit-make-reg;

define method femit-make-reg
    (be :: <powerpc-back-end>, operand :: <dfspill>, reg :: <float-register>)
 => (reg :: <float-register>)
  // double float load into reg provided               
  emit-d(be, lfd-op, reg, reg--frame, fspill-index(be, operand));
  reg;
end method femit-make-reg;

define method femit-make-reg
    (be :: <powerpc-back-end>, operand :: <sf-indirect-constant-reference>,
     reg :: <float-register>)
 => (reg :: <float-register>)
  // load hi part
  emit-d-high(be, addis-op, reg--constants, r0);
  emit-constant-ref(be, operand, high?: #t);
  // single float load into reg provided               
  emit-d-high(be, lfs-op, reg, reg--constants);
  emit-constant-ref(be, operand, low?: #t);
  reg;
end method femit-make-reg;

define method femit-make-reg
    (be :: <powerpc-back-end>, operand :: <df-indirect-constant-reference>,
     reg :: <float-register>)
 => (reg :: <float-register>)
  // load hi part
  emit-d-high(be, addis-op, reg--constants, r0);
  emit-constant-ref(be, operand, high?: #t);
  // double float load into reg provided               
  emit-d-high(be, lfd-op, reg, reg--constants);
  emit-constant-ref(be, operand, low?: #t);
  reg;
end method femit-make-reg;


define method move-float-register-to-memory
    (be :: <powerpc-back-end>, dst :: <sfspill>, src :: <float-register>)
 => ()
  emit-d(be, stfs-op, src, reg--frame, float-dst-emit-make-addr(be, dst))
end method move-float-register-to-memory;

define method move-float-register-to-memory
    (be :: <powerpc-back-end>, dst :: <dfspill>, src :: <float-register>)
 => ()
  emit-d(be, stfd-op, src, reg--frame, float-dst-emit-make-addr(be, dst))
end method move-float-register-to-memory;

define method move-float-register-to-memory
    (be :: <powerpc-back-end>, dst :: <sf-indirect-constant-reference>,
     src :: <float-register>)
 => ()
  // load hi part
  emit-d-high(be, addis-op, reg--constants, r0);
  emit-constant-ref(be, dst, high?: #t);
  emit-d-high(be, stfs-op, src, reg--constants);
  emit-constant-ref(be, dst, low?: #t);
end method move-float-register-to-memory;

define method move-float-register-to-memory
    (be :: <powerpc-back-end>, dst :: <df-indirect-constant-reference>,
     src :: <float-register>)
 => ()
  // load hi part
  emit-d-high(be, addis-op, reg--constants, r0);
  emit-constant-ref(be, dst, high?: #t);
  emit-d-high(be, stfd-op, src, reg--constants);
  emit-constant-ref(be, dst, low?: #t);
end method move-float-register-to-memory;


define method float-offset
    (be :: <powerpc-back-end>, dst :: <spill>)
 => (offset)
  float-dst-emit-make-addr(be, dst)
end method float-offset;

define method float-offset
    (be :: <powerpc-back-end>, dst :: <indirect-constant-reference>)
 => (offset)
  dst
end method float-offset;


define macro float-dst-place
  { float-dst-place(?dst:expression, ?reg:expression) }
    =>
  { f-ref(?dst) | ?reg  }
end macro;

define macro float-dst-move
  { float-dst-move(?be:name, ?dst:expression, ?reg:expression) }
    =>
  { 
    let dst = ?dst;
    let reg = ?reg;
    let dst-c = colour(dst);

    unless (instance?(dst-c, <float-register>))
      move-float-register-to-memory(?be, dst, reg)
    end
  }
end macro;



/// Some common routines for emitting instructions with float results.
/// Allow for general destination, using ftmp1 as an interim if necessary


define method float-emit-x-via-ftmp1
    (be :: <powerpc-back-end>,
     opcode :: <opcode>,
     dest,
     rega,
     regb,
     #key single = #f)
  let dest-reg = float-dst-place(dest, reg--ftmp1);
  emit-x(be, opcode, dest-reg, rega, regb);
  if (single)
    emit-x(be, frsp-op, dest-reg, r0, dest-reg)
  end;
  unless (instance?(dest, <float-register>))
    move-float-register-to-memory(be, dest, dest-reg)
  end;
end method float-emit-x-via-ftmp1;


/// unfortunately we have to handle the awkward FMUL-OP which has parameters
/// in the wrong place!


define method float-emit-x-f4-via-ftmp1
    (be :: <powerpc-back-end>,
     opcode :: <opcode>,
     dest,
     rega,
     regb,
     #key single = #f)
  let dest-reg = float-dst-place(dest, reg--ftmp1);
  emit-x-f4(be, opcode, dest-reg, rega, regb);
  if (single)
    emit-x(be, frsp-op, dest-reg, r0, dest-reg)
  end;
  unless (instance?(dest, <float-register>))
    move-float-register-to-memory(be, dest, dest-reg)
  end;
end method float-emit-x-f4-via-ftmp1;



define method float-emit-d-via-ftmp1
    (be :: <powerpc-back-end>, opcode :: <opcode>, dest, rega, data)
  let dest-reg = float-dst-place(dest, reg--ftmp1);
  emit-d(be, opcode, dest-reg, rega, data);
  unless (instance?(dest, <float-register>))
    move-float-register-to-memory(be, dest, dest-reg)
  end;
end method float-emit-d-via-ftmp1;



