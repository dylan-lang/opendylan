module:    powerpc-harp
Synopsis:  PowerPC common code generator functions and definitions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




/// lowest-preserved-register looks at all preserved registers, and
/// finds the lowest. It should be possible to call this less often,
/// as a future optimisation.


define method lowest-preserved-register
    (be :: <powerpc-back-end>)
 => (lowest :: <integer>)
  let vars = be.variables;
  let state = vars.vreg-state;
  // store the frame register at the very least
  let lowest = reg--frame.real-register-number;
  for (reg :: <real-register> in list-from-prset(be, state.allocated-preserved))
    if (instance?(reg, <integer-register>))
      lowest := min(lowest, reg.real-register-number)
    end;
  finally
    lowest
  end
end method lowest-preserved-register;


define method base-register
    (dst :: <spill>)
 => (reg :: <real-register>)
  reg--frame
end method base-register;

define method base-register
    (dst :: <constant-reference>)
 => (reg :: <real-register>)
  reg--tmp1
end method base-register;


define method emit-make-reg
    (be :: <powerpc-back-end>, operand :: <real-register>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  if (operand == r0)
    // Make a real register
    emit-d-via-tmp1-dest2(be, ori-op, reg, r0, 0);
    reg
  else
    // already in a register, so do nothing
    operand
  end;
end method emit-make-reg;

define method emit-make-reg
    (be :: <powerpc-back-end>, operand :: <spill>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  // load into the work register provided
  let (base, offset) = spill-base-and-offset(be, operand);
  emit-d(be, lwz-op, reg, base, offset);
  reg;
end method emit-make-reg;

define method emit-make-reg
    (be :: <powerpc-back-end>, operand :: <abstract-integer>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  let int32 = canonicalise-int(operand);
  if (signed-sixteen-bits?(int32))  // load integer into reg
    emit-d(be, addi-op, reg, r0, int32);
    reg;  // case of quick signed load
  else
    // Build a large integer in up to 2 stages
    let hi-16-bits :: <integer> = high-16(int32);
    let lo-16-bits :: <integer> = low-16(int32);
    emit-d(be, addis-op, reg, r0, hi-16-bits);  // else load hi part
    unless (zero?(lo-16-bits))            // and or in rest
      emit-d(be, ori-op, reg, reg, lo-16-bits)
    end;
    reg
  end;
end method emit-make-reg;

define method emit-make-reg
    (be :: <powerpc-back-end>, operand :: <i-address-constant-reference>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  emit-make-reg-direct(be, operand, reg)
end method emit-make-reg;

define inline method emit-make-reg-direct
    (be :: <powerpc-back-end>, operand :: <i-constant-reference>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  // load hi part
  emit-d-high(be, addis-op, reg, r0);
  emit-constant-ref(be, operand, high?: #t);
  // load lo part
  emit-d-high(be, addi-op, reg, reg);
  emit-constant-ref(be, operand, low?: #t);
  reg
end method emit-make-reg-direct;

define method emit-make-reg
    (be :: <powerpc-back-end>, operand :: <i-indirect-constant-reference>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  emit-make-reg-indirect(be, operand, reg)
end method emit-make-reg;

define inline method emit-make-reg-indirect
    (be :: <powerpc-back-end>, operand :: <i-constant-reference>,
     reg :: <real-register>)
 => (reg :: <real-register>)
  // load hi part
  emit-d-high(be, addis-op, reg, r0);
  emit-constant-ref(be, operand, high?: #t);
  // indirect using lo part
  emit-d-high(be, lwz-op, reg, reg);
  emit-constant-ref(be, operand, low?: #t);
  reg
end method emit-make-reg-indirect;


define method move-register-to-memory
    (be :: <powerpc-back-end>, dst :: <spill>, src :: <integer-register>)
 => ()
  let (base, offset) = spill-base-and-offset(be, dst);
  emit-d(be, stw-op, src, base, offset);
end method move-register-to-memory;

define method move-register-to-memory
    (be :: <powerpc-back-end>, dst :: <sfspill>, src :: <integer-register>)
 => ()
  emit-d(be, stw-op, src, reg--frame, float-dst-emit-make-addr(be, dst))
end method move-register-to-memory;

define method move-register-to-memory
    (be :: <powerpc-back-end>, dst :: <indirect-constant-reference>,
     src :: <integer-register>)
 => ()
  // load hi part
  emit-d-high(be, addis-op, reg--constants, r0);
  emit-constant-ref(be, dst, high?: #t);
  // indirect using lo part
  emit-d-high(be, stw-op, src, reg--constants);
  emit-constant-ref(be, dst, low?: #t);
end method move-register-to-memory;



/// For putting results into spills, we currently rely on being able to form 
/// the address with a 16 bit offset from the frame register.  In practice this 
/// will never(!) fail, but we still ought to better than generating an error.


define method address-distant-spill (offset)
  error("Stack frame too big for 16 bit displacement")  // for now
end method address-distant-spill;

/*
define method dst-emit-make-addr (be :: <powerpc-back-end>, ispill :: <spill>)
  let offset = spill-frame-pointer-offset(be, ispill, be.variables.with-stack);
  if (signed-16bit-const-ref(offset))
    offset;
  else
    address-distant-spill(offset)
  end
end method dst-emit-make-addr;
*/

define inline method i-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <integer-register>)) c end;
end method i-ref;

/*
define inline method special-reg-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <special-register>)) c end;
end method special-reg-ref;
*/

define macro dst-place
  { dst-place(?dst:expression, ?reg:expression) }
    =>
  { i-ref(?dst) | ?reg  }
end macro;


/// And this thing does the work afterward

define macro dst-move
  { dst-move(?be:name, ?dst:expression, ?reg:expression) }
    =>
  { 
    let dst = ?dst;
    let reg = ?reg;
    let dst-c = colour(dst);

    unless (instance?(dst-c, <integer-register>))
      move-register-to-memory(?be, dst, reg)
    end
  }
end macro;



/// Some common routines for emitting instructions.
/// Allow for general destination, using tmp1 as an interim if necessary

/// Unfortunately, the destination register field varies between instructions
/// E.g. it is field-1 for ADD and field-2 for AND. Hence we need 2 versions of
/// the routines for each instruction type. Yuk!

define method emit-x-via-tmp1-dest1
    (be :: <powerpc-back-end>, opcode :: <opcode>, dest, rega, regb)
  let dest-reg = dst-place(dest, reg--tmp1);
  emit-x(be, opcode, dest-reg, rega, regb);
  unless (dest == dest-reg)
    move-register-to-memory(be, dest, dest-reg)
  end
end method emit-x-via-tmp1-dest1;

define method emit-x-via-tmp1-dest2
    (be :: <powerpc-back-end>, opcode :: <opcode>, dest, rega, regb)
  let dest-reg = dst-place(dest, reg--tmp1);
  emit-x(be, opcode, rega, dest-reg, regb);
  unless (dest == dest-reg)
    move-register-to-memory(be, dest, dest-reg)
  end
end method emit-x-via-tmp1-dest2;

define method emit-rrd-via-tmp1-dest2
    (be :: <powerpc-back-end>, opcode :: <opcode>, dest, rega, data)
  let dest-reg = dst-place(dest, reg--tmp1);
  emit-rrd(be, opcode, rega, dest-reg, data);
  unless (dest == dest-reg)
    move-register-to-memory(be, dest, dest-reg)
  end
end method emit-rrd-via-tmp1-dest2;

define method emit-d-via-tmp1-dest1
    (be :: <powerpc-back-end>, opcode :: <opcode>, dest, rega, data)
  let dest-reg = dst-place(dest, reg--tmp1);

  if (instance?(data, <constant-reference>))
    // load hi part
    emit-d-high(be, addis-op, rega, r0);
    emit-constant-ref(be, data, high?: #t);
    // perform opcode using lo part
    emit-d-high(be, opcode, dest-reg, rega);
    emit-constant-ref(be, data, low?: #t);
  else
    emit-d(be, opcode, dest-reg, rega, data);
  end;
  unless (dest == dest-reg)
    move-register-to-memory(be, dest, dest-reg)
  end
end method emit-d-via-tmp1-dest1;

define method emit-d-via-tmp1-dest2
    (be :: <powerpc-back-end>, opcode :: <opcode>, dest, rega, data)
  let dest-reg = dst-place(dest, reg--tmp1);
  emit-d(be, opcode, rega, dest-reg, data);
  unless (dest == dest-reg)
    move-register-to-memory(be, dest, dest-reg)
  end
end method emit-d-via-tmp1-dest2;


define method split-32-to-signed-16 (x)
  let low-bits = logand(x, #xffff);
  let lo-sum =
    if (low-bits > #x7fff) low-bits - #x10000 else low-bits end;
  values(lo-sum, ash(x - lo-sum, -16))
end method split-32-to-signed-16;

// This instruction is *required* to clear XER[0-3]
// otherwise XER[SO] doesn't get cleared by subsequent instructions
// that don't overflow leading to the false impression that they
// overflowed! So machine's auto copying of XER[SO] in CR0[3] doesn't
// appear helpful since we can't rely on it!
// Might as well test moved XER[OV] bit if we are going to
// have to do the move anyway; this is set & cleared by instructions
// that enable XER[OV]
// Use CR2 field to be on the safe side since CR0 is auto used by integer
// instructions and CR1 auto used by floating-point instructions

define method branch-on-overflow(be :: <powerpc-back-end>, tag :: <tag>) => ()
  // Bit 9,10 of d1 are 0
  emit-ppc(be, mcrxr-op, d1: #b01000);
  emit-branch-sdi(be, bov-cc, tag);
end method;

define method branch-no-overflow(be :: <powerpc-back-end>, tag :: <tag>) => ()
  emit-ppc(be, mcrxr-op, d1: #b01000);
  emit-branch-sdi(be, bno-cc, tag);
end method;

define method branch-no-overflow(be :: <powerpc-back-end>, offset :: <integer>) => ()
  emit-ppc(be, mcrxr-op, d1: #b01000);
  emit-branch(be, bc-op, bno-cc, offset);
end method;

/*
define method split-instructions?
     (backend :: <powerpc-back-end>) => (b :: <boolean>)
  #t;
end method;
*/

define method code-item-increment 
     (backend :: <powerpc-back-end>) => (i :: <integer>)
  2;
end method;

define method labelled-constant-increment 
    (backend :: <powerpc-back-end>) => (res :: <integer>)
  2;
end method;

/*
define method offset-of-first-byte-in-word
     (backend :: <powerpc-back-end>) => (i :: <integer>)
  3;
end method;

define method c-stack-alignment
     (backend :: <powerpc-back-end>) => (b)
  #f;
end method;

define method foreign-aligned
     (backend :: <powerpc-back-end>) => (b)
  #t;
end method;
*/


define method spill-base-and-offset
    (backend :: <powerpc-back-end>, spill :: <spill>)
    => (base :: <real-register>, offset :: <integer>)
  let arg-spill :: <boolean> = arg-spill?(spill);
  let offset :: <integer> = 
    if (arg-spill)
      arg-offset(backend, spill)
    else
      signed-frame-pointer-offset(backend, spill);
    end if;
  let with-frame = backend.variables.with-stack;
  if (arg-spill & ~ with-frame)
    values(backend.registers.reg-stack, offset);
  else
    values(backend.registers.reg-frame, offset);
  end if;
end method;

define method real-register-debug-info-enumeration
    (backend :: <powerpc-back-end>, register :: <integer-register>)
    => (enumeration :: <integer>)
  register.real-register-number;
end method;


define method real-register-debug-info-enumeration
    (backend :: <powerpc-back-end>, register :: <float-register>)
    => (enumeration :: <integer>)
  register.real-register-number + 32;
end method;


define method real-register-from-debug-info-enumeration
    (backend :: <powerpc-back-end>, enumeration :: <integer>)
    => (reg :: <real-register>)

  local method report-unknown ()
          harp-error("Unknown register enumeration %d", enumeration);
        end method;

  if (enumeration < 32)
    // must be a general register
    let reg-num = enumeration;
    let key = find-key(powerpc-real-registers,
                       method (x) x.real-register-number == reg-num end);
    if (key)
      powerpc-real-registers[key];
    else report-unknown();
    end if;
  elseif (enumeration < 64)
    // must be a general register
    let reg-num = enumeration - 32;
    let key = find-key(powerpc-real-registers,
                       method (x) x.real-register-number == reg-num end);
    if (key)
      powerpc-real-registers[key];
    else report-unknown();
    end if;
  else
    // we don't expect to ever see these
    report-unknown();
  end if;
end method;
