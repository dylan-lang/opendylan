module:    native-instructions
Synopsis:  Outputters for specials of the native HARP instruction set.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/////// Define the special outputters for the basic HARP instruction set.
/////// The simple ones are defined along with the instructions in
/////// instruction-set.dyl


/// These are the funnies. ie those harp ops which take a variable number of
/// arguments and/or which define and/or use registers not explicitly given in
/// the instruction call


define instruction-function adjust-stack
    (backend :: <harp-native-back-end>, bytes-to-insert, #key op)
  output-u(backend, op, bytes-to-insert);
  backend.variables.current-bb.bb-needs-leaf := #t;
  make-fall-thru-bb(backend);
end;


// LOAD-COUNT-ADJUSTING-STACK is a combination of ADJUST-STACK and 
// LOAD-STACK-ARG-N. Conceptually, the stack is first adjusted, then 
// the count is updated on the stack, then the count virtual register
// is loaded. This is merged into a single instruction to simplify the 
// need to calculate stack indices in the prolog.

define instruction-function load-count-adjusting-stack
    (backend :: <harp-native-back-end>, 
     count-reg, bytes-to-insert, number-of-count-as-stack-arg,
     #key op)
  count-reg.virtual-register-colour := number-of-count-as-stack-arg;
  output-instruction(backend, op, #t, count-reg, bytes-to-insert, 
                     number-of-count-as-stack-arg);
  backend.variables.current-bb.bb-needs-leaf := #t;
  make-fall-thru-bb(backend);
end;

define instruction-function remove-optionals
    (backend :: <harp-native-back-end>,
     number-of-count-as-stack-arg, pointer-into-stack, #key op)
  make-fall-thru-bb(backend);
  output-uu(backend, op, number-of-count-as-stack-arg, pointer-into-stack);
end;


// The CALL family of instructions all take parameters:
//   nregs    -- the number of registers used in the calling convention
// And also the following keyword parameters for determining implicit uses
// of registers involved in the calling convention.
//   mlist       -- a boolean indicating whether mlist is implicitly used
//   function    -- a boolean indicating whether function is implicitly used
//   arg-count   -- a boolean indicating whether arg-count is implicitly used
//   nlx-tags    -- a list of tags which are potential NLX destinations in the call
// The indirect versions of the CALL instrucrtions default the implicit uses of
// function and arg-count to true. All other defaults are #f.


define instruction-function call
    (backend :: <harp-native-back-end>,
     dest, nregs :: <integer>, 
     #key op, mlist = #f, function = #f, arg-count = #f, nlx-tags = #())
  let res = backend.registers.reg-result-out;
  let uses = encode-raw-implicit-uses(backend, nregs, mlist, function, arg-count);
  output-instruction(backend, op, #F, res, dest, uses);
  add-destination-tags-to-bb(backend, nlx-tags);
  make-fall-thru-bb(backend);
end;

define instruction-function call-alien
    (backend :: <harp-native-back-end>,
     dest, nregs :: <integer>, 
     #key op, mlist = #f, function = #f, arg-count = #f, nlx-tags = #())
  let res = backend.registers.reg-result-out;
  let uses = encode-raw-implicit-uses(backend, nregs, mlist, function, arg-count);
  output-instruction(backend, op, #F, res, dest, uses);
  add-destination-tags-to-bb(backend, nlx-tags);  
  make-fall-thru-bb(backend);
end;


define instruction-function call-indirect
    (backend :: <harp-native-back-end>,
     dest, offset, nregs :: <integer>, 
     #key op, mlist = #f, function = #t, arg-count = #t, nlx-tags = #())
  let res = backend.registers.reg-result-out;
  let uses = encode-xep-implicit-uses(backend, nregs, mlist, function, arg-count);
  output-instruction(backend, op, #F, res, dest, offset, uses);
  add-destination-tags-to-bb(backend, nlx-tags);
  make-fall-thru-bb(backend);
end;


// The JMP family of instructions all take parameters:
//   nregs    -- the number of registers used in the calling convention
// And also the following keyword parameters for determining implicit uses
// of registers involved in the calling convention.
//   mlist       -- a boolean indicating whether mlist is implicitly used
//   function    -- a boolean indicating whether function is implicitly used
//   arg-count   -- a boolean indicating whether arg-count is implicitly used
// The indirect versions of the CALL instrucrtions default the implicit uses of
// function and arg-count to true. All other defaults are #f.
// The following keyword parameters are also accepted ...
//   return-address-shift -- the number of bytes to pop below the ret addr
//   optionals-marker     -- the location of the count register to update 
//                           for the ret-addr-shift. This may be given either
//                           as an arg-spill (in which case, it is updated, 
//                           and the count is adjusted), or as an integer 
//                           arg-spill index (in which case the optionals are
//                           dropped), or as #F (if there are no optionals).
//   pointer-into-stack   -- The location of a register which points into the
//                           stack at a position which will be affected by 
//                           the removal of the optionals. In this case, the
//                           instruction arranges to update the pointer. The
//                           location may either be given as an integer 
//                           arg-spill index or as a register (or #f to show 
//                           that nothing needs to be done).

define instruction-function jmp
    (backend :: <harp-native-back-end>,
     dest, nregs :: <integer>, 
     #key op, return-address-shift = 0, optionals-marker, pointer-into-stack,
          mlist = #f, function = #f, arg-count = #f)
  let uses = encode-raw-implicit-uses(backend, nregs, mlist, function, arg-count);
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #F, #F, dest, uses, 
                     return-address-shift, optionals-marker, 
                     pointer-into-stack);
  make-current-bb(backend);
end;

define instruction-function jmp-alien
    (backend :: <harp-native-back-end>, 
     dest, nregs :: <integer>,
     #key op, return-address-shift = 0, optionals-marker, pointer-into-stack,
          mlist = #f, function = #f, arg-count = #f)
  let uses = encode-raw-implicit-uses(backend, nregs, mlist, function, arg-count);
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #F, #F, dest, uses, 
                     return-address-shift, optionals-marker, 
                     pointer-into-stack);
  make-current-bb(backend);
end;


define instruction-function jmp-indirect
    (backend :: <harp-native-back-end>,
     dest, offset, nregs :: <integer>,
     #key op, return-address-shift = 0, optionals-marker, pointer-into-stack,
          mlist = #f, function = #t, arg-count = #t)
  let uses = encode-xep-implicit-uses(backend, nregs, mlist, function, arg-count);
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #F, #F, dest, offset, uses, 
                     return-address-shift, optionals-marker, 
                     pointer-into-stack); 
  make-current-bb(backend);
end;


// We encode the implicit uses depending for the expected defaults for the type
// of call instruction. A default instruction would be encoded with a number. 
// A non-default encoding is a vector with the number as the first element, and a
// symbol for each of the implicit calling-convention registers.

define method encode-xep-implicit-uses
    (backend :: <harp-native-back-end>, nregs :: <integer>, mlist, function, arg-count) 
    => (result)
  if (function & arg-count & ~ mlist)
    nregs
  else
    encode-implicit-uses-as-vector(backend, nregs, mlist, function, arg-count);
  end if;
end method;


define method encode-raw-implicit-uses
    (backend :: <harp-native-back-end>, nregs :: <integer>, mlist, function, arg-count) 
    => (result)
  if (function | arg-count | mlist)
    encode-implicit-uses-as-vector(backend, nregs, mlist, function, arg-count);
  else
    nregs
  end if;
end method;


define method encode-implicit-uses-as-vector
    (backend :: <harp-native-back-end>, nregs :: <integer>, mlist, function, arg-count) 
    => (result :: <simple-object-vector>)
  let specials = make(<stretchy-vector>);
  if (mlist)     add!(specials, #"mlist")     end;
  if (function)  add!(specials, #"function")  end;
  if (arg-count) add!(specials, #"arg-count") end;
  apply(vector, nregs, specials);
end method;


define macro encoded-mask
  { encoded-mask(?use:expression, ?accessor:name) }
    =>
  {
   if (member?(?use, ?=vec)) 
     ?=regs.?accessor.real-register-mask
   else 
     0
   end;
   }
end macro;

define method implicit-uses-from-encoding
    (backend :: <harp-native-back-end>, vec :: <simple-object-vector>)
    => (i :: <integer>)
  let regs = backend.registers;

  encoded-mask(#"mlist", reg-mlist) 
    + encoded-mask(#"function", reg-function) 
    + encoded-mask(#"arg-count", reg-arg-count);
end method;


// implicit-argument-uses gives the implicit arguments of the XEP calling
// convention. This includes arg-count.

define method implicit-argument-uses
    (backend :: <harp-native-back-end>, nregs :: <integer>) 
    => (i :: <integer>)
  let regs = backend.registers;
  let arg-count-mask :: <integer> = regs.reg-arg-count.real-register-mask;
  let function-mask  :: <integer> = regs.reg-function.real-register-mask;
  let implicit-mask = arg-count-mask + function-mask;
  if (nregs < 0)
    implicit-mask - nregs;
  else
    implicit-mask + regs.reg-arg-masks-out[nregs];
  end if
end;

define method implicit-argument-uses
    (backend :: <harp-native-back-end>, encoded-uses :: <simple-object-vector>) 
    => (i :: <integer>)
  let regs = backend.registers;
  let nregs :: <integer> = encoded-uses[0];
  let implicit-mask = implicit-uses-from-encoding(backend, encoded-uses);
  if (nregs < 0)
    implicit-mask - nregs;
  else
    implicit-mask + regs.reg-arg-masks-out[nregs];
  end if
end;


// implicit-iep-argument-uses gives the implicit arguments of the IEP calling
// convention. This does not include arg-count.

define method implicit-iep-argument-uses
    (backend :: <harp-native-back-end>, nregs :: <integer>) 
    => (i :: <integer>)
  let regs = backend.registers;
  if (nregs < 0)
    - nregs;
  else
    regs.reg-arg-masks-out[nregs];
  end if
end;

define method implicit-iep-argument-uses
    (backend :: <harp-native-back-end>, encoded-uses :: <simple-object-vector>) 
    => (i :: <integer>)
  let regs = backend.registers;
  let nregs :: <integer> = encoded-uses[0];
  let implicit-mask = implicit-uses-from-encoding(backend, encoded-uses);
  if (nregs < 0)
    implicit-mask - nregs;
  else
    implicit-mask + regs.reg-arg-masks-out[nregs];
  end if
end;


// implicit-c-argument-uses gives the implicit arguments of the C  calling
// convention. This does not include arg-count.

define method implicit-c-argument-uses
    (backend :: <harp-native-back-end>, nregs :: <integer>) 
    => (i :: <integer>)
  let regs = backend.registers;
  if (nregs < 0)
    - nregs;
  else
    regs.reg-c-arg-masks-out[nregs];
  end if
end;

define method implicit-c-argument-uses
    (backend :: <harp-native-back-end>, encoded-uses :: <simple-object-vector>) 
    => (i :: <integer>)
  let regs = backend.registers;
  let nregs :: <integer> = encoded-uses[0];
  let implicit-mask = implicit-uses-from-encoding(backend, encoded-uses);
  if (nregs < 0)
    implicit-mask - nregs;
  else
    implicit-mask + regs.reg-c-arg-masks-out[nregs];
  end if
end;



with-ops-in default-instructions (call)
  implicit-uses :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    let sv-ins = backend.variables.sv-instructions;
    with-uu (sv-ins at ins)
      implicit-iep-argument-uses(backend, uu-uze(2));
    end with-uu;
  end method;
end with-ops-in;

with-ops-in default-instructions (call-indirect)
  implicit-uses :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    let sv-ins = backend.variables.sv-instructions;
    with-uuu (sv-ins at ins)
      implicit-argument-uses(backend, uuu-uze(3));
    end with-uuu;
  end method;
end with-ops-in;

with-ops-in default-instructions (call-alien)
  implicit-uses :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    let sv-ins = backend.variables.sv-instructions;
    with-uu (sv-ins at ins)
      implicit-c-argument-uses(backend, uu-uze(2));
    end with-uu;
  end method;
end with-ops-in;

with-ops-in default-instructions (jmp)
  implicit-uses :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    let sv-ins = backend.variables.sv-instructions;
    with-uuuu (sv-ins at ins)
      implicit-iep-argument-uses(backend, uuuu-uze(2));
    end with-uuuu;
  end method;
end with-ops-in;

with-ops-in default-instructions (jmp-indirect)
  implicit-uses :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    let sv-ins = backend.variables.sv-instructions;
    with-uuuuu (sv-ins at ins)
      implicit-argument-uses(backend, uuuuu-uze(3));
    end with-uuuuu;
  end method;
end with-ops-in;

with-ops-in default-instructions (jmp-alien)
  implicit-uses :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    let sv-ins = backend.variables.sv-instructions;
    with-uuuu (sv-ins at ins)
      implicit-c-argument-uses(backend, uuuu-uze(2));
    end with-uuuu;
  end method;
end with-ops-in;


with-ops-in default-instructions (call, call-indirect)
  destroys-fn :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    backend.registers.not-preserved;
  end;
end with-ops-in;

with-ops-in default-instructions (call-alien)
  destroys-fn :=
  method (backend :: <harp-native-back-end>, ins :: <integer>)
    backend.registers.c-not-preserved;
  end;
end with-ops-in;


define instruction-function rts-and-drop 
    (backend :: <harp-native-back-end>, args-to-drop, #key op)
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #f, #f, args-to-drop);
  make-current-bb(backend);
end;


define instruction-function rts
    (backend :: <harp-native-back-end>, #key op)
  make-fall-thru-bb(backend);
  output-none(backend, op);
  make-current-bb(backend);
end;


/// end-cleanup is really an RTS instruction - but leafcase 
/// analysis doesn't want to see it as the end of the line. It
/// therefore takes a tag which should correspond to the return 
/// point of the rts.  It also takes a #rest of implicit definitions. 


define instruction-function end-cleanup
    (backend :: <harp-native-back-end>, tag :: <tag>, #rest defs, #key op)
  let implicit-defs = as(<simple-object-vector>, defs);
  output-td(backend, op, tag, implicit-defs);
  make-current-bb(backend);
end;


/// control-flow-link is really a NOP from the point of view of code 
/// generation. It exists to indicate a non-obvious control-flow path to
/// HARP. This is used at the join between protected code and cleanup code for
/// an unwind-protect.
///
/// The same is true for the FORCE-D and FORCE-U instructions. These
/// exist purely to effect the register usage information of HARP to keep
/// registers alive at appropriate times.
///
/// FORCE-D exists primarily to avoid unwarranted warnings from harp about
/// registers being live on entry to the cleanup code of an unwind protect.
/// If it's known that the destination of the end-cleanup
/// expects those registers to be defined, and yet there is the
/// possibility that there might be control flow paths to the end of 
/// the cleanup which don't include the definitions, and yet which
/// cannot actually be taken in practice (e.g. because of a NLX), 
/// then the registers may be forcibly defined at the start of the cleanup.
///
/// FORCE-U exists primarily to force HARP to keep a virtual register live
/// to ensure that it's contents will be pinned by the GC at least until
/// the force-u instruction.

with-ops-in default-instructions (control-flow-link, force-d, force-u)
  // Yes, this really is a NOP
  code-gen-fn := method (backend, op, comment) #f end;
end with-ops-in;


/// the t-pop and t-push duplicate their sp operand, to make it both used
/// and defined. I hope this does not cause problems

define instruction-function t-pop
    (backend :: <harp-native-back-end>, dest, sp, #key op)
  output-ddu(backend, op, sp, dest, sp);
end;


define instruction-function t-push 
    (backend :: <harp-native-back-end>, dest, sp, #key op) 
  output-duu(backend, op, sp, dest, sp);
end;



/// No need to worry about making sure that the pea dest block will get
/// code-gen'd as the active catcher mechanism should deal with this.


define method load-some-address
   (backend :: <harp-native-back-end>, 
    op :: <op>, dest, tag :: <tag>, offset)
  output-instruction(backend, op, tag, dest, offset);
  pushnew!(find-bb(backend, tag), backend.variables.current-bb.bb-other-set);
  // this forces it to end a bb which is important for leaf case so that it can
  // redirect next-set pointers correctly on insertion of stack entry/exit code
  // (cim 14/4/89)
  conditional-branch-windup(backend, tag);
end;

define instruction-function pea 
    (backend :: <harp-native-back-end>, tag :: <tag>, offset, #key op)
  load-some-address(backend, op, #f, tag, offset);
end;

define pea instruction-function pea0
    (backend :: <harp-native-back-end>, tag :: <tag>, #key op)
  load-some-address(backend, op, #f, tag, 0);
end;

define instruction-function lea
    (backend :: <harp-native-back-end>, dest, tag :: <tag>, offset,
     #key op)
  load-some-address(backend, op, dest, tag, offset);
end;

define lea instruction-function lea0
    (backend :: <harp-native-back-end>, dest, tag :: <tag>, #key op)
  load-some-address(backend, op, dest, tag, 0);
end;

define instruction-function load-nlx-address
    (backend :: <harp-native-back-end>, dest, tag :: <tag>, #key op)
  load-some-address(backend, op, dest, tag, 0);
end;

define instruction-function load-address-of-stack-arg
    (backend :: <harp-native-back-end>, def, #key op)
  output-instruction(backend, op, #t, def);
end;

define instruction-function load-address-of-stack-arg-n
    (backend :: <harp-native-back-end>, def, uze, #key op)
  output-instruction(backend, op, #t, def, uze);
end;

// Factors in preserved callee-safe register stack allocation in computing
// native offsets for stack operation.

define instruction-function load-frame-offset
    (backend :: <harp-native-back-end>, def, uze, #key op)
  output-instruction(backend, op, #t, def, uze);
end;



define method output-load-stack-arg
    (backend :: <harp-native-back-end>, 
     op :: <op>, def, uze :: <integer>)
  // first record the number of atack args in use
  let vars = backend.variables;
  let known = vars.arg-spill-count;
  let this-count = uze + 1;
  if (this-count > known) vars.arg-spill-count := this-count end;
  // now mark the colour, and output the instruction
  def.virtual-register-colour := uze;
  output-instruction(backend, op, #t, def, uze);
end method;


define instruction-function load-stack-arg-half-signed
    (backend :: <harp-native-back-end>, def, uze :: <integer>, #key op)
  output-load-stack-arg(backend, op, def, uze);
end;

define instruction-function load-stack-arg-byte-signed
    (backend :: <harp-native-back-end>, def, uze :: <integer>, #key op)
  output-load-stack-arg(backend, op, def, uze);
end;

define instruction-function load-stack-arg-half-unsigned
    (backend :: <harp-native-back-end>, def, uze :: <integer>, #key op)
  output-load-stack-arg(backend, op, def, uze);
end;

define instruction-function load-stack-arg-byte-unsigned
    (backend :: <harp-native-back-end>, def, uze :: <integer>, #key op)
  output-load-stack-arg(backend, op, def, uze);
end;


with-ops-in default-instructions (preserve-registers-exit, rts-and-drop, rts)
  external-transfer := #t;
  does-jump := #t;
end with-ops-in;

with-ops-in default-instructions (rts-and-drop, rts)
  implicit-uses := 
  method (backend :: <harp-native-back-end>, ins)
    backend.registers.reg-result.real-register-mask;
  end method;
end with-ops-in;


with-ops-in default-instructions (addv, taddv, subv, tsubv, mulv, divv, addc, subc)
  clash-fn := default-overflow-function-clashes;
end with-ops-in;



with-ops-in default-instructions (mulxv, aslxv)
  clash-fn := default-double-overflow-function-clashes;
end with-ops-in;


mark-reverse-ops (default-instructions)
  bhi  <-> bls;
  bhs  <-> blo;
  bit  <-> nbit;
  band <-> bnand;
end mark-reverse-ops;




define method move-reg
     (backend :: <harp-native-back-end>,
      toreg :: <sfreg>, 
      fromreg :: <sfreg>) => ()
  call-instruction(fmove, backend, toreg, fromreg);
end;

define method move-reg
     (backend :: <harp-native-back-end>,
      toreg :: <dfreg>, 
      fromreg :: <dfreg>) => ()
  call-instruction(dmove, backend, toreg, fromreg);
end;



/// Compound instructions which just pretend to be OPs


define instruction-function floorx
    (be :: <harp-native-back-end>, quot, rem, dividend, divisor)
  with-harp (be)
    nreg temp1;
    
    let rrem = rem | temp1;
    call-instruction(rem, be, "FloorX instruction");
    call-instruction(truncatex, be, quot, rrem, dividend, divisor);
    op--floor-adjust(be, quot, rem, rrem, dividend, divisor);
    call-instruction(rem, be, "End of FloorX instruction");
  end with-harp;
end;


define instruction-function floorxx
    (be :: <harp-native-back-end>, quot, rem, low, high, divisor)
  with-harp (be)
    nreg temp1;
    
    let rrem = rem | temp1;
    call-instruction(rem, be, "FloorXX instruction");
    call-instruction(truncatexx, be, quot, rrem, low, high, divisor);
    op--floor-adjust(be, quot, rem, rrem, high, divisor);
    call-instruction(rem, be, "End of FloorXX instruction");
  end with-harp;
end;


define method op--floor-adjust
    (be :: <harp-native-back-end>, quot, rem, rrem, dividend, divisor)
  with-harp (be)
    nreg temp2;
    tag done, adjust;
    
    call-instruction(beq, be, done, rrem, 0);

    // If the remainder is not 0, then adjust if dividend
    // and divisor have the opposite sign
    call-instruction(eor, be, temp2, dividend, divisor);
    call-instruction(bge, be, done, temp2, 0);

    call-instruction(tag, be, adjust);
    call-instruction(add, be, rem, rrem, divisor);
    if (quot) call-instruction(sub, be, quot, quot, 1) end;

    call-instruction(tag, be, done);

  end with-harp;
end method;


define instruction-function ceilingx
    (be :: <harp-native-back-end>, quot, rem, dividend, divisor)
  with-harp (be)
    nreg temp1;
    
    let rrem = rem | temp1;
    call-instruction(rem, be, "CeilingX instruction");
    call-instruction(truncatex, be, quot, rrem, dividend, divisor);
    op--ceiling-adjust(be, quot, rem, rrem, dividend, divisor);
    call-instruction(rem, be, "End of CeilingX instruction");
  end with-harp;
end;


define instruction-function ceilingxx
    (be :: <harp-native-back-end>, quot, rem, low, high, divisor)
  with-harp (be)
    nreg temp1;
    
    let rrem = rem | temp1;
    call-instruction(rem, be, "CeilingXX instruction");
    call-instruction(truncatexx, be, quot, rrem, low, high, divisor);
    op--ceiling-adjust(be, quot, rem, rrem, high, divisor);
    call-instruction(rem, be, "End of CeilingXX instruction");
  end with-harp;
end;


define method op--ceiling-adjust
    (be :: <harp-native-back-end>, quot, rem, rrem, dividend, divisor)
  with-harp (be)
    nreg temp2;
    tag done, adjust;
    
    call-instruction(beq, be, done, rrem, 0);

    // If the remainder is not 0, then adjust if dividend
    // and divisor have the same sign
    call-instruction(eor, be, temp2, dividend, divisor);
    call-instruction(blt, be, done, temp2, 0);

    call-instruction(tag, be, adjust);
    call-instruction(sub, be, rem, rrem, divisor);
    if (quot) call-instruction(add, be, quot, quot, 1) end;

    call-instruction(tag, be, done);

  end with-harp;
end method;



define instruction-function roundx
    (be :: <harp-native-back-end>, quot, rem, dividend, divisor)
  with-harp (be)
    nreg temp1, temp2;
    
    let rrem = rem | temp1;
    let rquot = quot | temp2;
    call-instruction(rem, be, "RoundX instruction");
    call-instruction(truncatex, be, rquot, rrem, dividend, divisor);
    op--round-adjust(be, quot, rquot, rem, rrem, dividend, divisor);
    call-instruction(rem, be, "End of RoundX instruction");
  end with-harp;
end instruction-function;


define instruction-function roundxx
    (be :: <harp-native-back-end>, quot, rem, low, high, divisor)
  with-harp (be)
    nreg temp1, temp2;
    
    let rrem = rem | temp1;
    let rquot = quot | temp2;
    call-instruction(rem, be, "RoundXX instruction");
    call-instruction(truncatexx, be, rquot, rrem, low, high, divisor);
    op--round-adjust(be, quot, rquot, rem, rrem, high, divisor);
    call-instruction(rem, be, "End of RoundXX instruction");
  end with-harp;
end instruction-function;


define method op--round-adjust
    (be :: <harp-native-back-end>, quot, rquot, rem, rrem, dividend, divisor)
  with-harp (be)
    nreg odd, threshold, mthreshold;
    tag done, adjust-up, adjust-down, t1, case1, case2, test-case2;
    
    // threshold := abs(divisor) / 2
    call-instruction(move, be, threshold, divisor);
    call-instruction(bge, be, t1, threshold, 0);
    call-instruction(sub, be, threshold, 0, divisor);
    call-instruction(tag, be, t1);
    call-instruction(asr, be, threshold, threshold, 1);

    // odd := odd?(quot)
    call-instruction(and, be, odd, rquot, 1);

    // test for case 1: rem > threshold | (rem = threshold & odd)
    call-instruction(bgt, be, case1, rrem, threshold);
    call-instruction(bne, be, test-case2, rrem, threshold);
    call-instruction(beq, be, test-case2, odd, 0);
    call-instruction(tag, be, case1);
    call-instruction(blt, be, adjust-down, divisor, 0);
    call-instruction(bra, be, adjust-up);

    // test for case 2: rem < -threshold | (rem = -threshold & odd)
    call-instruction(tag, be, test-case2);
    call-instruction(sub, be, mthreshold, 0, threshold);
    call-instruction(blt, be, case2, rrem, mthreshold);
    call-instruction(bne, be, done, rrem, mthreshold);
    call-instruction(beq, be, done, odd, 0);
    call-instruction(tag, be, case2);
    call-instruction(bge, be, adjust-down, divisor, 0);
    call-instruction(bra, be, adjust-up);

    call-instruction(tag, be, adjust-up);
    call-instruction(sub, be, rem, rrem, divisor);
    call-instruction(add, be, quot, rquot, 1);
    call-instruction(bra, be, done);

    call-instruction(tag, be, adjust-down);
    call-instruction(add, be, rem, rrem, divisor);
    call-instruction(sub, be, quot, rquot, 1);

    call-instruction(tag, be, done);

  end with-harp;
end;

