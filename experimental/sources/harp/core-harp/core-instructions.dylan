module:    harp-instructions
Synopsis:  The definition of the core HARP instruction set.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/////// Define the basic HARP instruction set

/// The core instruction set has no stack operations, and is common to 
/// all back ends, including the VM.


// For forward-referencing reasons, the spread for SCL gets defined first.
// The explanation follows later.

define uu spread-function scl (backend, op, fn, ins)
  let use1 :: <pair> = uu-uze(1);
  let use2 :: <simple-object-vector> = uu-uze(2);
  fn(backend, op, use1.head, use2, use1.tail );
end;



define instruction-set <core-instruction-set>
     (<abstract-instruction-set>, <harp-back-end>)
  create default-core-instructions, 
      inheriting: default-abstract-instruction-set;

  du   op move, eliminatable: #t, is-move: #t;
  duu  op add, sub, eliminatable: #t;
  tuu  op beq, bge, bgt, ble, blt, bne;
       op bra, spread: spread-t;
       op rem, spread: spread-t, is-rem: #t;
       op load-stack-arg-n, 
          spread: spread-tdu, eliminatable: #t, stack-dependent: #t;
  // SCL takes a locator object, and a #rest of named registers
       op scl, is-scl: #t, keep-virtual: #t, spread: spread-scl;
       op strong-scl, keep-virtual: #t, spread: spread-uu;
end;



with-ops-in default-core-instructions (rem)
  code-gen-fn := method (backend, op, comment)
                   ignore(op); ignore(comment);
                   let vars = backend.variables;
                   vars.current-instruction-can-be-moved :=
                     vars.prev-instruction-can-be-moved;
	  	 #f;
                 end;
end with-ops-in;


mark-reverse-ops (default-core-instructions)
  beq <-> bne;
  bge <-> blt;
  bgt <-> ble;
end mark-reverse-ops;


define instruction-function load-stack-arg-n 
    (backend, def, uze :: <integer>, #key op)
  // first record the number of atack args in use
  let vars = backend.variables;
  let known = vars.arg-spill-count;
  let this-count = uze + 1;
  if (this-count > known) vars.arg-spill-count := this-count end;
  // now mark the colour, and output the instruction
  def.virtual-register-colour := uze;
  output-instruction(backend, op, #t, def, uze);
end;

define instruction-function rem (backend, x, #key op) 
  output-instruction(backend, op, x);
end;


define instruction-function bra (backend, tag :: <tag>)
  output-unconditional-branch-instruction(backend, tag);
end;


define instruction-function taag (backend, tag :: <tag>)
  taag-out(backend, tag);
end;

define instruction-function tag (backend, tag :: <tag>)
  taag-out(backend, tag);
end;


define instruction-function immediate-literal (backend, lit :: <integer>)
  lit;
end;


define method move-reg
     (backend :: <harp-back-end>,
      toreg :: <integer-virtual-register>, 
      fromreg :: <integer-virtual-register>) => ()
  call-instruction(move, backend, toreg, fromreg);
end;




/// For the SCL instruction, the actual parameters are seen by HARP's
/// liveness analysis, so they are also strong. The use(1) parameter is
/// actually a pair. The head is the locator parameter; the tail is a vector
/// of the named registers which HARP calculated  were actually live at that 
/// point. This is initialized to the empty vector.


define instruction-function scl (backend, locator, live-vars :: <simple-object-vector>, #key op)
  output-instruction(backend, op, #f, #f, pair(locator, #[]), 
                     live-vars);
end;

define instruction-function strong-scl (backend, locator, live-vars :: <simple-object-vector>, #key op)
  output-instruction(backend, op, #f, #f, locator, 
                     live-vars);
end;
