module:    idvm-harp
Synopsis:  The Idvm HARP instruction set
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The idvm instruction set


define uuu spread-function call-n (backend, fn, ins)
  fn(backend, uze(1), uze(2));
end;

define duu spread-function load-vm-arg (backend, fn, ins)
  fn(backend, def(1), uze(2));
end;

define instruction-set <idvm-instruction-set>
     (<core-instruction-set>, <idvm-back-end>)
  
  create idvm-instructions, inheriting: default-core-instructions;

       op return-any-values, spread: spread-none;
       op return-value, spread: spread-u;
       op vm-call-0, vm-jmp-0, vm-call-0-returning, spread: spread-u;
       op vm-call-1, vm-jmp-1, vm-call-1-returning, spread: spread-uu;
       op vm-call-2, vm-jmp-2, vm-call-2-returning, spread: spread-uuu;
       op vm-call-n, vm-jmp-n, vm-call-n-returning, spread: spread-call-n;

  u    op vm-returning;

  tu   op vm-unwind-protect, vm-unwind-protect-returning;
  td   op vm-bind-exit, vm-bind-exit-returning;
  tu   op vm-mv-bind, vm-mv-bind-rest;
       op vm-mv-bind-finished, spread: spread-u;
       op store-vm-arg-n, spread: spread-uu, is-move: #t;
       op load-vm-arg-n, spread: spread-load-vm-arg, is-move: #t;

  // vm instructions for closures - nosa 6/1/95
       op make-closure, spread: spread-duu;
       op make-closure-with-specs, spread: spread-duuu;
  du   op move-env, ld-vc, ld-env, make-value-cell;
  uu   op st-vc, st-env;

end;


// make-closure is really a DUX instruction - but the templates
// don't want to get the arbitrary agruments. We use a DUU spread.

define instruction-function make-closure
    (backend :: <idvm-back-end>, dest, copy, #rest closed-vars, 
     #key op :: <op>)
  let var-vector = as(<simple-object-vector>, closed-vars);
  output-instruction(backend, op, #f, dest, copy, var-vector);
end instruction-function;

define instruction-function make-closure-with-specs
    (backend :: <idvm-back-end>, dest, copy, specs, #rest closed-vars, 
     #key op :: <op>)
  let var-vector = as(<simple-object-vector>, closed-vars);
  output-instruction(backend, op, #f, dest, copy, specs, var-vector);
end instruction-function;


// vm-mv-bind-finished is a dummy HARP instruction which just serves the 
// purpose of defining the argument registers which are bound by
// the return of vm-mv-bind

define instruction-function vm-mv-bind-finished
    (backend :: <idvm-back-end>, n-args :: <integer>, #key op :: <op>)
  // treat this as a d*u instruction - so the vector
  // of implicit argument uses can be outputted as a vector
  // which is followed by n-args
  let res = backend.registers.reg-result-out;
  let arg-regs = backend.variables.argument-registers;
  let nargs-in-locs = n-args - 1; // one arg is in res
  let arg-vec = map-as(<simple-object-vector>, 
                       curry(vm-arg-register, backend),
                       range(size: nargs-in-locs));
  output-instruction(backend, op, #f, arg-vec, n-args);
end method;



// For store-vm-arg-n we convert a DU op into a DUU op by inserting
// a vm-arg register from the cache as the first U. We use a hand-crafted
// spread to avoid making this visible in the template.

define instruction-function load-vm-arg-n
    (backend :: <idvm-back-end>, def, index, #key op)
  let reg-use = vm-arg-register(backend, index);
  output-instruction(backend, op, #f, def, reg-use, index);
end;


// For store-vm-arg-n we convert a UU op into a DUU op by using
// a vm-arg register from the cache. We avoid making this visible 
// in the spread, because the template can get the D value from
// the cache too.

define instruction-function store-vm-arg-n 
    (backend :: <idvm-back-end>, arg, index, #key op)
  let dest = vm-arg-register(backend, index);
  output-instruction(backend, op, #f, dest, arg, index);
end;


define instruction-function return-value
    (backend :: <idvm-back-end>, value-to-return, #key op)
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #f, #f, value-to-return);
  make-current-bb(backend);
end;


define instruction-function return-any-values
    (backend :: <idvm-back-end>, #key op)
  make-fall-thru-bb(backend);
  output-instruction(backend, op);
  make-current-bb(backend);
end;



define instruction-function vm-call-0
    (backend :: <idvm-back-end>, dest, #key op)
  let res = backend.registers.reg-result-out;
  output-instruction(backend, op, #f, res, dest);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-0-returning
    (backend :: <idvm-back-end>, dest, #key op)
  let res = backend.registers.reg-result-out;
  output-instruction(backend, op, #f, res, dest);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-1
    (backend :: <idvm-back-end>, dest, arg1, #key op)
  let res = backend.registers.reg-result-out;
  output-instruction(backend, op, #f, res, dest, arg1);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-1-returning
    (backend :: <idvm-back-end>, dest, arg1, #key op)
  let res = backend.registers.reg-result-out;
  output-instruction(backend, op, #f, res, dest, arg1);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-2
    (backend :: <idvm-back-end>, dest, arg1, arg2, #key op)
  let res = backend.registers.reg-result-out;
  output-instruction(backend, op, #f, res, dest, arg1, arg2);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-2-returning
    (backend :: <idvm-back-end>, dest, arg1, arg2, #key op)
  let res = backend.registers.reg-result-out;
  output-instruction(backend, op, #f, res, dest, arg1, arg2);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-n
    (backend :: <idvm-back-end>, dest, n-args, #key op)
  output-call-n-instruction(backend, op, dest, n-args);
  make-fall-thru-bb(backend);
end;

define instruction-function vm-call-n-returning
    (backend :: <idvm-back-end>, dest, n-args, #key op)
  output-call-n-instruction(backend, op, dest, n-args);
  make-fall-thru-bb(backend);
end;


define method output-call-n-instruction
    (backend :: <idvm-back-end>, op :: <op>, dest, n-args :: <integer>) => ()
  // treat this as a u* instruction - so the vector
  // of implicit argument uses can be outputted as a vector
  // which is preceeded with n-args
  let res = backend.registers.reg-result-out;
  let arg-regs = backend.variables.argument-registers;
  let arg-vec = make(<vector>, size: (1 + n-args), fill: n-args);
  for (inx from 0 below n-args)
    arg-vec[inx + 1] := arg-regs[inx];
  end for; 
  output-instruction(backend, op, #f, res, dest, arg-vec);
end method;

define instruction-function vm-jmp-0
    (backend :: <idvm-back-end>, dest, #key op)
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #f, #f, dest);
  make-current-bb(backend);
end;

define instruction-function vm-jmp-1
    (backend :: <idvm-back-end>, dest, arg1, #key op)
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #f, #f, dest, arg1);
  make-current-bb(backend);
end;

define instruction-function vm-jmp-2
    (backend :: <idvm-back-end>, dest, arg1, arg2, #key op)
  make-fall-thru-bb(backend);
  output-instruction(backend, op, #f, #f, dest, arg1, arg2);
  make-current-bb(backend);
end;

define instruction-function vm-jmp-n
    (backend :: <idvm-back-end>, dest, n-args, #key op)
  make-fall-thru-bb(backend);
  output-call-n-instruction(backend, op, dest, n-args);
  make-current-bb(backend);
end;


with-ops-in idvm-instructions (vm-call-0, vm-call-1, vm-call-2, vm-call-n,
                               vm-call-0-returning, vm-call-1-returning, 
                               vm-call-2-returning, vm-call-n-returning)
  destroys-fn :=
  method (backend :: <idvm-back-end>, ins :: <integer>)
    idvm-non-preserved;
  end;
end with-ops-in;
