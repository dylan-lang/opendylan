module:    idvm-harp
Synopsis:  Emitter functions for the Idvm backend.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Basic utilities to support the IDVM backend.


/// Predicates for templates:

define method env/spill-ref (r)
  env-ref(r) | spill-ref(r);
end method;


/// Separation of the locals vector:
///
/// This is organized as follows:
/// | env | arg-spills | arg-area | normal spills |
/// |  0  |  1  | ....


define method local-index (be :: <idvm-back-end>, loc == env) => (r :: <integer>)
  0;
end method;

define method local-index (be :: <idvm-back-end>, loc :: <spill>) => (r :: <integer>)
  if (arg-spill?(loc))
    be.arg-spill-start-index + loc.spill-offset.arg-spill-offset-to-arg-number;
  else
    be.arg-area-start-index + loc.spill-offset;
  end if;
end method;


define method arg-spill-start-index (be :: <idvm-back-end>) => (r :: <integer>)
  1;  // follows env
end method;


define method arg-area-start-index (be :: <idvm-back-end>) => (r :: <integer>)
  be.arg-spill-start-index + be.variables.arg-spill-count;
end method;


define method locals-vector-size (be :: <idvm-back-end>) => (r :: <integer>)
  be.arg-area-start-index +  be.variables.vreg-state.next-gc-spill;
end method;



/// Support for VM args

define method vm-arg-offset-to-arg-number
     (spill-offset :: <integer>) => (r :: <integer>)
  -1  - spill-offset;
end;

define method arg-number-to-vm-arg-offset
     (arg-num :: <integer>) => (r :: <integer>)
  -1  - arg-num;
end;


// vm-arg-register preserves a cache of vm-arg registers, which
// are appropriately coloured.

define method vm-arg-register 
    (backend :: <idvm-back-end>, index) => <greg>;
  let arg-regs :: <stretchy-vector> = backend.variables.argument-registers;
  let arg-size = arg-regs.size;
  if (arg-size <= index)
    for (i from arg-size to index)
      let new-reg = make-g-register(backend);
      new-reg.virtual-register-colour := i.arg-number-to-vm-arg-offset;
      add!(arg-regs, new-reg);
    end for;
  end if;
  arg-regs[index];
end method;


/// code to do register colouring of vm-args carefully:

define method vm-arg-colour? (colour :: <integer>) => (res :: <boolean>)
  colour < 0;
end method;

define method vm-arg-colour? (colour :: <object>) => (res :: <boolean>)
  #f;
end method;

// Ensure that vm-args get picked first at colouring time

define method sort-by-block-clashes
    (backend :: <idvm-back-end>, vrc-vect :: <simple-object-vector>)
    => (sorted-vect :: <simple-object-vector>)
  sort(vrc-vect, test: idvm-compare-block-clashes);
end method;

define method idvm-compare-block-clashes
    (x :: <virtual-register>, y :: <virtual-register>) => <boolean>;
  if (x.virtual-register-colour.vm-arg-colour?)
    #t;
  elseif (y.virtual-register-colour.vm-arg-colour?)
    #f;
  else
    x.virtual-register-clash-count > y.virtual-register-clash-count;
  end if;
end;



define method select-spill-by-colour 
    (backend :: <idvm-back-end>, vr :: <virtual-register>, 
     old-colour :: <integer>)
    => (spill :: <spill>)
  if (old-colour.vm-arg-colour?)
    select-vm-arg-spill(vr, old-colour);
  else
    make-arg-spill(vr, old-colour);
  end if;
end method;


define method select-vm-arg-spill 
    (vr :: <virtual-register>, colour :: <integer>) => (spill :: <spill>)
  let spill-num = colour.vm-arg-offset-to-arg-number;
  select-spill-by-number(vr, spill-num);
end method;




