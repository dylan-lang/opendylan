module:    base-harp
Synopsis:  Later support for the <basic-block> class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




define method make-bb (back-end :: <harp-back-end>) => (new :: <basic-block>)
  let vars = back-end.variables;
  let bb :: <basic-block> =
    make(<basic-block>,
	 start: vars.fp-instructions,
	 end: vars.fp-instructions);

  // Explicitly set these slots temporarily to avoid heavy
  // packed-slots initializer cost

  bb.bb-colour := vars.bb-colour-flag;
  bb.bb-loop-depth := 1;

  bb
end;


define alias-with-setter bb-fixed-offset   = bb-fall-thru;
define alias-with-setter bb-preceding-sdis = bb-last-entry;
define alias-with-setter bb-seen           = bb-defs;
define alias-with-setter bb-code-ptr       = bb-defs;

define alias-with-setter bb-branch-inf     = bb-live-entry;


// Support sharing of stretchy vectors and tables, so inherit variables
// from a prototype where possible

define method make-harp-variables
    (backend :: <harp-back-end>, name, #rest keys, #key prototype, #all-keys)
 => (new :: <harp-variables>)
  let vars = apply(make-harp-variables-internal, backend, name, keys);
  backend.variables := vars;
  vars.top-block := make-bb(backend);
  vars.current-bb := vars.top-block;
  vars.vreg-state := make(<vreg-state>);
  vars.vreg-state.vr-vect :=
    make(<stretchy-virtual-register-vector>,
	 capacity: estimate-harp-virtual-registers-size(backend, name));

  if (prototype)
      let prototype :: <harp-variables> = prototype;

      let instructions :: <instructions-vector> = prototype.sv-instructions;
      if (instructions.empty?)
	vars.sv-instructions := make-instructions-vector();
      else
        vars.sv-instructions := instructions;
      end;

      let code-vec :: <stretchy-vector> = prototype.code-vector;
      if (code-vec.empty-code?)
	vars.code-vector := make-code-vector();
      else
        vars.code-vector := code-vec;
      end;

      let vr-clashes :: <vector-32bit> = prototype.virtual-register-clashes;
      if (vr-clashes == $empty-bit-set)
	// Allows for a lambda of 64 virtual-registers
	vars.virtual-register-clashes := make-bit-set(65 * $bit-unit-size$);
      else
        vars.virtual-register-clashes := vr-clashes;
      end;

      let variables :: <simple-object-vector> = prototype.named-variables-tables;
      if (variables.empty-named-variables?)
	vars.named-variables-tables := make-named-variables();
      else
        vars.named-variables-tables := variables;
      end;
  else
    allocate-back-end-variables(vars);
  end;

  vars;
end method;

// Update shared back-end variables slots in prototype,
// since they may have been grown in the interim

define method copy-shared-variables
    (old-vars :: <harp-variables>, new-vars :: <harp-variables>) => ()
  old-vars.sv-instructions := new-vars.sv-instructions;
  old-vars.fp-instructions := new-vars.fp-instructions;
  old-vars.code-vector := new-vars.code-vector;
  old-vars.virtual-register-clashes := new-vars.virtual-register-clashes;
  old-vars.named-variables-tables := new-vars.named-variables-tables;
end method;

// Clear shared back-end variables slots

define method clear-shared-variables
    (backend :: <harp-back-end>, #key all?) => ()
  let vars :: <harp-variables> = backend.variables;
  if (all?)
    initialize-back-end-variables(backend);
  else

  vars.sv-instructions :=
    initialize-vector(vars.sv-instructions, vars.fp-instructions);
  vars.code-vector :=
    initialize-stretchy-vector(vars.code-vector);
  vars.virtual-register-clashes :=
    initialize-bit-set(vars.virtual-register-clashes);
  clear-named-variables(vars.named-variables-tables);

  end;
end method;

// Initialize shared back-end variables slots

define method initialize-back-end-variables
    (backend :: <harp-back-end>) => ()
  let vars :: <harp-variables> = backend.variables;
  vars.sv-instructions := #[];
  vars.code-vector := $empty-stretchy-vector;
  vars.virtual-register-clashes := $empty-bit-set;
  vars.named-variables-tables := $empty-named-variables-tables;
end;

// Allocate shared back-end variables slots

define method allocate-back-end-variables
    (vars :: <harp-variables>) => ()
  vars.sv-instructions := make-instructions-vector();
  vars.code-vector := make-code-vector();
  // Allows for a lambda of 64 virtual-registers
  vars.virtual-register-clashes := make-bit-set(65 * $bit-unit-size$);
  vars.named-variables-tables := make-named-variables();
end;

// Throw away shared vectors if their size exceeds 16 K

define constant $max-vector-size :: <integer> = 4000;

define method initialize-vector
    (vec :: <simple-object-vector>, last :: <integer>)
 => (vec :: <simple-object-vector>)
  if (vec.size > $max-vector-size)
    make(<simple-object-vector>, size: $max-vector-size);
  else
    fill!(vec, #f, end: last);
    vec
  end;
end method;

define method initialize-stretchy-vector
    (vec :: <stretchy-vector>)
 => (vec :: <stretchy-vector>)
  if (vec.size > $max-vector-size)
    make(<stretchy-vector>, capacity: $max-vector-size);
  else
    vec.size := 0;
    vec
  end;
end method;

define method initialize-bit-set
    (set :: <vector-32bit>)
 => (set :: <vector-32bit>)
  if (set.size > $max-vector-size)
    make-bit-set($max-vector-size * $bit-unit-size$);
  else
    set
  end;
end method;


define method make-instructions-vector()
 => (instructions :: <instructions-vector>)
  make(<instructions-vector>, size: 250);
end method;

define method make-code-vector()
 => (code-vector :: <stretchy-vector>)
  make(<stretchy-vector>, capacity: 150);
end method;


define open generic estimate-harp-instructions-size
    (backend :: <harp-back-end>, function) => (size :: <integer>);

define method estimate-harp-instructions-size
    (backend :: <harp-back-end>, function) => (size :: <integer>)
  250
end method;

define open generic estimate-harp-variables-size
    (backend :: <harp-back-end>) => (size :: <integer>);

define method estimate-harp-variables-size
    (backend :: <harp-back-end>) => (size :: <integer>)
  round(0.6 * backend.variables.fp-instructions);
end method;

define open generic estimate-harp-virtual-registers-size
    (backend :: <harp-back-end>, function) => (size :: <integer>);

define method estimate-harp-virtual-registers-size
    (backend :: <harp-back-end>, function) => (size :: <integer>)
  0
end method;
