module:    harp-vars
Synopsis:  The <harp-variables> class, which contains lambda-local variables
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Make the instructions vector a non-stretchy <simple-object-vector> to
// reduce lots of runtime dispatch because all internal vectors are
// <simple-object-vector>s.
// This means we have the responsibility of carefully managing references
// to sv-instructions, which reduces robustness for performance

// As a general rule, use vars.sv-instructions in preference to binding
// sv-instructions locally in methods.
// There is a preserving-instructions macro which updates local bindings
// in bodies of code that can grow the instructions vector; currently this
// only happens during linearisation in adding branch blocks after 
// code-generation is complete

define constant <instructions-vector> = <simple-object-vector>;

// To enable prototyping, introduce some initial constants
// Their slots are only allocated afresh in initializer method
// if they contain their initial values

define constant $empty-stretchy-vector = make(<stretchy-vector>);

define constant $empty-named-variables-tables = make-named-variables();


// A class which defines a random collection of variables for use during 
// HARP code generation.


define primary class <harp-variables> (<object>)
  // "The location of the outputted instructions"
  slot sv-instructions :: <instructions-vector> = #[];
  // "The fill pointer for the instructions"
  slot fp-instructions :: <integer> = 0;

  slot current-bb :: <basic-block>;
  slot top-block :: <basic-block>;
  slot bb-colour-flag = $no-colour;
  slot all-the-sdis :: <stretchy-vector> = make(<stretchy-vector>);
  slot optimize-leaf-case = 0;
  slot all-preserved-mask = #f;
  slot processor-specific-return-offset = 0;
  slot debug-code-select = #f;
  slot taag-no :: <integer> = 0;
  slot blocks-coalesced-yet :: <boolean> = #f;

  slot current-instruction-can-be-moved = #f;
  slot prev-instruction-can-be-moved = #f;

  slot word-size-of-bit-set :: <integer> = 0;

  //  "Offset in bytes in current block during code selection"
  slot block-fixed-offset :: <integer> = 0;

  //  "The current block during code selection"
  slot current-block = #f;

  //  "True when current block is with-stack during code selection"
  slot with-stack :: <boolean> = #f;

  //  "A vector of binary code and sdi's"
  slot code-vector :: <stretchy-vector> = $empty-stretchy-vector;

  // "A vector for the program"
  slot pgm-vect :: <stretchy-basic-block-vector>;

  //  "True when in the backend"
  slot in-back-end :: <boolean> = #f;

  //  "Flag if we are going to build the stack. "
  slot building-stack-frame = #f;

  //  "Flag if we have built a bind-exit frame. "
  slot bind-exit-frame? :: <boolean> = #f;

  //  "A list of registers to use for reloading spills"
  slot reload-registers :: <list> = #();

  //  "A list of registers to use for re-spilling"
  slot respill-registers :: <list> = #();

  //  "The virtual register state"
  slot vreg-state :: <vreg-state>;

  // "The function name or model-object"
  slot function-name, 
    init-value: "Unnamed", init-keyword: function-name:;

  // "Support for defasm"
  slot compiling-defasm :: <boolean>, 
    init-value: #f, init-keyword: defasm:;

  // "Support for defasm"
  slot compiling-call-in :: <boolean>, 
    init-value: #f, init-keyword: call-in:;

  // "A count of emitted integers on an assembler line"
  slot asm-line-pos :: <integer> = 0;

  // "A count of the number of stack arg-spills"
  slot arg-spill-count :: <integer> = 0;

  // "Used by the debug info management code"
  slot named-variables-tables :: <simple-object-vector> 
    = $empty-named-variables-tables;

  // "A record of the external references to constants"
  slot external-references :: <list> = #();

  // "A record of static data referenced relative to the lambda"
  slot referenced-data-words :: <list> = #();

  slot virtual-register-clashes :: <vector-32bit> = $empty-bit-set;

  // There is a global bit-set that represents the commutative
  // relationship between all virtual-registers in the lambda.
  // Each virtual-register entry contains (n - i) entries (one for
  // each younger virtual-register) where n = no-of-virtuals,
  // i = virtual-register-id. For every pair of virtual-registers,
  // there is only one bit entry. This enables tighter bit-packing
  // and reduces space by a factor of 3 for 128 virtual-registers,
  // 
  //   nwords = 128!/32 + 2 = 128.129/64 + 2 = 260 words
  // 
  // compared to:
  // 
  //   nwords = 128.(128/32 + 2) = 768 words
  // 
  // 
  // Saves space by a factor approaching 2 at infinity.
  // 
  //    Nosa  Jan 25, 1999
  // 
  // Each individual virtual-register entry is a superset of neighbours
  // 

end;
  

define generic make-harp-variables
    (backend, name, #key, #all-keys) => (new :: <harp-variables>);

define open generic make-harp-variables-internal
    (backend, name, #key) => (new :: <harp-variables>);

define method make-harp-variables-internal
    (backend, name, #rest keys, #key, #all-keys) => (new :: <harp-variables>)
  apply(make, <harp-variables>, function-name: name, keys);
end method;

define inline function empty-named-variables?(vars :: <simple-object-vector>)
  => (empty? :: <boolean>)
  vars == $empty-named-variables-tables
end function;


define constant $number-of-tables-for-named-variables :: <integer> = 5;

define function clear-named-variables(vars :: <simple-object-vector>)
  => ()
  for (i :: <integer> from 0 to $number-of-tables-for-named-variables - 1)
    let table = vars[i];
    if (table)
      remove-all-keys!(table);
    end;
  end;
end function;

define inline function make-named-variables()
  => (vector :: <simple-object-vector>)
  vector(#f, #f, #f, #f, #f);
end function;

define inline function empty-code?(code :: <stretchy-vector>)
  => (empty? :: <boolean>)
  code == $empty-stretchy-vector
end function;

