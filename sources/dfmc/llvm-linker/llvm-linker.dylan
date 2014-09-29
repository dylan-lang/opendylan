Module: dfmc-llvm-linker
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Main entry points

define sideways method emit-library-records
    (back-end :: <llvm-back-end>, ld :: <library-description>,
     #rest flags, 
     #key assembler-output? = unsupplied(), cr, debug-info?,
     #all-keys)
 => ();
  if (cr)
    apply(emit-library-record, back-end, cr, ld, force-link?: #t, flags);
  else
    for (cr in library-description-compilation-records(ld))
      apply(emit-library-record, back-end, cr, ld, flags);
    end for;
  end if;
end method;

define sideways method emit-library-record
    (back-end :: <llvm-back-end>,
     cr :: <compilation-record>,
     ld :: <library-description>,
     #rest flags, 
     #key assembler-output? = unsupplied(),
          force-link?, debug-info?,
     #all-keys)
 => ();
  if (compilation-record-needs-linking?(cr))
    with-dependent($compilation of cr)
      let locator
        = build-area-output-locator(ld,
                                    base: compilation-record-name(cr),
                                    type: "bc");
      progress-line("  Linking %s.dylan",
                    cr.compilation-record-source-record.source-record-name);

      // Retrieve the LLVM module instantiated during the code emission phase
      let m :: <llvm-module> = cr.compilation-record-back-end-data;
      back-end.llvm-builder-module := m;
      
      link-all(back-end, m, cr);

      // Output LLVM bitcode
      llvm-save-bitcode-file(m, locator);

      // Retract
      cr.compilation-record-back-end-data
        := back-end.llvm-builder-module
        := #f;
      llvm-retract-cached(back-end);
    end;
    compilation-record-needs-linking?(cr) := #f;
  end if;
end method;

define method link-all
    (back-end :: <llvm-back-end>,
     m :: <llvm-module>,
     cr :: <compilation-record>)
 => ()
  with-simple-abort-retry-restart 
      ("Abort the emission phase", "Restart the emission phase")
    let heap = cr.compilation-record-model-heap;

    let current-library-mode
      = current-library-description().library-description-compilation-mode;
    let loose-mode? = current-library-mode == #"loose";
    let interactive-mode? = current-library-mode == #"interactive";

    dynamic-bind (*loose-mode?*       = loose-mode?,
                  *interactive-mode?* = interactive-mode?)
      emit-externs(back-end, m, cr);
      emit-indirection-definitions(back-end, m, cr);

      // Objects
      for (literal in heap.heap-defined-object-sequence)
        emit-definition(back-end, m, literal);
      end for;

      // Variables
      for (binding in heap.heap-defined-bindings)
        emit-definition(back-end, m, binding);
      end for;

      // Init code
      let top-level-id = 
        cr-init-name(back-end,
                     compilation-record-library(cr),
                     compilation-record-name(cr));

      emit-init-code-definition(back-end, m, heap, top-level-id);

      // Externs referenced by the code generator
      emit-code-externs(back-end, m);
    end dynamic-bind;
  end with-simple-abort-retry-restart;
end method;

define variable *fake-transaction-id-counter* = 0;
define sideways method link-and-download
    (back-end :: <llvm-back-end>, il :: <interactive-layer>, runtime-context,
     #rest flags, 
     #key assembler-output? = unsupplied(),
          debug-info? = #f,
     #all-keys)
 => (transaction-id);
  // let cr-names = compilation-context-object-names(il);
  break("Interactive execution not implemented for %s, "
	  "continue from break to pretend that download completed", back-end);
  // Make sure to return distinct transaction ids, so can test condition
  // lookup, which is based on the transaction id's.
  *fake-transaction-id-counter* := *fake-transaction-id-counter* + 1
end method;

define method emit-externs
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     cr :: <compilation-record>)
 => ();
  let heap = cr.compilation-record-model-heap;
  for (object in heap.heap-referenced-objects)
    emit-extern(back-end, m, object);
  end for;

  for (object in heap.heap-referenced-bindings)
    emit-extern(back-end, m, object);
  end for;
end method;

define method emit-indirection-definitions
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     cr :: <compilation-record>)
 => ();
  let heap = cr.compilation-record-model-heap;
  for (refs in heap.heap-load-bound-references)
    let object = load-bound-referenced-object(first(refs));
    emit-indirection-definition(back-end, m, object);
  end for;
end method;


/// Fixups

define constant $system-init-fixups-tag = "fixups";

define method emit-fixups-elements
    (back-end :: <llvm-back-end>, m :: <llvm-module>, heap, name :: <string>)
 => (elements :: <sequence>);
  let fixup-struct-type = back-end.llvm-heap-fixup-entry-llvm-type;
  let fixup-ref-type = fixup-struct-type.llvm-struct-type-elements[1];

  let fixups-elements = make(<stretchy-object-vector>);
  for (refs in heap.heap-load-bound-references)
    let indirect? = #f;
    for (ref in refs)
      let symbol = ref.load-bound-referenced-object;
      let symbol-ref = emit-reference(back-end, m, symbol);
      let fixup = emit-fixup(back-end, m, ref);
      if (fixup)
        llvm-constrain-type(fixup.llvm-value-type, fixup-ref-type);
        add!(fixups-elements,
             make(<llvm-aggregate-constant>,
                  type: fixup-struct-type,
                  aggregate-values: vector(symbol-ref, fixup)));
      elseif (~indirect?)
        let indirection-name
          = concatenate($indirection-prefix, emit-name(back-end, m, symbol));
        let indirection = llvm-builder-global(back-end, indirection-name);
        add!(fixups-elements,
             make(<llvm-aggregate-constant>,
                  type: fixup-struct-type,
                  aggregate-values: vector(symbol-ref, indirection)));
        indirect? := #t;
      end if;
    end for;
  end for;
  fixups-elements
end method;

define method emit-fixup
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     ref :: <load-bound-binding-reference>)
 => (fixup :: <llvm-constant-value>);
  let name = emit-name(back-end, m, ref.load-bound-referencing-binding);
  llvm-builder-global(back-end, name)
end method;

define method emit-fixup
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     ref :: <load-bound-instance-slot-reference>)
 => (fixup :: <llvm-constant-value>);
  let object = ref.load-bound-referencing-object;
  let name = emit-name(back-end, m, object);
  let object-ref = llvm-builder-global(back-end, name);

  let header-words = dylan-value(#"$number-header-words");
  let slot-descriptor = ref.load-bound-referencing-slot;
  let slot-offset
    = header-words + ^slot-offset(slot-descriptor, object.^object-class);
  make(<llvm-gep-constant>,
       in-bounds?: #t,
       operands: vector(object-ref,
                        back-end.llvm-builder-value-function(back-end, 0),
                        make(<llvm-integer-constant>,
                             type: $llvm-i32-type,
                             integer: slot-offset)))
end method;

define method emit-fixup
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     ref :: <load-bound-repeated-slot-reference>)
 => (fixup :: <llvm-constant-value>);
  let object = ref.load-bound-referencing-object;
  let name = emit-name(back-end, m, object);
  let object-ref = llvm-builder-global(back-end, name);

  let header-words = dylan-value(#"$number-header-words");
  let slot-descriptor = ref.load-bound-referencing-slot;
  let index = ref.load-bound-referencing-slot-index;
  let slot-offset
    = header-words + ^slot-offset(slot-descriptor, object.^object-class);
  make(<llvm-gep-constant>,
       in-bounds?: #t,
       operands: vector(object-ref,
                        back-end.llvm-builder-value-function(back-end, 0),
                        make(<llvm-integer-constant>,
                             type: $llvm-i32-type,
                             integer: slot-offset),
                        back-end.llvm-builder-value-function(back-end, index)))
end method;

define method emit-fixup
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     ref :: <load-bound-code-reference>)
 => (fixup :: singleton(#f));
  #f
end method;


/// Per-compilation-unit initialization functions

define constant $system-init-code-tag = "for_system";
define constant $user-init-code-tag = "for_user";

define constant $system-init-ctor-priority = 0;
define constant $user-init-ctor-priority = 65535;

define constant $init-code-function-type
  = make(<llvm-function-type>,
         return-type: $llvm-void-type,
         parameter-types: #(),
         varargs?: #f);
define constant $init-code-function-ptr-type
  = make(<llvm-pointer-type>, pointee: $init-code-function-type);

define constant $ctor-struct-type
    = make(<llvm-struct-type>,
	   elements: vector($llvm-i32-type, $init-code-function-ptr-type));

define method emit-ctor-entry
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     priority :: <integer>, init-function :: <llvm-value>)
 => ();
  let priority-constant
    = make(<llvm-integer-constant>,
           type: $llvm-i32-type, integer: priority);

  let ctor-element
    = make(<llvm-aggregate-constant>,
           type: $ctor-struct-type,
           aggregate-values: vector(priority-constant, init-function));

  // Declare the constructors list
  let ctor-type
    = make(<llvm-array-type>,
	   size: 1,
	   element-type: $ctor-struct-type);
  let ctor-global
    = make(<llvm-global-variable>,
	   name: "llvm.global_ctors",
	   type: llvm-pointer-to(back-end, ctor-type),
	   initializer: make(<llvm-aggregate-constant>,
			     type: ctor-type,
			     aggregate-values: vector(ctor-element)),
           constant?: #f,
	   linkage: #"appending");
  llvm-builder-define-global(back-end,
			     ctor-global.llvm-global-name,
			     ctor-global);
end method;

define method emit-init-code-definition
    (back-end :: <llvm-back-end>, m :: <llvm-module>, heap, name :: <string>)
 => ();
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);

  let init-functions = make(<stretchy-object-vector>);

  // System init code
  block ()
    let system-init-name = concatenate(name, $system-init-code-tag);
    back-end.llvm-builder-function
      := make(<llvm-function>,
              name: system-init-name,
              type: $init-code-function-ptr-type,
              arguments: #(),
              linkage: #"internal",
              section: llvm-section-name(back-end, #"init-code"),
              calling-convention: $llvm-calling-convention-c);
    ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));

    // Generate the fixups table and a call to the fixups primitive
    let fixups-elements
      = emit-fixups-elements(back-end, m, heap, name);
    unless (empty?(fixups-elements))
      let fixups-type
        = make(<llvm-array-type>,
               size: fixups-elements.size,
               element-type: fixups-elements.first.llvm-value-type);
      let fixups-global
        = make(<llvm-global-variable>,
               name: concatenate(name, $system-init-fixups-tag),
               type: llvm-pointer-to(back-end, fixups-type),
               initializer: make(<llvm-aggregate-constant>,
                                 type: fixups-type,
                                 aggregate-values: fixups-elements),
               constant?: #t,
               linkage: #"internal");
      llvm-builder-define-global(back-end,
                                 fixups-global.llvm-global-name,
                                 fixups-global);

      let fixups-table-cast
        = make(<llvm-cast-constant>,
               operator: #"bitcast",
               type: $llvm-object-pointer-type,
               operands: vector(fixups-global));
      let fixups-primitive
        = $llvm-primitive-descriptors[#"primitive-symbol-fixup"];
      fixups-primitive.primitive-emitter(back-end,
                                         fixups-table-cast,
                                         fixups-elements.size);
    end unless;

    for (code in heap.heap-root-system-init-code)
      // Emit the generated init function
      emit-definition(back-end, m, code.^iep);

      // Generate a call to the init function
      let iep-name = emit-name(back-end, m, code.^iep);
      ins--call(back-end, llvm-builder-global(back-end, iep-name),
                vector(undef, undef),
                calling-convention: $llvm-calling-convention-fast);
    end for;
    
    ins--ret(back-end);

    unless (empty?(fixups-elements) & empty?(heap.heap-root-system-init-code))
      let global = llvm-builder-define-global(back-end, system-init-name,
                                              back-end.llvm-builder-function);

      // Add the init function to the constructor
      emit-ctor-entry(back-end, m, $system-init-ctor-priority, global);
    end unless;
  cleanup
    back-end.llvm-builder-function := #f;
  end block;

  // User init code
  block ()
    let user-init-name = concatenate(name, $user-init-code-tag);
    back-end.llvm-builder-function
      := make(<llvm-function>,
              name: user-init-name,
              type: $init-code-function-ptr-type,
              arguments: #(),
              linkage: #"external", // #"internal",
              section: llvm-section-name(back-end, #"init-code"),
              calling-convention: $llvm-calling-convention-c);
    ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));
    
    for (code in heap.heap-root-init-code)
      // Emit the generated init function
      emit-definition(back-end, m, code.^iep);

      // Generate a call to the init function
      let iep-name = emit-name(back-end, m, code.^iep);
      ins--call(back-end, llvm-builder-global(back-end, iep-name),
                vector(undef, undef),
                calling-convention: $llvm-calling-convention-fast);
    end for;

    ins--ret(back-end);

    llvm-builder-define-global(back-end, user-init-name,
                               back-end.llvm-builder-function);
  cleanup
    back-end.llvm-builder-function := #f;
  end block;
end method;


/// Externs referenced from <computation> expansions (and not explicitly in DFM)

define constant $code-extern-names
  = #[#"%resolve-symbol",
      #"unbound-instance-slot",
      #"type-check-error"];

define method emit-code-externs
    (back-end :: <llvm-back-end>, m :: <llvm-module>)
 => ();
  for (function-name in $code-extern-names)
    let iep = ^iep(dylan-value(function-name));
    let def = llvm-builder-global(back-end, emit-name(back-end, m, iep));
    if (instance?(def, <llvm-symbolic-constant>))
      emit-extern(back-end, m, iep);
    end if;
  end for;
end method;
