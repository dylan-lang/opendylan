Module: dfmc-llvm-linker
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
      progress-line("Linking %s.dylan",
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
      //emit-indirection-definitions(back-end, m, cr);

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


/// Per-compilation-unit initialization functions

define constant $system-init-code-tag = "for_system";
define constant $user-init-code-tag = "for_user";

define method emit-init-code-definition
    (back-end :: <llvm-back-end>, m :: <llvm-module>, heap, name :: <string>)
 => ();
  let init-code-function-type
    = make(<llvm-function-type>,
           return-type: $llvm-void-type,
           parameter-types: #(),
           varargs?: #f);
  let init-code-function-type
    = llvm-pointer-to(back-end, init-code-function-type);
  
  // System init code
  block ()
    let system-init-name = concatenate(name, $system-init-code-tag);
    back-end.llvm-builder-function
      := make(<llvm-function>,
              name: system-init-name,
              type: init-code-function-type,
              arguments: #(),
              linkage: #"internal",
              section: llvm-section-name(back-end, #"init-code"),
              calling-convention: $llvm-calling-convention-fast);
    ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));
    
    for (code in heap.heap-root-system-init-code)
      // Emit the generated init function
      emit-definition(back-end, m, code.^iep);

      // Generate a call to the init function
      let iep-name = emit-name(back-end, m, code.^iep);
      ins--call(back-end, llvm-builder-global(back-end, iep-name), #(),
                type: $llvm-object-pointer-type,
                calling-convention: $llvm-calling-convention-fast);
    end for;
    
    ins--ret(back-end);
    
    llvm-builder-define-global(back-end, system-init-name,
                               back-end.llvm-builder-function);
  cleanup
    back-end.llvm-builder-function := #f;
  end block;

  // User init code
  block ()
    let user-init-name = concatenate(name, $user-init-code-tag);
    back-end.llvm-builder-function
      := make(<llvm-function>,
              name: user-init-name,
              type: init-code-function-type,
              arguments: #(),
              linkage: #"internal",
              section: llvm-section-name(back-end, #"init-code"),
              calling-convention: $llvm-calling-convention-fast);
    ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));
    
    for (code in heap.heap-root-init-code)
      // Emit the generated init function
      emit-definition(back-end, m, code.^iep);

      // Generate a call to the init function
      let iep-name = emit-name(back-end, m, code.^iep);
      ins--call(back-end, llvm-builder-global(back-end, iep-name), #(),
                type: $llvm-object-pointer-type,
                calling-convention: $llvm-calling-convention-fast);
    end for;
    
    ins--ret(back-end);

    llvm-builder-define-global(back-end, user-init-name,
                               back-end.llvm-builder-function);
  cleanup
    back-end.llvm-builder-function := #f;
  end block;
end method;
