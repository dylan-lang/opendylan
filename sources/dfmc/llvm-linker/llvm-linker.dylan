Module: dfmc-llvm-linker
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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
    // FIXME
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

define sideways method emit-mainfile
    (back-end :: <llvm-back-end>, ld :: <library-description>,
     #rest keys, #key, #all-keys)
  // FIXME
end;

define sideways method emit-gluefile
    (back-end :: <llvm-back-end>, ld :: <library-description>, cr-names,
     #key assembler-output? = unsupplied(), 
          downloadable-data? = #f,
          debug-info? = #t,
          compilation-layer,
     #all-keys)
  // FIXME
end;
