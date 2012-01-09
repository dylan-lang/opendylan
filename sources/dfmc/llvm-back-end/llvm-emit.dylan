Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method emit-all (back-end :: <llvm-back-end>,
                        cr :: <compilation-record>,
			#rest flags,
                        #key dfm-output? = #f, #all-keys)
  with-simple-abort-retry-restart ("Abort the emission phase",
                                   "Restart the emission phase")
    // Initialize a new LLVM module
    let m = make(<llvm-module>,
                 name: compilation-record-name(cr),
                 target-triple: llvm-back-end-target-triple(back-end),
                 data-layout: llvm-back-end-data-layout(back-end));

    // Associate it with this compilation record
    block ()
      cr.compilation-record-back-end-data
        := back-end.llvm-builder-module
        := m;

      // Install type definitions
      llvm-register-types(back-end, m);

      // Items to emit from this compilation record
      let heap = cr.compilation-record-model-heap;
      let literals = heap.heap-defined-object-sequence;
      
      // Output DFM files
      if (dfm-output?)
        with-build-area-output (stream = current-library-description(),
                                base: compilation-record-name(cr),
                                type: "dfm")
          for (literal in literals)
            apply(emit-dfm, back-end, stream, literal, flags);
          end for;
        end with-build-area-output;
      end if;
      
      // Emit code
      for (literal in literals)
        emit-code(back-end, m, literal);
      end for;

      with-labeling-from-dynamic
        for (code in heap.heap-root-system-init-code)
          emit-code(back-end, m, code.^iep, init?: #t);
        end for;
        for (code in heap.heap-root-init-code)
          emit-code(back-end, m, code.^iep, init?: #t);
        end for;
      end;

      llvm-compilation-record-dbg-compile-unit(back-end, cr);

      retract-local-methods-in-heap(heap);
    cleanup
      remove-all-keys!(back-end.%dbg-type-table);
      remove-all-keys!(back-end.%source-record-dbg-file-table);
      back-end.llvm-back-end-dbg-functions.size := 0;
      back-end.llvm-builder-module := #f;
    end block;
  end;
end method;

define method retract-local-methods-in-heap(heap :: <model-heap>) => ()
  for (literal in heap.heap-defined-object-sequence)
    if (instance?(literal, <&iep>))
      unless (lambda-top-level?(literal) | ~*retract-dfm?*)
	retract-method-dfm(literal);
	retract-method-dfm(literal.function);
      end unless;
    end if;
  end for;
end method;


/// DFM output

define method emit-dfm
    (back-end :: <llvm-back-end>, stream :: <stream>, o :: <&iep>, 
     #rest flags, #key form?, force-emit?, #all-keys)
 => ();
  print-method(stream, o.function);
  format(stream, "\n");
end method emit-dfm;

define method emit-dfm (back-end :: <llvm-back-end>, stream :: <stream>, o,
                         #rest flags, #key, #all-keys) => ()
end method emit-dfm;
