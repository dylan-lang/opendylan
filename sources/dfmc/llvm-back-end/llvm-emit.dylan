Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method emit-all (back-end :: <llvm-back-end>, cr :: <compilation-record>,
			#rest flags, #key dfm-output? = #f, #all-keys)
  with-simple-abort-retry-restart ("Abort the emission phase",
                                   "Restart the emission phase")
    // Associate a new LLVM module with this compilation record
    cr.compilation-record-back-end-data
      := back-end.llvm-builder-module
      := make(<llvm-module>,
              name: compilation-record-name(cr),
              target-triple: llvm-back-end-target-triple(back-end),
              data-layout: llvm-back-end-data-layout(back-end));

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

    // Emit
    for (literal in literals)
      emit-code(back-end, literal);
    end for;

    with-labeling-from-dynamic
      for (code in heap.heap-root-system-init-code, count from 0)
        emit-init-code(back-end, code.^iep);
      end for;
      for (code in heap.heap-root-init-code, count from 0)
        emit-init-code(back-end, code.^iep);
      end for;
    end;
    retract-local-methods-in-heap(heap);
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

define method emit-code (back-end :: <llvm-back-end>, o) => ();
  // do nothing
end method;

define method emit-code (back-end :: <llvm-back-end>, o :: <&iep>) => ();
  unless (code(o))
    o.code := #t;
    if (*retract-dfm?*)
      if (lambda-top-level?(o))
	retract-method-dfm(o);
	retract-method-dfm(o.function);
      end if;
    end if;
  end unless;
end method;

define method emit-init-code (back-end :: <llvm-back-end>, o :: <&iep>)
  unless (code(o))
    o.code := #t;
  end unless;
end method emit-init-code;


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
