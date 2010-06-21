Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
    register-types(back-end, m);

    // Associate it with this compilation record
    block ()
      cr.compilation-record-back-end-data
        := back-end.llvm-builder-module
        := m;
      
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
          emit-init-code(back-end, m, code.^iep);
        end for;
        for (code in heap.heap-root-init-code)
          emit-init-code(back-end, m, code.^iep);
        end for;
      end;
      retract-local-methods-in-heap(heap);
    cleanup
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


define method emit-code
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o) => ();
  // Do nothing
end method;

define method emit-code
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&iep>) => ();
  unless (code(o))
    let name = emit-name(back-end, module, o);
    let function-type = llvm-lambda-type(back-end, o);

    let parameter-types = function-type.llvm-function-type-parameter-types;
    let arguments
      = map(method (arg-type, index) 
              make(<llvm-argument>,
                   type: arg-type,
                   name: format-to-string("arg%d", index),
                   index: index)
            end,
            parameter-types,
            range(below: parameter-types.size));

    let linkage
      = if (o.model-definition) #"external" else #"internal" end;

    block ()
      o.code
        := back-end.llvm-builder-function
        := make(<llvm-function>,
                name: name,
                type: llvm-pointer-to(back-end, function-type),
                arguments: arguments,
                linkage: linkage,
                section: llvm-section-name(back-end, #"code"),
                calling-convention:
                  llvm-calling-convention(back-end, o, o.function));
      ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));
      ins--ret(back-end, emit-reference(back-end, module, &false));
    cleanup
      back-end.llvm-builder-function := #f;
    end block;
    
    if (*retract-dfm?*)
      if (lambda-top-level?(o))
	retract-method-dfm(o);
	retract-method-dfm(o.function);
      end if;
    end if;
  end unless;
end method;

define method emit-init-code
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&iep>)
 => ();
  unless (code(o))
    let name = emit-name(back-end, module, o);
    let function-type = llvm-lambda-type(back-end, o);

    let parameter-types = function-type.llvm-function-type-parameter-types;
    let arguments
      = map(method (arg-type, index) 
              make(<llvm-argument>,
                   type: arg-type,
                   name: format-to-string("arg%d", index),
                   index: index)
            end,
            parameter-types,
            range(below: parameter-types.size));
    block ()
      o.code
        := back-end.llvm-builder-function
        := make(<llvm-function>,
                name: name,
                type: llvm-pointer-to(back-end, function-type),
                arguments: arguments,
                linkage: #"internal",
                section: llvm-section-name(back-end, #"init-code"),
                calling-convention: $llvm-calling-convention-fast);
      ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));
      ins--ret(back-end, emit-reference(back-end, module, &false));
    cleanup
      back-end.llvm-builder-function := #f;
    end block;

    if (*retract-dfm?*)
      retract-method-dfm(o);
      retract-method-dfm(o.function);
    end if;
  end unless;
end method emit-init-code;

// Calling convention for ordinary functions
define method llvm-calling-convention
    (back-end :: <llvm-back-end>, o :: <&iep>, fun :: <&callable-object>)
 => (calling-convention :: <integer>);
  $llvm-calling-convention-fast
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
