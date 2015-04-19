Module: dfmc-harp-cg-linker
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// define class <harp-linker> (<linker>) end;


// *default-debug-info?* controls whether debug-info is output with
// compiled lambdas by default.
//
define variable *default-debug-info?* = #t;


// *debug-info?* controls whether debug-info is output with compiled lambdas
// dynamically. It should be dynamically bound at the linker entry points
//
define thread variable *debug-info?* :: <boolean> = *default-debug-info?*; 



// DRIVER PROTOCOL FUNCTIONS
define sideways method emit-library-records
    (back-end :: <harp-back-end>, ld :: <library-description>,
     #rest flags, 
     #key harp-output? = unsupplied(),
          assembler-output? = unsupplied(), cr, debug-info?,
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
    (back-end :: <harp-back-end>,
     cr :: <compilation-record>,
     ld :: <library-description>,
     #rest flags, 
     #key harp-output? = unsupplied(),
          assembler-output? = unsupplied(),
          force-link?, debug-info?,
     #all-keys)
 => ();
  local
    method emitter (cr :: <compilation-record>)
      let stream = #f;
      with-harp-outputter(back-end,
                          stream,
                          ld,
                          name: compilation-record-name(cr),
                          harp-output?: harp-output?,
                          assembler-output?: assembler-output?)
        let name = cr.compilation-record-source-record.source-record-name;
        progress-line("  Linking %s.dylan", name);
        apply(link-all, back-end, stream, cr, ld, flags);
      end with-harp-outputter;
    end method emitter;
  if (force-link?)
    emitter(cr);
  else
    if (force-link? | compilation-record-needs-linking?(cr))
      with-dependent ($compilation of cr)
	emitter(cr)
      end with-dependent;
      compilation-record-needs-linking?(cr) := #f;
    end if;
  end if;
end method;


define sideways method link-and-download
    (back-end :: <harp-back-end>, il :: <interactive-layer>, runtime-context,
     #rest flags, 
     #key harp-output? = unsupplied(),
          assembler-output? = unsupplied(),
          debug-info? = #f,
     #all-keys)
 => (transaction-id);
  let crs = compilation-context-records(il);
  let coff-files = make(<vector>, size: crs.size + 1);
  let ld = il.interactive-layer-base;
  let component-name
    = as-lowercase(as(<byte-string>, ld.library-description-emit-name));
  let init-function-name = glue-name(component-name);
  let flags = vector(harp-output?: harp-output?, 
		     assembler-output?: assembler-output?,
		     debug-info?: debug-info?);

  local
    method emitter(cr :: <compilation-record>) => (data)
      let stream = #f;
      with-harp-outputter(back-end,
                          stream,
                          ld,
                          name: compilation-record-name(cr),
                          harp-output?: harp-output?,
                          assembler-output?: assembler-output?,
                          download?: #t)
        progress-line("Interactive linking %s.", cr);
        apply(link-all, back-end, stream, cr, ld, flags);
        outputter-downloadable-data(back-end, *harp-outputter*);
      end with-harp-outputter;
    end method emitter;

  for (cr in crs, i from 0)
    if (compilation-record-needs-linking?(cr))
      with-dependent ($compilation of cr)
        coff-files[i] := emitter(cr);
      end with-dependent;
      compilation-record-needs-linking?(cr) := #f;
    end if;
  end for;

  let cr-names = map-as(<vector>, compilation-record-name, crs);
  coff-files[crs.size]
    := emit-gluefile(back-end, ld, cr-names, 
                     harp-output?: harp-output?,
                     assembler-output?: assembler-output?,
                     downloadable-data?: #t,
                     debug-info?: debug-info?,
                     compilation-layer: il);

  download-for-interactive-execution
    (runtime-context, coff-files, component-name, init-function-name);
end method;


define method dll-imported-object?
    (back-end :: <harp-back-end>, object)
 => (res :: <boolean>);
  imported-object?(back-end, object) & (~ model-interactive?(object))
end method;

define method dll-imported-binding?
    (back-end :: <harp-back-end>, object)
 => (res :: <boolean>);
  library-imported-binding?(current-library-description(), object)
    & (~ binding-interactive?(object))
end method;


define method link-all 
    (back-end :: <harp-back-end>,
     stream,
     cr :: <compilation-record>,
     ld :: <library-description>,
     #key debug-info? = *default-debug-info?*,
     #all-keys)
 => ();
  with-simple-abort-retry-restart 
      ("Abort the emission phase", "Restart the emission phase")

    with-harp-variables(back-end)
      let heap = cr.compilation-record-model-heap;

      let current-library-mode
        = current-library-description().library-description-compilation-mode;
      let loose-mode? = current-library-mode == #"loose";
      let interactive-mode? = current-library-mode == #"interactive";

      dynamic-bind (*compiling-dylan?*      = compiling-dylan-library?(),
		    *current-heap*          = heap,
		    *current-compilation*   = cr,
		    *loose-mode?*           = loose-mode?,
		    *interactive-mode?*     = interactive-mode?,
		    *debug-info?*           = debug-info? )
        block()
          register-dylan-code-models();
          emit-header(back-end, stream);
          emit-externs(back-end, stream, cr);
          emit-forwards(back-end, stream, cr);
          emit-indirection-definitions(back-end, stream, cr);

          emit-comment(stream, "Variables");
          for (binding in heap.heap-defined-bindings)
            emit-definition(back-end, stream, binding);
          end for;

          emit-comment(stream, "Objects");
          for (literal in heap.heap-defined-object-sequence)
            emit-data-definition(back-end, stream, literal);
          end for;

          output-code-start(back-end, stream);
          for (literal in heap.heap-defined-object-sequence)
            emit-code-definition(back-end, stream, literal);
          end for;

          emit-comment(stream, "Top-level");
          let top-level-id = 
            cr-init-name(compilation-record-library(cr),
                         compilation-record-name(cr));

          emit-init-code-definition(back-end, stream, top-level-id);

          emit-comment(stream, "eof");

          emit-imports(back-end, cr, ld);

          emit-footer(back-end, stream);
        cleanup
          deregister-dylan-code-models();
        end block;
      end dynamic-bind;
    end with-harp-variables;
  end with-simple-abort-retry-restart;
end method;

define method emit-code-definition
    (back-end :: <harp-back-end>, stream, o :: <&iep>) => ()
  emit-definition(back-end, stream, o);
end method;

define method emit-code-definition
    (back-end :: <harp-back-end>, stream, o) => ()
end method;

define method emit-data-definition
    (back-end :: <harp-back-end>, stream, o :: <&iep>) => ()
end method;

define method emit-data-definition
    (back-end :: <harp-back-end>, stream, o) => ()
  emit-definition(back-end, stream, o);
end method;

define method emit-externs 
    (back-end :: <harp-back-end>, stream, cr :: <compilation-record>)
 => ();
  emit-comment(stream, "Referenced object declarations");

  let heap = cr.compilation-record-model-heap;

  local
    method emit-extern(object)
      let import? = dll-imported-object?(back-end, object);
      emit-extern/import(back-end, stream, object, import?)
    end method;

  let defined-c-functions = #f;
  let emitted-objects = make(<table>);

  // Avoid duplication of some objects like #[] and "" which
  // appear on the heap multiply

  local
    method emitted-object?(object)
      element(emitted-objects, object, default: #f);
    end method,
    
    method emitted-object(object)
      element(emitted-objects, object) := #t
    end method;

  // emit classes
  for (object in heap.heap-referenced-objects) 
    if (instance?(object, <&class>))
      emit-extern(object)
    end if;
  end for;

  // emit non-classes
  for (object in heap.heap-referenced-objects)
    select(object by instance?)
      <&class> => #f;

      // hack to get around local c-functions
      <&c-function> =>
        defined-c-functions
          | (defined-c-functions := c-callable-functions-in-heap(heap));

        let locally-defined? =
          locally-defined-c-function?(object, defined-c-functions);

        unless (locally-defined?)
	  emit-extern(object);
        end unless;
      otherwise => 
	let object = canonical-model-object(object);
	unless (emitted-object?(object))
          emit-extern(object);
	  emitted-object(object);
	end unless;
    end select;
  end for;

  for (object in heap.heap-referenced-bindings)
    let import? = dll-imported-binding?(back-end, object);
    emit-extern/import(back-end, stream, object, import?);
  end for;
end method;

define method emit-forwards
    (back-end :: <harp-back-end>, stream, cr :: <compilation-record>)
 => ();
  emit-comment(stream, "Defined object declarations");

  let heap = cr.compilation-record-model-heap;

  // emit classes
  for (object in heap.heap-defined-objects) 
    if (instance?(object, <&class>))
      emit-forward(back-end, stream, object);
    end if;
  end for;

  // emit non-classes
  for (object in heap.heap-defined-objects)
    if (~instance?(object, <&class>))
      emit-forward(back-end, stream, object);
    end if;
  end for;

  // emit variables
  for (binding in heap.heap-defined-bindings)
    emit-forward(back-end, stream, binding);
  end for;
end method;

define method emit-indirection-definitions
    (back-end :: <harp-back-end>, stream, cr :: <compilation-record>)
 => ();
  emit-comment(stream, "Indirection variables");
  
  let heap = cr.compilation-record-model-heap;
  
  for (refs in heap.heap-load-bound-references)
    let object = load-bound-referenced-object(first(refs));
    emit-indirection-definition(back-end, stream, object);
  end for;
end method;

define method c-callable-functions-in-heap
    (heap :: <model-heap>)
 => (c-functions :: <sequence>)
  reduce(method (result, key)
           let o = heap.heap-defined-objects[key];
           if (instance?(o, <&iep>)
                 & instance?(o.function, <&c-callable-function>))
             add(result, o)
           else
             result
           end if
         end method,
         #[],
         key-sequence(heap.heap-defined-objects))
end method;

define method locally-defined-c-function?
    (object :: <&c-function>, c-functions :: <sequence>)
 => (locally-defined? :: <boolean>);
  member?(object, c-functions,
          test: method (object, c-function)
                  let c-fun = c-function.function;
                  if (object.c-function-name = c-fun.c-function-name)
                    (object.c-modifiers = c-fun.c-modifiers)
                      | error("c-function %= has different calling convention "
                                "from its c-callable-function",
                              object.c-function-name)
                    end if
                end method)
end method;
