Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method emit-all (back-end :: <harp-back-end>, cr :: <compilation-record>, 
				 #rest flags, #key dfm-output? = #f, #all-keys) => ()
  with-simple-abort-retry-restart 
      ("Abort the emission phase", "Restart the emission phase")

    with-harp-variables(back-end)


      let heap = cr.compilation-record-model-heap;

      // compilation-record-data will be filled in later(during linking)
      // when lambda-names are known
      cr.compilation-record-back-end-data := make(<string-table>);

      let compiling-dylan? = compiling-dylan-library?();

      let current-library-mode = current-library-description().library-description-compilation-mode;
      let loose-mode? = current-library-mode = #"loose";
      let interactive-mode? = current-library-mode = #"interactive";


      dynamic-bind (*emitting-data?*     = #f,
		    *compiling-dylan?*   = compiling-dylan?,
		    *current-heap*       = heap,
		    *loose-mode?*        = loose-mode?,
		    *interactive-mode?*  = interactive-mode?,

		    $dylan-integer           = dylan-value(#"<integer>"),
		    $dylan-byte-character    = dylan-value(#"<byte-character>"),
		    $dylan-unicode-character = dylan-value(#"<unicode-character>"),
                    $current-handlers        = op--constant-ref(back-end, dylan-binding(#"*current-handlers*"), import?: #f),
		    $true                    = op--constant-ref(back-end, #t, import?: ~ compiling-dylan?),
		    $false                   = op--constant-ref(back-end, #f, import?: ~ compiling-dylan?))

      block()

      register-dylan-code-models();

      register-imports-in-heap(back-end, cr, heap);

      let literals = heap.heap-defined-object-sequence;
      when (dfm-output?)
	with-build-area-output (stream = current-library-description(),
				name: concatenate(cr.compilation-record-name, ".dfm"))
	  for (literal in literals)
	    apply(emit-dfm, back-end, stream, literal, flags);
	  end for;
	end with-build-area-output;
      end when;

      for (literal in heap.heap-defined-object-sequence)
        apply(emit-code, back-end, literal, flags);
      end for;

      with-labeling-from-dynamic
        let top-level-id = 
	  cr-init-name(compilation-record-library(cr),
		       compilation-record-name(cr));

        apply(emit-init-code-definition,
              back-end, #f, heap, top-level-id, flags);

        retract-local-methods-in-heap(heap);

      end with-labeling-from-dynamic;

      cleanup

	deregister-dylan-code-models();

      end block;

      end dynamic-bind;
    end with-harp-variables;

  end with-simple-abort-retry-restart;
end method emit-all;

// Must retract local methods only after code generation of compilation-record
// HACK: Also calculate total code-size

define method retract-local-methods-in-heap(heap :: <model-heap>) => ()
  let total-code-size :: <integer> = 0;
  for (literal in heap.heap-defined-object-sequence)
    if (instance?(literal, <&iep>))
      let code      = code(literal);
      let code-vec  = code & ~empty?(code) & lambda-code(head(code));
      let code-size = if (code-vec) size(code-vec) else 0 end;
      total-code-size := total-code-size + code-size;
      unless (lambda-top-level?(literal) | ~*retract-dfm?*)
	format-out?("\nRETRACTING %=\n", literal);
	retract-method-dfm(literal);
	retract-method-dfm(literal.function);
      end unless;
    end if;
  end for;
  compilation-record-code-size(heap-compilation-record(heap)) := total-code-size;
end method;

/*
define function no-code-for-lambda(back-end :: <harp-back-end>, name :: <string>) => (compiled-lambda :: <object>)
  with-harp-emitter(back-end, #f, name, static: #t, export: #f)
    back-end-primitive-emitter(back-end, #"primitive-break")(back-end, #f);
    ins--rts-and-drop(back-end, 0);
  end with-harp-emitter;
end function;
*/

define method emit-code (back-end :: <harp-back-end>, o :: <&iep>, 
			 #rest flags, #key form?, force-emit?, #all-keys) => ()
  let re-emit? =
    case
      force-emit? => #t;
      form? =>
	subsequence-position(as-lowercase(as(<string>, emit-name(back-end, #f, o))), form?);
      otherwise => ~ code(o) // DFM EXISTS?
    end case;

    if (re-emit?)
      o.code := #();
      apply(emit-lambda, back-end, #f, o, flags);
      if (*retract-dfm?*)
	if (lambda-top-level?(o))
	  format-out?("\nRETRACTING %=\n", o);
	  retract-method-dfm(o);
	  retract-method-dfm(o.function);
	end if;
      end if;
    end if;
end method emit-code;

define method emit-code (back-end :: <harp-back-end>, o,
                         #rest flags, #key, #all-keys) => ()
end method;

define method emit-dfm (back-end :: <harp-back-end>, stream :: <stream>, o :: <&iep>, 
			 #rest flags, #key form?, force-emit?, #all-keys) => ()

  print-method(stream, o.function);
  format(stream, "\n");

end method emit-dfm;

define method emit-dfm (back-end :: <harp-back-end>, stream :: <stream>, o,
                         #rest flags, #key, #all-keys) => ()
end method emit-dfm;

define constant $system-init-code-tag = "for_system";
define constant $user-init-code-tag = "for_user";

define method emit-init-code-definition
    (back-end :: <harp-back-end>, stream, heap, name :: <string>,
     #rest flags,
     #key harp-output? = unsupplied(), force-emit?, #all-keys) => ()

    let system-name = concatenate(name, $system-init-code-tag);
    let system-init-code = heap.heap-root-system-init-code;
    if (#t)
      let system-init-names = make(<table>);
      let fixups-name = ins--constant-ref(back-end, format-to-string("%s_fixups", system-name));
      let fixups-string = fixups-name.cr-refers-to-object;
      system-init-names[0] := fixups-name;
      for (o in system-init-code, count from 0)
	system-init-names[count + 1] :=
	  ins--constant-ref(back-end, format-to-string("%s_%d", system-name, count));
      end for;
      emitted-name(as(<symbol>, concatenate(fixups-string, "_code"))) := 
	apply(emit-system-init-code,
	      back-end, stream, heap,
	      fixups-string,
	      code?: #"fixups", flags);
      emitted-name(as(<symbol>, fixups-string)) := fixups-string;
      for (o in system-init-code, count from 0)
        let compiled-lambda = o.^iep.code;
	let re-emit? = force-emit? | ~ compiled-lambda;
	let init-name = system-init-names[count + 1];
	let init-string = init-name.cr-refers-to-object;
	if (re-emit?)
	  o.^iep.code :=
	    apply(emit-system-init-code,
		  back-end, stream, heap,
		  init-string,
		  code?: o, flags);
	end if;
	emitted-name(as(<symbol>, init-string)) := init-string;
      end for;
      emitted-name(as(<symbol>, system-name)) :=
	with-harp-init-emitter(back-end,
			       system-name,
			       harp-debug: harp-output?,
			       export: #f)
	  for (count from 0 below system-init-names.size)
	    ins--call(back-end, system-init-names[count], 0);
	  end for;
        end with-harp-init-emitter;
    else
      emitted-name(as(<symbol>, system-name)) := 
	apply(emit-system-init-code,
	      back-end, stream, heap, system-name, flags);
    end if;

    let user-name = concatenate(name, $user-init-code-tag);
    let user-init-code = heap.heap-root-init-code;
    if (#t)
      let user-init-names = make(<table>);
      for (o in user-init-code, count from 0)
	user-init-names[count] :=
	  ins--constant-ref(back-end, format-to-string("%s_%d", user-name, count));
      end for;
      for (o in user-init-code, count from 0)
        let compiled-lambda = o.^iep.code;
	let re-emit? = force-emit? | ~ compiled-lambda;
	let init-name = user-init-names[count];
	let init-string = init-name.cr-refers-to-object;
	if (re-emit?)
	  o.^iep.code :=
	    apply(emit-user-init-code,
		  back-end, stream, heap,
		  init-string,
		  code?: o, flags);
	end if;
	emitted-name(as(<symbol>, init-string)) := init-string;
      end for;
      emitted-name(as(<symbol>, user-name)) :=
	with-harp-init-emitter(back-end,
			       user-name,
			       harp-debug: harp-output?,
			       export: #f)
	  for (count from 0 below user-init-names.size)
	    ins--call(back-end, user-init-names[count], 0);
	  end for;
        end with-harp-init-emitter;
    else
      emitted-name(as(<symbol>, user-name)) := 
	apply(emit-user-init-code,
	      back-end, stream, heap, user-name, flags);
    end if;

end method;

define method emit-init-code-body(back-end :: <harp-back-end>, stream, code-literal,
				  #key source-locator)
  let o = code-literal.^iep;

  with-harp-variables(back-end,
		      prototype: back-end.cg-variables)
    back-end.cg-variables.current-lambda := o;
    back-end.cg-variables.current-scl := source-locator;
    back-end.cg-variables.exit-tag := make-tag(back-end);
    emit-lambda-body(back-end, stream, o);
    ins--tag(back-end, back-end.cg-variables.exit-tag);
  end with-harp-variables;
  if (*retract-dfm?*)
    format-out?("\nRETRACTING %=\n", o);
    retract-method-dfm(o);
    retract-method-dfm(o.function);
  end if;

end method;

define method emit-system-init-code(back-end :: <harp-back-end>, stream,
				    heap :: <model-heap>, name,
				    #key harp-output? = unsupplied(), force-emit?, code?, #all-keys)
 => (compiled-lambda)

  local method emit-fixups(back-end)
	  // Symbol Fixups

	  let resolve-ref = make-g-register(back-end);
	  ins--move(back-end, 
		    resolve-ref, 
		    op--dylan-constant-ref(back-end, $dylan-resolve-symbol-iep));

	  for (refs in heap.heap-load-bound-references)
	    emit-symbol-fixups
	      (back-end, stream, load-bound-referenced-object(refs.first), refs, resolve-ref);
	  end for;
	end method;

  with-harp-init-emitter(back-end,
			 name,
			 harp-debug: harp-output?,
			 export: #f)
    if (code?)
      if (code? = #"fixups")
	emit-fixups(back-end);
      else
	emit-init-code-body(back-end, stream, code?);
      end if;
    else
      emit-fixups(back-end);
      for (code in heap.heap-root-system-init-code)
	emit-init-code-body(back-end, stream, code);
      end for;
    end if;
  end with-harp-init-emitter;
end method emit-system-init-code;

define method emit-user-init-code(back-end :: <harp-back-end>, stream,
				  heap :: <model-heap>, name,
				  #key harp-output? = unsupplied(), force-emit?, code?, #all-keys)
 => (compiled-lambda)
  with-harp-init-emitter(back-end,
			 name,
			 source-locator: #f,
			 harp-debug: harp-output?,
			 export: #f)
    if (code?)
      let cr = heap-compilation-record(heap);
      let source-locator = make-dummy-source-locator(cr);
      emit-init-code-body(back-end, stream, code?, source-locator: source-locator);
    else
      let init-code = heap.heap-root-init-code;
      for (code in init-code)
	let cr = model-compilation-record(code);
	let source-locator = make-dummy-source-locator(cr);
	emit-init-code-body(back-end, stream, code, source-locator: source-locator);
      end for;
    end if;
  end with-harp-init-emitter;
end method emit-user-init-code;

define method emit-comment(stream, comment :: <string>, #rest args) => ()
    if (*stream-outputters?*)
      output-comment(*harp-back-end*,
		     stream,
		     if (args.empty?)
		       comment;
		     else
		       apply(format-to-string, comment, args);
		     end if);
    end if;
end method emit-comment;

define method emit-line-comment(stream, comment :: <string>, #rest args) => ()
    if (*stream-outputters?*)
      output-line-comment(*harp-back-end*,
			  stream,
			  if (args.empty?)
			    comment;
			  else
			    apply(format-to-string, comment, args);
			  end if);
    end if;
end method emit-line-comment;


// Register external symbols for runtime primitives and dylan functions referenced
// out of Heap

define method register-extern(back-end :: <harp-back-end>, object :: <runtime-object>,
			      #key reference) => (reference :: <constant-reference>)
  element(back-end.cg-variables.runtime-references, object, default: #f)
  | (begin
       let object-ref = reference | ins--constant-ref(back-end, object);
       ins--register-external(back-end, object-ref);
       back-end.cg-variables.runtime-references[object] := object-ref;
     end);
end method register-extern;

define method register-extern(back-end :: <harp-back-end>, object :: <constant-reference>,
			      #key reference) => (reference)
  register-extern(back-end, object.cr-refers-to-object,
		  reference: object)
end method;

define method register-extern(back-end :: <harp-back-end>, object,
			      #key reference) => (reference)
  reference
end method;


// GNU Linker Support -- cache and dump imports per used library to .import files

define open generic emit-imports
    (back-end :: <harp-back-end>, cr, ld :: <library-description>) => ();

define method emit-imports
    (back-end :: <harp-back-end>, cr, ld :: <library-description>) => ()
  unless (*compiling-dylan?* | *interactive-mode?*)
    let cr-name =
      select(cr by instance?)
	<compilation-record> => cr.compilation-record-name;
	otherwise => cr;
      end select;
    with-build-area-output (stream = ld, name: concatenate(cr-name, ".import"))
      for (library in back-end.cg-variables.imports.key-sequence)
	let (internal-library-name, library-name) = 
	  if (library == #"runtime")
	    values("runtime", "runtime")
	  else
	    let internal-library-name =
	      as-lowercase(as(<string>, library.library-description-emit-name));
	    values(internal-library-name,
		   apply(settings-executable, library.library-description-build-settings)
		     | internal-library-name)
	  end if;
	if (internal-library-name = "dylan")
	  format(stream, "%s\n", internal-library-name);
	end if;
	format(stream, "%s\n", library-name);
	let imports = back-end.cg-variables.imports[library];
	for (import in imports.key-sequence)
	  format(stream, "%s\n", import);
	end for;
	format(stream, "\n");
      end for;
    end with-build-area-output;
  end unless;
end method;

define method settings-executable 
  (#key executable = #f, #all-keys)
  => (executable :: false-or(<byte-string>))
  executable
end method;


define method cache-import-in-library
    (back-end :: <harp-back-end>, name :: <byte-string>, model-library) => ()
  unless (*compiling-dylan?*)
    let imports = imports-in-library(back-end, name, model-library);
    
    imports[name] := #t
  end unless;
end method;

define method imports-in-library
    (back-end :: <harp-back-end>, name :: <byte-string>, model-library) => (imports :: <table>)
    let entries = 
      element(back-end.cg-variables.imports, model-library, default: #f);
    let new-entries = entries | make(<table>);

    unless (entries)
      back-end.cg-variables.imports[model-library] := new-entries
    end unless;

    new-entries 
end method;

// Generate imports for runtime primitives

define method cache-imports-in-lambda
    (back-end :: <harp-back-end>, lambda-externals) => ()
  for (reference :: <constant-reference> in lambda-externals)
    let object = reference.cr-refers-to-object;
    let (name, import-domain) =
      select (object by instance?)
	<local-runtime-object> => #f;
        <c-runtime-object> =>
          values(c-name(back-end, object.runtime-object-name), #"runtime");
        <runtime-object> =>
          values(object.runtime-object-name, #"runtime");
	<&iep> =>
	  unless (object.function.lambda-runtime-function?)
	    values(emit-name(back-end, #f, object), dylan-library-description());
	  end;
	otherwise =>
	  values(emit-name(back-end, #f, object), dylan-library-description());
      end select;
    if (name)
      cache-import-in-library(back-end, name, import-domain);
    end if;
  end for;
end method;


define sideways method string-emitter
    (back-end :: <harp-back-end>, stream, name :: <byte-string>)
  name
end method;

define method string-emitter
    (back-end :: <harp-back-end>, stream, object :: <runtime-object>)
  object.runtime-object-name
end method;

define method string-emitter
    (back-end :: <harp-back-end>, stream, object :: <c-runtime-object>)
  c-name(back-end, object.runtime-object-name)
end method;


define method emit-cg-name
    (back-end :: <harp-back-end>, stream, o) => (name :: type-union(<string>, <constant-reference>))

  if (*emitting-data?*)
    emit-name(back-end, stream, o);
  else
    op--constant-ref(back-end, o);
  end if;

end method;

define sideways method emit-name-internal (back-end :: <harp-back-end>, stream, o :: <&iep>) => (name  :: <byte-string>)
  emit-iep-name(back-end, stream, o.function)
end method;

// Currently these aren't unique objects(one created per call site),
// so use the emitted-name hack to install a static runtime-object

define sideways method emit-name-internal (back-end :: <harp-back-end>, stream, o :: <&deferred-iep>) => (object :: <runtime-object>)
  let name = emit-iep-name(back-end, stream, o.function);
  entry-point-reference(name)
end method;

define function round-up-to-mod (int :: <integer>, modulus :: <integer>)
 => (i :: <integer>);
  modulus * ceiling/(int, modulus);
end;

define function stdcall-name(signature, name)
  let size = 0;

  for(sig in signature.^signature-required,
      i from 0 below signature.^signature-number-required)
    size := size + round-up-to-mod(sig.raw-type-size, 4);
  end for;

  concatenate("_", name, "@",
	      format-to-string("%d", size));
end function;

// There may be two model-objects refering to the same c-function in the
// unlikely event that a c-function is locally defined in Dylan Code in the
// same source record;
// So use names for c-functions instead of compiler models

define sideways method emit-name-internal
    (back-end :: <harp-back-end>, stream, o :: <&c-callable-function>) => (c-name :: <string>)
  let binding-name =
    o.binding-name | concatenate("c_callable_", local-mangle(back-end, o.alternate-name));
  let c-emitted-name =
    select(o.c-modifiers by \=)
      "__stdcall" =>
	stdcall-name(o.^function-signature, binding-name);
      otherwise => c-name(back-end, binding-name);
    end select;
  c-emitted-name
end method;

define sideways method emit-name-internal
    (back-end :: <harp-back-end>, stream, o :: <&c-function>) => (c-name :: <string>)
  let c-emitted-name =
    if (o.binding-name)
      select(o.c-modifiers by \=)
	"__stdcall" =>
	  stdcall-name(o.c-signature, o.binding-name);
	otherwise => c-name(back-end, o.binding-name);
      end select;
    else
      debug-assert(o.emitted-name, "Missing emitted-name for %s", o);
      format-to-string("%s%d", $constant-prefix, o.emitted-name);
    end if;
  c-emitted-name
end method emit-name-internal;

define method emit-iep-name
    (back-end :: <harp-back-end>, stream, o :: <&function>) => (name :: <string>)
    concatenate-as(<byte-string>, emit-name(back-end, stream, o), $iep-suffix);
end method;

define method emit-iep-name
    (back-end :: <harp-back-end>, stream, o :: <&c-callable-function>) => (name)
  emit-name(back-end, stream, o);
end method;

define method emit-header(back-end :: <harp-back-end>, outputter) => ()
  output-header(back-end, outputter);
  output-data-start(back-end, outputter);
end method;

define method emit-footer(back-end :: <harp-back-end>, outputter) => ()
  output-footer(back-end, outputter);
end method emit-footer;

/// SYMBOL FIXUPS

define method emit-symbol-fixups
    (back-end :: <harp-back-end>, stream, object, refs, resolve-ref) => ()

  let object-ref = make-g-register(back-end);
  ins--move(back-end, object-ref, emit-fixup-reference(back-end, stream, object));
  let fixup = emit-resolve-symbol-call(back-end, stream, object-ref, resolve-ref);
  let done-tag = make-tag(back-end);
  let worth-testing-limit = 2;

  if (refs.size > worth-testing-limit)  
    // It's more efficient to not perform this test if the number of fixups
    // is sufficiently small
    ins--beq(back-end, done-tag, fixup, object-ref) 
  end if;

  let fixed-indirection-variable = #f;
  for (ref in refs)
    if (instance?(ref, <load-bound-code-reference>))
      if (~fixed-indirection-variable)
	fixed-indirection-variable := #t;
	emit-fixup(back-end, stream, object, ref, fixup);
      end if;
    else
      emit-fixup(back-end, stream, object, ref, fixup);
    end if;
  end;

  ins--tag(back-end, done-tag);

end method;


define method emit-resolve-symbol-call
    (back-end :: <harp-back-end>, stream, object-ref, resolve-ref) => (fixedup-ref)

  let fixup = make-g-register(back-end);
  ins--move(back-end,
	    back-end.registers.reg-arg0,
	    object-ref);
  ins--call(back-end,
	    resolve-ref,
	    1);
  ins--move(back-end, fixup, back-end.registers.reg-result);
  fixup;

end method;

define function emit-fixup-reference
    (back-end :: <harp-back-end>, stream, o) => (fixup)
  
  if (instance?(o, <symbol>))
    let reference = emit-indirect-reference(back-end, stream, o);
    let local-symbol = element(heap-symbols(*current-heap*), o);
    if (symbol-emitted?(local-symbol))
      local-symbol.cg-uninterned-symbol
    else
      reference
    end if;
  else
    emit-reference(back-end, stream, o);
  end if;

end function;

define method emit-fixup
    (back-end :: <harp-back-end>, stream, object, ref, fixup) => ()
end method;

define method emit-fixup
    (back-end :: <harp-back-end>, stream, 
     object, ref :: <load-bound-code-reference>, fixup) => ()
  ins--move(back-end,
	    emit-indirect-reference(back-end, stream, object),
	    fixup);
end method;

define method emit-fixup
    (back-end :: <harp-back-end>, stream, 
     object, ref :: <load-bound-binding-reference>, fixup) => ()
  ins--move(back-end,
	    emit-fixup-reference(back-end, stream, load-bound-referencing-binding(ref)),
	    fixup);
end method;

define method emit-fixup
    (back-end :: <harp-back-end>, stream, 
     object, ref :: <load-bound-instance-slot-reference>, fixup) => ()
  let referencing-object = load-bound-referencing-object(ref);
  let slotd = load-bound-referencing-slot(ref);
  let (primitive, offset) 
    = fixed-slot-primitive-fixup-info
        (^object-class(referencing-object), slotd);

  primitive.emitter(back-end,
		    #f,
		    fixup,
		    emit-fixup-reference(back-end, stream, referencing-object),
		    offset);
end method;

define method emit-fixup
    (back-end :: <harp-back-end>, stream, 
     object, ref :: <load-bound-repeated-slot-reference>, fixup) => ()
  let referencing-object = load-bound-referencing-object(ref);
  let slotd = load-bound-referencing-slot(ref);
  let index = load-bound-referencing-slot-index(ref);
  let (primitive, base-offset) 
    = repeated-slot-primitive-fixup-info
        (^object-class(referencing-object), slotd);

  primitive.emitter(back-end,
		    #f,
		    fixup,
		    emit-fixup-reference(back-end, stream, referencing-object),
		    base-offset,
		    index);
end method;



define method imported-object?(back-end :: <harp-back-end>, object) => (import? :: <boolean>)
  ~ *compiling-dylan?* & library-imported-object?(current-library-description(), object);
end;

define method imported-object?(back-end :: <harp-back-end>, object :: <module-binding>) => (import? :: <boolean>)
  ~ *compiling-dylan?* & library-imported-binding?(current-library-description(), object);
end;

define method imported-object?(back-end :: <harp-back-end>, object :: <symbol>) => (import? :: <boolean>)
  case
    *compiling-dylan?* => #f;
    load-bound-object?(object) => #f;
    otherwise => #t;
  end;
end;

define method imported-object?(back-end :: <harp-back-end>, object :: <&c-variable>) => (import? :: <boolean>)
  object.dll-import?
end;

define method imported-object?(back-end :: <harp-back-end>, object == #()) => (import? :: <boolean>)
  ~ *compiling-dylan?*
end;

define method imported-object?(back-end :: <harp-back-end>, object :: <string>) => (import? :: <boolean>)
  if (object = "")
    ~ *compiling-dylan?*
  end if;
end;

define method imported-object?(back-end :: <harp-back-end>, object :: <&shared-entry-point>) => (import? :: <boolean>)
  #f
end;

define method imported-object?(back-end :: <harp-back-end>, object :: <&engine-node-ep>) => (import? :: <boolean>)
  #f
end;


define open generic register-imports-in-heap
  (back-end  :: <harp-back-end>, cr :: <compilation-record>, heap :: <model-heap>) => ();

define method register-imports-in-heap
  (back-end  :: <harp-back-end>, cr :: <compilation-record>, heap :: <model-heap>) => ()
end method;


// Make Code Generation canonical-objects for dylan values

define method canonical-model-object(o :: <string>) => (o :: <string>)
  if (o = "") $empty-string
  else  o
  end if
end method;

define method canonical-model-object(o :: <vector>) => (o :: <vector>)
  if (o = #[]) $empty-vector
  else  o
  end if
end method;

define method canonical-model-object(o) => (o)
  o
end method;


// Make appropriate unique model-objects for some models for which there is a many-to-one
// mapping to names

define method apropo-model-object
    (o :: <vector>) => (o)
  canonical-model-object(o)
end method;

define method apropo-model-object
    (o :: <byte-string>) => (o)
  let o = canonical-model-object(o);
  if (*emitting-data?*) o
  else make-string-model-object(o)
  end if
end method;

define method apropo-model-object
    (o :: <&raw-type>) => (o)
  raw-type-marker()
end method;

define method apropo-model-object
    (o :: <uninterned-symbol>) => (o)
  as(<symbol>, o.symbol-name)
end method;

define method apropo-model-object
    (o :: <symbol>) => (o)
  o
end method;

define method apropo-model-object
    (o :: <&kernel-ep>) => (o)
  o.emitted-name
end method;

define method apropo-model-object
    (o :: <&keyword-method-mep>) => (o)
  o.emitted-name
end method;

define method apropo-model-object
    (o :: <&deferred-iep>) => (o)
  o.emitted-name
end method;

define method apropo-model-object
    (o :: <&c-function>) => (o)
  if (*emitting-data?*)
    error("Harp Code Generator: Cannot determine appropriate model object for %=", o);
  end if;
  emit-name(*harp-back-end*, #f, o);
end method;

define method apropo-model-object
    (o :: <&iep>) => (o)
  let f = o.function; 
  if (instance?(f, <&c-callable-function>))
    if (*emitting-data?*)
      error("Harp Code Generator: Cannot determine appropriate model object for %=", o);
    end if;
    emit-name(*harp-back-end*, #f, o)
  else
    o
  end if
end method;

define method apropo-model-object
    (o) => (o)
  o
end method;


define method entry-point?(object) => (entry-point? :: <boolean>)
  select (object by instance?)
    <&kernel-ep>, <&mep> => #t;
    otherwise => #f;
  end select
end method;


define method model-library-description(object)
  model-library(object)
end method;

define method model-library-description(object :: <module-binding>)
  object.binding-home.namespace-library-description;
end method;

define method emit-extern
  (back-end :: <harp-back-end>, stream, name :: <byte-string>,
   object, import?,
   #key derived-model-object,
        model-library,
        really-import? = import?) => ()

  unless (binding-of-*current-handlers*?(object))
    let entry-point? = entry-point?(object);

    if (really-import? | entry-point?)
      let import-domain =
        if (entry-point?) #"runtime"
        else model-library | model-library-description(object)
        end if;
      let imports = imports-in-library(back-end, name, import-domain);
      let already-imported? = element(imports, name, default: #f);
      unless (already-imported?)
	imports[name] := #t;
	output-external(back-end, stream, name,
			model-object: apropo-model-object(object),
			derived-model-object: derived-model-object,
			import?: import?)
      end unless
    else
      output-external(back-end, stream, name,
		      model-object: apropo-model-object(object),
	              derived-model-object: derived-model-object,
		      import?: import?);
    end if;
  end unless;
end method;

define method emit-public
    (back-end :: <harp-back-end>, stream, object,
     #key name, export? = unsupplied(), derived-model-object) => ()

 let name = name | emit-name(back-end, stream, object);
 let export? =
    if (supplied?(export?)) export?
    else
      model-externally-visible?(object)
    end if;

 output-public(back-end, stream, name,
	       model-object: apropo-model-object(object),
	       derived-model-object: derived-model-object,
	       export?: export?);

end method;

define method emit-raw-data-item
  (back-end :: <harp-back-end>, stream, object) => ()
  output-data-item(back-end, stream, object);
end method;

define method emit-data-item
    (back-end :: <harp-back-end>, stream, object :: <integer>) => ()
  output-data-item(back-end, stream,
		   emit-object(back-end, stream, object));
end method;

define method emit-data-item
    (back-end :: <harp-back-end>, stream, object :: <byte-character>) => ()
  output-data-item(back-end, stream,
		   emit-object(back-end, stream, object));
end method;

define method emit-data-item
  (back-end :: <harp-back-end>, stream, object :: <&raw-double-float>) => ()
  let dfloat :: <double-float> = object.^raw-object-value;
  let (low, high) = decode-double-float(dfloat);
  let (first, second) =
    if (back-end.big-endian?) values(high, low) else values(low, high) end;

  output-data-item(back-end, stream,
		   coerce-machine-word-to-an-integer(first));
  output-data-item(back-end, stream,
		   coerce-machine-word-to-an-integer(second));
end method;

define method emit-data-item
    (back-end :: <harp-back-end>, stream, object :: <&raw-object>) => ()
  let raw-object = emit-object(back-end, stream, object);
  let raw-object =
    if (instance?(raw-object, <machine-word>))
      coerce-machine-word-to-an-integer(raw-object)
    else
      raw-object
    end if;
  output-data-item(back-end, stream, raw-object);
end method;

define method emit-data-item
    (back-end :: <harp-back-end>, stream, object :: <machine-word>) => ()
  output-data-item(back-end, stream,
		   coerce-machine-word-to-an-integer(object));
end method;

define method emit-data-item
    (back-end :: <harp-back-end>, stream, object :: <constant-reference>) => ()
  output-data-item(back-end, stream, object);
end method;

define method emit-data-item
    (back-end :: <harp-back-end>, stream, object) => ()
  let import? = ~ *compiling-dylan?* & imported-object?(back-end, object);

  output-data-item(back-end, stream, $dummy-name,
		   model-object: apropo-model-object(object),
		   import?: import?);
end method;

// ELF Outputters use this emitter to emit type and size of data, so the 
// Linker can create appropriate dynamic relocation records for them

define open generic emit-data-footer
    (back-end :: <harp-back-end>, stream, name,
     #key model-object) => ();

define sideways method emit-data-footer
    (back-end :: <harp-back-end>, stream, name,
     #key model-object = unsupplied()) => ()
end method;


// Make a Code reference to a compiler model-object

define function op--constant-ref
    (back-end :: <harp-back-end>, object,
     #key import? = unsupplied(),
          interactor? = instance?(object, <interactor-binding-reference>))
 => (name :: <constant-reference>, found? :: <boolean>)
  let found? = #t;
  let name =
  op--model-ref(back-end, object)
  |
    (begin
       found? := #f;
       let import? =
	 if (interactor?) #f;
	 elseif (supplied?(import?)) import?
	 else imported-object?(back-end, object)
	 end if;
       let apropo-model-object = apropo-model-object(object);
       let cref =
	 if (interactor?)
	   ins--interactor-constant-ref(back-end, apropo-model-object);
	 else
	   let indirect? = instance?(object, <module-binding>);
	   if (import?)
	     make-imported-constant-reference
	       (back-end, apropo-model-object, indirect?: indirect?);
	   else
	     let constant-ref =
	       if (indirect?) ins--indirect-constant-ref
	       else ins--constant-ref end if;
	     constant-ref(back-end, apropo-model-object);
	   end if;
	 end if;
       op--model-ref(back-end, object) := cref
     end);
  values(name, found?)
end function;


// Make a Code reference to a Dylan compiler model-object

define function op--dylan-constant-ref
    (back-end :: <harp-back-end>, dylan-object :: <dylan-object>) => (ref)
  let object = dylan-object.dylan-model-object;
  let dylan-library? = *compiling-dylan?*;
  let (ref, found?) = op--constant-ref(back-end, object, import?: ~ dylan-library?);
  let already-seen? =
    case
      found? => #t;
      dylan-library? =>
        member?(object, *current-heap*.heap-defined-objects)
        | member?(object, *current-heap*.heap-referenced-objects);
      otherwise =>
        member?(object, *current-heap*.heap-referenced-objects);
    end case;

  if (already-seen?) ref
  else
    ins--register-external(back-end, ref);
    ref
  end if;

end function;

define function register-dylan-code-models() => ()
  // Registration of dylan constants that may be referenced out-of-heap
  dylan-model-object($dylan-resolve-symbol-iep)         := ^iep(dylan-value($symbol-fixup-name));
  dylan-model-object($dylan-unbound-instance-slot-iep)  := ^iep(dylan-value(#"unbound-instance-slot"));
  dylan-model-object($dylan-type-check-error)           := ^iep(dylan-value(#"type-check-error"));
end function;

define function deregister-dylan-code-models() => ()
  dylan-model-object($dylan-resolve-symbol-iep)         := #f;
  dylan-model-object($dylan-unbound-instance-slot-iep)  := #f;
  dylan-model-object($dylan-type-check-error)           := #f;
end function;

define function op--model-ref
    (back-end :: <harp-back-end>, object) => (ref)
  element(back-end.cg-variables.model-references, object, default: #f)
end function;

define function op--model-ref-setter
    (ref, back-end :: <harp-back-end>, object) => (ref)
  element(back-end.cg-variables.model-references, object) := ref
end function;


// Determines if a model has been created for a symbol

define method symbol-emitted?(symbol) => (emitted? :: <boolean>)
  instance?(symbol, <symbol-object>)
end method;
