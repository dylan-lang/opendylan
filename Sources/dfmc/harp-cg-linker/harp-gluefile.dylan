Module: dfmc-harp-cg-linker
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method emit-mainfile
    (back-end :: <harp-back-end>, ld :: <library-description>,
     #rest keys, #key, #all-keys)
end;

define sideways method emit-gluefile
    (back-end :: <harp-back-end>, ld :: <library-description>, cr-names,
     #key harp-output? = unsupplied(),
          assembler-output? = unsupplied(), 
          downloadable-data? = #f,
          debug-info? = *default-debug-info?*,
          compilation-layer,
     #all-keys)
  let current-library-mode = ld.library-description-compilation-mode;
  let interactive-mode? = current-library-mode = #"interactive";

  dynamic-bind(*compiling-dylan?*      = dylan-library-library-description?(ld),
	       *interactive-mode?*     = interactive-mode?)
    emit-gluefile-internal(back-end, ld, cr-names,
			   harp-output?: harp-output?,
			   assembler-output?: assembler-output?,
			   downloadable-data?: downloadable-data?,
			   debug-info?: debug-info?,
			   compilation-layer: compilation-layer)
  end;
end method;


define method glue-unit-name 
    (lib-name, interactive?) => (name :: <byte-string>)
  let simple-name :: <byte-string> = glue-unit(lib-name);
  if (interactive?)
    concatenate("_interactive_", simple-name);
  else simple-name;
  end if;
end method;

define method main-unit-name 
    (lib-name, interactive?) => (name :: <byte-string>)
  let simple-name :: <byte-string> = main-unit(lib-name);
  if (interactive?)
    concatenate("_interactive_", simple-name);
  else simple-name;
  end if;
end method;

define open generic main-unit?(back-end :: <harp-back-end>) => (main? :: <boolean>);

define method main-unit?(back-end :: <harp-back-end>) => (main? :: <boolean>)
  #f
end method;

define dylan-reference %true internal dylan;
define dylan-reference %false internal dylan;


define open generic emit-library-initializer
    (back-end :: <harp-back-end>, stream, ld,
     emit-call-used :: <method>,
     emit-call-crs :: <method>,
     emit-branch-on-init :: <method>,
     init-name :: <byte-string>,
     harp-output?, debug-info?) => ();

define sideways method emit-library-initializer
    (back-end :: <harp-back-end>, stream, ld,
     emit-call-used :: <method>,
     emit-call-crs :: <method>,
     emit-branch-on-init :: <method>,
     init-name :: <byte-string>,
     harp-output?, debug-info?) => ()

  output-compiled-lambda(back-end, stream,
			 with-harp-emitter(back-end,
			                   stream,
					   init-name,
					   harp-debug: harp-output?,
                                           export: #t)
			       
                                 let return-tag = make-tag(back-end);
                                 emit-branch-on-init(back-end, return-tag);
                                 emit-call-used(back-end);
                                 emit-call-crs(back-end);
                                 ins--tag(back-end, return-tag);
                                 ins--rts-and-drop(back-end, 0);

			 end with-harp-emitter,
                         section: #"init-code",
                         debug-info?: debug-info?);
end method;



define method emit-gluefile-internal (back-end :: <harp-back-end>, ld, cr-names,
                                      #key harp-output? = unsupplied(),
				           assembler-output? = unsupplied(),
                                           downloadable-data? = #f,
				           debug-info? = *default-debug-info?*,
				           compilation-layer) 
                                  => (data)

  let lib-name = as-lowercase(as(<string>, library-description-emit-name(ld)));
  let name = glue-name(lib-name);
  let base-name = glue-unit-name(lib-name, downloadable-data?);
  let stream = #f;
  let dylan-library? = *compiling-dylan?*;
  let main-unit? = main-unit?(back-end);
  let data = #f;

  with-harp-outputter(back-end,
		      stream,
		      ld,
		      base: base-name,
		      harp-output?: harp-output?,
		      assembler-output?: assembler-output?,
		      model-object-protocol?: #f,
		      dynamic-linking-protocol?: *interactive-mode?*,
		      download?: downloadable-data?)

    let constant-ref = curry(ins--constant-ref, back-end);
    let imported-ref = curry(make-imported-constant-reference, back-end);
    let dylan-ref = if (dylan-library?) constant-ref else imported-ref end;
    let lds = library-description-used-descriptions(ld);
    let used-glue-names = map(library-description-glue-name, lds);
    let cr-init-names = cr-init-names(ld, cr-names);

    let initialize-library? =
      ins--indirect-constant-ref(back-end,
				 raw-mangle(back-end,
					    as-lowercase(format-to-string("%%%s-library-booted?",
								  lib-name))));

	local method emit-call-used (back-end :: <harp-back-end>)
			// initialize all used libraries
			let init-names = map(imported-ref, used-glue-names);
			for (name in init-names)
			  output-external(back-end, stream, name);
			  ins--call(back-end, name, 0);
			end for;
		  end method emit-call-used,

	  method emit-call-crs (back-end :: <harp-back-end>)
		// initialize all CRs for this  library
		let init-names = map(constant-ref, cr-init-names);
		for (name in init-names)
		  output-external(back-end, stream, name);
		  ins--call(back-end, name, 0);
		end for;
		if (dylan-library?)
		  without-dependency-tracking
			let install-boot-symbols =
			constant-ref(emit-name(back-end, #f, ^iep(dylan-value(#"%install-boot-symbols"))));
		    ins--register-external(back-end, install-boot-symbols);
		    ins--call(back-end, install-boot-symbols, 0);
		  end without-dependency-tracking;
	    end if;
      end method emit-call-crs,

	  method emit-branch-on-init (back-end :: <harp-back-end>, return-tag :: <tag>)
		ins--bne(back-end, return-tag, initialize-library?, $false);
		ins--move(back-end, initialize-library?, $true);
	  end method emit-branch-on-init;

      with-harp-variables(back-end)

      dynamic-bind (*emitting-init-code?* = #t,
		    $true   = dylan-ref($%true),
		    $false  = dylan-ref($%false))

        emit-header(back-end, stream);

        output-external(back-end, stream, $false);
        output-external(back-end, stream, $true);

	emit-library-imported-data(back-end, stream, ld,
				   compilation-layer: compilation-layer);

	emit-glue-data(back-end, stream, ld);

        output-variable(back-end, stream, initialize-library?, $false,
			section: #"variables", export?: #f);
	emit-data-footer(back-end, stream, initialize-library?);
        output-code-start(back-end, stream);

	emit-library-initializer(back-end, stream, ld,
                                 emit-call-used, emit-call-crs,
                                 emit-branch-on-init,
                                 name, harp-output?, debug-info?);

	emit-shared-library-entry-points
	  (back-end, stream, ld,
	   harp-output?: harp-output?,
	   debug-info?: debug-info?);

	unless (main-unit?)
	  emit-executable-entry-points
	    (back-end, stream, ld,
	     harp-output?: harp-output?,
	     debug-info?: debug-info?);
	end;

	for (init-name in used-glue-names,
	     library-name in lds)
	  cache-import-in-library(back-end, init-name, library-name);
	end for;
	emit-imports(back-end, base-name, ld);

	emit-footer(back-end, stream);

      end dynamic-bind;

      end with-harp-variables;

    if (downloadable-data?)
      data := outputter-downloadable-data(back-end, *harp-outputter*);
    end if;

  end with-harp-outputter;

  if (main-unit?)

  let base-name = main-unit-name(lib-name, downloadable-data?);

  with-harp-outputter(back-end,
		      stream,
		      ld,
		      base: base-name,
		      harp-output?: harp-output?,
		      assembler-output?: assembler-output?,
		      model-object-protocol?: #f,
		      dynamic-linking-protocol?: *interactive-mode?*,
		      download?: downloadable-data?)
    
      with-harp-variables(back-end)

      dynamic-bind (*emitting-init-code?* = #t)

        emit-header(back-end, stream);

        output-code-start(back-end, stream);

	emit-executable-entry-points
	  (back-end, stream, ld,
	   harp-output?: harp-output?,
	   debug-info?: debug-info?);

	emit-imports(back-end, base-name, ld);

	emit-footer(back-end, stream);

      end dynamic-bind;

      end with-harp-variables;

    if (downloadable-data?)
      assert(~data, "downloadable data with main unit not supported yet");
      data := outputter-downloadable-data(back-end, *harp-outputter*);
    end if;

  end with-harp-outputter;

  end if;

  data
end method;


define open generic emit-glue-data
    (back-end :: <harp-back-end>, stream, ld) => ();

define sideways method emit-glue-data
    (back-end :: <harp-back-end>, stream, ld) => ()
  // do nothing by default
end method;


define open generic emit-executable-entry-points
    (back-end :: <harp-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ();

define sideways method emit-executable-entry-points
    (back-end :: <harp-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()

  let constant-ref = curry(ins--constant-ref, back-end);
  let lib-name = as-lowercase(as(<string>, library-description-emit-name(ld)));
  let name = glue-name(lib-name);
  let name-ref = constant-ref(name);
  let dylan-library? = *compiling-dylan?*;
  let mangled-lib-name = harp-raw-mangle(lib-name);
  let init-dylan-library = ins--indirect-constant-ref(back-end, "_init_dylan_library");
  let dylandllentry = constant-ref(shared-library-runtime-entry-point-name(back-end));

  output-external(back-end, stream, init-dylan-library);
  output-external(back-end, stream, dylandllentry);

  let dllentry = 
    invoke-harp(back-end,
		method(back-end :: <harp-back-end>)
		    ins--move(back-end, init-dylan-library, name-ref);
		    ins--jmp(back-end, dylandllentry, 1);
		end method,
		shared-library-entry-point-name(back-end, mangled-lib-name),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, dllentry,
			 section: #"init-code",
			 debug-info?: debug-info?);

  let dylanexeentry = constant-ref(c-name(back-end, "dylan_main"));
  output-external(back-end, stream, dylanexeentry);
  let exeentry = 
    invoke-harp(back-end,
		method(back-end :: <harp-back-end>)
		    ins--move(back-end, init-dylan-library, name-ref);
		    ins--jmp(back-end, dylanexeentry, 1);
		end method,
		c-name(back-end, concatenate(mangled-lib-name, "Exe")),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, exeentry,
			 section: #"init-code",
			 debug-info?: debug-info?);

  let dylan-main = c-name(back-end, "dylan_main_0");
  let dylanexeentry = constant-ref(dylan-main);
  output-external(back-end, stream, dylanexeentry);

  let exeentry = 
    invoke-harp(back-end,
		method(back-end :: <harp-back-end>)
		    ins--move(back-end, init-dylan-library, name-ref);
		    ins--jmp(back-end, dylanexeentry, 1);
		end method,
		c-name(back-end, concatenate(mangled-lib-name, "Exe0")),
		section: #"init-code",
		harp-debug: harp-output?,
		export: #f);

  output-compiled-lambda(back-end, stream, exeentry,
			 section: #"init-code",
			 debug-info?: debug-info?);

  unless (dylan-library?)
    cache-import-in-library(back-end, dylan-main, dylan-library-description());
  end unless;

end method;

define open generic emit-shared-library-entry-points
    (back-end :: <harp-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ();

define sideways method emit-shared-library-entry-points
    (back-end :: <harp-back-end>, stream, ld,
     #key harp-output?, debug-info?) => ()
end method;


define method cr-init-names (ld, cr-names)
  concatenate
    (map(method (cr) 
           concatenate(cr-init-name(ld, cr), $system-init-code-tag);
         end,
         cr-names),
     map(method (cr) 
           concatenate(cr-init-name(ld, cr), $user-init-code-tag);
         end,
         cr-names))
end method;

define method glue-name-raw (name :: <byte-string>)
  concatenate("_Init_", name)
end method;

define method glue-name (name)
  glue-name-raw(harp-local-mangle(as-lowercase(as(<string>, name))))
end method;

define method library-description-glue-name (ld)
  glue-name(library-description-emit-name(ld))
end method;

// define method used-glue-names (ld)
//   map(library-description-glue-name, library-description-used-descriptions(ld))
// end method;

define sideways method output-basename
    (back-end :: <harp-back-end>, t :: <makefile-target>, basename :: <string>)
 => (harp-basename)
  basename
end method;


// 
// Support for Dynamic linking of Dylan derived implementation objects
// 
// An attempt is made to only import/export language bindings, and all
// other objects are fixed up at startup time by doing a number of
// indirections off that
// 
// Gluefile generator emits Binary "dyimp" section for dynamic linking
// in interactive compilation mode only; in batch compilation this has
// to be delayed until link-time in the build-system in order to support
// DLL Unification of Dylan libraries
// 
// 


// 
// Definition of offset masks -- an encoding of a byte sequence of 
// runtime indirections from parent to derived object
// 
// These are set up lazily during first compilation session
// 

define constant $offset-width = 8;
define constant $offset-mask = ash(-1, $offset-width);


define variable *mep-offset-mask* = #f;

define inline function mep-offset-mask ()
 => (offset-mask :: <integer>)
  *mep-offset-mask*
  | (begin
       *mep-offset-mask* :=
	 logior(mep-runtime-slot-offset() + 1, $offset-mask);
     end);
end;

define variable *iep-offset-mask* = #f;

define inline function iep-offset-mask ()
 => (offset-mask :: <integer>)
  *iep-offset-mask*
  | (begin
       *iep-offset-mask* :=
	 logior(iep-runtime-slot-offset() + 1, $offset-mask);
     end);
end;


define method function-offset-mask
      (back-end :: <harp-back-end>, o :: <&method>) => (i :: <integer>)
  mep-offset-mask();
end method;

define method function-offset-mask
      (back-end :: <harp-back-end>, o :: <&keyword-method>) => (i :: <integer>)
  iep-offset-mask();
end method;


define variable *method-offset-mask* = #f;

define inline function method-offset-mask ()
 => (offset-mask :: <integer>)
  *method-offset-mask*
  | (begin
       *method-offset-mask* :=
	 add-offset-mask(-1,
			 generic-function-methods-runtime-slot-offset() + 1,
			 0);
     end);
end;

define inline method add-offset-mask
    (mask :: <integer>, field :: <integer>, pos :: <integer>)
 => (mask :: <integer>)
  let offset = pos * $offset-width;
  logand(mask,
	 logior(ash(field, offset), offset-mask(offset)));
end;

define inline function offset-mask
    (offset :: <integer>) => (mask :: <integer>)
    lognot(ash(#xff, offset))
end function;

define method add-offset-masks(mask :: <integer>, #rest fields)
 => (mask :: <integer>)
  let result :: <integer> = mask;
  for (i :: <integer> from 0 below fields.size)
    let field :: <integer> = fields[i];
    result := add-offset-mask(result, field, i);
  end;
  result
end;

define constant $method-byte-offset-max = floor/(#xfe, 3) + 1;
define constant $method-byte-offset-fill = #xfe;

define method generic-method-offset-mask
    (mask :: <integer>, method-number :: <integer>, pos :: <integer>)
 => (mask :: <integer>)
  let factor :: <integer> = floor/(method-number, $method-byte-offset-max);
  let result :: <integer> = mask;

  for (i :: <integer> from 0 below factor)
    result := add-offset-mask(result,
			      $method-byte-offset-fill,
			      i + pos);
  end;
  add-offset-mask(result,
		  generic-method-offset(method-number - factor * $method-byte-offset-max),
		  factor + pos);
end method;

define inline method generic-method-offset(method-number :: <integer>)
 => (offset :: <integer>)
  1 + 3 * method-number
end method;

define inline method generic-method-offset?(method-number :: <integer>)
 => (encodeable? :: <boolean>)
  method-number < 2 * $method-byte-offset-max
end method;


define variable *wrapper-offset-mask* = #f;

define inline function wrapper-offset-mask ()
 => (offset-mask :: <integer>)
  *wrapper-offset-mask*
  | (begin
       *wrapper-offset-mask* :=
	 add-offset-masks(-1,
			  class-implementation-class-runtime-slot-offset() + 1,
			  class-mm-wrapper-runtime-slot-offset() + 1);
     end);
end;

define variable *class-constructor-offset-mask* = #f;

define inline function class-constructor-offset-mask ()
 => (offset-mask :: <integer>)
  *class-constructor-offset-mask*
  | (begin
       *class-constructor-offset-mask* :=
	 add-offset-masks(-1,
			  class-implementation-class-runtime-slot-offset() + 1,
			  class-constructor-runtime-slot-offset() + 1);
     end);
end;


// Emitters of the imported data fixups

define macro with-harp-imports-emitter
  { with-harp-imports-emitter (?description:expression) ?:body end }
    => {
	if (*interactive-mode?*)
	  ?body
	else
	  with-build-area-output (?=stream = ?description, name: "_imports.import")
	    ?body
	  end;
	end;
	}
end macro;

define open generic emit-library-imported-data
    (back-end :: <harp-back-end>, stream, description :: <library-description>,
     #key compilation-layer)
 => ();

define method emit-library-imported-data
    (back-end :: <harp-back-end>, stream, description :: <library-description>,
     #key compilation-layer)
 => ()
  unless (*compiling-dylan?*)

  with-harp-imports-emitter(description)

  let seen :: <table> = make(<table>);
  let first-cr = #f;
  let crs =
      if (*interactive-mode?*)
	compilation-context-records(compilation-layer)
      else
	let combined-cr =
	  library-description-combined-record(description);
	(combined-cr & list(combined-cr))
	| compilation-context-records(description);
      end;

  for (cr :: <compilation-record> in crs,
       first? = #t then #f)

    with-dependent ($compilation of cr)

    if (first?) first-cr := cr end;
    let heap = cr.compilation-record-model-heap;
    let objects
      = if (heap) 
	  heap.heap-referenced-objects
	else 
	  compilation-record-heap-referenced-objects(cr);
	end if;

    // dynamic-bind (*current-heap*          = heap)

      for (object in objects)
        emit-imported-data(back-end, stream, object, seen, first?);
      end for;

    // end dynamic-bind;

    end with-dependent;

  end for;

  unless (*interactive-mode?*)

    if (first-cr)

      with-dependent ($compilation of first-cr)

      // Registration of dylan constants that may be referenced out-of-heap
      emit-imported-data(back-end, stream, ^iep(dylan-value($symbol-fixup-name)), seen, #f,
			 import?: #t);
      emit-imported-data(back-end, stream, ^iep(dylan-value(#"unbound-instance-slot")), seen, #f,
			 import?: #t);
      emit-imported-data(back-end, stream, ^iep(dylan-value(#"type-check-error")), seen, #f,
			 import?: #t);

      format(stream, "\n");

      end;

    end;

  end unless;

  end with-harp-imports-emitter;

  end unless;
end method;


define macro emit-import-method-definer
  { define ?options emit-import-method ?:name (?class:name) ?:body end }
    =>
  {
    
   define ?options method ?name
       (?=back-end :: <harp-back-end>, ?=stream, ?=o :: ?class, ?=seen :: <table>, ?=first?,
	#key import?)
    => (emitted? :: <boolean>)
     let seen? =
       unless (?=first?)
         element(?=seen, ?=o, default: #f);
       end;
     let ?=emitted? :: <boolean> = #f;

     if (seen?)
       seen? == #"emitted"
     else

	 let import? = import? | dll-imported-object?(?=back-end, ?=o);

       if (import?)
	 ?body
       end if;

       if (?=emitted?) ?=seen[?=o] := #"emitted"; #t
       else ?=seen[?=o] := #t; #f end;

     end if;
   end method

  }

options:

    {} => {}

    {?:name} => {?name}
end macro;

define method emit-imported-data
    (back-end :: <harp-back-end>, stream, o, seen :: <table>, first?,
     #key) => (emitted? :: <boolean>)
end method;



// IEPs are derived from methods

define emit-import-method emit-imported-data (<&iep>)

  unless (model-externally-visible??(o))

    let name = emit-imported-name(back-end, stream, o);
    let f :: <&method> = o.function;
    let f-name = emit-name(back-end, stream, f);

    let emitted?? = emit-imported-data(back-end, stream, f, seen, #f, import?: #t);

    unless (emitted??)
      cache-import-in-library(back-end, f-name, model-library-description(f));
    end;

    output-imported-data(back-end, stream,
			 name, f-name, function-offset-mask(back-end, f));

    emitted? := #t;

  end unless;

end emit-import-method;


// methods are derived from generic methods list or bindings

define emit-import-method emit-imported-data (<&method>)

  let (internal?, method-number?) = model-internal-only?(o);

  if (internal?)

    if (method-number?)

      let method-number :: <integer> = method-number?;
      let name = emit-imported-name(back-end, stream, o);
      let gf :: <&generic-function> = internal?;
      let gf-name :: <byte-string> = emit-name(back-end, stream, gf);
      let gf-sealed? = gf.^generic-function-sealed?;
      let gf-root =
	if (gf-sealed?) gf-name
	else
	  emit-generic-methods-name(back-end, stream, gf-name);
	end;

      let emitted?? =
	gf-sealed? & emit-imported-data(back-end, stream, gf, seen, #f, import?: #t);

      unless (emitted??)
	cache-import-in-library(back-end, gf-root, model-library-description(gf));
      end;

      let offset-mask :: <integer> =
	if (gf-sealed?) method-offset-mask()
	else $offset-mask end;
      let offset :: <integer> =
	generic-method-offset-mask(offset-mask, method-number, 1);

      output-imported-data(back-end, stream,
			   name, gf-root,
			   offset);
    else
      let binding :: <module-binding> = internal?;
      emit-imported-binding-data(back-end, stream, o, binding);
    end if;

    emitted? := #t;
  end if;

end emit-import-method;


// class-constructor methods are derived from classes

define emit-import-method emit-imported-data (<&initializer-method>)

  let class :: <&class> = o.^function-signature.^signature-values.first;

  let name = emit-imported-name(back-end, stream, o);
  let class-name = emit-name(back-end, stream, class);

  let emitted?? = emit-imported-data(back-end, stream, class, seen, #f, import?: #t);

  unless (emitted??)
    cache-import-in-library(back-end, class-name, model-library-description(class));
  end;

  output-imported-data(back-end, stream,
		       name, class-name, class-constructor-offset-mask());
  emitted? := #t;

end emit-import-method;

define inline method emit-imported-binding-data
    (back-end :: <harp-back-end>, stream, o, binding :: <module-binding>) => ()

  let name = emit-imported-name(back-end, stream, o);
  let binding-name = emit-reference(back-end, stream, binding);

  cache-import-in-library(back-end, binding-name, model-library-description(o));

  output-imported-data(back-end, stream,
		       name, binding-name, $offset-mask);

end method;

define inline emit-import-method emit-imported-data-with-binding (<object>)

  let internal? = model-internal-only?(o);

  if (internal?)

    let binding :: <module-binding> = internal?;
    emit-imported-binding-data(back-end, stream, o, binding);

    emitted? := #t;

  end if;

end emit-import-method;


// generics are derived from bindings

define method emit-imported-data
    (back-end :: <harp-back-end>, stream, o :: <&generic-function>, seen :: <table>, first?,
     #key import?)
 => (emitted? :: <boolean>)
  emit-imported-data-with-binding
  (back-end, stream, o, seen, first?, import?: import?);
end method;


// classes are derived from bindings

define method emit-imported-data
    (back-end :: <harp-back-end>, stream, o :: <&class>, seen :: <table>, first?,
     #key import?)
 => (emitted? :: <boolean>)
  emit-imported-data-with-binding
  (back-end, stream, o, seen, first?, import?: import?);
end method;


// wrappers are derived from their classes

define emit-import-method emit-imported-data (<&mm-wrapper>)

  let class :: <&class> = o.^mm-wrapper-implementation-class.^iclass-class;

  let name = emit-imported-name(back-end, stream, o);
  let class-name = emit-name(back-end, stream, class);

  let emitted?? = emit-imported-data(back-end, stream, class, seen, #f, import?: #t);

  unless (emitted??)
    cache-import-in-library(back-end, class-name, model-library-description(class));
  end;

  output-imported-data(back-end, stream,
		       name, class-name, wrapper-offset-mask());

  emitted? := #t;

end emit-import-method;


// The emitter itself

define method output-imported-data
    (back-end :: <harp-back-end>, stream,
     name :: <byte-string>, import :: <byte-string>, offset :: <integer>) => ()
  if (*interactive-mode?*)

    output-external(back-end, stream, import, import?: #t);

    output-public(back-end, stream, name);

    output-definition(back-end, stream, name,
		      section: #"variables");

    output-data-item(back-end, stream, import,
		     import?: #t, offset: offset);

    emit-data-footer(back-end, stream, name);

  else
    format(stream, "%s\n", name);
    format(stream, "%s\n", import);
    format(stream, "%d\n", offset);
  end if;

end method;




// eof

