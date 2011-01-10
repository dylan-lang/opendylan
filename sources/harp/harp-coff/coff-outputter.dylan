module:    harp-coff
Synopsis:  COFF generation from the HARP outputter interface
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *write-debug-info-by-default* = #t;


//// The interface between the harp back end and the COFF builder
////
//// Specific back-ends will need to specialize this.
//// This default implementation is unashamedly Windows / Pentium
//// centric
////
//// This file defines the outputter types #"coff" and #"downloader"

define constant $coff-type$ = #"coff";

define constant $downloader-type$ = #"downloader";


define open class <harp-coff-builder>(<coff-builder>, <harp-binary-builder>)
end class;


/// Back ends may specialize FILE-EXTENSION-FOR-OUTPUTTER-TYPE
/// to get different file extensions.
///

define sideways method file-extension-for-outputter-type
       (backend :: <harp-back-end>, type == $coff-type$) 
       => (extension :: <byte-string>)
  "o";
end method;



define sideways method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $coff-type$)
    => (outputter :: <harp-coff-builder>)
  let file-string = as(<string>, filename);
  let (machine, big-endian?) = coff-machine-type(backend);
  let stream = open-output-stream(backend, file-string, type);
  let obj-name
    = concatenate(file-string, ".", 
                  file-extension-for-outputter-type(backend, type));
  let def-file = open-output-stream(backend, file-string, "def");
  let builder
    = make-binary-builder(<harp-coff-builder>,
			  machine: machine, big-endian?: big-endian?, 
			  destination: stream, def-file: def-file);
  add-source-file-definition(builder, concatenate(file-string, ".dylan"));
  initialize-debug-section(builder, obj-name);
  builder;
end method;

define sideways method make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type == $downloader-type$)
    => (outputter :: <harp-coff-builder>)
  let file-string = as(<string>, filename);
  let (machine, big-endian?) = coff-machine-type(backend);
  let builder
    = make-binary-builder(<harp-coff-builder>,
			  machine: machine, big-endian?: big-endian?, 
			  destination: #f, def-file: #f);
  add-source-file-definition(builder, concatenate(file-string, ".dylan"));
  builder;
end method;



define method outputter-downloadable-data
    (be :: <harp-back-end>, outputter :: <harp-coff-builder>) => (data)
 outputter.binary-file;
end method;


/// COFF-MACHINE-TYPE
/// May be specialized by back ends to fill in the machine type field in
/// a COFF file.
///
define open generic coff-machine-type
    (backend :: <harp-back-end>) 
    => (machine :: <integer>, big-endian? :: <boolean>);

define method coff-machine-type
    (backend :: <harp-back-end>) 
    => (machine :: <integer>, big-endian? :: <boolean>)
  values(#x14c, #f);
end method;



define method output-glue-symbols
    (be :: <harp-back-end>, builder :: <harp-coff-builder>,
     #key data-start = $data-start-symbol,
          data-end = $data-end-symbol,
          variables-start = $vars-start-symbol,
          variables-end = $vars-end-symbol,
          objects-start = $objs-start-symbol,
          objects-end = $objs-end-symbol,
          fixup-start = $fixup-start-symbol,
          fixup-end = $fixup-end-symbol,
          import-start = $import-start-symbol,
          import-end = $import-end-symbol) => ()

  local method put-symbol-in-section 
            (section, symbol, #key flags = $data-flags, alignment = 4)
          select-binary-section(builder, section, flags: flags, alignment: alignment);
          add-binary-symbol-definition(builder, symbol);
        end method;

  put-symbol-in-section($data-start-section, data-start);
  put-symbol-in-section($data-end-section, data-end);
  put-symbol-in-section($vars-start-section, variables-start);
  put-symbol-in-section($vars-end-section, variables-end);
  put-symbol-in-section($objs-start-section, objects-start);
  put-symbol-in-section($objs-end-section, objects-end);
  put-symbol-in-section($fixup-start-section, fixup-start,
                        alignment: 1, flags: fixup-flags(builder));
  put-symbol-in-section($fixup-end-section, fixup-end,
                        alignment: 1, flags: fixup-flags(builder));
  put-symbol-in-section($import-start-section, import-start,
                        alignment: 1, flags: fixup-flags(builder));
  put-symbol-in-section($import-end-section, import-end,
                        alignment: 1, flags: fixup-flags(builder));
end method;



define method output-code-start 
    (be :: <harp-back-end>, builder :: <harp-coff-builder>) => ()
  // don't actually have to do anything
end method;

define method output-data-start 
    (be :: <harp-back-end>, builder :: <harp-coff-builder>) => ()
  // don't actually have to do anything
end method;


define method output-header
    (be :: <harp-back-end>, builder :: <harp-coff-builder>) => ()
end method;

define method output-footer
    (be :: <harp-back-end>, builder :: <harp-coff-builder>) => ()
  let dest = builder.destination;
  add-imported-data-fixups(builder);
  fixup-coff-builder(builder);
  if (dest) write-binary(dest, builder) end;
end method;

define method output-external 
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>,
     name :: <byte-string>,
     #key import?,
          model-object = unsupplied(),
          derived-model-object) => ()
  let import? = import? & imported-name-mangler(be);
  let coff-symbol =
    define-external-symbol(builder, name, model-object, import?: import?);
  let name = coff-symbol.coff-symbol-name;

  // if there is a derived-model-object which is a component of this one,
  // register it at the same time to prevent multiply emitting the model-object

  if (derived-model-object)
    make-coff-symbol(builder,
		     outputter-model-object-name(derived-model-object, name),
		     outputter-model-object(derived-model-object));
  end if;

end method;

define method output-external 
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     name :: <constant-reference>,
     #key import?, #all-keys) => ()
  let import? = import? | instance?(name, <imported-constant-reference>);
  let (name, model-object) = canonical-code-object(builder, name);

  define-external-symbol(builder, name, model-object,
			 import?: import? & imported-name-mangler(be));
end method;

define method output-public
    (be :: <harp-back-end>, builder :: <harp-coff-builder>, name :: <byte-string>,
     #key model-object = unsupplied(), derived-model-object,
          export? = and-force-dll-exports?(#t)) => ()
  let coff-symbol = define-public-symbol(builder, name, model-object);
  let name = coff-symbol.coff-symbol-name;
  do-export(export?, builder, name);

  // if there is a derived-model-object which is a component of this one,
  // register it at the same time to prevent multiply emitting the model-object

  if (derived-model-object)
    make-coff-symbol(builder,
		     outputter-model-object-name(derived-model-object, name),
		     outputter-model-object(derived-model-object));
  end if;

end method;

define method output-public
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     name :: <constant-reference>,
     #key export? = and-force-dll-exports?(#t),
     #all-keys) => ()

  let (name, model-object) = canonical-code-object(builder, name);
  let coff-symbol = define-public-symbol(builder, name, model-object);

  do-export(export?, builder, coff-symbol.coff-symbol-name);
end method;


define method output-definition
    (be :: <harp-back-end>,
     builder :: <harp-coff-builder>,
     name :: <byte-string>,
     #key section = #f,
          public?,
          export? = public?.and-force-dll-exports?,
          model-object = unsupplied()) => ()

  select-dylan-section(builder, section | #"data", be.code-item-increment);
  let coff-symbol = add-symbol-definition(builder, name, model-object, public?: public?);
  let name = coff-symbol.coff-symbol-name;
  do-export(export?, builder, name);

end method;

define method output-definition
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     name :: <constant-reference>,
     #rest all-keys,
     #key section,
          public?,
          export? = public?.and-force-dll-exports?,
     #all-keys) => ()

  let (name, model-object) = canonical-code-object(builder, name);

  select-dylan-section(builder, section | #"data", be.code-item-increment);
  let coff-symbol =
    add-symbol-definition(builder, name, model-object,
			  public?: public?);
  do-export(export?, builder, coff-symbol.coff-symbol-name);
end method;


define method output-comment
    (be :: <harp-back-end>, builder :: <harp-coff-builder>, comment :: <string>) 
     => ()
end method;

define method output-line-comment
    (be :: <harp-back-end>, builder :: <harp-coff-builder>, comment :: <string>) 
     => ()
end method;


// And the code generation support:



define method output-compiled-lambda
    (be :: <harp-back-end>, builder :: <harp-coff-builder>,
     lambda :: <fully-compiled-lambda>,
     #key model-object = unsupplied(),
          section = #"code",
          debug-info? = (*write-debug-info-by-default* & builder.destination))
    => ()
  let (name, model-object) = canonical-lambda-object(lambda);
  let code = lambda.lambda-code;
  let labels = lambda.lambda-labels;
  let ref-data = lambda.lambda-referenced-data;

  select-dylan-section(builder, section, be.code-item-increment);

  if (ref-data)
    // output any referenced data BEFORE the definition, because constant
    // references use a negative offset. NB we assume that there will never
    // be any padding when the name gets defined after this data, because
    // of a suitable choice of alignment.
    add-data-vector(builder, ref-data);
  end if;

  let coff-symbol =
    output-function-definition(be, builder, name, model-object, lambda, debug-info?);
  let name = coff-symbol.coff-symbol-name;

  // Now we surely have a real lambda-name
  lambda.lambda-name-internal := name;
  
  // output any external references
  for (ext :: <constant-reference> in lambda.lambda-externals)
    let import? = instance?(ext, <imported-constant-reference>);
    let (name, model-object) = canonical-code-object(builder, ext);
    define-external-symbol(builder, name, model-object,
			   import?: import? & imported-name-mangler(be));
  end for;

  let code-start-pos = builder.current-section.current-position;

  add-data-vector(builder, code);

  for (label in labels)
    let section-pos = code-start-pos + label.labelled-constant-index;
    insert-code-label(be, builder, label, name, model-object, section-pos);
  end for;

  // For Compiler Clients only, record compiled-lambda as compilation data
  output-compilation-record-data(be, name, lambda);

end method;




define method output-function-definition
    (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
     name :: <byte-string>, model-object,
     lambda :: <fully-compiled-lambda>,
     debug-info?)
    => (coff-symbol :: <coff-symbol>)
  // If we know about the source record location details, then output a
  // function definition (including the symbolic line number record)

  let (start-line, end-line, file-name) = external-lambda-location(lambda);
  let public? = lambda.lambda-is-public?;
  let export? = lambda.lambda-is-export?.and-emit-dll?;          /// !"$% temporary interface
  let representation = if (public?) 
                         #"public-function" 
                       else #"static-function" 
                       end;
  let coff-symbol =
  if (debug-info? & start-line)
    let locators = lambda.lambda-selected-locators;
    let line-count = locators.size;
    unless (builder.source-file)
      builder.source-file := file-name;
    end;
    let coff-symbol =
      add-function-line-number-definition
      (builder, name, model-object, lambda.lambda-code.size, 
       line-count, start-line, end-line, representation: representation);
    output-function-line-numbers(builder, locators);
    coff-symbol
  else
    add-symbol-definition(builder, name, model-object, representation: representation);
  end if;

  let name = coff-symbol.coff-symbol-name;

  if (debug-info?)
    // Regardless of whether there are any line numbers, we also output
    // any variable mapping information here
    output-variable-scopes(be, builder, name, model-object, lambda);
  end if;

  do-export(export?, builder, name);

  coff-symbol
end method;


define constant pointer32-type = #x403;

define method output-variable-scopes
    (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
     name :: <byte-string>, model-object,
     lambda :: <fully-compiled-lambda>)
    => ()
  // This method is for CodeView builders only.
  // Currently variable maps are only supported when there is a function frame.
  // I.e. there is no support for Frame Pointer Omission records.
  // CodeView has no support for multiple transitions between with-frame / leaf-case
  // although that may be common with HARP. If we find such a lambda, we
  // pretend to CodeView that the region with the frame includes any non-frame
  // code in the middle - but we are careful not to emit any debug info about
  // live variables in the non-frame portions of the code.

  let (with-frame?, start-offset, end-offset)
    = find-full-scope-with-frame(lambda);
  if (with-frame?)
    let add-cv4-proc-start = if (lambda.lambda-is-public?)
                               add-cv4-global-proc-start
                             else
                               add-cv4-local-proc-start
                             end;
    add-cv4-proc-start(builder, name, model-object, lambda.lambda-code.size, 
                       start-offset, end-offset, pointer32-type);
    output-function-variable-scopes(be, builder, name, model-object, lambda);
    add-cv4-end-of-block(builder);
  end if;
end method;


// find-full-scope-with-frame
// Finds an enclosing start and end offset for the region of the code that 
// has a stack frame, whether or not there are interim regions with no frame.

define method find-full-scope-with-frame 
    (lambda :: <fully-compiled-lambda>)
    => (with-frame? :: <boolean>, start-offset :: <integer>, end-offset :: <integer>)
  let scopes :: <debug-scopes> = lambda.lambda-variable-scopes-internal;
  let all-scopes :: <simple-object-vector> = lambda.lambda-all-variable-scopes;
  let with-frame? = #f;
  let start-offset = 0;
  let end-offset = 0;
  for-debug-scope (scope in scopes of all-scopes)
    if (scope.debug-scope-with-frame?)
      end-offset := scope.end-code-offset;
      unless (with-frame?)
        with-frame? := #t;
        start-offset := scope.start-code-offset;
      end unless;
    end if;
  end for-debug-scope;
  values(with-frame?, start-offset, end-offset);
end method;



define method output-variables-for-scope
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, scope :: <debug-scope>,
      all-names :: <simple-object-vector>)
     => ()
  for-debug-var (var in scope.named-variables of all-names)
    output-variable-location(be, builder, var);
  end for-debug-var;
end method;


define method output-variable-location
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, var :: <named-variable>) => ()
  // Don't know about this type of variable - so do nothing
end method;


define method output-variable-location
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
      var :: <named-variable-in-spill>) => ()
  add-cv4-bp-relative(builder, var.variable-frame-pointer-offset,
                      pointer32-type, var.variable-name);
end method;


define method output-variable-location
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
      var :: <named-variable-in-register>) => ()
  add-cv4-register(builder, pointer32-type, 
                   var.variable-register-enumeration,
                   var.variable-name);
end method;



define method output-one-scope
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
      name :: <byte-string>, model-object,
      scope :: <debug-scope>,
      all-scopes :: <simple-object-vector>,
      all-names :: <simple-object-vector>) 
     => ()
  if (scope.debug-scope-with-frame?)
    if (scope.named-variables.empty-variables?)
      output-nested-scopes(be, builder, name, model-object, scope, all-scopes, all-names);
    else
      let start = scope.start-code-offset;
      let length = scope.end-code-offset - start;
      add-cv4-block-start(builder, name, model-object,
			  "", length, offset: start);
      output-variables-for-scope(be, builder, scope, all-names);
      output-nested-scopes(be, builder, name, model-object, scope, all-scopes, all-names);
      add-cv4-end-of-block(builder);
    end if;
  end if;
end method; 


define method output-nested-scopes
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
      name :: <byte-string>, model-object,
      scope :: <debug-scope>,
      all-scopes :: <simple-object-vector>,
      all-names :: <simple-object-vector>) 
     => ()
  for-debug-scope (scope in scope.nested-scopes of all-scopes)
    output-one-scope(be, builder, name, model-object, scope, all-scopes, all-names);
  end for-debug-scope;
end method; 


define method output-function-variable-scopes
     (be :: <harp-back-end>, builder :: <harp-coff-builder>, 
      name :: <byte-string>, model-object,
      lambda :: <fully-compiled-lambda>)
     => ()
  let scopes :: <debug-scopes> = lambda.lambda-variable-scopes-internal;
  let all-scopes :: <simple-object-vector> = lambda.lambda-all-variable-scopes;
  let all-names :: <simple-object-vector> = lambda.lambda-all-variable-names;

  for-debug-scope (scope in scopes of all-scopes)
    output-one-scope(be, builder, name, model-object, scope, all-scopes, all-names);
  end for-debug-scope;
end method; 



define method output-function-line-numbers
    (builder :: <harp-coff-builder>, locators :: <vector>) => ()
  for (item in locators)
    output-line-number-item(builder, item);
  end for;
end method;


define method output-line-number-item
    (builder :: <harp-coff-builder>, item :: <relative-source-position>) => ()
  // Source code locators as used by SCL
  if (builder.source-file) // Ignore lines if we don't know the source file
    let line = item.function-relative-line-number;
    let safe-line = if (line == 0) 1 else line end; // COFF can't handle 0
    let function-start-pos = builder.current-section.current-position;
    let scl-pos = function-start-pos + item.function-relative-code-position;
    add-line-number(builder, safe-line, pos: scl-pos);
  end if;
end method;


define method insert-code-label
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     item :: <relative-address-constant>,
     name :: <byte-string>,
     model-object,
     pos :: <integer>)
  // Relative addressing as used by LEA
  // Output values relative to the start of the lambda, using
  // the function name symbol. The code already contains the relative
  // offset from this address.
  insert-relocation(builder, name, model-object, pos: pos);
end method;


define method insert-code-label
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     item :: <labelled-absolute-constant>,
     name :: <byte-string>,
     model-object,
     pos :: <integer>)
  let ref = item.labelled-constant-reference;
  insert-abs-relocation(be, builder, ref, pos);
end method;


define method insert-abs-relocation
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     ref,
     pos :: <integer>)
  let (name, model-object) = canonical-code-object(builder, ref);
  insert-relocation(builder, name, model-object, pos: pos,
		    import?: #f);
end method;

define method insert-abs-relocation
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     ref :: <imported-constant-reference>,
     pos :: <integer>)
  let (name, model-object) = canonical-code-object(builder, ref);
  insert-relocation(builder, name, model-object, pos: pos,
		    import?: imported-name-mangler(be));
end method;

define method insert-abs-relocation
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     ref :: <interactor-constant-reference>,
     pos :: <integer>)
  let handle = canonical-interactor-object(ref);
  insert-interactor-relocation(builder, handle, pos: pos);
end method;


define method insert-code-label
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     item :: <labelled-relative-constant>,
     name :: <byte-string>,
     model-object,
     pos :: <integer>)
  let ref = item.labelled-constant-reference;
  let import? = instance?(ref, <imported-constant-reference>);
  let (name, model-object) = canonical-code-object(builder, ref);
  insert-relocation(builder, name, model-object,
		    pos: pos, type: #"relative",
		    import?: import? & imported-name-mangler(be));
end method;


define method insert-code-label
    (be :: <harp-back-end>, 
     builder :: <harp-coff-builder>, 
     item :: <labelled-constant-with-opcode>,
     name :: <byte-string>,
     model-object,
     pos :: <integer>)
  let reloc-pos = pos + 1; // relocation is after the opcode
  let ref = item.labelled-constant-reference;
  let import? = instance?(ref, <imported-constant-reference>);
  let (name, model-object) = canonical-code-object(builder, ref);

  insert-relocation(builder, name, model-object,
		    pos: reloc-pos, type: #"relative",
		    import?: import? & imported-name-mangler(be));
end method;


/// Support for imported constants

define open generic imported-name-mangler 
    (back-end :: <harp-back-end>) => (mangler :: <function>);


// Imported names are now only created at binary-symbol creation time because
// only then do we have for sure a real name in hand

define method imported-name-mangler 
    (back-end :: <harp-back-end>) => (mangler :: <function>)
  $imported-name-mangler
end method;



// If we are using model-objects as our canonical-object,
// we do not know the name of our object until a coff-symbol
// is first defined for it, or later retrieved for it

define method coff-symbol-name(cs :: <coff-symbol>) => (name :: <string>)
  cs.symbol-name.string-data
end method;


define sideways method builder-model-object-name(model-object, name :: <string>)
 => (name :: <string>)
  model-object-name(model-object, name);
end method;
