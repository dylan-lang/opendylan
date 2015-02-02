Module:       llvm-runtime-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2010-2014 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Classes whose wrappers are referenced within runtime primitives
define constant $wrapper-classes
  = #[#"<machine-word>",
      #"<double-integer>",
      #"<single-float>",
      #"<double-float>",
      #"<boolean>",
      #"<simple-object-vector>",
      #"<byte-string>",
      #"<empty-list>",
      #"<unbound>",
      #"<traceable-value-cell>",
      #"<untraceable-value-cell>",
      #"<untraceable-double-value-cell>",
      #"<symbol>"];

// Well-known objects referenced by the generated runtime
define constant $runtime-referenced-objects
  = #[#"%unbound",              // Value stored in uninitialized slots
      #"%true",                 // #t
      #"%false",                // #f
      #"%empty-list",           // #()
      #"%empty-vector",         // #[]
      #"%empty-string",         // ""
      #"$absent-engine-node",
      #"$inapplicable-engine-node"];

define constant $runtime-referenced-functions
  = #[#"type-check-error",
      #"argument-count-error",
      #"odd-keyword-arguments-error",
      #"invalid-keyword-trap",
      #"unbound-instance-slot",
      #"unbound-repeated-slot",
      #"repeated-slot-getter-index-out-of-range-trap",
      #"repeated-slot-setter-index-out-of-range-trap",
      #"%slotacc-single-Q-instance-getter",
      #"%slotacc-single-Q-class-getter",
      #"%slotacc-single-Q-instance-setter",
      #"%slotacc-single-Q-class-setter",
      #"%slotacc-repeated-instance-getter",
      #"%slotacc-repeated-instance-setter",
      #"%resolve-symbol",
      #"call-application-exit-functions"];

define constant $runtime-referenced-variables
  = #[#"$direct-object-mm-wrappers"];

define constant $runtime-referenced-classes
  = #[#"<thread>",
      #"<semaphore>",
      #"<recursive-lock>",
      #"<simple-lock>",
      #"<read-write-lock>",
      #"<notification>",
      #"<simple-object-vector>"];

define constant $debug-producer = "Open Dylan LLVM Runtime Generator";

define function generate-runtime-heap
    (be :: <llvm-back-end>, m :: <llvm-module>) => ();
  // Emit external declarations for class wrappers
  for (class-name in $wrapper-classes)
    emit-extern(be, m, ^class-mm-wrapper(dylan-value(class-name)));
  end for;

  // Emit external declarations for well-known objects
  for (name in $runtime-referenced-objects)
    emit-extern(be, m, dylan-value(name));
  end;

  // Emit external declarations for referenced Dylan entry points
  for (name in $runtime-referenced-functions)
    emit-extern(be, m, dylan-value(name).^iep);
  end;

  // Emit external declarations for referenced variables
  for (name in $runtime-referenced-variables)
    emit-extern(be, m, dylan-binding(name));
  end;

  // Emit runtime variable definitions
  for (descriptor :: <llvm-runtime-variable-descriptor>
	 keyed-by name :: <symbol> in $llvm-runtime-variable-descriptors)
    llvm-runtime-variable(be, m, descriptor, initialized?: #t);
  end for;
end function;

define function generate-runtime-primitive
    (be :: <llvm-back-end>, m :: <llvm-module>,
     dbg-file :: <llvm-metadata-value>,
     name :: <symbol>, descriptor :: <llvm-primitive-descriptor>)
 => ();
  // Generate the function definition and add it to the runtime module
  let function = llvm-primitive-function(be, descriptor);
  llvm-builder-declare-global(be, function.llvm-global-name, function);

  let function-type = function.llvm-value-type.llvm-pointer-type-pointee;

  // Add function arguments to the function's value table
  let arguments = function.llvm-function-arguments;
  let value-table = function.llvm-function-value-table;
  for (argument in arguments)
    value-table[argument.llvm-argument-name] := argument;
  end for;

  block ()
    be.llvm-builder-function := function;

    // Generate the entry basic block
    ins--block(be, make(<llvm-basic-block>, name: "bb.entry"));

    // Generate debug info
    llvm-emit-primitive-dbg-function(be, function, dbg-file, descriptor);

    // Generate the function body
    let (#rest results)
      = apply(descriptor.primitive-generator, be, arguments);

    // Generate the function return
    let return-type = function-type.llvm-function-type-return-type;
    if (llvm-void-type?(return-type))
      if (be.llvm-builder-basic-block)
        ins--ret(be);
      end if;
    else
      if (results.size = 1)
        ins--ret(be, results[0]);
      else
        // Build up a struct and return it
        let result-struct
          = for (result in results, index from 0,
                 struct = make(<llvm-undef-constant>, type: return-type)
                   then ins--insertvalue(be, struct, result, index))
            finally
              struct
            end for;
        ins--ret(be, result-struct);
      end if;
    end if;
    force-output(*standard-error*);
  exception (e :: <error>)
    format(*standard-error*, "Generation of %s failed: %s\n", name, e);
    force-output(*standard-error*);

    // Terminate the final basic block
    let instructions
      = be.llvm-builder-basic-block.llvm-basic-block-instructions;
    unless (~empty?(instructions)
              & instance?(instructions.last, <llvm-terminator-instruction>))
      ins--unreachable(be);
    end unless;
  end block;
  be.llvm-builder-basic-block := #f;
  be.llvm-builder-function := #f;
end function;

define function generate-runtime-primitives
    (be :: <llvm-back-end>, m :: <llvm-module>,
     dbg-file :: <llvm-metadata-value>)
 => ();
  // Generate a function for each defined runtime primitive
  for (descriptor :: <llvm-primitive-descriptor>
         keyed-by name :: <symbol>
         in $llvm-primitive-descriptors)
    if (descriptor.primitive-generator)
      generate-runtime-primitive(be, m, dbg-file, name, descriptor);
    end if;
  end for;
end function;

define function generate-runtime-entry-point
    (be :: <llvm-back-end>, m :: <llvm-module>,
     dbg-file :: <llvm-metadata-value>,
     name :: <symbol>, descriptor :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>), pos :: false-or(<integer>))
 => ();
  // Generate the function definition and add it to the runtime module
  let function = llvm-entry-point-function(be, descriptor, count, pos: pos);
  llvm-builder-declare-global(be, function.llvm-global-name, function);

  let function-type = function.llvm-value-type.llvm-pointer-type-pointee;

  // Add function arguments to the function's value table
  let arguments = function.llvm-function-arguments;
  let value-table = function.llvm-function-value-table;
  for (argument in arguments)
    value-table[argument.llvm-argument-name] := argument;
  end for;

  block ()
    be.llvm-builder-function := function;

    // Generate the entry basic block
    ins--block(be, make(<llvm-basic-block>, name: "bb.entry"));

    // Generate debug info
    llvm-emit-entry-point-dbg-function(be, function, dbg-file,
                                       descriptor, count);

    // Generate the function body
    let (result)
      = apply(descriptor.entry-point-generator, be, count, pos, arguments);

    // Generate the function return
    if (be.llvm-builder-basic-block)
      ins--ret(be, result);
    end if;
  exception (e :: <error>)
    format(*standard-error*, "Generation of %s failed: %s\n", name, e);
    force-output(*standard-error*);

    // Terminate the final basic block
    let instructions
      = be.llvm-builder-basic-block.llvm-basic-block-instructions;
    unless (~empty?(instructions)
              & instance?(instructions.last, <llvm-terminator-instruction>))
      ins--unreachable(be);
    end unless;
  end block;

  be.llvm-builder-basic-block := #f;
  be.llvm-builder-function := #f;
end function;

define function generate-runtime-entry-points
    (be :: <llvm-back-end>, m :: <llvm-module>,
     dbg-file :: <llvm-metadata-value>)
 => ();
  // Generate functions for each defined runtime entry point
  for (descriptor :: <llvm-entry-point-descriptor>
         keyed-by name :: <symbol>
         in $llvm-entry-point-descriptors)
    if (member?(#"singular", descriptor.entry-point-attributes))
      generate-runtime-entry-point(be, m, dbg-file, name, descriptor,
                                   #f, #f);
    elseif (member?(#"cross", descriptor.entry-point-attributes))
      for (count from 1 to $entry-point-argument-count)
        for (pos from 0 below count)
          generate-runtime-entry-point(be, m, dbg-file, name, descriptor,
                                       count, pos);
        end for;
      end for;
    else
      for (count from 0 to $entry-point-argument-count)
        generate-runtime-entry-point(be, m, dbg-file, name, descriptor,
                                     count, #f);
      end for;
    end if;
  end for;
end function;

define function generate-runtime-header
    (be :: <llvm-back-end>, locator :: <pathname>)
 => ();
  with-open-file (stream = locator, direction: #"output")
    format(stream, "// Generated by llvm-runtime-generator, do not edit\n");
    format(stream, "#ifndef LLVM_PLATFORM_RUNTIME_H_\n");
    format(stream, "#define LLVM_PLATFORM_RUNTIME_H_\n\n");

    // Generate struct declarations for raw types needed by the C code
    begin
      let types
        = vector(llvm-teb-struct-type(be),
                 llvm-bef-struct-type(be));
      for (type in types)
        print-raw-struct-c-declaration(be, type, stream);
      end for;
    end;

    // Generate struct declarations for classes needed by the C code
    for (class-name in $runtime-referenced-classes)
      print-class-c-struct-declaration(be, dylan-value(class-name), stream);
    end for;

    // Generate declarations for C-callable primitives
    for (descriptor :: <llvm-primitive-descriptor>
           keyed-by name :: <symbol>
           in $llvm-primitive-descriptors)
      let model
        = if (member?(#"auxiliary", descriptor.primitive-attributes))
            dylan-value(#"<primitive>") // catch-all for auxiliary primitives
          else
            dylan-value(name)
          end;
      with-dependent-context ($compilation of model-creator(model))
        if (member?(#"c-callable", descriptor.primitive-attributes))
          let signature = llvm-primitive-signature(be, descriptor);
          format(stream, "// %s\n", name);
          print-primitive-c-function-declaration(be, name, signature, stream);
          format(stream, "\n");
        end if;
      end;
    end for;

    // Generate declarations for runtime variables
    for (descriptor :: <llvm-runtime-variable-descriptor>
           keyed-by name :: <symbol> in $llvm-runtime-variable-descriptors)
      format(stream, "// %s\n", name);
      print-runtime-variable-declaration(be, name, descriptor, stream);
      format(stream, "\n");
    end for;

    format(stream, "#endif // LLVM_PLATFORM_RUNTIME_H_\n");
  end;
end function;

define function generate-runtime
    (lid-locator :: <file-locator>,
     platform-name :: <symbol>)
 => ();
  let output-basename
    = format-to-string("%s-runtime", platform-name);
  let dummy-source-locator
    = make(<file-locator>, base: output-basename, extension: "ll",
           directory: working-directory());
  let dbg-file
    = llvm-make-dbg-file(dummy-source-locator.locator-name,
                         dummy-source-locator.locator-directory);

  with-booted-dylan-context (lid-locator: lid-locator,
                             back-end: #"llvm",
                             platform-name: platform-name)
    without-dependency-tracking
      let back-end :: <llvm-back-end> = current-back-end();
      with-back-end-initialization(back-end)
        let m = make(<llvm-module>,
                     name: "runtime",
                     target-triple: llvm-back-end-target-triple(back-end),
                     data-layout: llvm-back-end-data-layout(back-end));
        llvm-register-types(back-end, m);
        back-end.llvm-builder-module := m;

        // Write heap definitions and external declarations to the module
        generate-runtime-heap(back-end, m);

        // Write primitives definitions to the module
        generate-runtime-primitives(back-end, m, dbg-file);

        // Write entry point definitions to the module
        generate-runtime-entry-points(back-end, m, dbg-file);

        // Create the debugging compile unit
        let functions = copy-sequence(back-end.llvm-back-end-dbg-functions);
        llvm-make-dbg-compile-unit($DW-LANG-Mips-Assembler,
                                   dummy-source-locator.locator-name,
                                   dummy-source-locator.locator-directory,
                                   $debug-producer,
                                   functions: functions,
                                   module: m);

	// Write out the generated module
        let output-locator
          = make(<file-locator>, base: output-basename, extension: "bc");
        llvm-save-bitcode-file(m, output-locator);

	// Write out the generated header file
	let header-basename
	  = format-to-string("llvm-%s-runtime",
			     platform-name);
	let header-locator
	  = make(<file-locator>, base: header-basename, extension: "h");
	generate-runtime-header(back-end, header-locator)
      end with-back-end-initialization;
    end without-dependency-tracking;
  end;
end function;


/// Main program

begin
  let arguments = application-arguments();
  if (arguments.size = 2)
    let lid-locator = as(<file-locator>, arguments[0]);
    let platform-name = arguments[1];

    // Generate runtime support for the requested platform
    generate-runtime(lid-locator,
                     as(<symbol>, platform-name));
  else
    format(*standard-error*,
           "Usage: llvm-runtime-generator dylan.lid architecture-os\n");
    exit-application(1);
  end if;
end;
