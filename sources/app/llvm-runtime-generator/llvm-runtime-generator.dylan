Module:       llvm-runtime-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
      #"<unbound>"];

// Well-known objects referenced by the generated runtime
define constant $runtime-referenced-objects
  = #[#"%unbound",              // Value stored in uninitialized slots
      #"%true",                 // #t
      #"%false",                // #f
      #"%empty-list",           // #()
      #"%empty-vector",         // #[]
      #"%empty-string"];        // ""

define function generate-runtime-variable
    (be :: <llvm-back-end>, m :: <llvm-module>,
     name :: <symbol>, descriptor :: <llvm-runtime-variable-descriptor>)
 => ();
  let mangled-name = raw-mangle(be, as(<string>, name));
  let type = llvm-reference-type
               (be, dylan-value(descriptor.runtime-variable-type-name));
  let init-value = descriptor.runtime-variable-init-function();
  let linkage = #"external";
  let global
    = make(<llvm-global-variable>,
           name: mangled-name,
           type: llvm-pointer-to(be, type),
           initializer: emit-reference(be, m, init-value),
           constant?: #f,
           linkage: linkage,
           section: llvm-section-name(be, descriptor.runtime-variable-section));
  descriptor.runtime-variable-global := global;
  llvm-builder-define-global(be, mangled-name, global);
end function;

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

  // Emit runtime variable definitions
  for (descriptor :: <llvm-runtime-variable-descriptor>
         keyed-by name :: <symbol>
         in $llvm-runtime-variable-descriptors)
    generate-runtime-variable(be, m, name, descriptor);
  end for;
end function;

define function generate-runtime-primitive
    (be :: <llvm-back-end>, m :: <llvm-module>,
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

    // Generate the function body
    let (#rest results)
      = apply(descriptor.primitive-generator, be, arguments);

    // Generate the function return
    let return-type = function-type.llvm-function-type-return-type;
    if (llvm-void-type?(return-type))
      ins--ret(be);
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
    (be :: <llvm-back-end>, m :: <llvm-module>) => ();
  // Generate a function for each defined runtime primitive
  for (descriptor :: <llvm-primitive-descriptor>
         keyed-by name :: <symbol>
         in $llvm-primitive-descriptors)
    if (descriptor.primitive-generator)
      generate-runtime-primitive(be, m, name, descriptor);
    end if;
  end for;
end function;

define function generate-runtime
    (lid-locator :: <file-locator>,
     processor :: <symbol>,
     operating-system :: <symbol>)
 => ();
  with-booted-dylan-context (lid-locator: lid-locator,
                             back-end: #"llvm",
                             processor: processor,
                             operating-system: operating-system)
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
        generate-runtime-primitives(back-end, m);

        let output-basename
          = format-to-string("%s-%s-runtime", processor, operating-system);
        let output-locator
          = make(<file-locator>, base: output-basename, extension: "bc");
        llvm-save-bitcode-file(m, output-locator);
      end with-back-end-initialization;
    end without-dependency-tracking;
  end;
end function;

begin
  let arguments = application-arguments();
  if (arguments.size = 2)
    let lid-locator = as(<file-locator>, arguments[0]);
    let name = arguments[1];

    // Split name into processor/architecture and os portions
    let separator-position = position(name, '-');
    let processor-name = copy-sequence(name, end: separator-position);
    let os-name = copy-sequence(name, start: separator-position + 1);

    // Generate runtime support for the requested platform
    generate-runtime(lid-locator,
                     as(<symbol>, processor-name),
                     as(<symbol>, os-name));
  else
    format(*standard-error*,
           "Usage: llvm-runtime-generator dylan.lid processor-os\n");
    exit-application(1);
  end if;
end;
