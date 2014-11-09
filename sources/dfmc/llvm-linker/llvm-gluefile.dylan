Module: dfmc-llvm-linker
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method emit-mainfile
    (back-end :: <llvm-back-end>, ld :: <library-description>,
     #rest keys, #key, #all-keys)
  let locator
    = build-area-output-locator(ld, base: "_main", type: "bc");
  let m = make(<llvm-module>,
               name: "_main",
               target-triple: llvm-back-end-target-triple(back-end),
               data-layout: llvm-back-end-data-layout(back-end));
  back-end.llvm-builder-module := m;

  // Compute the function type for main()
  let c-int-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-c-signed-int>"));
  let c-pointer-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-c-pointer>"));
  let main-function-type
    = make(<llvm-function-type>,
           return-type: c-int-type,
           parameter-types: vector(c-int-type, c-pointer-type),
           varargs?: #f);
  let main-function-ptr-type = llvm-pointer-to(back-end, main-function-type);

  // Generate argument values
  let main-argc
    = make(<llvm-argument>, type: c-int-type, name: "argc", index: 0);
  let main-argv
    = make(<llvm-argument>, type: c-pointer-type, name: "argv", index: 1);

  // Generate the main function
  block ()
    back-end.llvm-builder-function
      := make(<llvm-function>,
              name: "main",
              type: main-function-ptr-type,
              arguments: vector(main-argc, main-argv),
              linkage: #"external",
              calling-convention: $llvm-calling-convention-c);
    ins--local(back-end, main-argc.llvm-argument-name, main-argc);
    ins--local(back-end, main-argv.llvm-argument-name, main-argv);
    ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));

    // Store argc and argv
    ins--store(back-end, main-argc,
               llvm-runtime-variable(back-end, m, #"*argc*"));
    ins--store(back-end, main-argv,
               llvm-runtime-variable(back-end, m, #"*argv*"));

    // Call the library's glue function
    let my-glue
      = make(<llvm-function>,
             name: library-description-glue-name(back-end, ld),
             type: $init-code-function-ptr-type,
             arguments: #(),
             linkage: #"external",
             section: llvm-section-name(back-end, #"init-code"),
             calling-convention: $llvm-calling-convention-c);
    llvm-builder-declare-global(back-end, my-glue.llvm-global-name, my-glue);
    ins--call(back-end, my-glue, #[]);

    // Return 0
    let zero = make(<llvm-integer-constant>, type: c-int-type, integer: 0);
    ins--ret(back-end, zero);

    llvm-builder-define-global(back-end, "main",
                               back-end.llvm-builder-function);
  cleanup
    back-end.llvm-builder-function := #f;
  end block;

  // Output LLVM bitcode
  llvm-save-bitcode-file(m, locator);

  // Retract
  back-end.llvm-builder-module := #f;
end;

define sideways method emit-gluefile
    (back-end :: <llvm-back-end>, ld :: <library-description>, cr-names,
     #key assembler-output? = unsupplied(), 
          downloadable-data? = #f,
          debug-info? = #t,
          compilation-layer,
     #all-keys)
  let locator
    = build-area-output-locator(ld, base: "_glue", type: "bc");
  let m = make(<llvm-module>,
               name: "_glue",
               target-triple: llvm-back-end-target-triple(back-end),
               data-layout: llvm-back-end-data-layout(back-end));
  back-end.llvm-builder-module := m;

  let glue-name = library-description-glue-name(back-end, ld);

  if (dylan-library-library-description?(ld))
    // Call _Init_Run_Time() as a constructor just like the other system
    // init functions
    let init-run-time-global
      = make(<llvm-function>,
             name: "_Init_Run_Time",
             type: $init-code-function-ptr-type,
             arguments: #(),
             linkage: #"external");
    llvm-builder-declare-global(back-end,
                                init-run-time-global.llvm-global-name,
                                init-run-time-global);
    emit-ctor-entry(back-end, m,
                    $system-init-ctor-priority,
                    init-run-time-global);
  end if;

  // Add a flag to check whether the library has been initialized or not
  let i8-zero
    = make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 0);
  let init-flag-global
    = make(<llvm-global-variable>,
           name: concatenate(glue-name, ".init"),
           type: $llvm-i8*-type,
           initializer: i8-zero,
           constant?: #f,
           linkage: #"internal");
  llvm-builder-define-global(back-end,
                             init-flag-global.llvm-global-name,
                             init-flag-global);

  // Generate the library glue function
  block ()
    back-end.llvm-builder-function
      := make(<llvm-function>,
              name: glue-name,
              type: $init-code-function-ptr-type,
              arguments: #(),
              linkage: #"external",
              section: llvm-section-name(back-end, #"init-code"),
              calling-convention: $llvm-calling-convention-c);
    ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));

    let init-bb = make(<llvm-basic-block>);
    let return-bb = make(<llvm-basic-block>);

    // Check the init flag
    let flag-value = ins--load(back-end, init-flag-global);
    let cmp = ins--icmp-eq(back-end, flag-value, i8-zero);
    ins--br(back-end, cmp, init-bb, return-bb);

    ins--block(back-end, init-bb);
    // Set the init flag
    let i8-one
      = make(<llvm-integer-constant>, type: $llvm-i8-type, integer: 1);
    ins--store(back-end, i8-one, init-flag-global);

    // Emit calls to glue functions of referenced libraries
    for (used-ld in library-description-used-descriptions(ld))
      let used-glue
        = make(<llvm-function>,
               name: library-description-glue-name(back-end, used-ld),
               type: $init-code-function-ptr-type,
               arguments: #(),
               linkage: #"external",
               section: llvm-section-name(back-end, #"init-code"),
               calling-convention: $llvm-calling-convention-c);
      llvm-builder-declare-global(back-end, used-glue.llvm-global-name,
                                    used-glue);
      ins--call(back-end, used-glue, #[]);
    end for;

    // Emit calls to user init functions of compilation records
    for (cr-name in cr-names)
      let init-function
        = make(<llvm-function>,
               name: concatenate(cr-init-name(back-end, ld, cr-name),
                                 $user-init-code-tag),
               type: $init-code-function-ptr-type,
               arguments: #(),
               linkage: #"external",
               section: llvm-section-name(back-end, #"init-code"),
               calling-convention: $llvm-calling-convention-c);
      llvm-builder-declare-global(back-end, init-function.llvm-global-name,
                                  init-function);
      ins--call(back-end, init-function, #[]);
    end for;

    // Call %install-boot-symbols if this is the Dylan library
    if (dylan-library-library-description?(ld))
      without-dependency-tracking
        let install-boot-symbols = ^iep(dylan-value(#"%install-boot-symbols"));
        emit-extern(back-end, m, install-boot-symbols);
        let ibs-global
          = llvm-builder-global(back-end,
                                emit-name(back-end, m, install-boot-symbols));
        let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
        ins--call(back-end, ibs-global,
                  vector(undef, undef),
                  calling-convention:
                    llvm-calling-convention(back-end, install-boot-symbols));
      end;
    end if;

    // Branch to common return
    ins--br(back-end, return-bb);

    // Function return
    ins--block(back-end, return-bb);
    ins--ret(back-end);

    llvm-builder-define-global(back-end, glue-name,
                               back-end.llvm-builder-function);
  cleanup
    back-end.llvm-builder-function := #f;
  end block;

  // Output LLVM bitcode
  llvm-save-bitcode-file(m, locator);

  // Retract
  back-end.llvm-builder-module := #f;
end;


/// Compilation record init function naming

define method glue-name-raw (name :: <byte-string>)
  concatenate("_Init_", name)
end method;

define method glue-name (back-end :: <llvm-back-end>, name)
  glue-name-raw(local-mangle(back-end, as-lowercase(as(<string>, name))))
end method;

define method library-description-glue-name (back-end :: <llvm-back-end>, ld)
  glue-name(back-end, library-description-emit-name(ld))
end method;

define method cr-init-name (back-end :: <llvm-back-end>, ld, cr-name)
  concatenate(library-description-glue-name(back-end, ld), "_X_",
	      local-mangle(back-end, cr-name))
end method;
