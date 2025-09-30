Module:       dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-llvm-back-end
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-typist;
  use release-info;
  use llvm;

  export dfmc-llvm-back-end;
end library;

define module dfmc-llvm-back-end
  use common-dylan;
  use generic-arithmetic,
    prefix: "generic/";
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-imports;
  use dfmc-back-end;
  use dfmc-typist;
  use machine-words;
  use release-info;

  use llvm;
  use llvm-builder;
  use llvm-debug;

  export
    <llvm-back-end>,
    llvm-back-end-target-triple,
    llvm-back-end-data-layout,
    llvm-section-name,

    llvm-back-end-dbg-compile-unit,
    llvm-back-end-dbg-compile-unit-setter,

    llvm-runtime-thread-local-support?,
    llvm-thread-local-support?,
    llvm-teb-struct-type,

    llvm-bef-struct-type,

    *loose-mode?*,
    *interactive-mode?*,
    llvm-retract-cached,

    $llvm-object-pointer-type,
    llvm-register-types,
    llvm-pointer-to,
    llvm-object-type,
    llvm-class-type,
    llvm-reference-type,
    llvm-lambda-type,
    llvm-c-function-type,
    llvm-entry-point-info,

    llvm-heap-fixup-entry-llvm-type,

    <llvm-primitive-descriptor>,
    primitive-attributes,
    primitive-generator,
    $llvm-primitive-descriptors,
    llvm-primitive-function,
    llvm-primitive-signature,
    llvm-emit-primitive-dbg-function,

    <llvm-runtime-variable-descriptor>,
    runtime-variable-type-name,
    runtime-variable-attributes,
    $llvm-runtime-variable-descriptors,
    llvm-runtime-variable,

    $entry-point-argument-count,
    <llvm-entry-point-descriptor>,
    entry-point-attributes,
    entry-point-generator,
    entry-point-function-declarator,
    $llvm-entry-point-descriptors,
    llvm-entry-point-function,
    llvm-entry-point-rest?,
    llvm-emit-entry-point-dbg-function,

    llvm-calling-convention,
    llvm-c-function-calling-convention,

    llvm-raw-byte-character;
end module;
