Module:       dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
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
  use llvm;

  export dfmc-llvm-back-end;
end library;

define module dfmc-llvm-back-end
  use common-dylan, exclude: { format-to-string };
  use generic-arithmetic,
    prefix: "generic/";
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-imports;
  use dfmc-back-end;
  use machine-words;

  use llvm;
  use llvm-builder;
  use llvm-debug;

  export
    <llvm-back-end>,
    llvm-back-end-target-triple,
    llvm-back-end-data-layout,
    llvm-section-name,

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
    llvm-entry-point-type,

    <llvm-primitive-descriptor>,
    primitive-attributes,
    primitive-generator,
    $llvm-primitive-descriptors,
    llvm-primitive-function,

    <llvm-runtime-variable-descriptor>,
    runtime-variable-type-name,
    runtime-variable-init-function,
    runtime-variable-section,
    runtime-variable-global,
    runtime-variable-global-setter,
    $llvm-runtime-variable-descriptors,

    <llvm-entry-point-descriptor>,
    entry-point-attributes,
    entry-point-generator,
    entry-point-function-declarator,
    $llvm-entry-point-descriptors,
    llvm-entry-point-function,
    llvm-entry-point-rest?,

    llvm-calling-convention,

    llvm-raw-byte-character;
end module;
