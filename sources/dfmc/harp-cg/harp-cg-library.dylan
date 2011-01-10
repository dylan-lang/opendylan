Module: dylan-user
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-harp-cg
  use functional-dylan;
  use io;
  use big-integers;
  use dfmc-core;
  use dfmc-back-end;
  use dfmc-execution;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-management;

  use harp-cg-back-end;
  use native-harp;
  use mnemonic-assembler;

  export dfmc-harp-cg;
end library;

define module dfmc-harp-cg
  use functional-dylan;
  use dylan-extensions,
    import: {decode-single-float, decode-double-float,
	     <machine-word>, <double-integer>, $minimum-unsigned-machine-word},
    export: all;
  use streams-internals;
  use big-integers, prefix: "generic-", export: all;
  use dfmc-core, 
    exclude: 
      { engine-node$k-raw-byte-repeated-instance-slot-getter,
        smen$s-nrequired,
        stchen$v-checkedmask,
        engine-node$k-reserved-discriminator-b,
        engine-node$k-boxed-repeated-instance-slot-getter,
        engine-node$k-reserved-slot-a-getter,
        engine-node$k-first-slot-engine-node,
        engine-node$k-reserved-discriminator-e,
        engine-node$k-boxed-class-slot-setter,
        engine-node$k-reserved-discriminator-o,
        engine-node$k-reserved-terminal-n-a,
        engine-node$k-unrestricted-keyed-single-method,
        engine-node$k-reserved-terminal-n-g,
        engine-node$k-hashed-by-singleton-class,
        discriminator$m-argnum,
        engine-node$k-hashed-by-class,
        engine-node$k-reserved-discriminator-d,
        engine-node$k-value-object-linear-singleton,
        engine-node$k-reserved-repeated-slot-a-getter,
        engine-node$k-slot-engine-node-count,
        discriminator$v-restp,
        engine-node$k-immediate-linear-singleton,
        engine-node$k-reserved-discriminator-m,
        engine-node$k-reserved-discriminator-r,
        engine-node$k-linear-by-class,
        smen$m-restp,
        discriminator$s-nrequired,
        engine-node$k-reserved-discriminator-a,
        stchen$s-checkedmask,
        engine-node$k-reserved-terminal-n-e,
        engine-node$k-inapplicable,
        engine-node$k-profiling-cache-header,
        engine-node$k-explicit-keyed-single-method,
        engine-node$k-reserved-discriminator-n,
        engine-node$k-reserved-discriminator-g,
        engine-node$k-reserved-discriminator-p,
        engine-node$k-reserved-repeated-slot-b-setter,
        engine-node$k-reserved-discriminator-i,
        engine-node$k-reserved-terminal-n-d,
        engine-node$k-absent,
        engine-node$k-reserved-repeated-slot-a-setter,
        engine-node$k-boxed-instance-slot-setter,
        discriminator$m-restp,
        engine-node$k-unkeyed-single-method,
        discriminator$v-argnum,
        engine-node$k-immediate-hashed-singleton,
        engine-node$k-reserved-terminal-n-b,
        engine-node$k-reserved-terminal-n-f,
        properties$v-entry-type,
        engine-node$k-immediate-hashed-noreloc-singleton,
        engine-node$k-typecheck,
        engine-node$k-ambiguous-methods,
        engine-node$k-reserved-slot-b-setter,
        engine-node$k-if-type,
        smen$m-nrequired,
        engine-node$k-cache-header,
        engine-node$k-reserved-discriminator-t,
        properties$s-entry-type,
        engine-node$k-reserved-discriminator-h,
        engine-node$k-reserved-discriminator-s,
        engine-node$k-linear-by-singleton-class,
        engine-node$k-reserved-discriminator-k,
        discriminator$s-argnum,
        smen$v-nrequired,
        engine-node$k-reserved-repeated-slot-b-getter,
        stchen$m-checkedmask,
        engine-node$k-implicit-keyed-single-method,
        engine-node$k-reserved-discriminator-u,
        engine-node$k-reserved-discriminator-q,
        engine-node$k-reserved-discriminator-j,
        $simple-typechecked-cache-arguments-limit,
        engine-node$k-boxed-class-slot-getter,
        engine-node$k-reserved-discriminator-l,
        smen$v-restp,
        engine-node$k-reserved-terminal-n-c,
        engine-node$k-reserved-discriminator-f,
        engine-node$k-raw-byte-repeated-instance-slot-setter,
        discriminator$v-data-start,
        smen$v-data-start,
        engine-node$k-boxed-repeated-instance-slot-setter,
        discriminator$m-nrequired,
        engine-node$v-data-start,
        properties$m-entry-type,
        engine-node$k-reserved-slot-b-getter,
        engine-node$k-reserved-slot-a-setter,
        engine-node$k-boxed-instance-slot-getter,
        engine-node$k-reserved-discriminator-c,
        discriminator$v-nrequired },
    export: all;
  use dfmc-imports, export: all;
  use dfmc-back-end, export: all;
  use dfmc-execution, export: all;
  use dfmc-reader, export: all;
  use dfmc-conversion, export: all;
  use dfmc-optimization, export: all;
  use dfmc-typist, export: all;
  use dfmc-management, export: all;

  use harp-cg-back-end, export: all;
  use native-harp, export: all;
  use mnemonic-assembler;

  export


    open-emit-output, close-emit-output,

    stack-arguments-set-up, register-arguments-set-up, arguments-set-up,
    arguments-in-registers, arguments-on-stack,
    c-arguments-in-registers, c-arguments-on-stack,
    argument-register-padding,

    emit-comment, emit-line-comment,
    emit-extern, emit-public,
    emit-data-item, emit-raw-data-item, emit-data-footer,
    emit-imports,
    emit-imported-name,
    emit-import-adjustment?,
    emit-header, emit-footer,

    apropo-model-object, canonical-model-object,
    model-library-description,

    struct-field-name, cr-init-name,

    op--store-multiple-values-count,
    op--load-index, op--store-index,
    op--load-byte-index, op--store-byte-index,
    op--load-half-index, op--store-half-index,
    op--load-bit-index, op--store-bit-index,
    op--load-signed-byte-index, op--store-signed-byte-index,
    op--load-signed-half-index, op--store-signed-half-index,
    op--load-float-index, op--store-float-index,
    op--load-dfloat-index, op--store-dfloat-index,
    op--c-load-arguments,
    op--replace-bytes!,
    op--add,
    op--vector-as-raw,
    op--ld-mv-count,
    op--st-mv-count,
    op--bmvset,
    op--push-space-for-callee,
    op--pop-space-for-callee,
    op--cleanup-preserve-state-entry,
    op--cleanup-preserve-state-exit,

    op--wait-for-simple-lock, op--wait-for-simple-lock-timed, op--release-simple-lock,
    op--wait-for-recursive-lock, op--wait-for-recursive-lock-timed, op--release-recursive-lock,
    op--write-thread-variable, op--read-thread-variable,
    op--allocation-count, op--initialize-allocation-count,

    op--floge, op--fetox, op--fsin, op--fcos, op--ftan, op--fasin, op--facos, op--fatan,
    op--dloge, op--detox, op--dsin, op--dcos, op--dtan, op--dasin, op--dacos, op--datan, 

    <runtime-object>,
    make-runtime-reference, make-runtime-object,
    runtime-reference,
    op--constant-ref,
    op--dylan-constant-ref,
    make-imported-constant-reference,

    unset-tag-bit,

    c-name,
    shared-library-entry-point-name,
    shared-library-runtime-entry-point-name,

    call-c-primitive,

    rts-dropping-args,

    stack-vector-size,
    optional-arguments?,

    $true, $false,

    $system-init-code-tag, $user-init-code-tag,

    $dummy-name,
    $runtime-module-binding-type-marker,

    $symbol-fixup-name,

    bytes%,

    symbol-emitted?,
    cg-indirect-symbol, cg-uninterned-symbol,

    harp-local-mangle, harp-raw-mangle, harp-dylan-mangle,

    *harp-outputter*,
    *emitting-data?*,
    *emitting-init-code?*,
    *tail-calls*,
    *stream-outputters?*,
    *trace-harp?*,
    *compiling-dylan?*,
    *current-heap*,
    *current-compilation*,
    *loose-mode?*,
    *interactive-mode?*,

    stream-outputters?,

    \with-harp-emitter, \with-harp-variables, \with-harp-outputter,
    \dylan-reference-definer,
    \runtime-reference-definer, \c-runtime-reference-definer,


    register-dylan-code-models, deregister-dylan-code-models,
    imported-object?, register-imports-in-heap,
    cache-imports-in-lambda, cache-import-in-library

    ;

    
end module;
