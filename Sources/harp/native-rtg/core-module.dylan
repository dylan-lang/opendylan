module:    dylan-user
Synopsis:  The module definition for the NATIVE-CORE-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module native-rtg
  use functional-dylan;
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use native-harp;
  use threads;

  export

    genop--start-timer,
    genop--stop-timer,
    genop--exit-application,
    genop--run-application,
    genop--spy-fixup-imported-dylan-data,
    genop--spy-fixup-unimported-dylan-data,
    genop--spy-exit-application,
    genop--dylan-stack-overflow-handler,

    op--output-debug-string,
    op--pop-any-SEH-handlers,

    op--call-iep,
    op--call-xep,
    op--call-c,
    op--load-arguments,
    op--c-load-arguments,
    op--unwind-protect,
    op--sub64,
    op--rts-dropping-n-args,
    op--restore-multiple-values-from-vector,
    op--add,
    op--load-index,
    op--load-index-scaled,
    op--store-index,
    op--store-index-scaled,
    op--load-byte-index,
    op--store-byte-index,
    op--copy-words-with-update,
    op--divide-by-4,
    op--multiply-by-4,
    op--shuffle-stack,
    op--duplicate,
    op--vector-size,
    op--calculate-arg-count-for-apply,
    op--apply-xep-discriminating,
    op--check-apply-special-case,
    op--shuffle-size-for-apply,
    op--calculate-supplied-number-for-apply,
    op--calculate-required-number-for-apply,
    op--extend-stack-for-apply,
    op--preserve-mlist-for-mep-apply,
    op--restore-mlist-for-mep-apply,
    op--push-space-for-callee,
    op--pop-space-for-callee,
    op--push-registers-for-remove-optionals,
    op--pop-registers-for-remove-optionals,
    op--keywords-size,
    op--shuffle-size-for-requireds,
    op--preserve-return-address-for-apply,
    op--restore-return-address-for-apply,

    dylan-stack-overflow-error,
    dylan-error-function,

    primitive-allocate-vector-ref,
    primitive-nlx-ref,

    primitive-name,

    tag-as-integer,
    ensure-safe-key-space,

    BE-values-vector-offset,

    raw-malloc,

    c-mangle,
    stdcall-mangle,

    as-direct-ref,

    \runtime-function-aux-definer,
    \runtime-variable-definer,
    \runtime-external-definer,
    \runtime-primitive-definer,
    \runtime-literal-definer,

    \when-base, \when-client,

    create-dylan-runtime;
end module;
