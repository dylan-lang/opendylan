module:    dylan-user
Synopsis:  The module definition for the NATIVE-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module native-rtg
  use common-dylan, exclude: { format-to-string };
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
    op--create-TEB-tlv-index,
    op--get-teb-tlv,
    op--set-teb-tlv,
    op--free-teb-tlv,
    op--get-stack-bottom,
    op--get-module-handle,
    op--pop-any-SEH-handlers,
    op--init-dylan-data,

    op--call-iep,
    op--call-xep,
    op--call-c,
    op--load-arguments,
    op--c-load-arguments,
    op--unwind-protect,
    op--dylan-thread-trampoline,
    op--initialize-master-thread,
    op--maybe-uninitialize-thread-for-p-detach,
    op--shut-down-dll-library,
    op--shut-down-exe-library,
    op--shut-down-dylan-library,
    op--dylan-registration-error,
    op--maybe-uninitialize-thread,
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
    op--initialize-teb-register,
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

    op--initialize-TEB,
    op--initialize-GC-TEB,
    op--initialize-thread-with-gc,
    mm-dylan-init-mm,
    master-teb,
    master-gc-teb,
    $outside-dylan,

    primitive-allocate-vector-ref,
    primitive-dylan-initialize-ref,
    primitive-register-traced-roots-ref,
    primitive-deregister-traced-roots-ref,
    primitive-nlx-ref,

    primitive-name,

    tag-as-integer,
    ensure-safe-key-space,

    TEB-tlv-index,
    BE-values-vector-offset,

    mm-FreeMemory,
    raw-malloc,

    module-hinstance,

    c-mangle,
    stdcall-mangle,

    as-direct-ref,

    $uninitialized-teb,
    $data-start-symbol, $data-end-symbol,
    $objs-start-symbol, $objs-end-symbol,
    $vars-start-symbol, $vars-end-symbol,
    $fixup-start-symbol, $fixup-end-symbol,
    $import-start-symbol, $import-end-symbol,

    %ambig-root, %static-root, %exact-root,

    \runtime-function-aux-definer,
    \runtime-variable-definer,
    \runtime-external-definer,
    \runtime-primitive-definer,
    \runtime-literal-definer,

    \when-base, \when-client,

    create-dylan-runtime;
end module;
