module:    dylan-user
Synopsis:  The module definition for the NATIVE-GLUE-RTG library
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

    op--call-iep,
    op--call-xep,
    op--call-c,
    op--load-arguments,
    op--c-load-arguments,
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
    op--duplicate,
    op--vector-size,
    op--initialize-teb-register,
    op--push-space-for-callee,
    op--pop-space-for-callee,
    op--keywords-size,

    op--create-TEB-tlv-index,
    op--get-teb-tlv,
    op--set-teb-tlv,
    op--free-teb-tlv,
    op--get-module-handle,
    op--init-dylan-data,
    op--dylan-thread-trampoline,
    op--initialize-master-thread,
    op--maybe-uninitialize-thread-for-p-detach,
    op--shut-down-library,
    op--shut-down-dylan-library,

    op--initialize-TEB,
    op--initialize-GC-TEB,
    op--initialize-thread-with-gc,
    mm-dylan-init-mm,
    master-teb,
    master-gc-teb,
    $outside-dylan,

    primitive-dylan-initialize-ref,
    primitive-register-traced-roots-ref,

    $uninitialized-teb,
    $data-start-symbol, $data-end-symbol,
    $objs-start-symbol, $objs-end-symbol,
    $vars-start-symbol, $vars-end-symbol,
    $fixup-start-symbol, $fixup-end-symbol,
    $import-start-symbol, $import-end-symbol,

    %ambig-root, %static-root, %exact-root,

    module-hinstance,

    TEB-tlv-index,

    c-mangle,
    stdcall-mangle,


    \runtime-function-aux-definer,
    \runtime-variable-definer,
    \runtime-external-definer,
    \runtime-primitive-definer,
    \runtime-literal-definer,

    \when-base, \when-client,

    output-data,
    output-glue,
    output-functions,

    dummy-generate-runtime;
end module;
