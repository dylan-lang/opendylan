Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Operating System


define side-effecting stateless dynamic-extent &c-primitive-descriptor primitive-exit-application
    (code :: <raw-integer>) => ();

define runtime-variable *argc* :: <raw-c-signed-int>
  = ^make(<&raw-c-signed-int>, value: 0);

define runtime-variable *argv* :: <raw-c-pointer>
  = ^make(<&raw-c-pointer>, value: 0);

define c-callable auxiliary &runtime-primitive-descriptor call-application-exit-functions-internal () => ();
  let m = be.llvm-builder-module;
  let iep = dylan-value(#"call-application-exit-functions").^iep;
  let name = emit-name(be, m, iep);
  let global = llvm-builder-global(be, name);
  llvm-constrain-type
    (global.llvm-value-type,
     llvm-pointer-to(be, llvm-lambda-type(be, iep)));
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  op--call(be, global,
           vector(undef, undef),
           type: llvm-reference-type(be, be.%mv-struct-type),
           calling-convention: llvm-calling-convention(be, iep),
           tail-call?: #t);
end;



/// Support for keyboard-break handling

define runtime-variable dylan-keyboard-interrupt? :: <raw-machine-word>
  = make-raw-literal(as(<machine-word>, 0));

define runtime-variable dylan-keyboard-break-handler :: <function> = &unbound,
  section: #"variables";

define side-effect-free &c-primitive-descriptor primitive-keyboard-interrupt-signaled
  () => (interrupt? :: <raw-boolean>);

define side-effecting &c-primitive-descriptor primitive-keyboard-interrupt-signaled-setter
  (interrupt? :: <raw-boolean>)  => ();

define side-effect-free &c-primitive-descriptor primitive-keyboard-interrupt-polling ()
  => (interrupt-polling? :: <raw-boolean>);

define side-effecting &c-primitive-descriptor primitive-keyboard-interrupt-polling-setter
  (interrupt-polling? :: <raw-boolean>) => ();

define side-effect-free &c-primitive-descriptor primitive-keyboard-interrupt-polling-thread (hThread :: <raw-pointer>)
  => (interrupt-polling? :: <raw-boolean>);

define side-effecting &c-primitive-descriptor primitive-keyboard-interrupt-polling-thread-setter
  (interrupt-polling? :: <raw-boolean>, hThread :: <raw-pointer>) => ();


/// (Win32) DLL Support

// FIXME these both need to be per-DLL/local

// module_hInstance holds the module handle for Dylan DLLs
define runtime-variable module-hInstance :: <raw-machine-word>
  = make-raw-literal(as(<machine-word>, 0));

define side-effecting mapped &runtime-primitive-descriptor primitive-runtime-module-handle
    () => (handle :: <machine-word>);
  let m = be.llvm-builder-module;
  let global
    = llvm-runtime-variable(be, m, module-hInstance-descriptor);
  let raw-handle
    = ins--load(be, global);
  call-primitive(be, primitive-wrap-machine-word-descriptor, raw-handle)
end;
