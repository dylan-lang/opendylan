module:    dylan-user
Synopsis:  The module definition for the IDVM-HARP module
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module idvm-harp
  use dylan;
  use streams;
  use standard-io-streams, import: {*standard-output*};
  use format;
  use print;
  use locators;
  use syntax-case;
  use doss, export: {put-object, put-specially, put-apply, <doss-dumper>};
  use harp, export: all;
  use harp-for-extenders;

  export
    // main classes, accessors and creators

    ins--return-value, ins--return-any-values, 
    ins--store-vm-arg-n, ins--load-vm-arg-n,
    ins--vm-call-0, ins--vm-call-1, ins--vm-call-2, ins--vm-call-n, 
    ins--vm-jmp-0,  ins--vm-jmp-1,  ins--vm-jmp-2,  ins--vm-jmp-n,
    ins--vm-call-0-returning, ins--vm-call-1-returning, 
    ins--vm-call-2-returning, ins--vm-call-n-returning, 

    ins--vm-bind-exit, ins--vm-bind-exit-returning, 
    ins--vm-unwind-protect, ins--vm-unwind-protect-returning,
    ins--vm-mv-bind, ins--vm-mv-bind-rest, 
    ins--vm-mv-bind-finished, ins--vm-returning, 

    // closures - nosa 6/1/95
    ins--make-closure, ins--make-closure-with-specs, ins--move-env,
    ins--ld-vc, ins--st-vc,
    ins--ld-env, ins--st-env,
    ins--make-value-cell,
    notify-inner-closure-variables,

    // DOSS dumping support
    <idvm-code-dumping-policy>,
    <resolution-function-proxy>,
    doss-message, *debug-idvm-doss*, *idvm-debug-stream*,
    lookup-mangled,
    <variable-reader>,            ins--variable-reader,  
    <variable-writer>,            ins--variable-writer, 
    variable-reader, variable-writer,
    constant-reference-definer,
    <variable-definition>,        ins--variable-definer,
    <constant-definition>,        ins--constant-definer,
    <local-constant-reference>,   ins--local-constant-ref,
    constant-reference-value,     constant-reference-value-setter, 
    doss-stream-setter,

    definition-value, 

    <idvm-opcode>, opcode-name,
    
    // The machine registers
    res, env,

    // Oh, and the back end object itself
    <idvm-back-end>;

end module;

define module idvm-harp-test
  use dylan;
  use streams;
  use standard-io-streams, import: {*standard-output*};
  use format;
  use print;
  use syntax-case;
  use harp-for-extenders;
  use idvm-harp;

  export
    run-test,
    test0,
    test1,
    test2,
    test3,
    test4;
end module;
    
