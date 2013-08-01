Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2012 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Calling Convention

define constant $maximum-argument-count = 64;

define constant $function-parameter-name :: <string> = ".function";

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-function-parameter
    () => (fn :: <function>);
  llvm-builder-local(be, $function-parameter-name)
end;

define constant $next-methods-parameter-name :: <string> = ".next";

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-next-methods-parameter
    () => (nm :: <list>);
  llvm-builder-local(be, $next-methods-parameter-name)
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-set-generic-function-entrypoints // runtime
    (gf :: <generic-function>) => ();
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-set-accessor-method-xep
    (accessor-method :: <accessor-method>)
 => (accessor-method :: <accessor-method>);
  //---*** Fill this in...
end;


/// Apply

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-xep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-mep-apply // runtime
    (function :: <object>, next-methods :: <object>,
     args :: <simple-object-vector>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-mep-apply-with-optionals // runtime
    (function :: <object>, next-methods :: <object>, args :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-engine-node-apply-with-optionals // runtime
    (function :: <object>, next-methods :: <object>, args :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-iep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;

define side-effecting stateless indefinite-extent mapped-parameter &runtime-primitive-descriptor primitive-apply
    (fn :: <function>, arguments :: <simple-object-vector>)
 => (#rest values)
  let word-size = back-end-word-size(be);
  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");

  let fn-unmapped = ins--bitcast(be, fn, $llvm-object-pointer-type);

  // Retrieve the XEP function pointer
  let xep-slot-ptr = op--getslotptr(be, fn, #"<function>", #"xep");
  let xep = ins--load(be, xep-slot-ptr, alignment: word-size);

  // Read the size of the arguments vector
  let vector-size
    = call-primitive(be, primitive-vector-size-descriptor, arguments);

  // Create a basic block for each case
  let jump-table
    = make(<simple-object-vector>, size: 2 * $maximum-argument-count);
  for (count from 0 below $maximum-argument-count)
    jump-table[count * 2] := count;
    jump-table[count * 2 + 1]
      := make(<llvm-basic-block>, name: format-to-string("bb.arg%d", count));
  end;
  let default-bb = make(<llvm-basic-block>, name: "bb.default");
  let return-bb = make(<llvm-basic-block>, name: "bb.return");

  // Branch to the appropriate case
  apply(ins--switch, be, vector-size, default-bb, jump-table);

  // Generate all of the cases
  let result-phi-arguments = make(<stretchy-object-vector>);
  for (count from 0 below $maximum-argument-count)
    ins--block(be, jump-table[count * 2 + 1]);

    // Common XEP parameters
    let parameter-values = make(<simple-object-vector>, size: count + 2);
    parameter-values[0] := fn-unmapped;
    parameter-values[1] := vector-size;

    // Retrieve argument values
    for (i from 0 below count)
      parameter-values[2 + i]
        := call-primitive(be, primitive-vector-element-descriptor,
                          arguments, llvm-back-end-value-function(be, i));
    end for;

    // Cast to the appropriate XEP type
    let parameter-types = make(<simple-object-vector>, size: count + 2);
    parameter-types[0] := $llvm-object-pointer-type; // function
    parameter-types[1] := be.%type-table["iWord"]; // argument count
    fill!(parameter-types, $llvm-object-pointer-type, start: 2);
    let xep-type
      = make(<llvm-function-type>,
             return-type: llvm-reference-type(be, be.%mv-struct-type),
             parameter-types: parameter-types,
             varargs?: #f);
    let xep-cast = ins--bitcast(be, xep, llvm-pointer-to(be, xep-type));

    // Call the function
    let result
      = ins--tail-call(be, xep-cast, parameter-values,
                       calling-convention: $llvm-calling-convention-c);
    add!(result-phi-arguments, result);
    add!(result-phi-arguments, be.llvm-builder-basic-block);
    ins--br(be, return-bb);
  end for;

  // Default case (too many arguments)
  ins--block(be, default-bb);
  ins--call-intrinsic(be, "llvm.trap", vector());
  ins--unreachable(be);

  // Return
  ins--block(be, return-bb);
  apply(ins--phi, be, result-phi-arguments)
end;


/// Discriminator/engine-node Initialization

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-initialize-engine-node
    (engine-node :: <engine-node>) => (single-value :: <engine-node>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-initialize-discriminator
    (discriminator :: <discriminator>) => (single-value :: <discriminator>);
  //---*** Fill this in...
end;


/// Dynamic method and closure creation

define method op--init-signature
    (be :: <llvm-back-end>, class :: <&class>,
     meth :: <llvm-value>, signature :: <llvm-value>)
 => ();
  let meth-cast = op--object-pointer-cast(be, meth, class);
  let signature-ptr
    = op--getslotptr(be, meth-cast, class, #"function-signature");
  ins--store(be, signature, signature-ptr);
end method;

define method op--make-method-with-signature
    (be :: <llvm-back-end>, class :: <&class>,
     template :: <llvm-value>, signature :: <llvm-value>)
 => (closure :: <llvm-value>);
  let meth
    = call-primitive(be, primitive-copy-descriptor,
                     instance-storage-bytes(be, class),
                     template);
  op--init-signature(be, class, meth, signature);
  meth
end method;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-keyword-method-with-signature
    (template :: <keyword-method>, signature :: <signature>)
 => (meth :: <keyword-method>);
  op--make-method-with-signature(be, dylan-value(#"<keyword-method>"),
                                 template, signature)
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-method-with-signature
    (template :: <simple-method>, signature :: <signature>)
 => (meth :: <simple-method>);
  op--make-method-with-signature(be, dylan-value(#"<simple-method>"),
                                 template, signature)
end;

define method op--make-closure
    (be :: <llvm-back-end>, class :: <&class>,
     template :: <llvm-value>, closure-size :: <llvm-value>)
 => (closure :: <llvm-value>);
  let word-size = back-end-word-size(be);
  let header-words = dylan-value(#"$number-header-words");

  let rep-size-slot-descriptor
    = ^slot-descriptor(class, dylan-value(#"environment-size"));
  let rep-size-slot
    = header-words + ^slot-offset(rep-size-slot-descriptor, class);

  let fixed-size = header-words + ^instance-storage-size(class);
  let total-size = ins--add(be, closure-size, fixed-size);
  let byte-size = ins--mul(be, total-size, word-size);

  let closure
    = call-primitive(be, primitive-copy-r-descriptor,
                     byte-size,
                     closure-size,
                     rep-size-slot,
                     template);

  closure
end method;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-keyword-closure
    (template :: <keyword-closure-method>, closure-size :: <raw-integer>)
 => (closure :: <keyword-closure-method>);
  op--make-closure(be, dylan-value(#"<keyword-closure-method>"),
                   template, closure-size)
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-closure
    (template :: <simple-closure-method>, closure-size :: <raw-integer>)
 => (closure :: <simple-closure-method>);
  op--make-closure(be, dylan-value(#"<simple-closure-method>"),
                   template, closure-size)
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-keyword-closure-signature
    (template :: <keyword-closure-method>, signature :: <signature>, closure-size :: <raw-integer>)
 => (closure :: <keyword-closure-method>);
  let class :: <&class> = dylan-value(#"<keyword-closure-method>");
  let closure = op--make-closure(be, class, template, closure-size);
  op--init-signature(be, class, closure, signature);
  closure
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-closure-signature
    (template :: <simple-closure-method>, signature :: <signature>, closure-size :: <raw-integer>)
 => (closure :: <simple-closure-method>);
  let class :: <&class> = dylan-value(#"<simple-closure-method>");
  let closure = op--make-closure(be, class, template, closure-size);
  op--init-signature(be, class, closure, signature);
  closure
end;

define method op--init-closure-environment
    (be :: <llvm-back-end>, class :: <&class>,
     closure :: <llvm-value>, closure-size :: <llvm-value>)
 => ();
  let va-list = op--va-decl-start(be);
  let closure-cast = op--object-pointer-cast(be, closure, class);

  // Basic blocks
  let entry-bb = be.llvm-builder-basic-block;
  let loop-head-bb = make(<llvm-basic-block>);
  let loop-tail-bb = make(<llvm-basic-block>);
  let return-bb    = make(<llvm-basic-block>);

  ins--br(be, loop-head-bb);

  // Loop head
  ins--block(be, loop-head-bb);
  let index-placeholder
    = make(<llvm-symbolic-value>, type: be.%type-table["iWord"], name: "index");
  let index
    = ins--phi(be, 0, entry-bb, index-placeholder, loop-tail-bb);
  let cmp = ins--icmp-ult(be, index, closure-size);
  ins--br(be, cmp, loop-tail-bb, return-bb);

  // Loop tail: retrieve a varargs item and store it in the closure
  ins--block(be, loop-tail-bb);
  let arg = op--va-arg(be, va-list, $llvm-object-pointer-type);
  let ptr
    = op--getslotptr(be, closure-cast, class, #"environment-element", index);
  ins--store(be, arg, ptr);

  let next-index = ins--add(be, index, 1);
  index-placeholder.llvm-placeholder-value-forward := next-index;
  ins--br(be, loop-head-bb);

  ins--block(be, return-bb);
  op--va-end(be, va-list);
end method;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-keyword-closure-with-environment
    (template :: <keyword-closure-method>, closure-size :: <raw-integer>,
     #rest environment)
 => (closure :: <keyword-closure-method>);
  let class :: <&class> = dylan-value(#"<keyword-closure-method>");
  let closure
    = op--make-closure(be, class, template, closure-size);
   op--init-closure-environment(be, class, closure, closure-size);
  closure
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-closure-with-environment
    (template :: <simple-closure-method>, closure-size :: <raw-integer>,
     #rest environment)
 => (closure :: <simple-closure-method>);
  let class :: <&class> = dylan-value(#"<simple-closure-method>");
  let closure
    = op--make-closure(be, class, template, closure-size);
   op--init-closure-environment(be, class, closure, closure-size);
  closure
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-keyword-closure-with-environment-signature
    (template :: <keyword-closure-method>, signature :: <signature>, closure-size :: <raw-integer>,
     #rest environment)
 => (closure :: <keyword-closure-method>);
  let class :: <&class> = dylan-value(#"<keyword-closure-method>");
  let closure
    = op--make-closure(be, class, template, closure-size);
   op--init-signature(be, class, closure, signature);
   op--init-closure-environment(be, class, closure, closure-size);
  closure
end;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-make-closure-with-environment-signature
    (template :: <simple-closure-method>, signature :: <signature>, closure-size :: <raw-integer>,
     #rest environment)
 => (closure :: <simple-closure-method>);
  let class :: <&class> = dylan-value(#"<simple-closure-method>");
  let closure
    = op--make-closure(be, class, template, closure-size);
   op--init-signature(be, class, closure, signature);
   op--init-closure-environment(be, class, closure, closure-size);
  closure
end;

define side-effecting stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-initialize-keyword-closure
    (closure :: <simple-closure-method>, closure-size :: <raw-integer>,
     #rest environment)
 => ();
  let class :: <&class> = dylan-value(#"<keyword-closure-method>");
   op--init-closure-environment(be, class, closure, closure-size);
end;

define side-effecting stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-initialize-closure
    (closure :: <simple-closure-method>, closure-size :: <raw-integer>,
     #rest environment)
 => ();
  let class :: <&class> = dylan-value(#"<simple-closure-method>");
   op--init-closure-environment(be, class, closure, closure-size);
end;

// Mark the IEP's closure object as read-only to facilitate CSE
define method op--closure-invariant-start
    (back-end :: <llvm-back-end>, o :: <&iep>, closure :: <llvm-value>)
 => ();
  let word-size = back-end-word-size(back-end);
  let header-words = dylan-value(#"$number-header-words");

  let fun = o.function;
  let class :: <&class> = fun.^object-class;

  let fixed-size = header-words + ^instance-storage-size(class);
  let byte-size = (fixed-size + closure-size(o.environment)) * word-size;
  ins--call-intrinsic(back-end, "llvm.invariant.start",
                      vector(i64(byte-size), closure));
end method;

// Stack allocate a closure
define method op--stack-allocate-closure
    (back-end :: <llvm-back-end>, class :: <&class>,
     template :: <llvm-value>, closure-size :: <integer>)
 => (new-vector :: <llvm-value>);
  let module = back-end.llvm-builder-module;
  let word-size = back-end-word-size(back-end);
  let header-words = dylan-value(#"$number-header-words");

  let class-type
    = llvm-class-type(back-end, class, repeated-size: closure-size);
  let closure
    = ins--alloca(back-end, class-type, 1, alignment: word-size);

  // Copy from the template
  let fixed-size = header-words + ^instance-storage-size(class);
  let byte-size = fixed-size * word-size;

  let closure-raw = op--raw-pointer-cast(back-end, closure);
  ins--call-intrinsic(back-end, "llvm.memcpy",
                      vector(closure-raw, template, byte-size,
                             i32(word-size), $llvm-false));

  // Initialize the size slot
  let size-slot-ptr
    = op--getslotptr(back-end, closure, class, #"environment-size");
  let size-ref = emit-reference(back-end, module, closure-size);
  ins--store(back-end, size-ref, size-slot-ptr);

  closure
end method;

