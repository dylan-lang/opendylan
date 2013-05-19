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
