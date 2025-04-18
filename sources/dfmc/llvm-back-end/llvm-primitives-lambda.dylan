Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Calling Convention

define constant $maximum-argument-count = 64;
define constant $entry-point-argument-count = 20;

// Fields in signature-properties: see ^signature-properties
// packed-slots definition in dfmc-modeling
define constant $signature-number-required-mask = #x0000ff;
define constant $signature-key-p-mask           = #x010000;
define constant $signature-rest-p-mask          = #x040000;
define constant $signature-optionals-p-mask
  = logior($signature-key-p-mask, $signature-rest-p-mask);

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

define side-effecting stateless dynamic-extent mapped &runtime-primitive-descriptor primitive-set-generic-function-entrypoints
    (gf :: <generic-function>) => ();
  let word-size = back-end-word-size(be);

  // Extract the gf's signature
  let gf-class :: <&class> = dylan-value(#"<generic-function>");
  let signature-slot-ptr = op--getslotptr(be, gf, gf-class, #"function-signature");
  let signature = ins--load(be, signature-slot-ptr, alignment: word-size);

  // Extract the signature properties
  let sig-class :: <&class> = dylan-value(#"<signature>");
  let signature-cast = op--object-pointer-cast(be, signature, sig-class);
  let properties-slot-ptr
    = op--getslotptr(be, signature-cast, sig-class, #"signature-properties");
  let properties = ins--load(be, properties-slot-ptr, alignment: word-size);
  let raw-properties = op--untag-integer(be, properties);

  // Extract the required arguments count
  let nreq = ins--and(be, raw-properties, $signature-number-required-mask);

  // Location for storing entry point
  let entry-point-slot-ptr = op--getslotptr(be, gf, gf-class, #"xep");

  let default-bb = make(<llvm-basic-block>);
  let return-bb = make(<llvm-basic-block>);
  let optionals-switch-cases = make(<stretchy-object-vector>);
  let no-optionals-switch-cases = make(<stretchy-object-vector>);

  // Are optionals required?
  let optionals-masked
    = ins--and(be, raw-properties, $signature-optionals-p-mask);
  let optionals-cmp = ins--icmp-ne(be, optionals-masked, 0);
  ins--if (be, optionals-cmp)
    for (count from 0 below $entry-point-argument-count)
      add!(optionals-switch-cases, count);
      add!(optionals-switch-cases, make(<llvm-basic-block>));
    end for;
    ins--switch(be, nreq, default-bb, optionals-switch-cases);
  ins--else
    for (count from 0 to $entry-point-argument-count)
      add!(no-optionals-switch-cases, count);
      add!(no-optionals-switch-cases, make(<llvm-basic-block>));
    end for;
    ins--switch(be, nreq, default-bb, no-optionals-switch-cases);
  end ins--if;

  // Optionals cases
  for (count from 0 below $entry-point-argument-count)
    ins--block(be, optionals-switch-cases[count * 2 + 1]);
    let func
      = llvm-entry-point-function(be, gf-optional-xep-descriptor, count);
    let ref = make(<llvm-cast-constant>,
                   operator: #"BITCAST",
                   type: $llvm-object-pointer-type,
                   operands: vector(func));
    ins--store(be, ref, entry-point-slot-ptr);
    ins--br(be, return-bb);
  end for;

  // Non-optionals cases
  for (count from 0 to $entry-point-argument-count)
    ins--block(be, no-optionals-switch-cases[count * 2 + 1]);
    let func = llvm-entry-point-function(be, gf-xep-descriptor, count);
    let ref = make(<llvm-cast-constant>,
                   operator: #"BITCAST",
                   type: $llvm-object-pointer-type,
                   operands: vector(func));
    ins--store(be, ref, entry-point-slot-ptr);
    ins--br(be, return-bb);
  end for;

  // Default case (unknown entry type)
  ins--block(be, default-bb);
  ins--call-intrinsic(be, "llvm.trap", vector());
  ins--unreachable(be);

  // Return block
  ins--block(be, return-bb);
end;

define side-effecting stateless dynamic-extent mapped &primitive-descriptor primitive-set-accessor-method-xep
    (accessor-method :: <accessor-method>, what :: <integer>)
 => (accessor-method :: <accessor-method>);
  // Basic blocks
  let case0-bb = make(<llvm-basic-block>);
  let case1-bb = make(<llvm-basic-block>);
  let case2-bb = make(<llvm-basic-block>);
  let case3-bb = make(<llvm-basic-block>);
  let case4-bb = make(<llvm-basic-block>);
  let case5-bb = make(<llvm-basic-block>);
  let return   = make(<llvm-basic-block>);

  let xep-slot-ptr
    = op--getslotptr(be, accessor-method, #"<function>", #"xep");
  let raw-what = op--untag-integer(be, what);
  ins--switch*(be, raw-what, return,
               0, case0-bb,
               1, case1-bb,
               2, case2-bb,
               3, case3-bb,
               4, case4-bb,
               5, case5-bb);

  local method xep-ref
            (descriptor :: <llvm-entry-point-descriptor>)
         => (reference :: <llvm-value>);
          make(<llvm-cast-constant>,
               operator: #"BITCAST",
               type: $llvm-object-pointer-type,
               operands: vector(llvm-entry-point-function(be, descriptor, #f)))
        end method;

  ins--block(be, case0-bb);

  ins--store(be, xep-ref(slotacc-single-q-instance-getter-xep-descriptor),
             xep-slot-ptr);
  ins--br(be, return);

  ins--block(be, case1-bb);
  ins--store(be, xep-ref(slotacc-single-q-instance-setter-xep-descriptor),
             xep-slot-ptr);
  ins--br(be, return);

  ins--block(be, case2-bb);
  ins--store(be, xep-ref(slotacc-single-q-class-getter-xep-descriptor),
             xep-slot-ptr);
  ins--br(be, return);

  ins--block(be, case3-bb);
  ins--store(be, xep-ref(slotacc-single-q-class-setter-xep-descriptor),
             xep-slot-ptr);
  ins--br(be, return);

  ins--block(be, case4-bb);
  ins--store(be, xep-ref(slotacc-repeated-instance-getter-xep-descriptor),
             xep-slot-ptr);
  ins--br(be, return);

  ins--block(be, case5-bb);
  ins--store(be, xep-ref(slotacc-repeated-instance-setter-xep-descriptor),
             xep-slot-ptr);
  ins--br(be, return);

  ins--block(be, return);
  accessor-method
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-callable-as-engine-node?
    (meth :: <method>)
 => (mep-callable? :: <boolean>);
  // This depends on the fact that the mep slot in <method> and
  // engine-node-entry-point are in the same slot position, and the
  // entry-point calling conventions are similar.
  let km-class :: <&class> = dylan-value(#"<keyword-method>");
  let cmp = op--heap-object-subtype-bit-instance-cmp(be, meth, km-class);
  op--boolean(be, cmp)
end;


/// Discriminator/engine-node Initialization

define side-effecting stateless dynamic-extent &runtime-primitive-descriptor primitive-initialize-engine-node
    (engine-node :: <engine-node>) => (single-value :: <engine-node>);
  let word-size = back-end-word-size(be);

  let return-bb = make(<llvm-basic-block>);

  // Retrieve the properties slot from the engine node
  let engine-node-cast
    = op--object-pointer-cast(be, engine-node, #"<engine-node>");
  let properties-slot-ptr
    = op--getslotptr(be, engine-node-cast, #"<engine-node>", #"properties");
  let properties = ins--load(be, properties-slot-ptr, alignment: word-size);
  let raw-properties = op--untag-integer(be, properties);

  // Point to the entry-point slot
  let entry-point-slot-ptr
    = op--getslotptr(be, engine-node-cast,
                     #"<engine-node>", #"engine-node-entry-point");

  // Jump table for each engine node type:
  let default-bb = make(<llvm-basic-block>);
  let switch-cases = make(<stretchy-object-vector>);

  // Create a basic block for each distinct entry point name, and add
  // each case to switch-cases
  let entry-point-table = make(<object-table>);
  for (entry-point-name in $engine-node-entry-point-names, index from 0 below 32)
    unless (element(entry-point-table, entry-point-name, default: #f))
      entry-point-table[entry-point-name] := make(<llvm-basic-block>);
    end unless;

    add!(switch-cases, index);
    add!(switch-cases, entry-point-table[entry-point-name]);
  end for;

  // Branch on the engine node type
  let engine-node-type = ins--and(be, raw-properties, properties$m-entry-type);
  ins--switch(be, engine-node-type, default-bb, switch-cases);

  for (bb keyed-by entry-point-name in entry-point-table)
    ins--block(be, bb);

    let desc
      = element($llvm-entry-point-descriptors, entry-point-name, default: #f);
    if (desc)
      let attributes = desc.entry-point-attributes;
      if (member?(#"singular", attributes))
        let func = llvm-entry-point-function(be, desc, #f);
        let ref = make(<llvm-cast-constant>,
                       operator: #"BITCAST",
                       type: $llvm-object-pointer-type,
                       operands: vector(func));
        ins--store(be, ref, entry-point-slot-ptr);
      elseif (member?(#"single-method", attributes))
        op--initialize-single-method-engine-node(be, desc,
                                                 raw-properties,
                                                 entry-point-slot-ptr);
      elseif (member?(#"cache-header", attributes))
        op--initialize-cache-header-engine-node(be, desc,
                                                engine-node,
                                                entry-point-slot-ptr);
      else
        error("No strategy for initializing %s", entry-point-name);
      end if;
    else
      error("No descriptor for %s", entry-point-name);
    end if;

    ins--br(be, return-bb);
  end for;

  // Default case (unknown entry type)
  ins--block(be, default-bb);
  ins--call-intrinsic(be, "llvm.trap", vector());
  ins--unreachable(be);

  // Exit block
  ins--block(be, return-bb);
  engine-node
end;

define method op--initialize-single-method-engine-node
    (be :: <llvm-back-end>, desc :: <llvm-entry-point-descriptor>,
     raw-properties :: <llvm-value>, entry-point-slot-ptr :: <llvm-value>)
 => ();
  let word-size = back-end-word-size(be);

  let continue-bb = make(<llvm-basic-block>);

  // Extract the number of function arguments
  let masked = ins--and(be, raw-properties, smen$m-nrequired);
  let nrequired = ins--lshr(be, masked, smen$v-nrequired);

  let rest-mask = ins--and(be, raw-properties, smen$m-restp);
  let rest-cmp = ins--icmp-ne(be, rest-mask, 0);
  let nrequired-inc = ins--add(be, nrequired, 1);
  let impargs = ins--select(be, rest-cmp, nrequired-inc, nrequired);

  // Create a basic block for each switch case
  let inner-switch-cases = make(<stretchy-object-vector>);
  for (count from 0 to $entry-point-argument-count)
    add!(inner-switch-cases, count);
    add!(inner-switch-cases, make(<llvm-basic-block>));
  end for;

  // Branch on the count of implementation arguments
  ins--switch(be, impargs, continue-bb, inner-switch-cases);

  // Initialization cases
  for (count from 0 to $entry-point-argument-count)
    ins--block(be, inner-switch-cases[count * 2 + 1]);
    let func = llvm-entry-point-function(be, desc, count);
    let ref = make(<llvm-cast-constant>,
                   operator: #"BITCAST",
                   type: $llvm-object-pointer-type,
                   operands: vector(func));
    ins--store(be, ref, entry-point-slot-ptr);
    ins--br(be, continue-bb);
  end for;

  ins--block(be, continue-bb);
end method;

define method op--initialize-cache-header-engine-node
    (be :: <llvm-back-end>, desc :: <llvm-entry-point-descriptor>,
     engine-node :: <llvm-value>, entry-point-slot-ptr :: <llvm-value>)
 => ();
  let word-size = back-end-word-size(be);

  let continue-bb = make(<llvm-basic-block>);

  // Identify the corresponding generic function
  let parent = op--parent-gf(be, engine-node);

  // Extract the gf's signature
  let gf-class :: <&class> = dylan-value(#"<generic-function>");
  let gf-cast = op--object-pointer-cast(be, parent, gf-class);
  let signature-slot-ptr
    = op--getslotptr(be, gf-cast, gf-class, #"function-signature");
  let signature = ins--load(be, signature-slot-ptr, alignment: word-size);

  // Extract the signature properties
  let sig-class :: <&class> = dylan-value(#"<signature>");
  let signature-cast = op--object-pointer-cast(be, signature, sig-class);
  let properties-slot-ptr
    = op--getslotptr(be, signature-cast, sig-class, #"signature-properties");
  let properties = ins--load(be, properties-slot-ptr, alignment: word-size);
  let raw-properties = op--untag-integer(be, properties);

  // Extract the required arguments count
  let nreq = ins--and(be, raw-properties, $signature-number-required-mask);

  // Add one argument for the #rest vector if needed
  let optionals-masked
    = ins--and(be, raw-properties, $signature-optionals-p-mask);
  let optionals-cmp = ins--icmp-ne(be, optionals-masked, 0);
  let nreq-inc = ins--add(be, nreq, 1);
  let impargs = ins--select(be, optionals-cmp, nreq-inc, nreq);

  // Create a basic block for each switch case
  let inner-switch-cases = make(<stretchy-object-vector>);
  for (count from 0 to $entry-point-argument-count)
    add!(inner-switch-cases, count);
    add!(inner-switch-cases, make(<llvm-basic-block>));
  end for;

  // Branch on the count of implementation arguments
  ins--switch(be, impargs, continue-bb, inner-switch-cases);

  // Initialization cases
  for (count from 0 to $entry-point-argument-count)
    ins--block(be, inner-switch-cases[count * 2 + 1]);
    let func = llvm-entry-point-function(be, desc, count);
    let ref = make(<llvm-cast-constant>,
                   operator: #"BITCAST",
                   type: $llvm-object-pointer-type,
                   operands: vector(func));
    ins--store(be, ref, entry-point-slot-ptr);
    ins--br(be, continue-bb);
  end for;

  ins--block(be, continue-bb);
end method;

define side-effecting stateless dynamic-extent mapped &runtime-primitive-descriptor primitive-initialize-discriminator
    (discriminator :: <discriminator>) => (single-value :: <discriminator>);
  let word-size = back-end-word-size(be);

  let return-bb = make(<llvm-basic-block>);

  // Retrieve the discriminator's properties slot value
  let properties-slot-ptr
    = op--getslotptr(be, discriminator, #"<discriminator>", #"properties");
  let properties = ins--load(be, properties-slot-ptr, alignment: word-size);
  let raw-properties = op--untag-integer(be, properties);

  // Extract the discriminator type
  let entry-type = ins--and(be, raw-properties, properties$m-entry-type);

  // Extract the argument number to discriminate on
  let argnum-masked = ins--and(be, raw-properties, discriminator$m-argnum);
  let argnum = ins--ashr(be, argnum-masked, discriminator$v-argnum);

  // Extract the number of required arguments
  let nrequired-masked = ins--and(be, raw-properties, discriminator$m-nrequired);
  let nrequired = ins--ashr(be, nrequired-masked, discriminator$v-nrequired);

  // Determine the number of implementation arguments
  let rest-mask = ins--and(be, raw-properties, discriminator$m-restp);
  let rest-cmp = ins--icmp-ne(be, rest-mask, 0);
  let nrequired-inc = ins--add(be, nrequired, 1);
  let impargs = ins--select(be, rest-cmp, nrequired-inc, nrequired);

  // Point to the entry-point slot
  let entry-point-slot-ptr
    = op--getslotptr(be, discriminator,
                     #"<engine-node>", #"engine-node-entry-point");

  // Jump table for each engine node type:
  let default-bb = make(<llvm-basic-block>);
  let switch-cases = make(<stretchy-object-vector>);

  // Create a basic block for each distinct entry point name, and add
  // each case to switch-cases
  let entry-point-table = make(<object-table>);
  for (index from 32 below 63)
    let entry-point-name = $engine-node-entry-point-names[index];
    unless (element(entry-point-table, entry-point-name, default: #f))
      entry-point-table[entry-point-name] := make(<llvm-basic-block>);
    end unless;

    add!(switch-cases, index);
    add!(switch-cases, entry-point-table[entry-point-name]);
  end for;

  // Branch on the discriminator node type
  ins--switch(be, entry-type, default-bb, switch-cases);

  // Emit code for each distinct entry point
  for (bb keyed-by entry-point-name in entry-point-table)
    ins--block(be, bb);

    let desc
      = element($llvm-entry-point-descriptors, entry-point-name, default: #f);
    if (desc)
      assert(member?(#"cross", desc.entry-point-attributes));
      // Create a basic block for each argument count
      let impargs-switch-cases = make(<stretchy-object-vector>);
      for (count from 1 to $entry-point-argument-count)
        add!(impargs-switch-cases, count);
        add!(impargs-switch-cases, make(<llvm-basic-block>));
      end for;
      // Branch on the number of implementation arguments
      ins--switch(be, impargs, default-bb, impargs-switch-cases);

      // Emit code for each argument count for this entry point
      for (count from 1 to $entry-point-argument-count)
        ins--block(be, impargs-switch-cases[(count - 1) * 2 + 1]);

        // Create a basic block for each argument position
        let argnum-switch-cases = make(<stretchy-object-vector>);
        for (pos from 0 below count)
          add!(argnum-switch-cases, pos);
          add!(argnum-switch-cases, make(<llvm-basic-block>));
        end for;

        // Branch on the argument position
        ins--switch(be, argnum, default-bb, argnum-switch-cases);

        // Generate initializer for each argument position
        for (pos from 0 below count)
          ins--block(be, argnum-switch-cases[pos * 2 + 1]);

          let func = llvm-entry-point-function(be, desc, count, pos: pos);
          let ref = make(<llvm-cast-constant>,
                         operator: #"BITCAST",
                         type: $llvm-object-pointer-type,
                         operands: vector(func));
          ins--store(be, ref, entry-point-slot-ptr);
          ins--br(be, return-bb);
        end for;
      end for;
    else
      error("No descriptor for %s", entry-point-name);
    end if;
  end for;

  // Default case (unknown entry type, count, or position)
  ins--block(be, default-bb);
  ins--call-intrinsic(be, "llvm.trap", vector());
  ins--unreachable(be);

  // Exit block
  ins--block(be, return-bb);
  discriminator
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

define side-effecting stateless indefinite-extent can-unwind &runtime-primitive-descriptor primitive-mep-apply-with-optionals
    (meth :: <object>, next-methods :: <object>, mepargs :: <object>)
 => (#rest values);
  let word-size = back-end-word-size(be);
  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let lambda-class :: <&class> = dylan-value(#"<keyword-method>");

  // Read the method's MEP
  let meth-cast = op--object-pointer-cast(be, meth, lambda-class);
  let mep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"mep");
  let mep = ins--load(be, mep-slot-ptr, alignment: word-size);

  // Read the size of the arguments vector
  let mepargs-cast = op--object-pointer-cast(be, mepargs, sov-class);
  let vector-size
    = call-primitive(be, primitive-vector-size-descriptor, mepargs-cast);

  // Create a basic block for each case
  let default-bb = make(<llvm-basic-block>, name: "bb.default");
  let return-bb = make(<llvm-basic-block>, name: "bb.return");
  let switch-cases = make(<stretchy-object-vector>);
  for (count from 0 to $entry-point-argument-count)
    add!(switch-cases, count);
    add!(switch-cases, make(<llvm-basic-block>));
  end;

  // Branch to the appropriate case
  ins--switch(be, vector-size, default-bb, switch-cases);

  // Generate all of the cases
  let result-phi-arguments = make(<stretchy-object-vector>);
  for (count from 0 to $entry-point-argument-count)
    ins--block(be, switch-cases[count * 2 + 1]);

    let parameter-values = make(<stretchy-object-vector>);
    // Retrieve argument values from vector
    for (i from 0 below count)
      add!(parameter-values,
           call-primitive(be, primitive-vector-element-descriptor,
                          mepargs-cast, llvm-back-end-value-function(be, i)));
    end for;

    // Chain to the MEP
    let parameter-types
      = vector($llvm-object-pointer-type,  // method
               $llvm-object-pointer-type); // next-methods
    let mep-type
      = make(<llvm-function-type>,
             return-type: llvm-reference-type(be, be.%mv-struct-type),
             parameter-types: parameter-types,
             varargs?: #t);
    let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
    let result
      = ins--tail-call
          (be, mep-cast,
           concatenate(vector(meth, next-methods), parameter-values),
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
  ins--phi(be, result-phi-arguments)
end;

define side-effecting stateless indefinite-extent can-unwind &runtime-primitive-descriptor primitive-engine-node-apply-with-optionals
    (engine :: <engine-node>, parent :: <function>,
     args :: <simple-object-vector>)
 => (#rest values);
  let word-size = back-end-word-size(be);
  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let args-cast = op--object-pointer-cast(be, args, sov-class);

  // Read the size of the arguments vector
  let vector-size
    = call-primitive(be, primitive-vector-size-descriptor, args-cast);

  // Create a basic block for each case
  let default-bb = make(<llvm-basic-block>, name: "bb.default");
  let return-bb = make(<llvm-basic-block>, name: "bb.return");
  let switch-cases = make(<stretchy-object-vector>);
  for (count from 0 to $entry-point-argument-count)
    add!(switch-cases, count);
    add!(switch-cases, make(<llvm-basic-block>, name: format-to-string("bb.arg%d", count)));
  end;

  // Branch to the appropriate case
  ins--switch(be, vector-size, default-bb, switch-cases);

  // Generate all of the cases
  let result-phi-arguments = make(<stretchy-object-vector>);
  for (count from 0 to $entry-point-argument-count)
    ins--block(be, switch-cases[count * 2 + 1]);

    let parameter-values = make(<stretchy-object-vector>);
    // Retrieve argument values from vector
    for (i from 0 below count)
      add!(parameter-values,
           call-primitive(be, primitive-vector-element-descriptor,
                          args-cast, llvm-back-end-value-function(be, i)));
    end for;

    // Chain to engine node
    let result
      = op--chain-to-engine-entry-point(be, engine, parent, parameter-values,
					tail-call?: #t);

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
  ins--phi(be, result-phi-arguments)
end;

/*
define side-effecting stateless indefinite-extent &unimplemented-primitive-descriptor primitive-iep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);
  //---*** Fill this in...
end;
*/

define side-effecting stateless indefinite-extent can-unwind mapped-parameter &runtime-primitive-descriptor primitive-apply
    (fn :: <function>, arguments :: <simple-object-vector>)
 => (#rest values)
  let word-size = back-end-word-size(be);
  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let function-class :: <&class> = dylan-value(#"<function>");

  let fn-unmapped = ins--bitcast(be, fn, $llvm-object-pointer-type);

  // Retrieve the XEP function pointer
  let xep-slot-ptr = op--getslotptr(be, fn, function-class, #"xep");
  let xep = ins--load(be, xep-slot-ptr, alignment: word-size);

  // Read the size of the arguments vector
  let arguments-size
    = call-primitive(be, primitive-vector-size-descriptor, arguments);

  // Retrieve the last argument (a vector of arguments to pass)
  let last-index = ins--sub(be, arguments-size, 1);
  let last-argument
    = call-primitive(be, primitive-vector-element-descriptor,
                     arguments, last-index);
  let last-argument-mapped
    = op--object-pointer-cast(be, last-argument, sov-class);
  let last-argument-size
    = call-primitive(be, primitive-vector-size-descriptor,
                     last-argument-mapped);

  // Allocate a buffer for all of the arguments
  let argument-count = ins--add(be, last-index, last-argument-size);
  let buf = ins--alloca(be, $llvm-object-pointer-type, argument-count,
                        alignment: word-size);
  let buf-cast = ins--bitcast(be, buf, $llvm-object-pointer-type);

  // Copy values from arguments into the buffer
  let zero = llvm-back-end-value-function(be, 0);
  let src1 = op--getslotptr(be, arguments, sov-class,
                            #"vector-element", 0);
  call-primitive(be, primitive-replace!-descriptor,
                 buf-cast, zero, zero,
                 src1, zero, zero,
                 last-index);

  // Copy values from last-argument into the buffer
  let src2 = op--getslotptr(be, last-argument-mapped, sov-class,
                            #"vector-element", 0);
  call-primitive(be, primitive-replace!-descriptor,
                 buf-cast, last-index, zero,
                 src2, zero, zero,
                 last-argument-size);

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
  ins--switch(be, argument-count, default-bb, jump-table);

  // Generate all of the cases
  let result-phi-arguments = make(<stretchy-object-vector>);
  for (count from 0 below $maximum-argument-count)
    ins--block(be, jump-table[count * 2 + 1]);

    // Common XEP parameters
    let parameter-values = make(<simple-object-vector>, size: count + 2);
    parameter-values[0] := fn-unmapped;
    parameter-values[1] := argument-count;

    // Retrieve argument values
    for (i from 0 below count)
      parameter-values[2 + i]
        := begin
             let element-ptr = ins--gep(be, buf, i);
             ins--load(be, element-ptr, alignment: word-size);
           end;
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
      = op--call(be, xep-cast, parameter-values,
                 calling-convention: $llvm-calling-convention-c,
                 tail-call?: #t);
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
  ins--phi(be, result-phi-arguments)
end;

// Used for calls to Dylan from C
define side-effecting stateless indefinite-extent can-unwind c-callable auxiliary &runtime-primitive-descriptor call-dylan-function
    (fn :: <function>, n :: <raw-integer>, #rest args)
 => (primary :: <object>)
  let va-list = op--va-decl-start(be);
  let optargs = op--va-list-to-stack-vector(be, va-list, n);
  op--va-end(be, va-list);

  let args = op--stack-allocate-vector(be, 1);
  call-primitive(be, primitive-vector-element-setter-descriptor,
                 optargs, args, llvm-back-end-value-function(be, 0));

  let fn-cast = op--object-pointer-cast(be, fn, #"<function>");
  let args-cast = op--object-pointer-cast(be, args, #"<simple-object-vector>");

  let mv = call-primitive(be, primitive-apply-descriptor, fn-cast, args-cast);
  ins--extractvalue(be, mv, 0)
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

  ins--iterate loop (be, index = 0)
    let cmp = ins--icmp-ult(be, index, closure-size);
    ins--if (be, cmp)
      let arg = op--va-arg(be, va-list, $llvm-object-pointer-type);
      let ptr
        = op--getslotptr(be, closure-cast, class, #"environment-element", index);
      ins--store(be, arg, ptr);
      loop(ins--add(be, index, 1));
    end ins--if;
  end ins--iterate;

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
     closure-size :: <integer>)
 => (closure :: <llvm-value>);
  let word-size = back-end-word-size(back-end);

  let class-type
    = llvm-class-type(back-end, class, repeated-size: closure-size);
  ins--alloca(back-end, class-type, 1, alignment: word-size)
end method;

// Initialize a stack-allocated closure
define method op--initialize-stack-allocated-closure
    (back-end :: <llvm-back-end>, class :: <&class>,
     template :: <llvm-value>, closure :: <llvm-value>,
     closure-size :: <integer>)
 => ();
  let module = back-end.llvm-builder-module;
  let word-size = back-end-word-size(back-end);
  let header-words = dylan-value(#"$number-header-words");

  // Copy from the template
  let fixed-size = header-words + ^instance-storage-size(class);
  let byte-size
    = llvm-back-end-value-function(back-end, fixed-size * word-size);

  let closure-raw = op--raw-pointer-cast(back-end, closure);
  op--memcpy(back-end, closure-raw, template, byte-size, $llvm-false,
             dst-alignment: word-size, src-alignment: word-size);

  // Initialize the size slot
  let size-slot-ptr
    = op--getslotptr(back-end, closure, class, #"environment-size");
  let size-ref = emit-reference(back-end, module, closure-size);
  ins--store(back-end, size-ref, size-slot-ptr);
end method;

