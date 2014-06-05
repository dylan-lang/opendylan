Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2014 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Entry point definition macros

define class <llvm-entry-point-descriptor> (<object>)
  constant slot entry-point-name :: <string>,
    required-init-keyword: name:;
  constant slot entry-point-attributes :: <sequence>,
    required-init-keyword: attributes:;
  constant slot entry-point-generator :: false-or(<function>),
    init-value: #f, init-keyword: generator:;
  constant slot entry-point-function-declarator :: <simple-object-vector>,
    init-value: #[], init-keyword: declarator:;
end class;

// Define an entry point
// Adjectives:
//   - singular:       Generate only one instance
//   - cross:          Generate position 1..n variants for n-argument
//                     entry points
//   - outer:          Called by callers that may not know the exact signature
//                     (thus requiring the C calling convention)
//   - variable-arity: Uses varargs
//   - single-method:  Entry point for a <single-method-engine-node>
//   - cache-header:   Entry point for a <cache-header-engine-node>
//
define macro entry-point-descriptor-definer
  { define ?adj:* entry-point-descriptor ?:name
        (?parameters:*) => (?values:*);
      ?:body
    end }
    => { define ?adj entry-point-descriptor ?name
             (?parameters) => (?values)
           ?body
         end }
  { define ?adjectives:* entry-point-descriptor ?:name
        (?parameters:*) => (?values:*)
      ?:body
    end }
    => { define constant ?name ## "-descriptor"
           = begin
               let declarator
                 = vector(parameter-names:
                            primitive-parameter-names(?parameters),
                          parameter-types-spec:
                            primitive-parameter-types(?parameters));
               let attributes = #[?adjectives];
               make(<llvm-entry-point-descriptor>,
                    name: ?"name",
                    generator:
                      entry-point-emitter-method (?parameters) => (?values)
                        ?body
                      end,
                    declarator: declarator,
                    attributes: attributes);
             end;
         do-define-llvm-entry-point-descriptor
           (?#"name", ?name ## "-descriptor") }
adjectives:
    { } => { }
    { ?adjective:name ...} => { ?#"adjective", ... }
end macro;

// Emitter method with declared parameter types changed to <llvm-value>
define macro entry-point-emitter-method
  { entry-point-emitter-method (?parameters) => (?values) ?:body end }
    => { method
             (?=be :: <llvm-back-end>,
              ?=num :: false-or(<integer>),
              ?=pos :: false-or(<integer>),
              ?parameters)
          => (?values)
           ?body
         end }
parameters:
    { } => { }
    { \#rest ?:name } => { #rest ?name }
    { ?variable-name, ... } => { ?variable-name, ... }
values:
    { \#rest ?:name } => { ?name :: <llvm-value> }
variable-name:
  { ?:name :: ?:expression }
    => { ?name :: <llvm-value> }
end macro;

define constant $llvm-entry-point-descriptors = make(<object-table>);

define function do-define-llvm-entry-point-descriptor
    (name :: <symbol>, descriptor :: <llvm-entry-point-descriptor>)
 => ();
  $llvm-entry-point-descriptors[name] := descriptor;
end function;

define method llvm-entry-point-function
    (back-end :: <llvm-back-end>, desc :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>), #key pos :: false-or(<integer>) = #f)
 => (function :: <llvm-function>);
  let mangled-base = raw-mangle(back-end, desc.entry-point-name);
  let mangled-name
    = if (pos)
        format-to-string("%s_%d_%d", mangled-base, pos, count);
      elseif (count)
        format-to-string("%s_%d", mangled-base, count);
      else
        mangled-base
      end if;
  if (llvm-builder-global-defined?(back-end, mangled-name))
    llvm-builder-global(back-end, mangled-name)
  else
    let function
      = apply(make-entry-point-function, back-end, mangled-name,
              desc, count, pos, desc.entry-point-function-declarator);
    llvm-builder-define-global(back-end, mangled-name, function)
  end;
end method;

define method make-entry-point-function
    (back-end :: <llvm-back-end>, mangled-name :: <string>,
     descriptor :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>), pos :: false-or(<integer>),
     #key parameter-names :: <simple-object-vector> = #[],
          parameter-types-spec :: <simple-object-vector>,
     #all-keys)
 => (function :: <llvm-function>);
  let (required-parameter-type-specs, required-parameter-names, rest-parameter-name)
    = if (~empty?(parameter-types-spec) & parameter-types-spec.last == #"rest")
        let required-count = parameter-types-spec.size - 1;
        values(copy-sequence(parameter-types-spec, end: required-count),
               copy-sequence(parameter-names, end: required-count),
               parameter-names.last)
      else
        values(parameter-types-spec, parameter-names, #f)
      end if;
  let function-type
    = llvm-entry-point-descriptor-function-type(back-end, descriptor,
                                                required-parameter-type-specs,
                                                true?(rest-parameter-name),
                                                count);

  let parameter-types = function-type.llvm-function-type-parameter-types;
  let arguments
    = map(method (arg-type, index)
	    let name
	      = if (index < required-parameter-names.size)
		  raw-mangle(back-end, required-parameter-names[index])
		    else
		  format-to-string("a%d", index)
		end if;
            make(<llvm-argument>,
                 type: arg-type,
                 name: name,
                 index: index)
          end,
          parameter-types,
          range(below: parameter-types.size));

  let calling-convention
    = if (function-type.llvm-function-type-varargs?
            | member?(#"outer", descriptor.entry-point-attributes))
        $llvm-calling-convention-c
      else
        $llvm-calling-convention-fast
      end if;

  make(<llvm-function>,
       name: mangled-name,
       type: llvm-pointer-to(back-end, function-type),
       arguments: arguments,
       linkage: #"external",
       section: llvm-section-name(back-end, #"code"),
       calling-convention: calling-convention)
end method;

define method llvm-entry-point-rest?
    (back-end :: <llvm-back-end>, desc :: <llvm-entry-point-descriptor>)
 => (rest? :: <boolean>);
  apply(entry-point-rest-test, back-end, desc.entry-point-function-declarator)
end method;

define function entry-point-rest-test
    (back-end :: <llvm-back-end>,
     #key parameter-names :: <simple-object-vector> = #[],
          parameter-types-spec :: <simple-object-vector>,
     #all-keys)
 => (rest? :: <boolean>);
  ~empty?(parameter-types-spec)
    & parameter-types-spec.last == #"rest"
end function;

// Function type for a runtime entry point function
define method llvm-entry-point-descriptor-function-type
    (back-end :: <llvm-back-end>,
     descriptor :: <llvm-entry-point-descriptor>,
     required-parameter-type-specs :: <simple-object-vector>,
     parameters-rest? :: <boolean>,
     count :: false-or(<integer>))
 => (type :: <llvm-function-type>);
  local
    method parameter-type (type-name :: <symbol>) => (type :: <llvm-type>);
      llvm-reference-type(back-end, dylan-value(type-name))
    end method;

  // Compute parameter types
  let required-parameter-types
    = map(parameter-type, required-parameter-type-specs);
  let parameter-types
    = if (llvm-entry-point-rest?(back-end, descriptor))
	concatenate(required-parameter-types,
		    make(<simple-object-vector>,
			 size: count,
			 fill: $llvm-object-pointer-type))
      else
	required-parameter-types
      end if;

  make(<llvm-function-type>,
       parameter-types: parameter-types,
       return-type: llvm-reference-type(back-end, back-end.%mv-struct-type),
       varargs?: true?(member?(#"variable-arity",
                               descriptor.entry-point-attributes)))
end method;


/// References

define method llvm-entry-point-info
    (back-end :: <llvm-back-end>, ep :: <&lambda-xep>)
 => (descriptor :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
  let req-size = ^entry-point-number-required(ep);
  let key-size = ^entry-point-number-keys(ep);
  if (^entry-point-key?(ep))
    values(rest-key-xep-descriptor, req-size + key-size + 1)
  elseif (^entry-point-rest?(ep))
    values(rest-xep-descriptor, req-size)
  else
    values(xep-descriptor, req-size)
  end if;
end method;

define method llvm-entry-point-info
    (back-end :: <llvm-back-end>, ep :: <&generic-function-xep>)
 => (descriptor :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
  let req-size :: <integer> = ^entry-point-number-required(ep);
  if (^entry-point-optionals?(ep))
    values(gf-optional-xep-descriptor, req-size)
  else
    values(gf-xep-descriptor, req-size)
  end if
end method;

define method llvm-entry-point-info
    (back-end :: <llvm-back-end>, ep :: <&keyword-method-mep>)
 => (descriptor :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
  values(rest-key-mep-descriptor,
         ^entry-point-number-required(ep) + ^entry-point-number-keys(ep) + 1)
end method;

define method llvm-entry-point-info
    (back-end :: <llvm-back-end>, ep :: <&slot-accessor-xep>)
 => (descriptor :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
  select (ep.^entry-point-name by \=)
    "slotacc_single_q_instance_getter_xep" =>
      values(slotacc-single-q-instance-getter-xep-descriptor, #f);
    "slotacc_single_q_class_getter_xep" =>
      values(slotacc-single-q-class-getter-xep-descriptor, #f);
    "slotacc_single_q_instance_setter_xep" =>
      values(slotacc-single-q-instance-setter-xep-descriptor, #f);
    "slotacc_single_q_class_setter_xep" =>
      values(slotacc-single-q-class-setter-xep-descriptor, #f);
    "slotacc_repeated_instance_getter_xep" =>
      values(slotacc-repeated-instance-getter-xep-descriptor, #f);
    "slotacc_repeated_instance_setter_xep" =>
      values(slotacc-repeated-instance-setter-xep-descriptor, #f);
  end
end method;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&shared-entry-point>)
 => (name :: <string>);
  let (desc :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
    = llvm-entry-point-info(back-end, ep);
  let mangled-base = raw-mangle(back-end, desc.entry-point-name);
  if (count)
    format-to-string("%s_%d", mangled-base, count);
  else
    mangled-base
  end if;
end method;


/// Entry point utility operations

define function op--argument-count-error
    (back-end :: <llvm-back-end>, function :: <llvm-value>, n :: <llvm-value>)
 => ();
  let n-tagged = op--tag-integer(back-end, n);
  op--call-error-iep(back-end, #"argument-count-error", function, n-tagged)
end function;

define function op--odd-keyword-arguments-error
    (back-end :: <llvm-back-end>, function :: <llvm-value>)
 => ();
  op--call-error-iep(back-end, #"odd-keyword-arguments-error", function);
end function;

define function op--invalid-keyword-trap
    (back-end :: <llvm-back-end>,
     mepargs :: <llvm-value>, disphdr :: <llvm-value>,
     engine-node :: <llvm-value>, key :: <llvm-value>,
     keyvec :: <llvm-value>, implicit? :: <llvm-value>)
 => ();
  op--call-error-iep(back-end, #"invalid-keyword-trap",
                     mepargs, disphdr, engine-node, key, keyvec, implicit?)
end function;

define function op--type-check-lambda-arguments
    (back-end :: <llvm-back-end>, function :: <llvm-value>, arguments :: <sequence>)
 => ();
  let word-size = back-end-word-size(back-end);
  let required-cast
    = if (~empty?(arguments))
        let function-signature-slot-ptr
          = op--getslotptr(back-end, function,
                           #"<lambda>", #"function-signature");
        let signature
          = ins--load(back-end, function-signature-slot-ptr,
                      alignment: word-size);
        let signature-cast
          = op--object-pointer-cast(back-end, signature, #"<signature>");
        let signature-required-slot-ptr
          = op--getslotptr(back-end, signature-cast,
                           #"<signature>", #"signature-required");
        let required = ins--load(back-end, signature-required-slot-ptr,
                                 alignment: word-size);
        op--object-pointer-cast(back-end, required, #"<simple-object-vector>");
      end;

  for (argument in arguments, i from 0)
    let specializer
      = call-primitive(back-end, primitive-vector-element-descriptor,
                       required-cast,
                       llvm-back-end-value-function(back-end, i));
    do-emit-type-check(back-end, argument, #f, specializer)
  end for;
end function;

define method op--engine-node-callback
    (be :: <llvm-back-end>, engine :: <llvm-value>)
 => (callback-iep :: <llvm-value>);
  let class :: <&class> = dylan-value(#"<engine-node>");
  let engine-cast = op--object-pointer-cast(be, engine, class);

  let callback-iep-ptr
    = op--getslotptr(be, engine-cast, class, #"engine-node-callback");
  ins--load(be, callback-iep-ptr, alignment: back-end-word-size(be))
end method;

define function op--parent-gf
    (back-end :: <llvm-back-end>, parent :: <llvm-value>)
 => (gf :: <llvm-value>);
  let word-size = back-end-word-size(back-end);

  let chen-class :: <&class> = dylan-value(#"<cache-header-engine-node>");

  ins--iterate loop (back-end, parent = parent)
    // Is this a <cache-header-engine-node> instance?
    let cmp
      = op--heap-object-subtype-bit-instance-cmp(back-end, parent, chen-class);
    ins--if (back-end, cmp)
      // Yes, retrieve the node's parent and try again
      let engine-node-cast
        = op--object-pointer-cast(back-end, parent, chen-class);
      let parent-ptr
        = op--getslotptr(back-end, engine-node-cast, chen-class,
                         #"cache-header-engine-node-parent");
      loop(ins--load(back-end, parent-ptr, alignment: word-size));
    ins--else
      // No, this must be the <generic-function>
      parent
    end ins--if;
  end ins--iterate
end function;


/// Apply entry point

define entry-point-descriptor apply-xep
    (function :: <function>, #rest arguments) => (#rest values)
  if (empty?(arguments))
    // The 0-argument apply-xep will never be called
    make(<llvm-undef-constant>,
         type: llvm-reference-type(be, be.%mv-struct-type))
  else
    let word-size = back-end-word-size(be);

    let function-cast = op--object-pointer-cast(be, function, #"<function>");

    // Retrieve the XEP function pointer
    let xep-slot-ptr = op--getslotptr(be, function-cast, #"<function>", #"xep");
    let xep = ins--load(be, xep-slot-ptr, alignment: word-size);

    // Read the size of the arguments vector
    let arguments-vector
      = op--object-pointer-cast(be, arguments.last, #"<simple-object-vector>");
    let vector-size
      = call-primitive(be, primitive-vector-size-descriptor, arguments-vector);

    // Count of arguments passed directly
    let n = num - 1;

    // Create a basic block for each case
    let jump-table
      = make(<simple-object-vector>, size: 2 * ($maximum-argument-count - n));
    for (count from 0 below $maximum-argument-count - n)
      jump-table[count * 2] := count;
      jump-table[count * 2 + 1]
        := make(<llvm-basic-block>, name: format-to-string("bb.arg%d", count));
    end;
    let default-bb = make(<llvm-basic-block>, name: "bb.default");
    let return-bb = make(<llvm-basic-block>, name: "bb.return");

    // Branch to the appropriate case
    ins--switch(be, vector-size, default-bb, jump-table);

    // Generate all of the cases
    let result-phi-arguments = make(<stretchy-object-vector>);
    for (count from 0 below $maximum-argument-count - n)
      ins--block(be, jump-table[count * 2 + 1]);

      // Common XEP parameters
      let parameter-values = make(<simple-object-vector>, size: count + 2 + n);
      parameter-values[0] := function;
      parameter-values[1] := n + count;

      // Directly passed parameters
      for (i from 0 below n)
        parameter-values[2 + i] := arguments[i];
      end for;

      // Retrieve argument values from vector
      for (i from 0 below count)
        parameter-values[2 + n + i]
          := call-primitive(be, primitive-vector-element-descriptor,
                            arguments-vector,
                            llvm-back-end-value-function(be, i));
      end for;

      // Cast to the appropriate XEP type
      let parameter-types = make(<simple-object-vector>, size: count + 2 + n);
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
    ins--phi(be, result-phi-arguments)
  end if
end entry-point-descriptor;

define entry-point-descriptor apply-mep
    (next :: <list>, function :: <lambda>, #rest arguments) => (#rest values)
  // FIXME placeholder
  ins--call-intrinsic(be, "llvm.trap", #[]);
  ins--unreachable(be);
end entry-point-descriptor;


/// Dispatcher entry points

// For direct method calls with fixed arguments only
define outer entry-point-descriptor xep
    (function :: <function>, n :: <raw-integer>, #rest arguments)
 => (#rest values)
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let error-bb = make(<llvm-basic-block>);
  let typecheck-bb = make(<llvm-basic-block>);
  let call-bb  = make(<llvm-basic-block>);

  // Check argument count
  let cmp = ins--icmp-ne(be, n, num);
  ins--br(be, cmp, error-bb, typecheck-bb);

  // If argument counts do not match, throw an error
  ins--block(be, error-bb);
  op--argument-count-error(be, function, n);

  // Type check against the signature's required specializers
  ins--block(be, typecheck-bb);
  let function-cast = op--object-pointer-cast(be, function, #"<lambda>");
  op--type-check-lambda-arguments(be, function-cast, arguments);

  // Chain to the IEP
  let mep-slot-ptr = op--getslotptr(be, function-cast, #"<lambda>", #"mep");
  let iep = ins--load(be, mep-slot-ptr, alignment: word-size);

  let iep-type
    = make(<llvm-function-type>,
           return-type: llvm-reference-type(be, be.%mv-struct-type),
           parameter-types: make(<simple-object-vector>,
                                 size: num + 2,
                                 fill: $llvm-object-pointer-type),
           varargs?: #f);
  let iep-cast = ins--bitcast(be, iep, llvm-pointer-to(be, iep-type));
  ins--tail-call
    (be, iep-cast,
     concatenate(arguments,
                 vector(emit-reference(be, module, &false), function)),
     calling-convention: $llvm-calling-convention-fast)
end entry-point-descriptor;

// For direct method calls with #rest
define variable-arity outer entry-point-descriptor rest-xep
    (function :: <function>, n :: <raw-integer>, #rest arguments)
 => (#rest values)
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let error-bb = make(<llvm-basic-block>);
  let typecheck-bb = make(<llvm-basic-block>);
  let call-bb  = make(<llvm-basic-block>);

  // Check argument count
  let cmp = ins--icmp-slt(be, n, num);
  ins--br(be, cmp, error-bb, typecheck-bb);

  // If argument counts do not match, throw an error
  ins--block(be, error-bb);
  op--argument-count-error(be, function, n);

  // Type check against the signature's required specializers
  ins--block(be, typecheck-bb);
  let function-cast = op--object-pointer-cast(be, function, #"<lambda>");
  op--type-check-lambda-arguments(be, function-cast, arguments);

  // Retrieve the #rest arguments as a vector
  let va-list = op--va-decl-start(be);
  let count = ins--sub(be, n, num);
  let rest-vector = op--va-list-to-stack-vector(be, va-list, count);
  op--va-end(be, va-list);

  // Chain to the IEP
  let mep-slot-ptr = op--getslotptr(be, function-cast, #"<lambda>", #"mep");
  let iep = ins--load(be, mep-slot-ptr, alignment: word-size);

  let iep-type
    = make(<llvm-function-type>,
           return-type: llvm-reference-type(be, be.%mv-struct-type),
           parameter-types: make(<simple-object-vector>,
                                 size: num + 3,
                                 fill: $llvm-object-pointer-type),
           varargs?: #f);
  let iep-cast = ins--bitcast(be, iep, llvm-pointer-to(be, iep-type));
  ins--tail-call
    (be, iep-cast,
     concatenate(arguments,
                 vector(rest-vector,
                        emit-reference(be, module, &false),
                        function)),
     calling-convention: $llvm-calling-convention-fast)
end entry-point-descriptor;

// For direct method calls with #key (and possibly #rest)
// (numbered by the total number of parameters in the IEP)
define variable-arity outer entry-point-descriptor rest-key-xep
    (function :: <function>, n :: <raw-integer>)
 => (#rest values);
  //---*** Fill this in...
  make(<llvm-undef-constant>,
       type: llvm-reference-type(be, be.%mv-struct-type))
end entry-point-descriptor;

define method op--engine-node-call
    (be :: <llvm-back-end>, function :: <llvm-value>, arguments :: <sequence>)
 => (call :: <llvm-value>);
  let word-size = back-end-word-size(be);

  // Retrieve the dispatch engine
  let gf-class :: <&class> = dylan-value(#"<generic-function>");
  let function-cast = op--object-pointer-cast(be, function, gf-class);

  let discriminator-slot-ptr
    = op--getslotptr(be, function-cast, gf-class, #"discriminator");
  let engine = ins--load(be, discriminator-slot-ptr, alignment: word-size);

  op--chain-to-engine-entry-point(be, engine, function, arguments)
end method;

define method op--chain-to-engine-entry-point
    (be :: <llvm-back-end>, engine :: <llvm-value>, function :: <llvm-value>,
     arguments :: <sequence>)
 => (call :: <llvm-value>);
  // Retrieve the engine entry point from the engine
  let entry-point = op--engine-node-entry-point(be, engine);

  // Chain to the engine entry point function
  let parameter-types
    = make(<simple-object-vector>,
           size: 2 + arguments.size,
           fill: $llvm-object-pointer-type);
  let entry-point-type
    = make(<llvm-function-type>,
           return-type: llvm-reference-type(be, be.%mv-struct-type),
           parameter-types: parameter-types,
           varargs?: #f);
  let entry-point-cast
    = ins--bitcast(be, entry-point, llvm-pointer-to(be, entry-point-type));
  op--call
    (be, entry-point-cast,
     concatenate(vector(engine, function), arguments),
     calling-convention: $llvm-calling-convention-c,
     tail-call?: #t)
end method;

define method op--engine-node-entry-point
    (be :: <llvm-back-end>, engine-node :: <llvm-value>)
 => (entry-point :: <llvm-value>);
  let word-size = back-end-word-size(be);
  let en-class :: <&class> = dylan-value(#"<engine-node>");
  let engine-node-cast
    = op--object-pointer-cast(be, engine-node, en-class);
  let entry-point-ptr
    = op--getslotptr(be, engine-node-cast, en-class,
                     #"engine-node-entry-point");
  ins--load(be, entry-point-ptr, alignment: word-size)
end method;

// For GF calls with fixed arguments only
define outer entry-point-descriptor gf-xep
    (function :: <generic-function>, n :: <raw-integer>, #rest arguments)
 => (#rest values)
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let error-bb = make(<llvm-basic-block>);
  let call-bb  = make(<llvm-basic-block>);

  // Check argument count
  let cmp = ins--icmp-ne(be, n, num);
  ins--br(be, cmp, error-bb, call-bb);

  // If argument counts do not match, throw an error
  ins--block(be, error-bb);
  op--argument-count-error(be, function, n);

  // Call using the dispatch engine
  ins--block(be, call-bb);
  op--engine-node-call(be, function, arguments)
end entry-point-descriptor;

// For GF calls with optional arguments
define variable-arity outer entry-point-descriptor gf-optional-xep
    (function :: <generic-function>, n :: <raw-integer>, #rest arguments)
 => (#rest values)
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let error-bb = make(<llvm-basic-block>);
  let call-bb  = make(<llvm-basic-block>);

  // Check argument count
  let cmp = ins--icmp-slt(be, n, num);
  ins--br(be, cmp, error-bb, call-bb);

  // If argument counts do not match, throw an error
  ins--block(be, error-bb);
  op--argument-count-error(be, function, n);

  // Retrieve the #rest arguments as a vector
  ins--block(be, call-bb);
  let va-list = op--va-decl-start(be);
  let count = ins--sub(be, n, num);
  let rest-vector = op--va-list-to-stack-vector(be, va-list, count);
  op--va-end(be, va-list);

  // Call using the dispatch engine
  op--engine-node-call(be, function,
                       concatenate(arguments, vector(rest-vector)))
end entry-point-descriptor;

// For MEP calls with #key (and possibly #rest)
define variable-arity outer entry-point-descriptor rest-key-mep
    (next-methods :: <object>, function :: <function>, n :: <raw-integer>)
 => (#rest values);
  //---*** Fill this in...
  make(<llvm-undef-constant>,
       type: llvm-reference-type(be, be.%mv-struct-type))
end entry-point-descriptor;


/// Accessor entry points

define method op--slotacc-xep
    (be :: <llvm-back-end>, iep-name :: <symbol>,
     function :: <llvm-value>, n :: <llvm-value>, #rest arguments)
 => (call :: <llvm-value>);
  let error-bb = make(<llvm-basic-block>);
  let call-bb  = make(<llvm-basic-block>);

  // Check argument count
  let cmp = ins--icmp-ne(be, n, arguments.size);
  ins--br(be, cmp, error-bb, call-bb);

  // If argument counts do not match, throw an error
  ins--block(be, error-bb);
  op--argument-count-error(be, function, n);

  // Chain to the accessor IEP
  ins--block(be, call-bb);
  let f = dylan-value(iep-name).^iep;
  let name = emit-name(be, be.llvm-builder-module, f);
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  ins--tail-call(be, llvm-builder-global(be, name),
                 concatenate(arguments, vector(undef, undef)),
                 type: llvm-reference-type(be, be.%mv-struct-type),
                 calling-convention: llvm-calling-convention(be, f))
end method;

define singular outer entry-point-descriptor slotacc-single-q-instance-getter-xep
    (function :: <function>, n :: <raw-integer>, a :: <getter-method>, inst)
 => (#rest values);
  op--slotacc-xep(be, #"%slotacc-single-Q-instance-getter", function, n,
                  a, inst)
end entry-point-descriptor;

define singular outer entry-point-descriptor slotacc-single-q-class-getter-xep
    (function :: <function>, n :: <raw-integer>,
     a :: <getter-method>, inst)
 => (#rest values);
  op--slotacc-xep(be, #"%slotacc-single-Q-class-getter", function, n,
                  a, inst)
end entry-point-descriptor;

define singular outer entry-point-descriptor slotacc-single-q-instance-setter-xep
    (function :: <function>, n :: <raw-integer>,
     value, a :: <setter-method>, inst)
 => (#rest values);
  op--slotacc-xep(be, #"%slotacc-single-Q-instance-setter", function, n,
                  value, a, inst)
end entry-point-descriptor;

define singular outer entry-point-descriptor slotacc-single-q-class-setter-xep
    (function :: <function>, n :: <raw-integer>,
     value, a :: <setter-method>, inst)
 => (#rest values);
  op--slotacc-xep(be, #"%slotacc-single-Q-class-setter", function, n,
                  value, a, inst)
end entry-point-descriptor;

define singular outer entry-point-descriptor slotacc-repeated-instance-getter-xep
    (function :: <function>, n :: <raw-integer>,
     a :: <repeated-getter-method>, inst, idx)
 => (#rest values);
  op--slotacc-xep(be, #"%slotacc-repeated-instance-getter", function, n,
                  a, inst, idx)
end entry-point-descriptor;

define singular outer entry-point-descriptor slotacc-repeated-instance-setter-xep
    (function :: <function>, n :: <raw-integer>,
     value, a :: <repeated-setter-method>, inst, idx)
 => (#rest values);
  op--slotacc-xep(be, #"%slotacc-repeated-instance-setter", function, n,
                  value, a, inst, idx)
end entry-point-descriptor;


/// Dispatch engine entry points

// Terminals

define singular variable-arity outer entry-point-descriptor general-engine-node-n
    (engine :: <engine-node>, function :: <generic-function>)
 => (#rest values);
  let word-size = back-end-word-size(be);

  // Identify the corresponding generic function
  let parent = op--parent-gf(be, function);

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

  // Extract the function arguments
  let va-list = op--va-decl-start(be);
  let mepargs-vector = op--va-list-to-stack-vector(be, va-list, impargs);
  op--va-end(be, va-list);

  // Invoke the dispatch callback with on the argument to be
  // discriminated on, the function (or cache header), and the engine node.
  let callback-iep = op--engine-node-callback(be, engine);

  let typical-callback-iep = dylan-value(#"%gf-dispatch-absent").^iep;
  let func-type
    = llvm-pointer-to(be, llvm-lambda-type(be, typical-callback-iep));
  let iep-func = ins--bitcast(be, callback-iep, func-type);

  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  ins--tail-call(be, iep-func,
                 vector(mepargs-vector, parent, engine, undef, undef),
                 calling-convention:
                   llvm-calling-convention(be, typical-callback-iep));
end entry-point-descriptor;

define singular outer entry-point-descriptor general-engine-node-1
    (engine :: <engine-node>, function :: <generic-function>, inst :: <object>)
 => (#rest values);
  let word-size = back-end-word-size(be);

  let callback-iep = op--engine-node-callback(be, engine);

  // Invoke the dispatch callback with on the argument to be
  // discriminated on, the function (or cache header), and the engine node.
  let typical-callback-iep
    = dylan-value(#"%gf-dispatch-boxed-class-slot-getter").^iep;
  let func-type
    = llvm-pointer-to(be, llvm-lambda-type(be, typical-callback-iep));
  let iep-func = ins--bitcast(be, callback-iep, func-type);

  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  ins--tail-call(be, iep-func,
                 vector(inst, engine, function, undef, undef),
                 calling-convention:
                   llvm-calling-convention(be, typical-callback-iep));
end entry-point-descriptor;

define singular outer entry-point-descriptor general-engine-node-2
    (engine :: <engine-node>, function :: <generic-function>,
     value :: <object>, inst :: <object>)
 => (#rest values);
  let word-size = back-end-word-size(be);

  let callback-iep = op--engine-node-callback(be, engine);

  // Invoke the dispatch callback with on the argument to be
  // discriminated on, the function (or cache header), and the engine node.
  let typical-callback-iep
    = dylan-value(#"%gf-dispatch-boxed-class-slot-setter").^iep;
  let func-type
    = llvm-pointer-to(be, llvm-lambda-type(be, typical-callback-iep));
  let iep-func = ins--bitcast(be, callback-iep, func-type);

  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  ins--tail-call(be, iep-func,
                 vector(value, inst, engine, function, undef, undef),
                 calling-convention:
                   llvm-calling-convention(be, typical-callback-iep));
end entry-point-descriptor;

define singular outer entry-point-descriptor general-engine-node-3
    (engine :: <engine-node>, function :: <generic-function>, a1, a2, a3)
 => (#rest values);
  // This entry point is not currently being used
  ins--call-intrinsic(be, "llvm.trap", #[]);
  ins--unreachable(be);
end entry-point-descriptor;

define singular variable-arity outer entry-point-descriptor general-engine-node-spread
    (engine :: <engine-node>, function :: <generic-function>)
 => (#rest values);
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  // Identify the corresponding generic function
  let parent = op--parent-gf(be, function);

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

  // See if an optionals vector was passed
  let optionals-masked
    = ins--and(be, raw-properties, $signature-optionals-p-mask);
  let optionals-cmp = ins--icmp-ne(be, optionals-masked, 0);

  // Extract the function arguments
  let spreadargs-vector
    = ins--if (be, optionals-cmp)
        op--reconstruct-args-from-mepargs(be, nreq)
      ins--else
        // There is no optionals vector, so just pack up all of the args
        let va-list = op--va-decl-start(be);
        let spreadargs = op--va-list-to-stack-vector(be, va-list, nreq);
        op--va-end(be, va-list);
        spreadargs
      end ins--if;

  // Invoke the dispatch callback with on the argument to be
  // discriminated on, the function (or cache header), and the engine node.
  let callback-iep = op--engine-node-callback(be, engine);

  let typical-callback-iep = dylan-value(#"%gf-dispatch-inapplicable").^iep;
  let func-type
    = llvm-pointer-to(be, llvm-lambda-type(be, typical-callback-iep));
  let iep-func = ins--bitcast(be, callback-iep, func-type);

  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  ins--tail-call(be, iep-func,
                 vector(spreadargs-vector, parent, engine, undef, undef),
                 calling-convention:
                   llvm-calling-convention(be, typical-callback-iep));
end entry-point-descriptor;

define method op--reconstruct-args-from-mepargs
    (be :: <llvm-back-end>, nreq :: <llvm-value>)
 => (spreadargs :: <llvm-value>);
  // First skip the required arguments to find the optionals vector
  let va-list-1 = op--va-decl-start(be);
  ins--iterate skip-args-loop (be, i = 0)
    let cmp = ins--icmp-slt(be, i, nreq);
    ins--if (be, cmp)
      op--va-arg(be, va-list-1, $llvm-object-pointer-type);
      skip-args-loop(ins--add(be, i, 1));
    end ins--if;
  end ins--iterate;
  let optionals-vector = op--va-arg(be, va-list-1, $llvm-object-pointer-type);
  op--va-end(be, va-list-1);

  // Find its length
  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let optionals-vector-cast
    = op--object-pointer-cast(be, optionals-vector, sov-class);
  let vector-size
    = call-primitive(be, primitive-vector-size-descriptor,
                     optionals-vector-cast);

  // Allocate a new stack vector large enough to hold everything
  let count = ins--add(be, nreq, vector-size);
  let new-vector = op--stack-allocate-vector(be, count);

  // Store the required arguments into the vector
  let va-list-2 = op--va-decl-start(be);
  ins--iterate requireds-loop (be, i = 0)
    let cmp = ins--icmp-slt(be, i, nreq);
    ins--if (be, cmp)
      let arg = op--va-arg(be, va-list-2, $llvm-object-pointer-type);
      call-primitive(be, primitive-vector-element-setter-descriptor,
                     arg, new-vector, i);
      requireds-loop(ins--add(be, i, 1));
    end ins--if;
  end ins--iterate;
  op--va-end(be, va-list-2);

  // Store the optionals into the vector
  ins--iterate optionals-loop (be, i = 0)
    let cmp = ins--icmp-slt(be, i, vector-size);
    ins--if (be, cmp)
      let arg
        = call-primitive(be, primitive-vector-element-descriptor,
                         optionals-vector-cast, i);
      let new-index = ins--add(be, nreq, i);
      call-primitive(be, primitive-vector-element-setter-descriptor,
                     arg, new-vector, new-index);
      optionals-loop(ins--add(be, i, 1));
    end ins--if;
  end ins--iterate;

  new-vector
end method;

// For <unkeyed-single-method-engine-node>
define single-method outer entry-point-descriptor single-method
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let word-size = back-end-word-size(be);

  // Retrieve the <method> object
  let smen-class :: <&class> = dylan-value(#"<single-method-engine-node>");
  let engine-node-cast = op--object-pointer-cast(be, engine, smen-class);
  let method-ptr
    = op--getslotptr(be, engine-node-cast, smen-class,
                     #"single-method-engine-node-method");
  let meth = ins--load(be, method-ptr, alignment: word-size);

  // Retrieve the next-methods data slot
  let data-ptr
    = op--getslotptr(be, engine-node-cast, smen-class,
                     #"single-method-engine-node-data");
  let data = ins--load(be, data-ptr, alignment: word-size);

  // Retrieve the IEP from the method
  let lambda-class :: <&class> = dylan-value(#"<lambda>");
  let meth-cast = op--object-pointer-cast(be, meth, lambda-class);

  let mep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"mep");
  let iep = ins--load(be, mep-slot-ptr, alignment: word-size);

  // Chain to the method's IEP
  let iep-type
    = make(<llvm-function-type>,
           return-type: llvm-reference-type(be, be.%mv-struct-type),
           parameter-types: make(<simple-object-vector>,
                                 size: num + 2,
                                 fill: $llvm-object-pointer-type),
           varargs?: #f);
  let iep-cast = ins--bitcast(be, iep, llvm-pointer-to(be, iep-type));
  ins--tail-call
    (be, iep-cast,
     concatenate(arguments, vector(data, meth)),
     calling-convention: $llvm-calling-convention-fast)
end entry-point-descriptor;

define constant $null-object-pointer
  = make(<llvm-null-constant>, type: $llvm-object-pointer-type);

// For <implicit-keyed-single-method-engine-node>
define single-method outer entry-point-descriptor implicit-keyed-single-method
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  if (empty?(arguments))
    ins--call-intrinsic(be, "llvm.trap", #[]);
    ins--unreachable(be);
  else
    let module = be.llvm-builder-module;
    let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
    let optionals = op--object-pointer-cast(be, arguments.last, sov-class);
    let optionals-count
      = call-primitive(be, primitive-vector-size-descriptor, optionals);
    let masked = ins--and(be, optionals-count, 1);
    let odd-cmp = ins--icmp-ne(be, masked, 0);
    ins--if (be, odd-cmp)
      op--odd-keyword-arguments-error(be, function);
    ins--else
      let word-size = back-end-word-size(be);

      // Retrieve the <method> object
      let smen-class :: <&class> = dylan-value(#"<single-method-engine-node>");
      let engine-node-cast = op--object-pointer-cast(be, engine, smen-class);
      let method-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-method");
      let meth = ins--load(be, method-ptr, alignment: word-size);

      // Retrieve the next-methods data slot
      let data-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-data");
      let data = ins--load(be, data-ptr, alignment: word-size);

      // Retrieve the keyword specifiers from the method
      let lambda-class :: <&class> = dylan-value(#"<keyword-method>");
      let meth-cast = op--object-pointer-cast(be, meth, lambda-class);

      let keyword-specifiers-slot-ptr
        = op--getslotptr(be, meth-cast, lambda-class, #"keyword-specifiers");
      let keyword-specifiers
        = ins--load(be, keyword-specifiers-slot-ptr, alignment: word-size);
      let keyword-specifiers-cast
        = op--object-pointer-cast(be, keyword-specifiers, sov-class);

      // Verify that each keyword is valid
      let bad-key
        = op--verify-keywords(be, optionals, optionals-count,
                              keyword-specifiers-cast, 2);
      let bad-key-cmp = ins--icmp-ne(be, bad-key, $null-object-pointer);
      ins--if (be, bad-key-cmp)
        // This wasn't one of the keywords accepted by the method, so
        // call invalid-keyword-trap with a vector of function arguments.
        let mepargs = op--stack-allocate-vector(be, num);
        let mepargs-cast = op--object-pointer-cast(be, mepargs, sov-class);
        for (argument in arguments, i from 0)
          call-primitive(be, primitive-vector-element-setter-descriptor,
                         argument, mepargs-cast,
                         llvm-back-end-value-function(be, i));
        end for;
        op--invalid-keyword-trap(be, mepargs, function, engine, bad-key,
                                 keyword-specifiers,
                                 emit-reference(be, module, &true));
      ins--else
        // Retrieve the MEP from the method
        let mep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"mep");
        let mep = ins--load(be, mep-slot-ptr, alignment: word-size);

        // Chain to the method's MEP
        let parameter-types
          = make(<simple-object-vector>, size: num + 3);
        parameter-types[0] := $llvm-object-pointer-type; // next
        parameter-types[1] := $llvm-object-pointer-type; // function
        parameter-types[2] := be.%type-table["iWord"]; // argument count
        fill!(parameter-types, $llvm-object-pointer-type, start: 3);
        let mep-type
          = make(<llvm-function-type>,
                 return-type: llvm-reference-type(be, be.%mv-struct-type),
                 parameter-types: parameter-types,
                 varargs?: #f);
        let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
        ins--tail-call
          (be, mep-cast,
           concatenate(vector(data, meth, num), arguments),
           calling-convention: $llvm-calling-convention-c)
      end ins--if
    end ins--if
  end if
end entry-point-descriptor;

// For <explicit-keyed-single-method-engine-node>
define single-method outer entry-point-descriptor explicit-keyed-single-method
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  if (empty?(arguments))
    ins--call-intrinsic(be, "llvm.trap", #[]);
    ins--unreachable(be);
  else
    let module = be.llvm-builder-module;
    let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
    let optionals = op--object-pointer-cast(be, arguments.last, sov-class);
    let optionals-count
      = call-primitive(be, primitive-vector-size-descriptor, optionals);
    let masked = ins--and(be, optionals-count, 1);
    let odd-cmp = ins--icmp-ne(be, masked, 0);
    ins--if (be, odd-cmp)
      op--odd-keyword-arguments-error(be, function);
    ins--else
      let word-size = back-end-word-size(be);

      // Retrieve the <method> object
      let smen-class :: <&class>
        = dylan-value(#"<explicit-keyed-single-method-engine-node>");
      let engine-node-cast = op--object-pointer-cast(be, engine, smen-class);
      let method-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-method");
      let meth = ins--load(be, method-ptr, alignment: word-size);

      // Retrieve the next-methods data slot
      let data-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-data");
      let data = ins--load(be, data-ptr, alignment: word-size);

      // Retrieve the valid keys
      let keys-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-keys");
      let keys = ins--load(be, data-ptr, alignment: word-size);
      let keys-cast
        = op--object-pointer-cast(be, keys, sov-class);

      // Retrieve the keyword specifiers from the method
      // Verify that each keyword is valid
      let bad-key
        = op--verify-keywords(be, optionals, optionals-count,
                              keys-cast, 1);
      let bad-key-cmp = ins--icmp-ne(be, bad-key, $null-object-pointer);
      ins--if (be, bad-key-cmp)
        // This wasn't one of the keywords accepted by the method, so
        // call invalid-keyword-trap with a vector of function arguments.
        let mepargs = op--stack-allocate-vector(be, num);
        let mepargs-cast = op--object-pointer-cast(be, mepargs, sov-class);
        for (argument in arguments, i from 0)
          call-primitive(be, primitive-vector-element-setter-descriptor,
                         argument, mepargs-cast,
                         llvm-back-end-value-function(be, i));
        end for;
        op--invalid-keyword-trap(be, mepargs, function, engine, bad-key,
                                 keys, emit-reference(be, module, &false));
      ins--else
        // Retrieve the MEP from the method
        let lambda-class :: <&class> = dylan-value(#"<keyword-method>");
        let meth-cast = op--object-pointer-cast(be, meth, lambda-class);

        let mep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"mep");
        let mep = ins--load(be, mep-slot-ptr, alignment: word-size);

        // Chain to the method's MEP
        let parameter-types
          = make(<simple-object-vector>, size: num + 3);
        parameter-types[0] := $llvm-object-pointer-type; // next
        parameter-types[1] := $llvm-object-pointer-type; // function
        parameter-types[2] := be.%type-table["iWord"]; // argument count
        fill!(parameter-types, $llvm-object-pointer-type, start: 3);
        let mep-type
          = make(<llvm-function-type>,
                 return-type: llvm-reference-type(be, be.%mv-struct-type),
                 parameter-types: parameter-types,
                 varargs?: #f);
        let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
        ins--tail-call
          (be, mep-cast,
           concatenate(vector(data, meth, num), arguments),
           calling-convention: $llvm-calling-convention-c)
      end ins--if
    end ins--if
  end if
end entry-point-descriptor;

define method op--verify-keywords
    (be :: <llvm-back-end>,
     optionals :: <llvm-value>, optionals-count :: <llvm-value>,
     keyword-specifiers :: <llvm-value>, skip :: <integer>)
 => (bad-keyword :: <llvm-value>);
  let module = be.llvm-builder-module;
  let keyword-specifiers-count
    = call-primitive(be, primitive-vector-size-descriptor, keyword-specifiers);

  // Check each keyword in optionals in turn
  ins--iterate optionals-loop (be, i = 0)
    let i-cmp = ins--icmp-slt(be, i, optionals-count);
    ins--if (be, i-cmp)
      let keyword
        = call-primitive(be, primitive-vector-element-descriptor, optionals, i);

      // Check against each keyword specifier
      ins--iterate keywords-loop (be, j = 0)
        let j-cmp = ins--icmp-slt(be, j, keyword-specifiers-count);
        ins--if (be, j-cmp)
          let spec-keyword
            = call-primitive(be, primitive-vector-element-descriptor,
                             keyword-specifiers, j);
          let cmp = ins--icmp-eq(be, keyword, spec-keyword);
          ins--if (be, cmp)
            // Matched, go check the next keyword in optionals
            optionals-loop(ins--add(be, i, 2));
          ins--else
            // Didn't match, try the next specifier
            keywords-loop(ins--add(be, j, skip));
          end ins--if
        ins--else
          // Reached the end of the specifiers without finding a match:
          // this is a bad keyword
          keyword
        end ins--if
      end ins--iterate
    ins--else
      // Reached the end of optionals, so everything was okay
      $null-object-pointer
    end ins--if
  end ins--iterate
end method;

// For <unrestricted-keyed-single-method-engine-node>
define single-method outer entry-point-descriptor unrestricted-keyed-single-method
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  if (empty?(arguments))
    ins--call-intrinsic(be, "llvm.trap", #[]);
    ins--unreachable(be);
  else
    let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
    let optionals = op--object-pointer-cast(be, arguments.last, sov-class);
    let count = call-primitive(be, primitive-vector-size-descriptor, optionals);
    let masked = ins--and(be, count, 1);
    let odd-cmp = ins--icmp-ne(be, masked, 0);
    ins--if (be, odd-cmp)
      op--odd-keyword-arguments-error(be, function);
    ins--else
      let word-size = back-end-word-size(be);

      // Retrieve the <method> object
      let smen-class :: <&class> = dylan-value(#"<single-method-engine-node>");
      let engine-node-cast = op--object-pointer-cast(be, engine, smen-class);
      let method-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-method");
      let meth = ins--load(be, method-ptr, alignment: word-size);

      // Retrieve the next-methods data slot
      let data-ptr
        = op--getslotptr(be, engine-node-cast, smen-class,
                         #"single-method-engine-node-data");
      let data = ins--load(be, data-ptr, alignment: word-size);

      // Retrieve the MEP from the method
      let lambda-class :: <&class> = dylan-value(#"<keyword-method>");
      let meth-cast = op--object-pointer-cast(be, meth, lambda-class);

      let mep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"mep");
      let mep = ins--load(be, mep-slot-ptr, alignment: word-size);

      // Chain to the method's MEP
      let parameter-types
        = make(<simple-object-vector>, size: num + 3);
      parameter-types[0] := $llvm-object-pointer-type; // next
      parameter-types[1] := $llvm-object-pointer-type; // function
      parameter-types[2] := be.%type-table["iWord"]; // argument count
      fill!(parameter-types, $llvm-object-pointer-type, start: 3);
      let mep-type
        = make(<llvm-function-type>,
               return-type: llvm-reference-type(be, be.%mv-struct-type),
               parameter-types: parameter-types,
               varargs?: #f);
      let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
      ins--tail-call
        (be, mep-cast,
         concatenate(vector(data, meth, num), arguments),
         calling-convention: $llvm-calling-convention-c)
    end ins--if
  end if
end entry-point-descriptor;

define outer cache-header entry-point-descriptor cache-header
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let chen-class :: <&class> = dylan-value(#"<cache-header-engine-node>");
  let engine-cast = op--object-pointer-cast(be, engine, chen-class);

  let next-engine-ptr
    = op--getslotptr(be, engine-cast,
                     chen-class, #"cache-header-engine-node-next");
  let next-engine
    = ins--load(be, next-engine-ptr, alignment: back-end-word-size(be));

  // Chain to the next engine node's entry point; this cache-header engine node
  // becomes the new <dispatch-starter>
  op--chain-to-engine-entry-point(be, next-engine, engine, arguments)
end entry-point-descriptor;

define outer cache-header entry-point-descriptor profiling-cache-header
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let pcschen-class :: <&class>
    = dylan-value(#"<profiling-call-site-cache-header-engine-node>");
  let engine-cast = op--object-pointer-cast(be, engine, pcschen-class);

  // Increment the profiling counter atomically
  // FIXME declare engine node count fields as <raw-machine-word>
  let rmw-slotptr-type
    = llvm-pointer-to(be, llvm-reference-type(be, dylan-value(#"<raw-machine-word>")));
  let count-1-ptr
    = op--getslotptr(be, engine-cast,
                     pcschen-class,
                     #"profiling-call-site-cache-header-engine-node-count-1");
  let count-1-ptr-cast
    = ins--bitcast(be, count-1-ptr, rmw-slotptr-type);
  let old = ins--atomicrmw-add(be, count-1-ptr-cast, 1,
                               ordering: #"sequentially-consistent");

  // Check to see if count-1 overflowed
  let overflow-cmp = ins--icmp-eq(be, old, -1);
  ins--if (be, overflow-cmp)
    // It did, so increment count-2 as well
    let count-2-ptr
      = op--getslotptr(be, engine-cast,
                       pcschen-class,
                       #"profiling-call-site-cache-header-engine-node-count-2");
    let count-2-ptr-cast
      = ins--bitcast(be, count-2-ptr, rmw-slotptr-type);
    ins--atomicrmw-add(be, count-2-ptr-cast, 1,
                       ordering: #"sequentially-consistent");
  end ins--if;

  let callback-iep-ptr
    = op--getslotptr(be, engine-cast,
                     pcschen-class, #"cache-header-engine-node-next");
  let next-engine
    = ins--load(be, callback-iep-ptr, alignment: back-end-word-size(be));

  // Chain to the next engine node's entry point; this cache-header engine node
  // becomes the new <dispatch-starter>
  op--chain-to-engine-entry-point(be, next-engine, engine, arguments)
end entry-point-descriptor;

define method op--slot-access-engine-node-offset
    (be :: <llvm-back-end>, engine :: <llvm-value>)
 => (offset :: <llvm-value>)
  let word-size = back-end-word-size(be);

  // Retrieve the properties slot from the engine node
  let saen-class :: <&class> = dylan-value(#"<slot-access-engine-node>");
  let engine-node-cast = op--object-pointer-cast(be, engine, saen-class);
  let properties-slot-ptr
    = op--getslotptr(be, engine-node-cast, saen-class, #"properties");
  let properties = ins--load(be, properties-slot-ptr, alignment: word-size);
  let raw-properties = op--untag-integer(be, properties);

  // Extract the slot offset
  ins--ashr(be, raw-properties, slotdiscrim$v-offset)
end method;

define singular outer entry-point-descriptor boxed-instance-slot-getter
    (engine :: <engine-node>, function :: <generic-function>,
     object :: <object>)
 => (#rest values);
  let offset = op--slot-access-engine-node-offset(be, engine);
  let value = call-primitive(be, primitive-slot-value-descriptor, object, offset);
  op--global-mv-struct(be, value, i8(1))
end entry-point-descriptor;

define singular outer entry-point-descriptor boxed-instance-slot-setter
    (engine :: <engine-node>, function :: <generic-function>,
     value :: <object>, object :: <object>)
 => (#rest values);
  let offset = op--slot-access-engine-node-offset(be, engine);
  call-primitive(be, primitive-slot-value-setter-descriptor,
                 value, object, offset);
  op--global-mv-struct(be, value, i8(1))
end entry-point-descriptor;

define singular outer entry-point-descriptor boxed-repeated-instance-slot-getter
    (engine :: <engine-node>, function :: <generic-function>,
     object :: <object>, index :: <integer>)
 => (#rest values);
  let module = be.llvm-builder-module;
  let index-raw = op--untag-integer(be, index);

  // Find the repeated slot and its size
  let repeated-slot-offset = op--slot-access-engine-node-offset(be, engine);
  let repeated-size-offset = ins--sub(be, repeated-slot-offset, 1);
  let repeated-size
    = call-primitive(be, primitive-initialized-slot-value-descriptor,
                     object, repeated-size-offset);
  let repeated-size-raw = op--untag-integer(be, repeated-size);

  // Check for out-of-range index values
  let range-cmp = ins--icmp-ult(be, index-raw, repeated-size-raw);
  ins--if (be, range-cmp)
    // Read the value and check for unbound
    let value = call-primitive(be, primitive-repeated-slot-value-descriptor,
                               object, repeated-slot-offset, index-raw);
    let unbound-cmp
      = ins--icmp-ne(be, value, emit-reference(be, module, &unbound));
    ins--if (be, unbound-cmp)
      op--global-mv-struct(be, value, i8(1))
    ins--else
      op--call-error-iep(be, #"unbound-repeated-slot", object, index);
    end ins--if
  ins--else
    op--call-error-iep(be, #"repeated-slot-getter-index-out-of-range-trap",
                       object, index);
  end ins--if
end entry-point-descriptor;

define singular outer entry-point-descriptor boxed-repeated-instance-slot-setter
    (engine :: <engine-node>, function :: <generic-function>,
     value :: <object>, object :: <object>, index :: <integer>)
 => (#rest values);
  let module = be.llvm-builder-module;
  let index-raw = op--untag-integer(be, index);

  // Find the repeated slot and its size
  let repeated-slot-offset = op--slot-access-engine-node-offset(be, engine);
  let repeated-size-offset = ins--sub(be, repeated-slot-offset, 1);
  let repeated-size
    = call-primitive(be, primitive-initialized-slot-value-descriptor,
                     object, repeated-size-offset);
  let repeated-size-raw = op--untag-integer(be, repeated-size);

  // Check for out-of-range index values
  let range-cmp = ins--icmp-ult(be, index-raw, repeated-size-raw);
  ins--if (be, range-cmp)
    // Set the value
    call-primitive(be, primitive-repeated-slot-value-setter-descriptor,
                   value, object, repeated-slot-offset, index-raw);
    op--global-mv-struct(be, value, i8(1))
  ins--else
    op--call-error-iep(be, #"repeated-slot-setter-index-out-of-range-trap",
                       value, object, index);
  end ins--if
end entry-point-descriptor;

define singular outer entry-point-descriptor raw-byte-repeated-instance-slot-getter
    (engine :: <engine-node>, function :: <generic-function>,
     object :: <object>, index :: <integer>)
 => (#rest values);
  let index-raw = op--untag-integer(be, index);

  // Find the repeated slot and its size
  let repeated-slot-offset = op--slot-access-engine-node-offset(be, engine);
  let repeated-size-offset = ins--sub(be, repeated-slot-offset, 1);
  let repeated-size
    = call-primitive(be, primitive-initialized-slot-value-descriptor,
                     object, repeated-size-offset);
  let repeated-size-raw = op--untag-integer(be, repeated-size);

  // Check for out-of-range index values
  let range-cmp = ins--icmp-ult(be, index-raw, repeated-size-raw);
  ins--if (be, range-cmp)
    let repeated-element-offset
      = ins--add(be, repeated-slot-offset,
                 dylan-value(#"$number-header-words"));
    let value = call-primitive(be, primitive-byte-element-descriptor,
                               object, repeated-element-offset, index-raw);
    let zext = ins--zext(be, value, be.%type-table["iWord"]);
    let character-value = op--tag-character(be, zext);
    op--global-mv-struct(be, character-value, i8(1))
  ins--else
    op--call-error-iep(be, #"repeated-slot-getter-index-out-of-range-trap",
                       object, index);
  end ins--if
end entry-point-descriptor;

define singular outer entry-point-descriptor raw-byte-repeated-instance-slot-setter
    (engine :: <engine-node>, function :: <generic-function>,
     value :: <object>, object :: <object>, index :: <integer>)
 => (#rest values);
  let module = be.llvm-builder-module;
  let index-raw = op--untag-integer(be, index);

  // Find the repeated slot and its size
  let repeated-slot-offset = op--slot-access-engine-node-offset(be, engine);
  let repeated-size-offset = ins--sub(be, repeated-slot-offset, 1);
  let repeated-size
    = call-primitive(be, primitive-initialized-slot-value-descriptor,
                     object, repeated-size-offset);
  let repeated-size-raw = op--untag-integer(be, repeated-size);

  // Check for out-of-range index values
  let range-cmp = ins--icmp-ult(be, index-raw, repeated-size-raw);
  ins--if (be, range-cmp)
    // Set the value
    let repeated-element-offset
      = ins--add(be, repeated-slot-offset,
                 dylan-value(#"$number-header-words"));
    let raw-value = op--untag-character(be, value);
    call-primitive(be, primitive-byte-element-setter-descriptor,
                   raw-value, object, repeated-element-offset, index-raw);
    op--global-mv-struct(be, value, i8(1))
  ins--else
    op--call-error-iep(be, #"repeated-slot-setter-index-out-of-range-trap",
                       value, object, index);
  end ins--if
end entry-point-descriptor;

// Discriminators

define cross outer entry-point-descriptor discriminate-on-argument
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let callback-iep = op--engine-node-callback(be, engine);

  // Invoke the discriminator callback with on the argument to be
  // discriminated on, the function (or cache header), and the engine node.
  let typical-callback-iep = dylan-value(#"%gf-dispatch-linear-by-class").^iep;
  let func-type
    = llvm-pointer-to(be, llvm-lambda-type(be, typical-callback-iep));
  let iep-func = ins--bitcast(be, callback-iep, func-type);

  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  let callback-ret
    = ins--call(be, iep-func,
                vector(arguments[pos], function, engine, undef, undef),
                calling-convention:
                  llvm-calling-convention(be, typical-callback-iep));
  let next-engine = ins--extractvalue(be, callback-ret, 0);

  // Chain to the engine node's entry point
  op--chain-to-engine-entry-point(be, next-engine, function, arguments)
end entry-point-descriptor;

define cross outer entry-point-descriptor if-type-discriminator
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let word-size = back-end-word-size(be);
  let class :: <&class> = dylan-value(#"<if-type-discriminator>");
  let discriminator = op--object-pointer-cast(be, engine, class);

  let discriminator-type-ptr
    = op--getslotptr(be, discriminator,
                     class, #"if-type-discriminator-type");
  let discriminator-type
    = ins--load(be, discriminator-type-ptr, alignment: word-size);

  let cmp = do-emit-instance-cmp(be, arguments[pos], #f, discriminator-type);
  let next-engine-ptr
    = ins--if (be, cmp)
        op--getslotptr(be, discriminator,
                       class, #"if-type-discriminator-then");
      ins--else
        op--getslotptr(be, discriminator,
                       class, #"if-type-discriminator-else");
      end ins--if;
  let next-engine = ins--load(be, next-engine-ptr, alignment: word-size);

  // Chain to the engine node's entry point
  op--chain-to-engine-entry-point(be, next-engine, function, arguments)
end entry-point-descriptor;

define cross outer entry-point-descriptor typecheck-discriminator
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let class :: <&class> = dylan-value(#"<typecheck-discriminator>");
  let discriminator = op--object-pointer-cast(be, engine, class);

  let discriminator-type-ptr
    = op--getslotptr(be, discriminator,
                     class, #"typecheck-discriminator-type");
  let discriminator-type
    = ins--load(be, discriminator-type-ptr, alignment: word-size);

  let cmp = do-emit-instance-cmp(be, arguments[pos], #f, discriminator-type);
  let next-engine
    = ins--if (be, cmp)
        let ptr = op--getslotptr(be, discriminator,
                                 class, #"typecheck-discriminator-type");
        ins--load(be, ptr, alignment: word-size);
      ins--else
        emit-reference(be, module, dylan-value(#"$inapplicable-engine-node"))
      end ins--if;

  // Chain to the engine node's entry point
  op--chain-to-engine-entry-point(be, next-engine, function, arguments)
end entry-point-descriptor;

// Discriminate using a one-class discriminator
define cross outer entry-point-descriptor monomorphic-by-class-discriminator
    (engine :: <engine-node>, function :: <generic-function>, #rest arguments)
 => (#rest values);
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  // Retrieve the <mm-wrapper> for the argument
  let wrapper = op--object-mm-wrapper(be, arguments[pos]);

  // The unique-key is the wrapper address re-tagged as an integer
  let wrapper-cast = ins--bitcast(be, wrapper, $llvm-object-pointer-type);
  let key = ins--gep(be, wrapper-cast, $dylan-tag-integer - $dylan-tag-pointer);

  // Retrieve the key from the engine node
  let class :: <&class> = dylan-value(#"<monomorphic-by-class-discriminator>");
  let discriminator = op--object-pointer-cast(be, engine, class);
  let discriminator-key-ptr
    = op--getslotptr(be, discriminator,
                     class, #"monomorphic-by-class-discriminator-key");
  let discriminator-key
    = ins--load(be, discriminator-key-ptr, alignment: word-size);

  // Check that the class matches
  let cmp = ins--icmp-eq(be, key, discriminator-key);
  let next-engine
    = ins--if (be, cmp)
        let discriminator-next-ptr
          = op--getslotptr(be, discriminator,
                           class, #"monomorphic-by-class-discriminator-next");
        ins--load(be, discriminator-next-ptr, alignment: word-size)
      ins--else
        emit-reference(be, module, dylan-value(#"$absent-engine-node"))
      end ins--if;

  // Chain to the engine node's entry point
  op--chain-to-engine-entry-point(be, next-engine, function, arguments)
end entry-point-descriptor;
