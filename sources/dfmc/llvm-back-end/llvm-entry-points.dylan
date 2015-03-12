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

define function llvm-emit-entry-point-dbg-function
    (back-end :: <llvm-back-end>, function :: <llvm-function>,
     dbg-file :: <llvm-metadata-value>,
     desc :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
 => ();
  let (dbg-function :: <llvm-metadata-value>, dbg-parameters :: <sequence>)
    = apply(make-entry-point-dbg-function, back-end, function, dbg-file,
            desc, count, desc.entry-point-function-declarator);
  add!(back-end.llvm-back-end-dbg-functions, dbg-function);

  // Emit a llvm.dbg.value call for each parameter
  ins--dbg(back-end, 0, 0, dbg-function, #f);
  for (dbg-parameter in dbg-parameters,
       argument in function.llvm-function-arguments)
    let v
      = make(<llvm-metadata-node>,
             function-local?: #t,
             node-values: list(argument));
    ins--call-intrinsic(back-end, "llvm.dbg.value",
                        vector(v, i64(0), dbg-parameter));
  end for;
end function;

define function make-entry-point-dbg-function
    (back-end :: <llvm-back-end>, function :: <llvm-function>,
     dbg-file :: <llvm-metadata-value>,
     descriptor :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>),
     #key parameter-names :: <simple-object-vector> = #[],
          parameter-types-spec :: <simple-object-vector>,
     #all-keys)
 => (dbg-function :: <llvm-metadata-value>, dbg-parameters :: <sequence>);
  let dbg-parameter-types = make(<stretchy-object-vector>);
  let (required-parameter-type-specs, required-parameter-names,
       rest-parameter-name)
    = if (~empty?(parameter-types-spec) & parameter-types-spec.last == #"rest")
        let required-count = parameter-types-spec.size - 1;
        values(copy-sequence(parameter-types-spec, end: required-count),
               copy-sequence(parameter-names, end: required-count),
               parameter-names.last)
      else
        values(parameter-types-spec, parameter-names, #f)
      end if;
  // Required parameters
  for (type-spec in required-parameter-type-specs)
    add!(dbg-parameter-types,
         llvm-reference-dbg-type(back-end, dylan-value(type-spec)));
  end for;
  // Remaining parameters
  if (rest-parameter-name)
    let obj-dbg-type
      = llvm-reference-dbg-type(back-end, dylan-value(#"<object>"));
    for (i from 0 below count)
      add!(dbg-parameter-types, obj-dbg-type);
    end for;
  end if;

  if (member?(#"variable-arity", descriptor.entry-point-attributes))
    add!(dbg-parameter-types, llvm-make-dbg-unspecified-parameters());
  end if;

  let dbg-return-type
    = llvm-reference-dbg-type(back-end, back-end.%mv-struct-type);
  let dbg-function-type
    = llvm-make-dbg-function-type(dbg-file, dbg-return-type,
                                  dbg-parameter-types);

  let dbg-name = descriptor.entry-point-name;
  let dbg-function
    = llvm-make-dbg-function(dbg-file,
                             dbg-name,
                             function.llvm-global-name,
                             dbg-file,
                             0,
                             dbg-function-type,
                             definition?: #t,
                             function: function);

  let all-parameter-names
    = if (rest-parameter-name)
        concatenate(required-parameter-names,
                    map(curry(format-to-string, "%s-%d", rest-parameter-name),
                        range(below: count)))
      else
        required-parameter-names
      end;
  let dbg-parameters
    = map(method (parameter-name, dbg-parameter-type, index)
            llvm-make-dbg-local-variable(#"argument",
                                         dbg-function,
                                         as(<string>, parameter-name),
                                         dbg-file, 0,
                                         dbg-parameter-type,
                                         arg: index)
          end,
          all-parameter-names, dbg-parameter-types, range(from: 1));

  values(dbg-function, dbg-parameters)
end function;


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

define method llvm-engine-node-ep-info
    (back-end :: <llvm-back-end>, ep :: <&engine-node-ep>)
 => (descriptor :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>),
     pos :: false-or(<integer>));
  values($llvm-entry-point-descriptors[ep.^entry-point-name], #f, #f)
end method;

define method llvm-engine-node-ep-info
    (back-end :: <llvm-back-end>, ep :: <&function-linked-engine-node-ep>)
 => (descriptor :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>),
     pos :: false-or(<integer>));
  let mepargs-count
    = if (^engine-node-ep-optionals?(ep))
        ^engine-node-ep-number-required(ep) + 1
      else
        ^engine-node-ep-number-required(ep)
      end;
  values($llvm-entry-point-descriptors[ep.^entry-point-name], mepargs-count, #f)
end method;

define method llvm-engine-node-ep-info
    (back-end :: <llvm-back-end>, ep :: <&discriminator-ep>)
 => (descriptor :: <llvm-entry-point-descriptor>,
     count :: false-or(<integer>),
     pos :: false-or(<integer>));
  let e = ^engine-node(ep);
  let count = ^discriminator-nrequired(e);
  let pos = ^discriminator-argnum(e) + 1;
  values($llvm-entry-point-descriptors[ep.^entry-point-name], count, pos)
end method;

define method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, ep :: <&engine-node-ep>)
 => (reference :: <llvm-constant-value>)
  let (desc :: <llvm-entry-point-descriptor>,
       count :: false-or(<integer>),
       pos :: false-or(<integer>)) = llvm-engine-node-ep-info(back-end, ep);
  let func = llvm-entry-point-function(back-end, desc, count, pos: pos);
  make(<llvm-cast-constant>, operator: #"BITCAST",
       type: $llvm-object-pointer-type,
       operands: vector(func))
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
    (next :: <list>, meth :: <lambda>, #rest arguments) => (#rest values);
  if (empty?(arguments))
    // The 0-argument apply-mep will never be called
    make(<llvm-undef-constant>,
         type: llvm-reference-type(be, be.%mv-struct-type))
  else
    let word-size = back-end-word-size(be);
    let return-type = llvm-reference-type(be, be.%mv-struct-type);
    let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");

    let lambda-class :: <&class> = dylan-value(#"<lambda>");
    let meth-cast = op--object-pointer-cast(be, meth, lambda-class);

    // Retrieve the MEP from the method
    let mep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"mep");
    let mep = ins--load(be, mep-slot-ptr, alignment: word-size);

    // Extract the required arguments count
    let (signature, raw-properties)
      = op--function-signature-properties(be, meth-cast, lambda-class);
    let nreq = ins--and(be, raw-properties, $signature-number-required-mask);

    // Determine which calling convention applies
    let km-class :: <&class> = dylan-value(#"<keyword-method>");
    let cmp = op--heap-object-subtype-bit-instance-cmp(be, meth, km-class);
    ins--if (be, cmp)
      // Determine if there are extra arguments
      let excess = ins--sub(be, num - 1, nreq);

      // Create a basic block for each case
      let min-count
        = if (num = 1) -$entry-point-argument-count else 0 end if;
      let jump-table = make(<stretchy-object-vector>);
      for (count from min-count below num)
        let name
          = if (count < 0)
              format-to-string("bb.deficit%d", -count)
            else
              format-to-string("bb.surplus%d", count)
            end if;
        add!(jump-table, count);
        add!(jump-table, make(<llvm-basic-block>, name: name));
      end;
      let default-bb = make(<llvm-basic-block>, name: "bb.default");
      let return-bb = make(<llvm-basic-block>, name: "bb.return");

      // Branch to the appropriate case
      ins--switch(be, excess, default-bb, jump-table);

      // Generate all of the cases
      let result-phi-arguments = make(<stretchy-object-vector>);
      for (i from 0 below jump-table.size by 2)
        let count = jump-table[i];
        ins--block(be, jump-table[i + 1]);

        let new-arguments
          = if (count < 0)
              // Allocate a new (reduced) optionals vector
              let optionals-cast
                = op--object-pointer-cast(be, arguments.last, sov-class);
              let optionals-size
                = call-primitive(be, primitive-vector-size-descriptor,
                                 optionals-cast);
              let cmp = ins--icmp-slt(be, optionals-size, -count);
              ins--if (be, op--unlikely(be, cmp))
                op--argument-count-error(be, meth, optionals-size);
              end ins--if;

              let new-optionals-size = ins--add(be, count, optionals-size);
              let new-optionals
                = op--stack-allocate-vector(be, new-optionals-size);

              // Copy the contents of the original vector into the new one
              let zero = llvm-back-end-value-function(be, 0);
              let src = op--getslotptr(be, optionals-cast, sov-class,
                                       #"vector-element", -count);
              let new-optionals-cast
                = op--object-pointer-cast(be, new-optionals, sov-class);
              let dst = op--getslotptr(be, new-optionals-cast, sov-class,
                                       #"vector-element", 0);
              call-primitive(be, primitive-replace!-descriptor,
                             dst, zero, zero,
                             src, zero, zero,
                             new-optionals-size);

              // Extract needed arguments from the optionals vector
              let extracted-arguments
                = map(method (i)
                        call-primitive(be, primitive-vector-element-descriptor,
                                       optionals-cast,
                                       llvm-back-end-value-function(be, i))
                      end, range(below: -count));
              concatenate(extracted-arguments, vector(new-optionals))
            elseif (zero?(count))
              arguments
            else
              // Allocate a new (expanded) optionals vector
              let optionals-cast
                = op--object-pointer-cast(be, arguments.last, sov-class);
              let optionals-size
                = call-primitive(be, primitive-vector-size-descriptor,
                                 optionals-cast);

              let new-optionals-size = ins--add(be, count, optionals-size);
              let new-optionals
                = op--stack-allocate-vector(be, new-optionals-size);

              // Store the excess arguments at the beginning of the new vector
              for (i from 0 below count)
                call-primitive(be, primitive-vector-element-setter-descriptor,
                               arguments[num - 1 - count + i], new-optionals,
                               llvm-back-end-value-function(be, i));
              end for;

              // Copy the contents of the original vector into the new one
              let zero = llvm-back-end-value-function(be, 0);
              let src = op--getslotptr(be, optionals-cast, sov-class,
                                       #"vector-element", 0);
              let new-optionals-cast
                = op--object-pointer-cast(be, new-optionals, sov-class);
              let dst = op--getslotptr(be, new-optionals-cast, sov-class,
                                       #"vector-element", count);
              call-primitive(be, primitive-replace!-descriptor,
                             dst, zero, zero,
                             src, zero, zero,
                             optionals-size);

              concatenate(copy-sequence(arguments, end: num - 1 - count),
                          vector(new-optionals))
            end if;

        // Cast to the appropriate MEP type
        let parameter-types
          = make(<simple-object-vector>,
                 size: 2 + num - count,
                 fill: $llvm-object-pointer-type);
        let mep-type
          = make(<llvm-function-type>,
                 return-type: return-type,
                 parameter-types: parameter-types,
                 varargs?: #f);
        let mep-cast
          = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));

        // Call the method
        let result
          = op--call(be, mep-cast,
                     concatenate(vector(meth, next), new-arguments),
                     type: return-type,
                     calling-convention: $llvm-calling-convention-c);
        add!(result-phi-arguments, result);
        add!(result-phi-arguments, be.llvm-builder-basic-block);
        ins--br(be, return-bb);
      end for;

      // Unhandled case
      ins--block(be, default-bb);
      ins--call-intrinsic(be, "llvm.trap", vector());
      ins--unreachable(be);

      // Return
      ins--block(be, return-bb);
      ins--phi(be, result-phi-arguments)
    ins--else
      // FIXME
      ins--call-intrinsic(be, "llvm.trap", #[]);
      ins--unreachable(be);
    end ins--if
  end if
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
  ins--br(be, op--unlikely(be, cmp), error-bb, typecheck-bb);

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
  ins--br(be, op--unlikely(be, cmp), error-bb, typecheck-bb);

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
  ins--call
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
  if (num < 1)
    ins--call-intrinsic(be, "llvm.trap", #[]);
    ins--unreachable(be);
  else
    let module = be.llvm-builder-module;
    let word-size = back-end-word-size(be);

    let lambda-class :: <&class> = dylan-value(#"<keyword-method>");
    let meth-cast = op--object-pointer-cast(be, function, lambda-class);

    let (signature, raw-properties)
      = op--function-signature-properties(be, meth-cast, lambda-class);

    // Extract the required arguments count
    let nreq = ins--and(be, raw-properties, $signature-number-required-mask);

    // Check argument count
    let cmp = ins--icmp-slt(be, n, nreq);
    ins--if (be, op--unlikely(be, cmp))
      op--argument-count-error(be, function, n);
    ins--else
      // Allocate a buffer for the required, optionals, and keyword arguments
      let buf = ins--alloca(be, $llvm-object-pointer-type, num,
                            alignment: word-size);

      // Retrieve the types of the required arguments
      let signature-required-slot-ptr
        = op--getslotptr(be, signature,
                         #"<signature>", #"signature-required");
      let required = ins--load(be, signature-required-slot-ptr,
                               alignment: word-size);
      let required-cast
        = op--object-pointer-cast(be, required,
                                  #"<simple-object-vector>");

      // Type-check and buffer the required arguments
      let va-list = op--va-decl-start(be);
      ins--iterate required-args-loop (be, i = 0)
        let loop-cmp = ins--icmp-slt(be, i, nreq);
        ins--if (be, loop-cmp)
          let arg = op--va-arg(be, va-list, $llvm-object-pointer-type);

          let specializer
            = call-primitive(be, primitive-vector-element-descriptor,
                             required-cast, i);
          do-emit-type-check(be, arg, #f, specializer);

          let element-ptr = ins--gep(be, buf, i);
          ins--store(be, arg, element-ptr, alignment: word-size);
          required-args-loop(ins--add(be, i, 1));
        end ins--if;
      end ins--iterate;

      // Retrieve the #rest arguments as a vector
      let count = ins--sub(be, n, nreq);
      let optionals = op--va-list-to-stack-vector(be, va-list, count);
      op--va-end(be, va-list);

      // Retrieve the keyword specifiers
      let keyword-specifiers-slot-ptr
        = op--getslotptr(be, meth-cast, lambda-class, #"keyword-specifiers");
      let keyword-specifiers
        = ins--load(be, keyword-specifiers-slot-ptr, alignment: word-size);

      // Place keyword values into their proper slots
      op--process-keyword-optionals(be, num, buf, nreq,
                                    optionals, keyword-specifiers);

      let arguments
        = map(method (i)
                let element-ptr = ins--gep(be, buf, i);
                ins--load(be, element-ptr, alignment: word-size);
              end, range(below: num));

      // Retrieve the IEP from the method
      let iep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"iep");
      let iep = ins--load(be, iep-slot-ptr, alignment: word-size);

      // Chain to the method's IEP
      let iep-type
        = make(<llvm-function-type>,
               return-type: llvm-reference-type(be, be.%mv-struct-type),
               parameter-types: make(<simple-object-vector>,
                                     size: num + 2,
                                     fill: $llvm-object-pointer-type),
               varargs?: #f);
      let iep-cast = ins--bitcast(be, iep, llvm-pointer-to(be, iep-type));
      ins--call
        (be, iep-cast,
         concatenate(arguments,
                     vector(emit-reference(be, module, &false), function)),
         calling-convention: $llvm-calling-convention-fast);
    end ins--if;
  end if
end entry-point-descriptor;

define method op--engine-node-call
    (be :: <llvm-back-end>, function :: <llvm-value>, arguments :: <sequence>,
     #key tail-call? = #f)
 => (call :: <llvm-value>);
  let word-size = back-end-word-size(be);

  // Retrieve the dispatch engine
  let gf-class :: <&class> = dylan-value(#"<generic-function>");
  let function-cast = op--object-pointer-cast(be, function, gf-class);

  let discriminator-slot-ptr
    = op--getslotptr(be, function-cast, gf-class, #"discriminator");
  let engine = ins--load(be, discriminator-slot-ptr, alignment: word-size);

  op--chain-to-engine-entry-point(be, engine, function, arguments,
				  tail-call?: tail-call?)
end method;

define method op--chain-to-engine-entry-point
    (be :: <llvm-back-end>, engine :: <llvm-value>, function :: <llvm-value>,
     arguments :: <sequence>,
     #key tail-call? = #f)
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
     tail-call?: tail-call?)
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
  ins--br(be, op--unlikely(be, cmp), error-bb, call-bb);

  // If argument counts do not match, throw an error
  ins--block(be, error-bb);
  op--argument-count-error(be, function, n);

  // Call using the dispatch engine
  ins--block(be, call-bb);
  op--engine-node-call(be, function, arguments, tail-call?: #t)
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
  ins--br(be, op--unlikely(be, cmp), error-bb, call-bb);

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

define method op--function-signature-properties
    (be :: <llvm-back-end>, function :: <llvm-value>, class :: <&class>)
 => (signature :: <llvm-value>, raw-properties :: <llvm-value>)
  let word-size = back-end-word-size(be);

  let signature-slot-ptr
    = op--getslotptr(be, function, class, #"function-signature");
  let signature = ins--load(be, signature-slot-ptr, alignment: word-size);

  // Extract the signature properties
  let sig-class :: <&class> = dylan-value(#"<signature>");
  let signature-cast = op--object-pointer-cast(be, signature, sig-class);
  let properties-slot-ptr
    = op--getslotptr(be, signature-cast, sig-class, #"signature-properties");
  let properties = ins--load(be, properties-slot-ptr, alignment: word-size);
  let raw-properties = op--untag-integer(be, properties);

  values(signature-cast, raw-properties)
end method;

define method op--process-keyword-optionals
    (be :: <llvm-back-end>, num :: <integer>,
     buf :: <llvm-value>, nreq :: <llvm-value>,
     optionals :: <llvm-value>, keyword-specifiers :: <llvm-value>)
 => ();
  let word-size = back-end-word-size(be);

  // The optionals are passed as the first argument following the
  // required arguments
  let optionals-element-ptr = ins--gep(be, buf, nreq);
  ins--store(be, optionals, optionals-element-ptr, alignment: word-size);

  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let optionals-cast = op--object-pointer-cast(be, optionals, sov-class);
  let optionals-size
    = call-primitive(be, primitive-vector-size-descriptor, optionals-cast);

  let keyword-specifiers-cast
    = op--object-pointer-cast(be, keyword-specifiers, sov-class);
  let keyword-specifiers-size
    = call-primitive(be, primitive-vector-size-descriptor,
                     keyword-specifiers-cast);

  // Initialize default values for each keyword argument
  let key0 = ins--add(be, nreq, 1);
  ins--iterate default-keywords-loop (be, i = key0, ki = 1)
    let cmp = ins--icmp-slt(be, i, num);
    ins--if (be, cmp)
      let default = call-primitive(be, primitive-vector-element-descriptor,
                                   keyword-specifiers-cast, ki);
      let element-ptr = ins--gep(be, buf, i);
      ins--store(be, default, element-ptr, alignment: word-size);
      default-keywords-loop(ins--add(be, i, 1), ins--add(be, ki, 2));
    end ins--if;
  end ins--iterate;

  // Put keyword values in the proper slots
  let optionals-last-index = ins--sub(be, optionals-size, 2);
  ins--iterate process-keywords-loop (be, i = optionals-last-index)
    let keywords-cmp = ins--icmp-sge(be, i, 0);
    ins--if (be, keywords-cmp)
      let keyword = call-primitive(be, primitive-vector-element-descriptor,
                                   optionals-cast, i);

      // Check against each keyword specifier
      ins--iterate search-specifiers-loop (be, j = 0)
        let specifiers-cmp = ins--icmp-slt(be, j, keyword-specifiers-size);
        ins--if (be, specifiers-cmp)
          let spec-keyword
            = call-primitive(be, primitive-vector-element-descriptor,
                             keyword-specifiers-cast, j);
          let cmp = ins--icmp-eq(be, keyword, spec-keyword);
          ins--if (be, cmp)
            // Matched, read the value
            let value-index = ins--add(be, i, 1);
            let value = call-primitive(be, primitive-vector-element-descriptor,
                                       optionals-cast, value-index);

            // Store the value in its corresponding slot
            let offset = ins--ashr(be, j, 1);
            let slot = ins--add(be, key0, offset);
            let element-ptr = ins--gep(be, buf, slot);
            ins--store(be, value, element-ptr, alignment: word-size);

            // Go to the next keyword in optionals
            process-keywords-loop(ins--sub(be, i, 2));
          ins--else
            // Didn't match, try the next specifier
            search-specifiers-loop(ins--add(be, j, 2));
          end ins--if;
        ins--else
          // Reached the end of the specifiers without finding a match:
          // go to the next keyword in optionals
          process-keywords-loop(ins--sub(be, i, 2));
        end ins--if;
      end ins--iterate;
    end ins--if;
  end ins--iterate;
end method;

// For MEP calls with #key (and possibly #rest)
// (numbered by the total number of parameters in the IEP)
// Note that this is called as if it were an engine-node entry point
// if the method has no keyword restrictions and the next-methods
// argument is unused
define variable-arity outer entry-point-descriptor rest-key-mep
    (meth :: <keyword-method>, next-methods :: <object>)
 => (#rest values);
  if (num < 1)
    ins--call-intrinsic(be, "llvm.trap", #[]);
    ins--unreachable(be);
  else
    let word-size = back-end-word-size(be);

    let lambda-class :: <&class> = dylan-value(#"<keyword-method>");
    let meth-cast = op--object-pointer-cast(be, meth, lambda-class);

    let (signature, raw-properties)
      = op--function-signature-properties(be, meth-cast, lambda-class);

    // Extract the required arguments count
    let nreq = ins--and(be, raw-properties, $signature-number-required-mask);

    // Allocate a buffer for the required and keyword arguments
    let buf = ins--alloca(be, $llvm-object-pointer-type, num,
                          alignment: word-size);

    // Buffer the required arguments
    let va-list = op--va-decl-start(be);
    ins--iterate required-args-loop (be, i = 0)
      let cmp = ins--icmp-slt(be, i, nreq);
      ins--if (be, cmp)
        let arg = op--va-arg(be, va-list, $llvm-object-pointer-type);
        let element-ptr = ins--gep(be, buf, i);
        ins--store(be, arg, element-ptr, alignment: word-size);
        required-args-loop(ins--add(be, i, 1));
      end ins--if;
    end ins--iterate;

    // Retrieve the optionals vector
    let optionals = op--va-arg(be, va-list, $llvm-object-pointer-type);
    op--va-end(be, va-list);

    // Retrieve the keyword specifiers
    let keyword-specifiers-slot-ptr
      = op--getslotptr(be, meth-cast, lambda-class, #"keyword-specifiers");
    let keyword-specifiers
      = ins--load(be, keyword-specifiers-slot-ptr, alignment: word-size);

    // Place keyword values into their proper slots
    op--process-keyword-optionals(be, num, buf, nreq,
                                  optionals, keyword-specifiers);

    let arguments
      = map(method (i)
              let element-ptr = ins--gep(be, buf, i);
              ins--load(be, element-ptr, alignment: word-size);
            end, range(below: num));

    // Retrieve the IEP from the method
    let iep-slot-ptr = op--getslotptr(be, meth-cast, lambda-class, #"iep");
    let iep = ins--load(be, iep-slot-ptr, alignment: word-size);

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
       concatenate(arguments, vector(next-methods, meth)),
       calling-convention: $llvm-calling-convention-fast);
  end if
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
  ins--br(be, op--unlikely(be, cmp), error-bb, call-bb);

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
  ins--call(be, iep-func,
	    vector(mepargs-vector, engine, parent, undef, undef),
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
  ins--call(be, iep-func,
	    vector(spreadargs-vector, engine, parent, undef, undef),
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
    ins--if (be, op--unlikely(be, odd-cmp))
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
      ins--if (be, op--unlikely(be, bad-key-cmp))
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
          = make(<simple-object-vector>,
                 size: num + 2,
                 fill: $llvm-object-pointer-type);
        let mep-type
          = make(<llvm-function-type>,
                 return-type: llvm-reference-type(be, be.%mv-struct-type),
                 parameter-types: parameter-types,
                 varargs?: #f);
        let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
        ins--call
          (be, mep-cast,
           concatenate(vector(meth, data), arguments),
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
      let keys = ins--load(be, keys-ptr, alignment: word-size);
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
          = make(<simple-object-vector>,
                 size: num + 2,
                 fill: $llvm-object-pointer-type);
        let mep-type
          = make(<llvm-function-type>,
                 return-type: llvm-reference-type(be, be.%mv-struct-type),
                 parameter-types: parameter-types,
                 varargs?: #f);
        let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
        ins--call
          (be, mep-cast,
           concatenate(vector(meth, data), arguments),
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
        = make(<simple-object-vector>,
               size: num + 2,
               fill: $llvm-object-pointer-type);
      let mep-type
        = make(<llvm-function-type>,
               return-type: llvm-reference-type(be, be.%mv-struct-type),
               parameter-types: parameter-types,
               varargs?: #f);
      let mep-cast = ins--bitcast(be, mep, llvm-pointer-to(be, mep-type));
      ins--tail-call
        (be, mep-cast,
         concatenate(vector(meth, data), arguments),
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
  op--chain-to-engine-entry-point(be, next-engine, engine, arguments,
				  tail-call?: #t)
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
  op--chain-to-engine-entry-point(be, next-engine, engine, arguments,
				  tail-call?: #t)
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
  ins--if (be, op--likely(be, range-cmp))
    // Read the value and check for unbound
    let value = call-primitive(be, primitive-repeated-slot-value-descriptor,
                               object, repeated-slot-offset, index-raw);
    let bound-cmp
      = ins--icmp-ne(be, value, emit-reference(be, module, &unbound));
    ins--if (be, op--likely(be, bound-cmp))
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
  ins--if (be, op--likely(be, range-cmp))
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
  ins--if (be, op--likely(be, range-cmp))
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
  ins--if (be, op--likely(be, range-cmp))
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
  op--chain-to-engine-entry-point(be, next-engine, function, arguments,
				  tail-call?: #t)
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
  op--chain-to-engine-entry-point(be, next-engine, function, arguments,
				  tail-call?: #t)
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
  op--chain-to-engine-entry-point(be, next-engine, function, arguments,
				  tail-call?: #t)
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
  op--chain-to-engine-entry-point(be, next-engine, function, arguments,
				  tail-call?: #t)
end entry-point-descriptor;
