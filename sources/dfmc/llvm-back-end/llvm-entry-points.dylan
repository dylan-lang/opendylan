Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// References

define constant $number-xeps = 10;
define constant $number-meps = $number-xeps;

define constant $xep-string             = "xep";
define constant $rest-xep-string        = "rest_xep";
define constant $rest-key-xep-string    = "rest_key_xep";
define constant $gf-xep-string          = "gf_xep";
define constant $gf-optional-xep-string = "gf_optional_xep";
define constant $rest-key-mep-string    = "rest_key_mep";

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&lambda-xep>)
 => (name :: <string>);
  let req-size = ^entry-point-number-required(ep);
  let size
    = if (^entry-point-key?(ep))
        req-size + ^entry-point-number-keys(ep) + 1
      else
        req-size
      end if;
  format-to-string(if (size < $number-xeps) "%s_%d" else "%s" end,
                   if (^entry-point-key?(ep))
                     $rest-key-xep-string
                   elseif (^entry-point-rest?(ep))
                     $rest-xep-string
                   else
                     $xep-string
                   end if,
                   size)
end method;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&slot-accessor-xep>)
  ^entry-point-name(ep)
end method;

// This reflects the number of implementation args (required args plus
// maybe optionals vector) the GF takes.  Zero through this many
// implementation args are passed spread as separate arguments by
// the dispatch engine routines; more will be passed as a single
// (hopefully) stack-allocated vector.

define constant $special-gf-engine-max-args = 7;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&generic-function-xep>)
 => (name :: <string>);
  let req-size :: <integer> = ^entry-point-number-required(ep);
  let optionals? = ^entry-point-optionals?(ep);
  let impargs :: <integer> = if (optionals?) req-size + 1 else req-size end;
  format-to-string(if (impargs <= $special-gf-engine-max-args)
                     "%s_%d"
                   else
                     "%s"
                   end,
                   if (optionals?)
                     $gf-optional-xep-string
                   else
                     $gf-xep-string
                   end if,
                   req-size)
end method;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, o :: <&keyword-method-mep>)
 => (name :: <string>);
  let size
    = ^entry-point-number-required(o) + ^entry-point-number-keys(o) + 1;
  format-to-string(if (size < $number-meps) "%s_%d" else "%s" end,
                   $rest-key-mep-string, size)
end method;


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
//   - outer:          Called by callers that may not know the exact signature
//                     (thus requiring the C calling convention)
//   - variable-arity: Uses varargs
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
             (?=be :: <llvm-back-end>, ?=num :: <integer>, ?parameters)
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
     count :: <integer>)
 => (function :: <llvm-function>);
  let mangled-name
    = format-to-string("%s_%d",
                       raw-mangle(back-end, desc.entry-point-name),
                       count);
  let global-table = back-end.llvm-builder-module.llvm-global-table;
  element(global-table, mangled-name, default: #f)
    | begin
        let function
          = apply(make-entry-point-function, back-end, mangled-name,
                  desc, count, desc.entry-point-function-declarator);
        llvm-builder-define-global(back-end, mangled-name, function)
      end
end method;

define method make-entry-point-function
    (back-end :: <llvm-back-end>, mangled-name :: <string>,
     descriptor :: <llvm-entry-point-descriptor>,
     count :: <integer>,
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
    = llvm-entry-point-function-type(back-end, descriptor,
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
define method llvm-entry-point-function-type
    (back-end :: <llvm-back-end>,
     descriptor :: <llvm-entry-point-descriptor>,
     required-parameter-type-specs :: <simple-object-vector>,
     parameters-rest? :: <boolean>,
     count :: <integer>)
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


/// Entry point utility operations

define method op--call-error-iep
    (back-end :: <llvm-back-end>, name :: <symbol>, #rest arguments) => ();
  let module = back-end.llvm-builder-module;

  let err-iep = dylan-value(name).^iep;
  let err-name = emit-name(back-end, module, err-iep);
  let err-global = llvm-builder-global(back-end, err-name);
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  ins--tail-call(back-end, err-global,
                 concatenate(arguments, vector(undef, undef)),
                 type: llvm-reference-type(back-end, back-end.%mv-struct-type),
                 calling-convention:
                   llvm-calling-convention(back-end, err-iep));
  ins--unreachable(back-end);
end method;

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

  // Retrieve the dispatch engine
  ins--block(be, call-bb);
  let gf-class :: <&class> = dylan-value(#"<generic-function>");
  let function-cast = op--object-pointer-cast(be, function, gf-class);

  let discriminator-slot-ptr
    = op--getslotptr(be, function-cast, gf-class, #"discriminator");
  let engine = ins--load(be, discriminator-slot-ptr, alignment: word-size);

  // Retrieve the engine entry point from the engine
  let engine-node-class :: <&class> = dylan-value(#"<engine-node>");
  let engine-cast = op--object-pointer-cast(be, engine, engine-node-class);
  let entry-point-slot-ptr
    = op--getslotptr(be, engine-cast,
                     engine-node-class, #"engine-node-entry-point");
  let entry-point = ins--load(be, entry-point-slot-ptr, alignment: word-size);

  // Chain to the engine entry point function
  let entry-point-type
    = make(<llvm-function-type>,
           return-type: llvm-reference-type(be, be.%mv-struct-type),
           parameter-types: make(<simple-object-vector>,
                                 size: num + 2,
                                 fill: $llvm-object-pointer-type),
           varargs?: #f);
  let entry-point-cast
    = ins--bitcast(be, entry-point, llvm-pointer-to(be, entry-point-type));
  ins--tail-call
    (be, entry-point-cast,
     concatenate(vector(function, engine), arguments),
     calling-convention: $llvm-calling-convention-fast)
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

  // Retrieve the dispatch engine
  let gf-class :: <&class> = dylan-value(#"<generic-function>");
  let function-cast = op--object-pointer-cast(be, function, gf-class);

  let discriminator-slot-ptr
    = op--getslotptr(be, function-cast, gf-class, #"discriminator");
  let engine = ins--load(be, discriminator-slot-ptr, alignment: word-size);

  // Retrieve the engine entry point from the engine
  let engine-node-class :: <&class> = dylan-value(#"<engine-node>");
  let engine-cast = op--object-pointer-cast(be, engine, engine-node-class);
  let entry-point-slot-ptr
    = op--getslotptr(be, engine-cast,
                     engine-node-class, #"engine-node-entry-point");
  let entry-point = ins--load(be, entry-point-slot-ptr, alignment: word-size);

  // Chain to the engine entry point function
  let entry-point-type
    = make(<llvm-function-type>,
           return-type: llvm-reference-type(be, be.%mv-struct-type),
           parameter-types: make(<simple-object-vector>,
                                 size: num + 3,
                                 fill: $llvm-object-pointer-type),
           varargs?: #f);
  let entry-point-cast
    = ins--bitcast(be, entry-point, llvm-pointer-to(be, entry-point-type));
  ins--tail-call
    (be, entry-point-cast,
     concatenate(vector(function, engine), arguments, vector(rest-vector)),
     calling-convention: $llvm-calling-convention-fast)
end entry-point-descriptor;

// For MEP calls with #key (and possibly #rest)
define variable-arity outer entry-point-descriptor rest-key-mep
    (next-methods :: <object>, function :: <function>, n :: <raw-integer>)
 => (#rest values);
  //---*** Fill this in...
  make(<llvm-undef-constant>,
       type: llvm-reference-type(be, be.%mv-struct-type))
end entry-point-descriptor;
