Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Notes for implementing primitives for the LLVM back-end:
//
// 1. Primitive generators should emit code that expects to be entered
//    from the basic block that is current when the generator is
//    called, and always exits through the basic block that is current
//    when the generator returns.
//
// 2. The generator should return the instruction value(s) computing
//    the primitive's return value(s). Do not insert any "ret"
//    instructions.

define class <llvm-primitive-descriptor> (<primitive-descriptor>)
  constant slot primitive-attributes :: <sequence>,
    required-init-keyword: attributes:;
  constant slot primitive-generator :: false-or(<function>),
    init-value: #f, init-keyword: generator:;
  constant slot primitive-function-declarator :: <simple-object-vector>,
    init-value: #[], init-keyword: declarator:;
end class;

// Descriptor for primitives emitted inline into functions
define macro &primitive-descriptor-definer
  { define ?adj:* &primitive-descriptor ?:name
        (?parameters:*) => (?values:*);
      ?:body
    end }
    => { define ?adj &primitive-descriptor ?name
             (?parameters) => (?values)
           ?body
         end }
  { define ?adjectives:* &primitive-descriptor ?:name
        (?parameters:*) => (?values:*)
      ?:body
    end }
    => { define constant ?name ## "-descriptor"
           = make(<llvm-primitive-descriptor>,
                  emitter: primitive-emitter-method (?parameters) => (?values)
                             ?body
                           end,
                  attributes: #[?adjectives]);
         do-define-llvm-primitive-descriptor(?#"name", ?name ## "-descriptor") }
adjectives:
    { } => { }
    { ?adjective:name ...} => { ?#"adjective", ... }
end macro;

// Descriptor for primitives kept as separate functions in the
// generated runtime support code
define macro &runtime-primitive-descriptor-definer
  { define ?adj:* &runtime-primitive-descriptor ?:name
        (?parameters:*) => (?values:*);
      ?:body
    end }
    => { define ?adj &runtime-primitive-descriptor ?name
             (?parameters) => (?values)
           ?body
         end }
  { define ?adjectives:* &runtime-primitive-descriptor ?:name
        (?parameters:*) => (?values:*)
      ?:body
    end }
    => { define constant ?name ## "-descriptor"
           = make(<llvm-primitive-descriptor>,
                  emitter: primitive-call-emitter-method
                               (?parameters) => (?values)
                             ?name ## "-descriptor"
                           end,
                  generator: primitive-emitter-method
                                 (?parameters) => (?values)
                               ?body
                             end,
                  declarator:
                    vector(name: ?#"name",
                           parameter-names:
                             primitive-parameter-names(?parameters),
                           parameter-types-spec:
                             primitive-parameter-types(?parameters),
                           value-types-spec:
                             primitive-parameter-types(?values)),
                  attributes: #[?adjectives]);
         do-define-llvm-primitive-descriptor(?#"name", ?name ## "-descriptor") }
adjectives:
    { } => { }
    { ?adjective:name ...} => { ?#"adjective", ... }
end macro;

// Descriptors for primitives externally defined in C
define macro &c-primitive-descriptor-definer
  { define ?adjectives:* &c-primitive-descriptor ?:name
        (?parameters:*) => (?values:*) }
    => { define constant ?name ## "-descriptor"
           = make(<llvm-primitive-descriptor>,
                  emitter: primitive-call-emitter-method
                               (?parameters) => (?values)
                             ?name ## "-descriptor"
                           end,
                  declarator:
                    vector(name: ?#"name",
                           parameter-types-spec:
                             primitive-parameter-types(?parameters),
                           value-types-spec:
                             primitive-parameter-types(?values)),
                  attributes: #[#"c-callable", ?adjectives]);
         do-define-llvm-primitive-descriptor(?#"name", ?name ## "-descriptor") }
adjectives:
    { } => { }
    { ?adjective:name ...} => { ?#"adjective", ... }
end macro;

// Descriptors for as-yet unimplemented primitives
define macro &unimplemented-primitive-descriptor-definer
  { define ?adjectives:* &unimplemented-primitive-descriptor ?:name
        (?parameters:*) => (?values:*)
      // Empty
    end }
    => { define constant ?name ## "-descriptor"
           = make(<llvm-primitive-descriptor>,
                  emitter: method (#rest args) => (no-return :: <bottom>)
                             error(?"name" " is currently unimplemented")
                           end,
                  attributes: #[?adjectives]);
         do-define-llvm-primitive-descriptor(?#"name", ?name ## "-descriptor") }
  { define ?adjectives:* &unimplemented-primitive-descriptor ?:name
        (?parameters:*) => (?values:*);
      // Empty
    end }
    => { define constant ?name ## "-descriptor"
           = make(<llvm-primitive-descriptor>,
                  emitter: method (#rest args) => (no-return :: <bottom>)
                             error(?"name" " is currently unimplemented")
                           end,
                  attributes: #[?adjectives]);
         do-define-llvm-primitive-descriptor(?#"name", ?name ## "-descriptor") }
adjectives:
    { } => { }
    { ?adjective:name ...} => { ?#"adjective", ... }
end macro;

// Emitter method with declared parameter types changed to <llvm-value>
define macro primitive-emitter-method
  { primitive-emitter-method (?parameters) => (?values) ?:body end }
    => { method (?=be :: <llvm-back-end>, ?parameters) => (?values) ?body end }

parameters:
    { } => { }
    { \#rest ?:name } => { #rest ?name }
    { ?variable-name, ... } => { ?variable-name, ... }
values:
    { } => { }
    { \#rest ?:name } => { #rest ?name }
    { ?variable-name, ... } => { ?variable-name, ... }
variable-name:
  { ?:name :: ?:expression }
    => { ?name :: <llvm-value> }
end macro;

// Emitter method that calls the primitive's function
define macro primitive-call-emitter-method
  { primitive-call-emitter-method (?parameters:*) => (?values)
      ?descriptor:expression
    end }
    => { method (be :: <llvm-back-end>, #rest parameters) => (?values)
           let function = llvm-primitive-function(be, ?descriptor);
           let calling-convention = function.llvm-function-calling-convention;
           let attribute-list = function.llvm-function-attribute-list;
           llvm-builder-declare-global(be, function.llvm-global-name,
                                       function);
           ins--call(be, function, parameters,
                     calling-convention: calling-convention,
                     attribute-list: attribute-list)
         end }
values:
    { } => { }
    { \#rest ?:name } => { #rest ?name }
    { ?variable-name, ... } => { ?variable-name, ... }
variable-name:
  { ?:name :: ?:expression }
    => { ?name :: <llvm-value> }
end macro;

// Auxiliary macro returning parameter names as a vector of symbols
define macro primitive-parameter-names
  { primitive-parameter-names(?parameters) }
    => { #[?parameters] }

parameters:
    { } => { }
    { \#rest ?:name } => { ?#"name" }
    { ?variable-name, ... } => { ?variable-name, ... }
variable-name:
  { ?:name :: ?:expression }
    => { ?#"name" }
end macro;

// Auxiliary macro returning parameter types as a vector of type name symbols
define macro primitive-parameter-types
  { primitive-parameter-types(?parameters) }
    => { #[?parameters] }

parameters:
    { } => { }
    { \#rest ?:name } => { #"rest" }
    { ?variable-name, ... } => { ?variable-name, ... }
variable-name:
  { ?:name :: ?type:name }
    => { ?#"type" }
end macro;

define constant $llvm-primitive-descriptors = make(<object-table>);

define function do-define-llvm-primitive-descriptor
    (name :: <symbol>, descriptor :: <llvm-primitive-descriptor>)
 => ();
  $llvm-primitive-descriptors[name] := descriptor;
end function;

define method llvm-primitive-function
    (back-end :: <llvm-back-end>, desc :: <llvm-primitive-descriptor>)
 => (function :: <llvm-function>);
  element(back-end.%primitive-function-table, desc, default: #f)
    | (element(back-end.%primitive-function-table, desc)
         := apply(make-primitive-function, back-end, desc,
                  desc.primitive-function-declarator))
end method;

define method make-primitive-function
    (back-end :: <llvm-back-end>, descriptor :: <llvm-primitive-descriptor>,
     #key name :: <symbol>,
          parameter-names :: <simple-object-vector> = #[],
          parameter-types-spec :: <simple-object-vector>,
          value-types-spec :: <simple-object-vector>,
     #all-keys)
 => (function :: <llvm-function>);
  let mangled-name = raw-mangle(back-end, as(<string>, name));

  let (required-parameter-type-specs, required-parameter-names, rest-parameter-name)
    = if (~empty?(parameter-types-spec) & parameter-types-spec.last == #"rest")
        let required-count = parameter-types-spec.size - 1;
        values(copy-sequence(parameter-types-spec, end: required-count),
               copy-sequence(parameter-names, end: required-count),
               parameter-names.last)
      else
        values(parameter-types-spec, parameter-names, #f)
      end if;
  let (required-value-type-specs, values-rest?)
    = if (~empty?(value-types-spec) & value-types-spec.last == #"rest")
        let required-count = value-types-spec.size - 1;
        values(copy-sequence(value-types-spec, end: required-count), #t)
      else
        values(value-types-spec, #f)
      end if;

  let function-type
    = llvm-primitive-function-type(back-end,
                                   required-parameter-type-specs,
                                   true?(rest-parameter-name),
                                   required-value-type-specs,
                                   values-rest?);

  let parameter-types = function-type.llvm-function-type-parameter-types;
  let arguments
    = map(method (arg-type, arg-name, index)
            make(<llvm-argument>,
                 type: arg-type,
                 name: raw-mangle(back-end, arg-name),
                 index: index)
          end,
          parameter-types,
          required-parameter-names,
          range(below: parameter-types.size));

  let linkage = #"external";  // FIXME
  let calling-convention
    = if (function-type.llvm-function-type-varargs?
            | member?(#"c-callable", descriptor.primitive-attributes))
        $llvm-calling-convention-c
      else
        $llvm-calling-convention-fast
      end if;

  make(<llvm-function>,
       name: mangled-name,
       type: llvm-pointer-to(back-end, function-type),
       arguments: arguments,
       linkage: linkage,
       section: llvm-section-name(back-end, #"code"),
       calling-convention: calling-convention)
end method;

// Function type for a runtime primitive function
define method llvm-primitive-function-type
    (back-end :: <llvm-back-end>,
     required-parameter-type-specs :: <simple-object-vector>,
     parameters-rest? :: <boolean>,
     required-value-type-specs :: <simple-object-vector>,
     values-rest? :: <boolean>)
 => (type :: <llvm-function-type>);
  local
    method parameter-type (type-name :: <symbol>) => (type :: <llvm-type>);
      llvm-primitive-parameter-type(back-end, dylan-value(type-name))
    end method;

  // Compute parameter types
  let parameter-types = map(parameter-type, required-parameter-type-specs);

  // Compute return type
  let return-type
    = if (values-rest?)
        $llvm-object-pointer-type // FIXME
      elseif (required-value-type-specs.empty?)
        $llvm-void-type
      else
        let return-types
          = map(parameter-type, required-value-type-specs);
        if (return-types.size = 1)
          return-types.first
        else
          make(<llvm-struct-type>, elements: return-types)
        end if
      end if;
  make(<llvm-function-type>,
       parameter-types: parameter-types,
       return-type: return-type,
       varargs?: parameters-rest?)
end method;

define method llvm-primitive-parameter-type
    (back-end :: <llvm-back-end>, class :: <&class>)
 => (type :: <llvm-type>);
  llvm-pointer-to(back-end, llvm-class-type(back-end, class))
end method;

define method llvm-primitive-parameter-type
    (back-end :: <llvm-back-end>, o)
 => (type :: <llvm-type>);
  llvm-reference-type(back-end, o)
end method;

define function call-primitive
    (back-end :: <llvm-back-end>, primitive :: <primitive-descriptor>,
     #rest arguments)
 => (#rest results)
  apply(primitive.primitive-emitter, back-end, arguments)
end function;


/// Runtime support variables

define class <llvm-runtime-variable-descriptor> (<object>)
  constant slot runtime-variable-type-name :: <symbol>,
    required-init-keyword: type-name:;
  constant slot runtime-variable-init-function :: <function>,
    required-init-keyword: init-function:;
  constant slot runtime-variable-section :: <symbol>,
    required-init-keyword: section:;
  slot runtime-variable-global :: <llvm-global-variable>;
end class;

define macro runtime-variable-definer
  { define runtime-variable ?:name :: ?type-name:name = ?init:expression,
           #key ?section:expression = #"untraced-data"; }
    => { define constant ?name ## "-descriptor"
           = make(<llvm-runtime-variable-descriptor>,
                  type-name: ?#"type-name",
                  init-function: method() ?init end,
                  section: ?section);
         do-define-llvm-runtime-variable-descriptor
           (?#"name", ?name ## "-descriptor") }
end macro;

define constant $llvm-runtime-variable-descriptors = make(<object-table>);

define function do-define-llvm-runtime-variable-descriptor
    (name :: <symbol>, descriptor :: <llvm-runtime-variable-descriptor>)
 => ();
  $llvm-runtime-variable-descriptors[name] := descriptor;
end function;
