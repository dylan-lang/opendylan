Module:    scepter-ir-back-end-test-suite
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dummy-scepter> (<scepter>)
end class;

set-scepter(make(<dummy-scepter>, directory: as(<directory-locator>, "")));
define variable *back-end* :: <ir-back-end> = make(<ir-back-end>, scepter: get-scepter());
define variable *repository* :: corba/<repository> = make-nil(corba/<repository>);
define variable *root* = make(<ast-root>, local-name: make(<ast-identifier>, label: ""));
define variable *cleanup?* = #t;

define function destroy-definitions (#rest definitions)
 => ()
  if (*cleanup?*)
    for (def in definitions)
      def.corba/IRObject/destroy;
    end for;
  end if;
end function;

define test setup ()
  let orb = corba/orb-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  *repository* := as(corba/<repository>, corba/orb/resolve-initial-references(orb, "InterfaceRepository"));
  check-false("Reference to interface repository is not nil", corba/object/is-nil(*repository*));
  *back-end*.ir-back-end-repository := *repository*;
  push(get-scepter().scepter-scopes, *root*);
end test;

define function get-ast-predefined-type (idl-type :: <idl-type>)
 => (type :: <ast-predefined-type>)
  let type = resolve-primitive-type(*root*, idl-type);
  unless (type)
    let identifier = make(<ast-identifier>, label: idl-type.idl-type-name);
    type := make(<ast-predefined-type>, type: idl-type, local-name: identifier, scope: *root*);
    add-declarator(*root*, type);
  end unless;
  type;
end function;

define test get-primitive-type-test ()
  let def = #f;
  check("get primitive type void", method () def := get-primitive-type(*repository*, $void-idl-type) end);
  check-equal("Check kind of PrimitiveDef is pk_void", def.corba/PrimitiveDef/kind, #"pk-void");
  check("get primitive type short", method () def := get-primitive-type(*repository*, $short-idl-type) end);
  check-equal("Check kind of PrimitiveDef is pk_short", def.corba/PrimitiveDef/kind, #"pk-short");
  check("get primitive type long", method () def := get-primitive-type(*repository*, $long-idl-type) end);
  check-equal("PrimitiveDef kind is pk_long", def.corba/PrimitiveDef/kind, #"pk-long");
  check("get primitive type ushort", method () def := get-primitive-type(*repository*, $ushort-idl-type) end);
  check-equal("PrimitiveDef kind is pk_ushort", def.corba/PrimitiveDef/kind, #"pk-ushort");
  check("get primitive type ulong", method () def := get-primitive-type(*repository*, $ulong-idl-type) end);
  check-equal("PrimitiveDef kind is pk_ulong", def.corba/PrimitiveDef/kind, #"pk-ulong");
  check("get primitive type float", method () def := get-primitive-type(*repository*, $float-idl-type) end);
  check-equal("PrimitiveDef kind is pk_float", def.corba/PrimitiveDef/kind, #"pk-float");
  check("get primitive type double", method () def := get-primitive-type(*repository*, $double-idl-type) end);
  check-equal("PrimitiveDef kind is pk_double", def.corba/PrimitiveDef/kind, #"pk-double");
  check("get primitive type boolean", method () def := get-primitive-type(*repository*, $boolean-idl-type) end);
  check-equal("PrimitiveDef kind is pk_boolean", def.corba/PrimitiveDef/kind, #"pk-boolean");
  check("get primitive type char", method () def := get-primitive-type(*repository*, $char-idl-type) end);
  check-equal("PrimitiveDef kind is pk_char", def.corba/PrimitiveDef/kind, #"pk-char");
  check("get primitive type octet", method () def := get-primitive-type(*repository*, $octet-idl-type) end);
  check-equal("PrimitiveDef kind is pk_octet", def.corba/PrimitiveDef/kind, #"pk-octet");
  check("get primitive type any", method () def := get-primitive-type(*repository*, $any-idl-type) end);
  check-equal("PrimitiveDef kind is pk_any", def.corba/PrimitiveDef/kind, #"pk-any");
  check("get primitive type TypeCode", method () def := get-primitive-type(*repository*, $TypeCode-idl-type) end);
  check-equal("PrimitiveDef kind is pk_TypeCode", def.corba/PrimitiveDef/kind, #"pk-TypeCode");
  check("get primitive type Principal", method () def := get-primitive-type(*repository*, $Principal-idl-type) end);
  check-equal("PrimitiveDef kind is pk_Principal", def.corba/PrimitiveDef/kind, #"pk-Principal");
  check("get primitive type string", method () def := get-primitive-type(*repository*, $string-idl-type) end);
  check-equal("PrimitiveDef kind is pk_string", def.corba/PrimitiveDef/kind, #"pk-string");
  check("get primitive type Object", method () def := get-primitive-type(*repository*, $Object-idl-type) end);
  check-equal("PrimitiveDef kind is pk_objref", def.corba/PrimitiveDef/kind, #"pk-objref");
//  check("get primitive type longlong", method () def := get-primitive-type(*repository*, $longlong-idl-type) end);
//  check-equal("PrimitiveDef kind is pk_longlong", def.corba/PrimitiveDef/kind, #"pk-longlong");
//  check("get primitive type ulonglong", method () def := get-primitive-type(*repository*, $ulonglong-idl-type) end);
//  check-equal("PrimitiveDef kind is pk_ulonglong", def.corba/PrimitiveDef/kind, #"pk-ulonglong");
//  check("get primitive type longdouble", method () def := get-primitive-type(*repository*, $longdouble-idl-type) end);
//  check-equal("PrimitiveDef kind is pk_longdouble", def.corba/PrimitiveDef/kind, #"pk-longdouble");
//  check("get primitive type wchar", method () def := get-primitive-type(*repository*, $wchar-idl-type) end);
//  check-equal("PrimitiveDef kind is pk_wchar", def.corba/PrimitiveDef/kind, #"pk-wchar");
//  check("get primitive type wstring", method () def := get-primitive-type(*repository*, $wstring-idl-type) end);
//  check-equal("PrimitiveDef kind is pk_wstring", def.corba/PrimitiveDef/kind, #"pk-wstring");
end test;

define suite get-primitive-type-suite ()
  test get-primitive-type-test;
end suite;

define test primitive-load-definition-test ()
  let def = #f;
  check("load-definition on primitive type void", method () def := load-definition(*back-end*, get-ast-predefined-type($void-idl-type)) end);
  check-equal("PrimitiveDef has kind pk_void", def.corba/PrimitiveDef/kind, #"pk-void");
end test;

define test string-load-definition-test ()
  let unbounded-string = make(<ast-string>);
  add-declarator(*root*, unbounded-string);
  let unbounded-string-def = #f;
  check("load-definition on unbounded string", method () unbounded-string-def := load-definition(*back-end*, unbounded-string) end);
  check-true("unbounded-string-def is a corba/<PrimitiveDef>", instance?(unbounded-string-def, corba/<PrimitiveDef>));
  check-equal("unbounded-string-def is of kind pk_string", unbounded-string-def.corba/PrimitiveDef/kind, #"pk-string");
  let expr = make(<ast-expression>, combinator: $no-combinator, value: 10);
  let bounded-string = make(<ast-string>, size: expr);
  add-declarator(*root*, bounded-string);
  let bounded-string-def = #f;
  check("load-definition on bounded string", method () bounded-string-def := load-definition(*back-end*, bounded-string) end);
  check-true("bounded-string-def is a corba/<StringDef>", instance?(bounded-string-def, corba/<StringDef>));
  check-equal("bounded-string-def bound attribute", bounded-string-def.corba/StringDef/bound, 10);
  destroy-definitions(bounded-string-def);
end test;

//define test wstring-load-definition-test ()
//  let unbounded-wstring = make(<ast-wstring>);
//  add-declarator(*root*, unbounded-wstring);
//  let unbounded-wstring-def = #f;
//  check("load-definition on unbounded wstring", method () unbounded-wstring-def := load-definition(*back-end*, unbounded-wstring) end);
//  check-true("unbounded-wstring-def is a corba/<PrimitiveDef>", instance?(unbounded-wstring-def, corba/<PrimitiveDef>));
//  check-equal("unbounded-wstring-def is of kind pk_wstring", unbounded-wstring-def.corba/PrimitiveDef/kind, #"pk-wstring");
//  let expr = make(<ast-expression>, combinator: $no-combinator, value: 20);
//  let bounded-wstring = make(<ast-wstring>, size: expr);
//  add-declarator(*root*, bounded-wstring);
//  let bounded-wstring-def = #f;
//  check("load-definition on bounded wstring", method () bounded-wstring-def := load-definition(*back-end*, bounded-wstring) end);
//  check-true("bounded-wstring-def is a corba/<WstringDef>", instance?(bounded-wstring-def, corba/<WstringDef>));
//  check-equal("bounded-wstring-def bound attribute", bounded-wstring-def.corba/WstringDef/bound, 20);
//  destroy-definitions(unbounded-wstring-def, bounded-wstring-def);
//end test;

//define test fixed-load-definition-test ()
//end test;

define test sequence-load-definition-test ()
  let unbounded-sequence = make(<ast-sequence>, type: get-ast-predefined-type($short-idl-type));
  add-declarator(*root*, unbounded-sequence);
  let unbounded-sequence-def = #f;
  check("load-definition on unbounded sequence", method () unbounded-sequence-def := load-definition(*back-end*, unbounded-sequence) end);
  check-equal("unbounded sequence bound attribute", unbounded-sequence-def.corba/SequenceDef/bound, 0);
  check-equal("unbounded sequence element_type attribute", unbounded-sequence-def.corba/SequenceDef/element-type, CORBA/$short-typecode);
  let expr = make(<ast-expression>, combinator: $no-combinator, value: 50);
  let bounded-sequence = make(<ast-sequence>, type: get-ast-predefined-type($ushort-idl-type), size: expr);
  add-declarator(*root*, bounded-sequence);
  let bounded-sequence-def = #f;
  check("load-definition on bounded sequence", method () bounded-sequence-def := load-definition(*back-end*, bounded-sequence) end);
  check-equal("bounded sequence bound attribute", bounded-sequence-def.corba/SequenceDef/bound, 50);
  check-equal("bounded sequence element_type attribute", bounded-sequence-def.corba/SequenceDef/element-type, CORBA/$unsigned-short-typecode);
  destroy-definitions(unbounded-sequence-def, bounded-sequence-def);
end test;

define test array-load-definition-test ()
  let three = make(<ast-expression>, combinator: $no-combinator, value: 3);
  let array = make(<ast-array>, type: get-ast-predefined-type($long-idl-type), dimensions: vector(three));
  add-declarator(*root*, array);
  let array-def = #f;
  check("load-definition on single dimensional array", method () array-def := load-definition(*back-end*, array) end);
  check-equal("single dimensional array length", array-def.corba/ArrayDef/length, 3);
  check-equal("single dimensional array element type", array-def.corba/ArrayDef/element-type, CORBA/$long-typecode);
  let two = make(<ast-expression>, combinator: $no-combinator, value: 2);
  let four = make(<ast-expression>, combinator: $no-combinator, value: 4);
  let multi-array = make(<ast-array>, type: get-ast-predefined-type($ulong-idl-type), dimensions: vector(four, three, two));
  add-declarator(*root*, multi-array);
  let multi-array-def-1 = #f;
  check("load-definition on multi-dimensional array", method () multi-array-def-1 := load-definition(*back-end*, multi-array) end);
  check-equal("length of first multi-array dimension", multi-array-def-1.corba/ArrayDef/length, 4);
  let multi-array-def-2 = #f;
  check("multi-array first element type", method () multi-array-def-2 := multi-array-def-1.corba/ArrayDef/element-type-def end);
  check-equal("length of second multi-array dimension", multi-array-def-2.corba/ArrayDef/length, 3);
  let multi-array-def-3 = #f;
  check("multi-array second element type", method () multi-array-def-3 := multi-array-def-2.corba/ArrayDef/element-type-def end);
  check-equal("length of third multi-array dimension", multi-array-def-3.corba/ArrayDef/length, 2);
  check-equal("multi-array third element type", multi-array-def-3.corba/ArrayDef/element-type, CORBA/$unsigned-long-typecode);
  destroy-definitions(array-def, multi-array-def-1, multi-array-def-2, multi-array-def-3);
end test;

define test constant-load-definition-test ()
  let value = make(<ast-expression>, combinator: $no-combinator, value: 100.0);
  let type = get-ast-predefined-type($float-idl-type);
  let id = make(<ast-identifier>, label: "LoadConstant");
  let constant = make(<ast-constant>, value: value, type: type, local-name: id, scope: *root*);
  add-declarator(*root*, constant);
  let def = #f;
  check("load constant", method () def := load-definition(*back-end*, constant) end);
  check("load constant def is corba/<ConstantDef>", instance?, def, corba/<ConstantDef>);
  check-equal("load constant def name", def.corba/Contained/name, "LoadConstant");
  check-equal("load constant def typecode", def.corba/ConstantDef/type, CORBA/$float-typecode);
  check-equal("load constant def value", as(CORBA/<float>, def.corba/ConstantDef/value), 100.0);
  destroy-definitions(def);
end test;

define suite load-definition-suite ()
  test primitive-load-definition-test;
  test string-load-definition-test;
//  test wstring-load-definition-test;
//  test fixed-load-definition-test;
  test sequence-load-definition-test;
  test array-load-definition-test;
  test constant-load-definition-test;
  test struct-load-definition-test;
  test union-load-definition-test;
end suite;

define test constant-create-definition-in-container ()
  let value = "Steve the bird";
  let expr =  make(<ast-expression>, combinator: $no-combinator, value: value);
  let type = get-ast-predefined-type($string-idl-type);
  let constant = make(<ast-constant>, value: expr, type: type, local-name: make(<ast-identifier>, label: "ConstantTest"), scope: *root*);
  add-declarator(*root*, constant);
  let def = #f;
  check("load constant in container", method () def := create-definition-in-container(*back-end*, constant, *repository*) end);
  check("load constant in container def is corba/<ConstantDef>", instance?, def, corba/<ConstantDef>);
  check-equal("load constant in container def id", def.corba/Contained/id, declarator-repository-id(constant));
  check-equal("load constant in container def name", def.corba/Contained/name, "ConstantTest");
  check-equal("load constant in container def version", def.corba/Contained/version, declarator-repository-id-version(constant));
  check-equal("load constant in container def typecode", def.corba/ConstantDef/type, CORBA/$string-typecode);
  check-equal("load constant in container def value", as(CORBA/<string>, def.corba/ConstantDef/value), value);
  destroy-definitions(def);
end test;

define test enum-create-definition-in-container ()
  let enum = make(<ast-enum>, local-name: make(<ast-identifier>, label: "Planet"), scope: *root*);
  add-declarator(*root*, enum);
  push(get-scepter().scepter-scopes, enum);
  let members = #["Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto"];
  for (name in members)
    let id = make(<ast-identifier>, label: name);
    add-declarator(enum, make(<ast-enum-value>, local-name: id, scope: enum));
  end for;
  pop(get-scepter().scepter-scopes);
  let def = #f;
  check("load enum in container", method () def := create-definition-in-container(*back-end*, enum, *repository*) end);
  check("load enum in container def is corba/<EnumDef>", instance?, def, corba/<EnumDef>);
  check-equal("load enum in container def members", def.corba/EnumDef/members, members);
  destroy-definitions(def);
end test;

define test exception-create-definition-in-container ()
  let id = make(<ast-identifier>, label: "EmptyException");
  let empty-exception = make(<ast-exception>, local-name: id, scope: *root*);
  add-declarator(*root*, empty-exception);
  let def = #f;
  check("create empty exception in container", method () def := create-definition-in-container(*back-end*, empty-exception, *repository*) end);
  check("empty exception def is corba/<ExceptionDef>", instance?, def, corba/<ExceptionDef>);
  check-equal("empty exception def name", def.corba/Contained/name, "EmptyException");
  check-equal("empty exception def id", def.corba/Contained/id, declarator-repository-id(empty-exception));
  check-equal("empty exception def version", def.corba/Contained/version, declarator-repository-id-version(empty-exception));
//  check-equal("empty exception def typecode", def.corba/ExceptionDef/type, make(<string-typecode>));
  check-equal("empty exception def members", def.corba/ExceptionDef/members, make(CORBA/<StructMemberSeq>));
  destroy-definitions(def);
end test;

define test exception-load-definition-test ()
  let id = make(<ast-identifier>, label: "SimpleException");
  let simple-exception = make(<ast-exception>, local-name: id, scope: *root*);
  add-declarator(*root*, simple-exception);
  push(get-scepter().scepter-scopes, simple-exception);
  let id-1 = make(<ast-identifier>, label: "SimpleExceptionField_1");
  let type-1 = get-ast-predefined-type($octet-idl-type);
  let field-1 = make(<ast-field>, type: type-1, local-name: id-1, scope: simple-exception);
  add-declarator(simple-exception, field-1);
  let id-2 = make(<ast-identifier>, label: "SimpleExceptionField_2");
  let type-2 = get-ast-predefined-type($string-idl-type);
  let field-2 = make(<ast-field>, type: type-2, local-name: id-2, scope: simple-exception);
  add-declarator(simple-exception, field-2);
  pop(get-scepter().scepter-scopes);
  let def = #f;
  check("load-definition simple exception", method () def := load-definition(*back-end*, simple-exception) end);
  check("simple exception def is corba/<ExceptionDef>", instance?, def, corba/<ExceptionDef>);
  destroy-definitions(def);

  let id = make(<ast-identifier>, label: "ComplexException");
  let complex-exception = make(<ast-exception>, local-name: id, scope: *root*);
  add-declarator(*root*, complex-exception);
  push(get-scepter().scepter-scopes, complex-exception);
//  let id-1 = make(<ast-identifier>, label: "ComplexExceptionField_1");
//  let type-1 = make(<ast-sequence>, type: get-ast-predefined-type($char-idl-type));
//  add-declarator(*root*, type-1);
//  let field-1 = make(<ast-field>, type: type-1, local-name: id-1, scope: complex-exception);
//  add-declarator(complex-exception, field-1);
  pop(get-scepter().scepter-scopes);
  check("load-definition complex exception", method () def := load-definition(*back-end*, complex-exception) end);
  check("complex exception def is corba/<ExceptionDef>", instance?, def, corba/<ExceptionDef>);
  destroy-definitions(def);
end test;

define test interface-create-definition-in-container ()
  let id = make(<ast-identifier>, label: "EmptyInterface");
  let empty-interface = make(<ast-interface>, local-name: id, inherits: make(<stretchy-vector>), scope: *root*);
  add-declarator(*root*, empty-interface);
  let def = #f;
  check("create empty interface in container", method () def := create-definition-in-container(*back-end*, empty-interface, *repository*) end);
  destroy-definitions(def);
end test;

define test module-create-definition-in-container ()
  let id = make(<ast-identifier>, label: "EmptyModule");
  let empty-module = make(<ast-module>, local-name: id, scope: *root*);
  add-declarator(*root*, empty-module);
  let def = #f;
  check("create empty module in container", method () def := create-definition-in-container(*back-end*, empty-module, *repository*) end);
  destroy-definitions(def);
end test;

define method contained-checks (prefix :: <string>, node :: <ast-declarator>, def :: corba/<Contained>)
 => ()
  check-equal(concatenate(prefix, " id"), def.corba/Contained/id, node.declarator-repository-id);
  check-equal(concatenate(prefix, " name"), def.corba/Contained/name, identifier-label(node.declarator-local-name));
  check-equal(concatenate(prefix, " version"), def.corba/Contained/version, node.declarator-repository-id-version);
end method;

define test struct-create-definition-in-container ()
  let id = make(<ast-identifier>, label: "EmptyStruct");
  let empty-struct = make(<ast-structure>, local-name: id, scope: *root*);
  add-declarator(*root*, empty-struct);
  let def = #f;
  check("create empty struct in container", method () def := create-definition-in-container(*back-end*, empty-struct, *repository*) end);
  contained-checks("empty struct", empty-struct, def);
  check-equal("empty struct typecode", def.corba/IDLType/type.corba/TypeCode/kind, #"tk-struct");
  check-equal("empty struct has no members", size(def.corba/StructDef/members), 0);
  destroy-definitions(def);
end test;

define test struct-load-definition-test ()
  let id = make(<ast-identifier>, label: "SimpleStruct");
  let simple-struct = make(<ast-structure>, local-name: id, scope: *root*);
  add-declarator(*root*, simple-struct);
  push(get-scepter().scepter-scopes, simple-struct);
  let id-1 = make(<ast-identifier>, label: "SimpleStructField_1");
  let type-1 = get-ast-predefined-type($TypeCode-idl-type);
  let field-1 = make(<ast-field>, type: type-1, local-name: id-1, scope: simple-struct);
  add-declarator(simple-struct, field-1);
  let id-2 = make(<ast-identifier>, label: "SimpleStructField_2");
  let type-2 = get-ast-predefined-type($Object-idl-type);
  let field-2 = make(<ast-field>, type: type-2, local-name: id-2, scope: simple-struct);
  add-declarator(simple-struct, field-2);
  pop(get-scepter().scepter-scopes);
  let def = #f;
  check("load-definition simple struct", method () def := load-definition(*back-end*, simple-struct) end);
  contained-checks("simple struct", simple-struct, def);
  check-equal("simple struct typecode", def.corba/IDLType/type.corba/TypeCode/kind, #"tk-struct");
  check-equal("simple struct has 2 members", size(def.corba/StructDef/members), 2);
  destroy-definitions(def);

//  let id = make(<ast-identifier>, label: "RecursiveStruct");
//  let recursive-struct = make(<ast-structure>, local-name: id, scope: *root*);
//  add-declarator(*root*, recursive-struct);
//  push(*scopes*, recursive-struct);
//  let id-1 = make(<ast-identifier>, label: "RecursiveStructField_1");
//  let type-1 = make(<ast-sequence>, type: recursive-struct);
//  add-declarator(*root*, type-1);
//  let field-1 = make(<ast-field>, type: type-1, local-name: id-1, scope: recursive-struct);
//  add-declarator(recursive-struct, field-1);
//  pop(*scopes*);
//  check("load-definition recursive struct", method () def := load-definition(*back-end*, recursive-struct) end);
//  contained-checks("recursive struct", recursive-struct, def);
//  check-equal("recursive struct typecode", def.corba/IDLType/type.corba/TypeCode/kind, #"tk-struct");
//  check-equal("recursive struct has 1 member", size(def.corba/StructDef/members), 1);
//  destroy-definitions(def);
end test;

define test typedef-create-definition-in-container ()
  let id = make(<ast-identifier>, label: "charAlias");
  let typedef = make(<ast-typedef>, type: get-ast-predefined-type($char-idl-type), local-name: id, scope: *root*);
  add-declarator(*root*, typedef);
  let def = #f;
  check("load char typedef", method () def := create-definition-in-container(*back-end*, typedef, *repository*) end);
  check-equal("charAlias original type is char", corba/PrimitiveDef/kind(def.corba/AliasDef/original-type-def), #"pk-char");
  destroy-definitions(def);

  let id = make(<ast-identifier>, label: "OriginalStruct");
  let struct = make(<ast-structure>, local-name: id, scope: *root*);
  add-declarator(*root*, struct);
  let id = make(<ast-identifier>, label: "StructAlias");
  let typedef = make(<ast-typedef>, type: struct, local-name: id, scope: *root*);
  add-declarator(*root*, typedef);
  check("load struct typedef", method () def := create-definition-in-container(*back-end*, typedef, *repository*) end);
  let original-def = #f;
  check("StructAlias original type", method () original-def := def.corba/AliasDef/original-type-def end);

  destroy-definitions(def, original-def);
end test;

define test union-load-definition-test ()
  let type = get-ast-predefined-type($short-idl-type);
  let id = make(<ast-identifier>, label: "SimpleUnion");
  let simple-union = make(<ast-union>, type: type, local-name: id, scope: *root*);
  add-declarator(*root*, simple-union);
  push(get-scepter().scepter-scopes, simple-union);
  let value =  make(<ast-expression>, combinator: $no-combinator, value: 1);
  let branch1 = make(<ast-union-branch>,
                     labels: vector(make(<ast-union-branch-label>, value: value)),
                     type: get-ast-predefined-type($boolean-idl-type),
                     local-name: make(<ast-identifier>, label: "SimpleUnionBranch1"),
                     scope: simple-union);
  let branch2 = make(<ast-union-branch>,
                     labels: vector(make(<ast-default-union-branch-label>)),
                     type: get-ast-predefined-type($octet-idl-type),
                     local-name: make(<ast-identifier>, label: "SimpleUnionBranch2"),
                     scope: simple-union);
  add-declarator(simple-union, branch1);
  add-declarator(simple-union, branch2);
  pop(get-scepter().scepter-scopes);
  let def = #f;
  check("load-definition simple union", method () def := load-definition(*back-end*, simple-union) end);
  destroy-definitions(def);
end test;

define suite create-definition-in-container-suite ()
  test constant-create-definition-in-container;
  test enum-create-definition-in-container;
  test exception-create-definition-in-container;
  test interface-create-definition-in-container;
  test module-create-definition-in-container;
  test struct-create-definition-in-container;
  test typedef-create-definition-in-container;
end suite;

define suite scepter-ir-back-end-test-suite ()
  test setup;
  suite get-primitive-type-suite;
  suite load-definition-suite;
  suite create-definition-in-container-suite;
end suite;

run-test-application (scepter-ir-back-end-test-suite);

