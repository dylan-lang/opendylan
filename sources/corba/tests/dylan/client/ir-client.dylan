Module:    ir-client
Author:    Jason Trneouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// ---*** UNTESTED: attribute setters, struct getters and setters

define variable *count* :: <integer> = 10;

define method next-length () => (n :: <integer>)
  *count* := *count* + 1;
end method;

define method orb-interface-repository ()
 => (orb :: corba/<orb>, ir :: corba/<repository>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  values(orb, as(corba/<repository>, corba/orb/resolve-initial-references(orb, "InterfaceRepository")));
end method;

define variable *created* :: <stretchy-vector> = make(<stretchy-vector>);

define method note-created (x) => (x)
  *created* := add!(*created*, x);
  x
end method;

define method created () => (xs :: <sequence>)
  *created*
end method;

define suite ir-test-suite ()
  test container-test; // works except for describe-contents
  test contained-test;
  test repository-test;
  test interface-test;
  test irobject-test; 
  test create-typecode-test;
end suite;

define test irobject-test ()
  let (orb, ir) = orb-interface-repository();
  ignore(orb);
  check-equal(format-to-string("corba/irobject/def-kind(%=)", ir),
	      corba/irobject/def-kind(ir),
	      #"dk-repository");
  for (object in created())
    check-false(format-to-string("corba/irobject/destroy(%=)", object),
		corba/irobject/destroy(object));
  end for;
end test;

define test contained-test ()
  let (orb, ir) = orb-interface-repository();
  ignore(orb);
  let contents = #f;
  check("corba/container/contents",
	instance?,
	(contents := corba/container/contents(ir, #"dk-all", #f)),
	corba/<containedseq>);
  let object = find-element(contents,
			    method (x :: corba/<contained>)
			      ~instance?(x, corba/<moduledef>)
			    end method);
  check(format-to-string("corba/contained/id(%=)", object),
	instance?,
	corba/contained/id(object),
	corba/<repositoryid>);
  check(format-to-string("corba/contained/name(%=)", object),
	instance?,
	corba/contained/name(object),
	corba/<identifier>);
  check(format-to-string("corba/contained/version(%=)", object),
	instance?,
	corba/contained/version(object),
	corba/<versionspec>);
  check(format-to-string("corba/contained/defined-in(%=)", object),
	instance?,
	corba/contained/defined-in(object),
	corba/<container>);
  check(format-to-string("corba/contained/absolute-name(%=)", object),
	instance?,
	corba/contained/absolute-name(object),
	corba/<scopedname>);
  check(format-to-string("corba/contained/containing-repository(%=)", object),
	instance?,
	corba/contained/containing-repository(object),
	corba/<repository>);
  check(format-to-string("corba/contained/describe(%=)", object),
	instance?,
	corba/contained/describe(object),
	corba/contained/<description>);
  let module = find-element(contents,
			    method (x :: corba/<contained>)
			      instance?(x, corba/<moduledef>)
			    end method);
  let name = corba/contained/name(object);
  let version = corba/contained/version(object);
  check-false(format-to-string("corba/contained/move(%=, %=, %=, %=)", object, module, name, version),
	      corba/contained/move(object, module, name, version));
end test;

define test container-test ()
  let (orb, ir) = orb-interface-repository();
  ignore(orb);
  let object = #f;
  check(format-to-string("corba/container/create-module(%=, ...)", ir),
	instance?,
	(object := note-created(corba/container/create-module(ir, "LOCAL:test-module", "test-module", "1.0"))),
	corba/<moduledef>);
  let primitivedef = #f;
  check(format-to-string("corba/repository/get-primitive(%=, ...)", ir),
	instance?,
	(primitivedef := corba/repository/get-primitive(ir, #"pk-short")),
	corba/<primitivedef>);
  let typecode = #f;
  check(format-to-string("corba/idltype/type(%=)", primitivedef),
	instance?,
	(typecode := corba/idltype/type(primitivedef)),
	corba/<typecode>);
  let any-zero = make(corba/<any>, type: typecode, value: 0);
  let constantdef = #f;
  check(format-to-string("corba/container/create-constant(%=, .., %=, %=)", ir, primitivedef, any-zero),
	instance?,
	(constantdef := note-created(corba/container/create-constant(ir,
								     "LOCAL:test-constant",
								     "test-constant",
								     "1.0",
								     primitivedef,
								     any-zero))),
	corba/<constantdef>);
  check-equal(format-to-string("corba/constantdef/type(%=)", constantdef),
	      corba/constantdef/type(constantdef),
	      typecode);
  check-equal(format-to-string("corba/constantdef/type-def(%=)", constantdef),
	      corba/constantdef/type-def(constantdef),
	      primitivedef);
  check-equal(format-to-string("corba/constantdef/value(%=)", constantdef),
	      corba/constantdef/value(constantdef),
	      any-zero);
  let structmemberseq = make(corba/<structmemberseq>);
  structmemberseq := add!(structmemberseq,
			  make(corba/<structmember>,
			       name: "test-slot",
			       type: typecode,
			       type-def: primitivedef));
  let structdef = #f;
  check(format-to-string("corba/container/create-struct(%=, .., %=)", ir, structmemberseq),
	instance?,
	(structdef := note-created(corba/container/create-struct(ir, "LOCAL:test-struct", "test-struct", "1.0", structmemberseq))),
	corba/<structdef>);
  check-equal(format-to-string("corba/structdef/members(%=)", structdef),
	      corba/structdef/members(structdef),
	      structmemberseq);
  let unionmemberseq = make(corba/<unionmemberseq>);
  unionmemberseq := add!(unionmemberseq,
			 make(corba/<unionmember>,
			      name: "test-branch",
			      label: any-zero,
			      type: typecode,
			      type-def: primitivedef));
  let uniondef = #f;
  check(format-to-string("corba/container/create-union(%=, .., %=, %=)", ir, primitivedef, unionmemberseq),
	instance?,
	(uniondef := note-created(corba/container/create-union(ir, "LOCAL:test-union", "test-union", "1.0", primitivedef, unionmemberseq))),
	corba/<uniondef>);
  check-equal(format-to-string("corba/uniondef/discriminator-type(%=)", uniondef),
	      corba/uniondef/discriminator-type(uniondef),
	      typecode);
  check-equal(format-to-string("corba/uniondef/discriminator-type-def(%=)", uniondef),
	      corba/uniondef/discriminator-type-def(uniondef),
	      primitivedef);
  check-equal(format-to-string("corba/uniondef/members(%=)", uniondef),
	      corba/uniondef/members(uniondef),
	      unionmemberseq);
  let enummemberseq = as(corba/<enummemberseq>,
			 #["arrakis", "caladan", "geddi-prime", "salsa-secondus"]);
  let enumdef = #f;
  check(format-to-string("corba/container/create-enum(%=, .., %=)", ir, enummemberseq),
	instance?,
	(enumdef := note-created(corba/container/create-enum(ir, "LOCAL:test-enum", "test-enum", "1.0", enummemberseq))),
	corba/<enumdef>);
  check-equal(format-to-string("corba/enumdef/members(%=)", enumdef),
	      corba/enumdef/members(enumdef),
	      enummemberseq);
  let aliasdef = #f;
  check(format-to-string("corba/container/create-alias(%=, .., %=)", ir, primitivedef),
	instance?,
	(aliasdef := note-created(corba/container/create-alias(ir, "LOCAL:test-alias", "test-alias", "1.0", primitivedef))),
	corba/<aliasdef>);
  check-equal(format-to-string("corba/aliasdef/original-type-def(%=)", aliasdef),
	      corba/aliasdef/original-type-def(aliasdef),
	      primitivedef);
  let exceptiondef = #f;
  check(format-to-string("corba/container/create-exception(%=, .., %=)", ir, structmemberseq),
	instance?,
	(exceptiondef := note-created(corba/container/create-exception(ir, "LOCAL:test-exception", "test-exception", "1.0", structmemberseq))),
	corba/<exceptiondef>);
  check-equal(format-to-string("corba/exceptiondef/members(%=)", exceptiondef),
	      corba/exceptiondef/members(exceptiondef),
	      structmemberseq);
  let name = #f;
  check(format-to-string("corba/contained/absolute-name(%=)", object),
	instance?,
	(name := corba/contained/absolute-name(object)),
	corba/<scopedname>);
  check-equal(format-to-string("corba/container/lookup(%=, %=)", ir, name),
	      corba/container/lookup(ir, name),
	      object);
  check(format-to-string("corba/container/contents(%=, ..)", ir),
	instance?,
	corba/container/contents(ir, #"dk-all", #f),
	corba/<containedseq>);
  check(format-to-string("corba/container/lookup-name(%=, %=, ..)", ir, name),
	instance?,
	corba/container/lookup-name(ir, name, 1, #"dk-all", #f),
	corba/<containedseq>);
//  check(format-to-string("corba/container/describe-contents(%=, ..)", ir),
//	instance?,
//	corba/container/describe-contents(ir, #"dk-all", #f, 10),
//	corba/container/<descriptionseq>);
end test;

define test repository-test ()
  let (orb, ir) = orb-interface-repository();
  ignore(orb);
  check(format-to-string("corba/repository/lookup-id(%=, ..)", ir),
	instance?,
	corba/repository/lookup-id(ir, "LOCAL:test-interface"),
	corba/<contained>);
  let primitivedef = #f;
  check(format-to-string("corba/repository/get-primitive(%=, ..)", ir),
	instance?,
	(primitivedef := corba/repository/get-primitive(ir, #"pk-objref")),
	corba/<primitivedef>);
  let typecode = #f;
  check(format-to-string("corba/idltype/type(%=)", primitivedef),
	instance?,
	(typecode := corba/idltype/type(primitivedef)),
	corba/<typecode>);
  check-equal(format-to-string("corba/primitivedef/kind(%=)", primitivedef),
	      corba/primitivedef/kind(primitivedef),
	      #"pk-objref");
  let length = next-length();
  let stringdef = #f;
  check(format-to-string("corba/repository/create-string(%=, %=)", ir, length),
	instance?,
	(stringdef := corba/repository/create-string(ir, length)),
	corba/<stringdef>);
  check-equal(format-to-string("corba/stringdef/bound(%=)", stringdef),
	      corba/stringdef/bound(stringdef),
	      length);
  let sequencedef = #f;
  check(format-to-string("corba/repository/create-sequence(%=)", ir, length, primitivedef),
	instance?,
	(sequencedef := corba/repository/create-sequence(ir, length, primitivedef)),
	corba/<sequencedef>);
  check-equal(format-to-string("corba/sequencedef/bound(%=)", sequencedef),
	      corba/sequencedef/bound(sequencedef),
	      length);
  check-equal(format-to-string("corba/sequencedef/element-type(%=)", sequencedef),
	      corba/sequencedef/element-type(sequencedef),
	      typecode);
  check-equal(format-to-string("corba/sequencedef/element-type-def(%=)", sequencedef),
	      corba/sequencedef/element-type-def(sequencedef),
	      primitivedef);
  let arraydef = #f;
  check(format-to-string("corba/repository/create-array(%=, %=, %=)", ir, length, primitivedef),
	instance?,
	(arraydef := corba/repository/create-array(ir, length, primitivedef)),
	corba/<arraydef>);
  check-equal(format-to-string("corba/arraydef/length(%=)", arraydef),
	      corba/arraydef/length(arraydef),
	      length);
  check-equal(format-to-string("corba/arraydef/element-type(%=)", arraydef),
	      corba/arraydef/element-type(arraydef),
	      typecode);
  check-equal(format-to-string("corba/arraydef/element-type-def(%=)", arraydef),
	      corba/arraydef/element-type-def(arraydef),
	      primitivedef);
end test;

define test interface-test ()
  let (orb, ir) = orb-interface-repository();
  ignore(orb);
  let primitivedef = #f;
  check(format-to-string("corba/repository/get-primitive(%=, ..)", ir),
	instance?,
	(primitivedef := corba/repository/get-primitive(ir, #"pk-objref")),
	corba/<primitivedef>);
  let typecode = #f;
  check(format-to-string("corba/idltype/type(%=)", primitivedef),
	instance?,
	(typecode := corba/idltype/type(primitivedef)),
	corba/<typecode>);
  let interfacedef = #f;
  check(format-to-string("corba/container/create-interface(%=, ..)", ir),
	instance?,
	(interfacedef := note-created(corba/container/create-interface(ir,
							  "LOCAL:test-interface",
							  "test-interface",
							  "1.0",
							  make(corba/<interfacedefseq>)))),
	corba/<interfacedef>);
  let base-interfaces = as(corba/<interfacedefseq>, list(interfacedef));
  let subinterfacedef = #f;
  check(format-to-string("corba/container/create-interface(%=, .., %=)", ir, base-interfaces),
	instance?,
	(subinterfacedef := note-created(corba/container/create-interface(ir,
							     "LOCAL:test-subinterface",
							     "test-subinterface",
							     "1.0",
							     base-interfaces))),
	corba/<interfacedef>);
  check-equal(format-to-string("corba/interfacedef/base-interfaces(%=)", subinterfacedef),
	      corba/interfacedef/base-interfaces(subinterfacedef),
	      base-interfaces);
  check-true(format-to-string("corba/interfacedef/is-a(%=, ..)", subinterfacedef),
	     corba/interfacedef/is-a(subinterfacedef, "LOCAL:test-subinterface"));
  check(format-to-string("corba/interfacedef/describe-interface(%=)", subinterfacedef),
	instance?,
	corba/interfacedef/describe-interface(subinterfacedef),
	corba/interfacedef/<fullinterfacedescription>);
  let attributedef = #f;
  check(format-to-string("corba/interfacedef/create-attribute(%=, .., %=, ..)", subinterfacedef, primitivedef),
	instance?,
	(attributedef := corba/interfacedef/create-attribute(subinterfacedef,
							     "LOCAL:test-subinterface/test-attribute",
							     "test-attribute",
							     "1.0",
							     primitivedef,
							     #"attr-normal")),
	corba/<attributedef>);
  check-equal(format-to-string("corba/attributedef/type(%=)", attributedef),
	      corba/attributedef/type(attributedef),
	      typecode);
  check-equal(format-to-string("corba/attributedef/type-def(%=)", attributedef),
	      corba/attributedef/type-def(attributedef),
	      primitivedef);
  check-equal(format-to-string("corba/attributedef/mode(%=)", attributedef),
	      corba/attributedef/mode(attributedef),
	      #"attr-normal");
  let operationdef = #f;
  let pardescriptionseq = make(corba/<pardescriptionseq>);
  pardescriptionseq := add!(pardescriptionseq,
			    make(corba/<parameterdescription>,
				 name: "foo",
				 type: typecode,
				 type-def: primitivedef,
				 mode: #"param-inout"));
  let exceptiondefseq = make(corba/<exceptiondefseq>);
  let contextseq = make(corba/<contextidseq>);
  check(format-to-string("corba/interfacedef/create-operation(%=, .., %=, .., %=, %=, %=)",
			 subinterfacedef, primitivedef, pardescriptionseq, exceptiondefseq, contextseq),
	instance?,
	(operationdef := corba/interfacedef/create-operation(subinterfacedef,
							     "LOCAL:test-subinterface/test-operation",
							     "test-operation",
							     "1.0",
							     primitivedef,
							     #"op-normal",
							     pardescriptionseq,
							     exceptiondefseq,
							     contextseq)),
	corba/<operationdef>);
  check-equal(format-to-string("corba/operationdef/result(%=)", operationdef),
	      corba/operationdef/result(operationdef),
	      typecode);
  check-equal(format-to-string("corba/operationdef/result-def(%=)", operationdef),
	      corba/operationdef/result-def(operationdef),
	      primitivedef);
  check-equal(format-to-string("corba/operationdef/params(%=)", operationdef),
	      corba/operationdef/params(operationdef),
	      pardescriptionseq);
  check-equal(format-to-string("corba/operationdef/mode(%=)", operationdef),
	      corba/operationdef/mode(operationdef),
	      #"op-normal");
  check-equal(format-to-string("corba/operationdef/contexts(%=)", operationdef),
	      corba/operationdef/contexts(operationdef),
	      contextseq);
  check-equal(format-to-string("corba/operationdef/exceptions(%=)", operationdef),
	      corba/operationdef/exceptions(operationdef),
	      exceptiondefseq);
  check(format-to-string("corba/object/get-interface(%=)", ir),
	instance?,
	corba/object/get-interface(ir),
	corba/<interfacedef>);
end test;

define test create-typecode-test ()
  let (orb, ir) = orb-interface-repository();
  let primitivedef = #f;
  check(format-to-string("corba/repository/get-primitive(%=, ..)", ir),
	instance?,
	(primitivedef := corba/repository/get-primitive(ir, #"pk-short")),
	corba/<primitivedef>);
  let typecode = #f;
  check(format-to-string("corba/idltype/type(%=)", primitivedef),
	instance?,
	(typecode := corba/idltype/type(primitivedef)),
	corba/<typecode>);
  let any-zero = make(corba/<any>, type: typecode, value: 0);
  let structmemberseq = make(corba/<structmemberseq>);
  structmemberseq := add!(structmemberseq,
			  make(corba/<structmember>,
			       name: "test-slot",
			       type: typecode,
			       type-def: primitivedef));
  let struct-tc = #f;
  check(format-to-string("corba/orb/create-struct-tc(%=, .., %=)", orb, structmemberseq),
	instance?,
	(struct-tc := corba/orb/create-struct-tc(orb, "LOCAL:test-struct-tc", "test-struct-tc", structmemberseq)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", struct-tc),
	      corba/typecode/kind(struct-tc),
	      #"tk-struct");
  let unionmemberseq = make(corba/<unionmemberseq>);
  unionmemberseq := add!(unionmemberseq,
			 make(corba/<unionmember>,
			      name: "test-branch",
			      label: any-zero,
			      type: typecode,
			      type-def: primitivedef));
  let union-tc = #f;
  check(format-to-string("corba/orb/create-union-tc(%=, .., %=, %=)", orb, typecode, unionmemberseq),
	instance?,
	(union-tc := corba/orb/create-union-tc(orb, "LOCAL:test-union-tc", "test-union-tc", typecode, unionmemberseq)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", union-tc),
	      corba/typecode/kind(union-tc),
	      #"tk-union");
  let enummemberseq = as(corba/<enummemberseq>,
			 #["arrakis", "caladan", "geddi-prime", "salsa-secondus"]);
  let enum-tc = #f;
  check(format-to-string("corba/orb/create-enum-tc(%=, .., %=)", orb, enummemberseq),
	instance?,
	(enum-tc := corba/orb/create-enum-tc(orb, "LOCAL:test-enum-tc", "test-enum-tc", enummemberseq)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", enum-tc),
	      corba/typecode/kind(enum-tc),
	      #"tk-enum");
  let alias-tc = #f;
  check(format-to-string("corba/orb/create-alias-tc(%=, .., %=)", orb, struct-tc),
	instance?,
	(alias-tc := corba/orb/create-alias-tc(orb, "LOCAL:test-alias-tc", "test-alias-tc", struct-tc)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", alias-tc),
	      corba/typecode/kind(alias-tc),
	      #"tk-alias");
  let exception-tc = #f;
  check(format-to-string("corba/orb/create-exception-tc(%=m .., %=)", orb, structmemberseq),
	instance?,
	(exception-tc := corba/orb/create-exception-tc(orb, "LOCAL:test-exception-tc", "test-exception-tc", structmemberseq)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", exception-tc),
	      corba/typecode/kind(exception-tc),
	      #"tk-except");
  let interface-tc = #f;
  check(format-to-string("corba/orb/create-interface-tc(%=, ..)", orb),
	instance?,
	(interface-tc := corba/orb/create-interface-tc(orb, "LOCAL:test-interface-tc", "test-interface-tc")),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", interface-tc),
	      corba/typecode/kind(interface-tc),
	      #"tk-objref");
  let string-tc = #f;
  check(format-to-string("corba/orb/create-string-tc(%=, ..)", orb),
	instance?,
	(string-tc := corba/orb/create-string-tc(orb, 10)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", struct-tc),
	      corba/typecode/kind(string-tc),
	      #"tk-string");
  let sequence-tc = #f;
  check(format-to-string("corba/orb/create-sequence-tc(%=, .., %=)", orb, struct-tc),
	instance?,
	(sequence-tc := corba/orb/create-sequence-tc(orb, 10, struct-tc)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", sequence-tc),
	      corba/typecode/kind(sequence-tc),
	      #"tk-sequence");
  let recursive-sequence-tc = #f;
  check(format-to-string("corba/orb/create-recursive-sequence-tc(%=, ..)", orb),
	instance?,
	(recursive-sequence-tc := corba/orb/create-recursive-sequence-tc(orb, 10, 1)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", recursive-sequence-tc),
	      corba/typecode/kind(recursive-sequence-tc),
	      #"tk-sequence");
  let array-tc = #f;
  check(format-to-string("corba/orb/create-array-tc(%=, .., %=)", orb, struct-tc),
	instance?,
	(array-tc := corba/orb/create-array-tc(orb, 10, struct-tc)),
	corba/<typecode>);
  check-equal(format-to-string("corba/typecode/kind(%=)", array-tc),
	      corba/typecode/kind(array-tc),
	      #"tk-array");
end test;

