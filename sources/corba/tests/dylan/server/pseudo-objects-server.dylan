Module:    pseudo-objects-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <TestObjectX-implementation> (<TestObjectX-servant>)
end class;

define class <TestObjectA-implementation> (<TestObjectA-servant>)
  slot TestObjectA/id :: CORBA/<long> = 0, init-keyword: id:;
  slot TestObjectA/ior :: CORBA/<string> = "";
end class;

define class <TestObjectB-implementation> (<TestObjectB-servant>, <TestObjectA-implementation>)
end class;

define class <TestObjectC-implementation> (<TestObjectC-servant>, <TestObjectA-implementation>, <TestObjectX-implementation>)
end class;

define class <TestObjectD-implementation> (<TestObjectD-servant>, <TestObjectB-implementation>, <TestObjectC-implementation>)
end class;

define method TestObjectA/destroy (object :: <TestObjectA-implementation>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let object-id = PortableServer/POA/servant-to-id(root-poa, object);
  PortableServer/POA/deactivate-object(root-poa, object-id);
end method;

define method initialize (object :: <TestObjectA-implementation>, #key poa :: PortableServer/<POA>)
  next-method();
  let orb = CORBA/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let reference = portableserver/poa/servant-to-reference(poa, object);
  object.TestObjectA/ior := corba/orb/object-to-string(orb, reference);
end method;


define class <PseudoObjectsTest-implementation> (<PseudoObjectsTest-servant>)
  constant slot poa :: PortableServer/<POA>, required-init-keyword: poa:;
  slot PseudoObjectsTest/object-attribute :: CORBA/<Object>;
  slot PseudoObjectsTest/typecode-attribute :: corba/<TypeCode>;
end class;

define method PseudoObjectsTest/TestObjectX-factory (object :: <PseudoObjectsTest-implementation>, id :: CORBA/<long>)
 => (result :: <TestObjectX>)
  let result = make(<TestObjectX-implementation>);
  as(<TestObjectX>, PortableServer/POA/servant-to-reference(object.poa, result));
end method;

define method PseudoObjectsTest/TestObjectA-factory (object :: <PseudoObjectsTest-implementation>, id :: CORBA/<long>)
 => (result :: <TestObjectA>)
  let result = make(<TestObjectA-implementation>, id: id, poa: object.poa);
  as(<TestObjectA>, PortableServer/POA/servant-to-reference(object.poa, result));
end method;

define method PseudoObjectsTest/TestObjectB-factory (object :: <PseudoObjectsTest-implementation>, id :: CORBA/<long>)
 => (result :: <TestObjectB>)
  let result = make(<TestObjectB-implementation>, id: id, poa: object.poa);
  as(<TestObjectB>, PortableServer/POA/servant-to-reference(object.poa, result));
end method;

define method PseudoObjectsTest/TestObjectC-factory (object :: <PseudoObjectsTest-implementation>, id :: CORBA/<long>)
 => (result :: <TestObjectC>)
  let result = make(<TestObjectC-implementation>, id: id, poa: object.poa);
  as(<TestObjectC>, PortableServer/POA/servant-to-reference(object.poa, result));
end method;

define method PseudoObjectsTest/TestObjectD-factory (object :: <PseudoObjectsTest-implementation>, id :: CORBA/<long>)
 => (result :: <TestObjectD>)
  let result = make(<TestObjectD-implementation>, id: id, poa: object.poa);
  as(<TestObjectD>, PortableServer/POA/servant-to-reference(object.poa, result));
end method;

define method PseudoObjectsTest/TestObjectX-nil-factory (object :: <PseudoObjectsTest-implementation>)
 => (result :: <TestObjectX>)
  make-nil(<TestObjectX>);
end method;

define method PseudoObjectsTest/identity (object :: <PseudoObjectsTest-implementation>, x :: CORBA/<Object>)
 => (result :: CORBA/<Object>)
  x;
end method;

define method PseudoObjectsTest/check-object-attribute (object :: <PseudoObjectsTest-implementation>, ior :: CORBA/<string>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  unless (ior = corba/orb/object-to-string(orb, PseudoObjectsTest/object-attribute(object)))
    error(make(PseudoObjectsTest/<failure>));
  end unless;
end method;

define method PseudoObjectsTest/object-operation (object :: <PseudoObjectsTest-implementation>, one :: CORBA/<Object>, two :: CORBA/<Object>)
 => (result :: CORBA/<Object>, two :: CORBA/<Object>, three :: CORBA/<Object>)
  values(one, one, two);
end method;

define method PseudoObjectsTest/check-typecode-attribute (object :: <PseudoObjectsTest-implementation>)
 => ()
  unless (PseudoObjectsTest/typecode-attribute(object) = class-typecode(PseudoObjectsTest/<failure>))
    error(make(PseudoObjectsTest/<failure>));
  end unless;
end method;

define method PseudoObjectsTest/typecode-operation
    (object :: <PseudoObjectsTest-implementation>, one :: corba/<TypeCode>, two :: corba/<TypeCode>)
 => (result :: corba/<TypeCode>, two :: corba/<TypeCode>, three :: corba/<TypeCode>)
  values(one, one, two);
end method;


define constant *PseudoObjectsTest-ior-file* = "c:\\temp\\PseudoObjectsTest.ior";

define method start-PseudoObjectsTest-server () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let PseudoObjectsTest = make(<PseudoObjectsTest-implementation>, poa: root-poa);
  let PseudoObjectsTestRef = portableserver/poa/servant-to-reference(root-poa, PseudoObjectsTest);
  corba/orb/object-to-file(orb, *PseudoObjectsTest-ior-file*, PseudoObjectsTestRef);
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);
end method;

register-server(start-pseudoobjectstest-server);
