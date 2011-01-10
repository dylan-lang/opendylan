Module:    pseudo-objects-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $PseudoObjectsTest-ior-file = "c:\\temp\\PseudoObjectsTest.ior";

define constant $TestObjectX-repositoryID = "IDL:TestObjectX:1.0";
define constant $TestObjectA-repositoryID = "IDL:TestObjectA:1.0";
define constant $TestObjectB-repositoryID = "IDL:TestObjectB:1.0";
define constant $TestObjectC-repositoryID = "IDL:TestObjectC:1.0";
define constant $TestObjectD-repositoryID = "IDL:TestObjectD:1.0";

define method TestObjectA-is-equivalent (object-1 :: <TestObjectA>, object-2 :: <TestObjectA>)
 => (equivalent? :: <boolean>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  (corba/orb/object-to-string(orb, object-1) = corba/orb/object-to-string(orb, object-2))
    & (TestObjectA/id(object-1) = TestObjectA/id(object-2));
end method;

define test standard-object-operations ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let PseudoObjectsTest = as(<PseudoObjectsTest>, corba/orb/file-to-object(orb, $PseudoObjectsTest-ior-file));  
  let object = PseudoObjectsTest/TestObjectA-factory(PseudoObjectsTest, 1);
  let nil-object = #f;
  check("NIL TestObjectX returned from server", method () nil-object := PseudoObjectsTest/TestObjectX-nil-factory(PseudoObjectsTest) end method);

  check-false("CORBA/Object/is-nil on non-nil object reference", CORBA/Object/is-nil(object));
  check-true("CORBA/Object/is-nil on nil object reference", CORBA/Object/is-nil(nil-object));
  check-true("CORBA/Object/is-a(TestObjectX) on nil object reference",
	     CORBA/Object/is-a(nil-object, $TestObjectX-repositoryID));
  check-true("CORBA/Object/is-a(TestObjectA) on nil object reference",
	     CORBA/Object/is-a(nil-object, $TestObjectA-repositoryID));
  check-true("CORBA/Object/is-a(TestObjectB) on nil object reference",
	     CORBA/Object/is-a(nil-object, $TestObjectB-repositoryID));
  check-true("CORBA/Object/is-a(TestObjectC) on nil object reference",
	     CORBA/Object/is-a(nil-object, $TestObjectC-repositoryID));
  check-true("CORBA/Object/is-a(TestObjectD) on nil object reference",
	     CORBA/Object/is-a(nil-object, $TestObjectD-repositoryID));

  check-true("CORBA/Object/duplicate", TestObjectA-is-equivalent(CORBA/Object/duplicate(object), object));

  let objectX = PseudoObjectsTest/TestObjectX-factory(PseudoObjectsTest, 0);
  let objectA = object;
  let objectB = PseudoObjectsTest/TestObjectB-factory(PseudoObjectsTest, 2);
  let objectC = PseudoObjectsTest/TestObjectC-factory(PseudoObjectsTest, 3);
  let objectD = PseudoObjectsTest/TestObjectD-factory(PseudoObjectsTest, 4);
  check-true("objectX is a TestObjectX", CORBA/Object/is-a(objectX, $TestObjectX-repositoryID));
  check-true("objectA is a TestObjectA", CORBA/Object/is-a(objectA, $TestObjectA-repositoryID));
  check-true("objectB is a TestObjectA", CORBA/Object/is-a(objectB, $TestObjectA-repositoryID));
  check-true("objectB is a TestObjectB", CORBA/Object/is-a(objectB, $TestObjectB-repositoryID));
  check-true("objectC is a TestObjectX", CORBA/Object/is-a(objectC, $TestObjectX-repositoryID));
  check-true("objectC is a TestObjectA", CORBA/Object/is-a(objectC, $TestObjectA-repositoryID));
  check-true("objectC is a TestObjectC", CORBA/Object/is-a(objectC, $TestObjectC-repositoryID));
  check-true("objectD is a TestObjectX", CORBA/Object/is-a(objectD, $TestObjectX-repositoryID));
  check-true("objectD is a TestObjectA", CORBA/Object/is-a(objectD, $TestObjectA-repositoryID));
  check-true("objectD is a TestObjectB", CORBA/Object/is-a(objectD, $TestObjectB-repositoryID));
  check-true("objectD is a TestObjectC", CORBA/Object/is-a(objectD, $TestObjectC-repositoryID));
  check-true("objectD is a TestObjectD", CORBA/Object/is-a(objectD, $TestObjectD-repositoryID));

  check-false("CORBA/Object/non-existent on object which exists", CORBA/Object/non-existent(object));
  TestObjectA/destroy(object);
  check-true("CORBA/Object/non-existent on non-existent object", CORBA/Object/non-existent(object));

  let duplicate = PseudoObjectsTest/identity(PseudoObjectsTest, object);
  check-true("CORBA/Object/is-equivalent true", CORBA/Object/is-equivalent(duplicate, object));
  check-false("CORBA/Object/is-equivalent false", CORBA/Object/is-equivalent(objectB, objectD));

  check("CORBA/Object/hash: 0", CORBA/Object/hash, object, 0);
  check("CORBA/Object/hash: 1000", CORBA/Object/hash, object, 1000);

  check-false("CORBA/Object/release", CORBA/Object/release(object));
end test;

define test object-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let PseudoObjectsTest = as(<PseudoObjectsTest>, corba/orb/file-to-object(orb, $PseudoObjectsTest-ior-file));
  let one = PseudoObjectsTest/TestObjectA-factory(PseudoObjectsTest, 0);
  check("Object attribute setter", PseudoObjectsTest/object-attribute-setter, one, PseudoObjectsTest);
  check-false("Check Object attribute", PseudoObjectsTest/check-object-attribute(PseudoObjectsTest, corba/orb/object-to-string(orb, one)));
  check-true("Object attribute getter", CORBA/Object/is-equivalent(PseudoObjectsTest/object-attribute(PseudoObjectsTest), one));
  let two = PseudoObjectsTest/TestObjectA-factory(PseudoObjectsTest, 1);
  let (i, ii, iii) = PseudoObjectsTest/object-operation(PseudoObjectsTest, one, two);
  check-true("Object operation first return value", CORBA/Object/is-equivalent(one, i));
  check-true("Object operation second return value",CORBA/Object/is-equivalent(one, ii));
  check-true("Object operation third return value", CORBA/Object/is-equivalent(two, iii));
end test;

define test typecode-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let PseudoObjectsTest = as(<PseudoObjectsTest>, corba/orb/file-to-object(orb, $PseudoObjectsTest-ior-file));
  let one = class-typecode(PseudoObjectsTest/<failure>);
  check("Typecode attribute setter", PseudoObjectsTest/typecode-attribute-setter, one, PseudoObjectsTest);
  check-false("Check Typecode attribute", PseudoObjectsTest/check-typecode-attribute(PseudoObjectsTest));
  check-equal("Typecode attribute getter", PseudoObjectsTest/typecode-attribute(PseudoObjectsTest), one);
  let two = corba/$short-typecode;
  let (i, ii, iii) = PseudoObjectsTest/typecode-operation(PseudoObjectsTest, one, two);
  check-equal("Typecode operation first return value", one, i);
  check-equal("Typecode operation second return value", one, ii);
  check-equal("Typecode operation third return value", two, iii);
end test;

define suite pseudo-objects-test-suite ()
  test standard-object-operations;
  test object-test;
  test typecode-test;
end suite;
