Module:    union-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $UnionTest-ior-file = "c:\\temp\\UnionTest.ior";

define method get-UnionTest-reference ()
 => (reference :: <UnionTest>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  as(<UnionTest>, corba/orb/file-to-object(orb, $UnionTest-ior-file));
end method;

define variable *rle-entity-1a* = make(<RLE-entity-1>, length: 5);
define variable *rle-entity-1b* = make(<RLE-entity-1>, character: 'a');
define variable *rle-entity-1c* = make(<RLE-entity-1>, discriminator: 0, value: 10);
define variable *rle-entity-1d* = make(<RLE-entity-1>, discriminator: 1, value: 15);
define variable *rle-entity-1e* = make(<RLE-entity-1>, discriminator: 2, value: 'b');

define variable *rle-entity-2a* = make(<RLE-entity-2>, length: 20);
define variable *rle-entity-2b* = make(<RLE-entity-2>, character: 'c');
define variable *rle-entity-2c* = make(<RLE-entity-2>, discriminator: 'n', value: 25);
define variable *rle-entity-2d* = make(<RLE-entity-2>, discriminator: ' ', value: 'd');

define variable *rle-entity-3a* = make(<RLE-entity-3>, length: 30);
define variable *rle-entity-3b* = make(<RLE-entity-3>, character: 'e');
define variable *rle-entity-3c* = make(<RLE-entity-3>, discriminator: #f, value: 35);
define variable *rle-entity-3d* = make(<RLE-entity-3>, discriminator: #t, value: 'f');

define variable *rle-entity-4a* = make(<RLE-entity-4>, length: 40);
define variable *rle-entity-4b* = make(<RLE-entity-4>, character: 'g');
define variable *rle-entity-4c* = make(<RLE-entity-4>, code: 111);
define variable *rle-entity-4d* = make(<RLE-entity-4>, discriminator: #"one", value: 45);
define variable *rle-entity-4e* = make(<RLE-entity-4>, discriminator: #"two", value: 'h');
define variable *rle-entity-4f* = make(<RLE-entity-4>, discriminator: #"three", value: 222);

define variable *rle-entity-5a* = make(<RLE-entity-5>, length: 50);
define variable *rle-entity-5b* = make(<RLE-entity-5>, character: 'i');
define variable *rle-entity-5c* = make(<RLE-entity-5>, discriminator: #"one", value: 55);
define variable *rle-entity-5d* = make(<RLE-entity-5>, discriminator: #"two", value: 'j');
define variable *rle-entity-5e* = make(<RLE-entity-5>, discriminator: #"three", value: 'k');

define test union-make-rle-entity-1-test ()
  check-true("discriminator = 0 or 1", CORBA/union/discriminator(*rle-entity-1a*) = 0 | CORBA/union/discriminator(*rle-entity-1a*) = 1);
  check-equal("value = 5", CORBA/union/value(*rle-entity-1a*), 5);
  check-equal("discriminator = 2", CORBA/union/discriminator(*rle-entity-1b*), 2);
  check-equal("value = 'a'", CORBA/union/value(*rle-entity-1b*), 'a');
  check-equal("discriminator = 0", CORBA/union/discriminator(*rle-entity-1c*), 0);
  check-equal("value = 10", CORBA/union/value(*rle-entity-1c*), 10);
  check-equal("discriminator = 1", CORBA/union/discriminator(*rle-entity-1d*), 1);
  check-equal("value = 15", CORBA/union/value(*rle-entity-1d*), 15);
  check-equal("discriminator = 2", CORBA/union/discriminator(*rle-entity-1e*), 2);
  check-equal("value = 'b'", CORBA/union/value(*rle-entity-1e*), 'b');
end test;

define test union-make-rle-entity-2-test ()
  check-equal("discriminator = 'n'", CORBA/union/discriminator(*rle-entity-2a*), 'n');
  check-equal("value = 20", CORBA/union/value(*rle-entity-2a*), 20);
  check-true("discriminator ~= 'n'", CORBA/union/discriminator(*rle-entity-2b*) ~= 'n');
  check-equal("value = 'c'", CORBA/union/value(*rle-entity-2b*), 'c');
  check-equal("discriminator = 'n'", CORBA/union/discriminator(*rle-entity-2c*), 'n');
  check-equal("value = 25", CORBA/union/value(*rle-entity-2c*), 25);
  check-equal("discriminator = ' '", CORBA/union/discriminator(*rle-entity-2d*), ' ');
  check-equal("value = 'd'", CORBA/union/value(*rle-entity-2d*), 'd');
end test;

define test union-make-rle-entity-3-test ()
  check-equal("discriminator = #f", CORBA/union/discriminator(*rle-entity-3a*), #f);
  check-equal("value = 30", CORBA/union/value(*rle-entity-3a*), 30);
  check-equal("discriminator = #t", CORBA/union/discriminator(*rle-entity-3b*), #t);
  check-equal("value = 'e'", CORBA/union/value(*rle-entity-3b*), 'e');
  check-equal("discriminator = #f", CORBA/union/discriminator(*rle-entity-3c*), #f);
  check-equal("value = 35", CORBA/union/value(*rle-entity-3c*), 35);
  check-equal("discriminator = #t", CORBA/union/discriminator(*rle-entity-3d*), #t);
  check-equal("value = 'f'", CORBA/union/value(*rle-entity-3d*), 'f');
end test;

define test union-make-rle-entity-4-test ()
  check-equal("discriminator = #\"one\"", CORBA/union/discriminator(*rle-entity-4a*), #"one");
  check-equal("value = 40", CORBA/union/value(*rle-entity-4a*), 40);
  check-equal("discriminator = #\"two\"", CORBA/union/discriminator(*rle-entity-4b*), #"two");
  check-equal("value = 'g'", CORBA/union/value(*rle-entity-4b*), 'g');
  check-equal("discriminator = #\"three\"", CORBA/union/discriminator(*rle-entity-4c*), #"three");
  check-equal("value = 111", CORBA/union/value(*rle-entity-4c*), 111);
  check-equal("discriminator = #\"one\"", CORBA/union/discriminator(*rle-entity-4d*), #"one");
  check-equal("value = 45", CORBA/union/value(*rle-entity-4d*), 45);
  check-equal("discriminator = #\"two\"", CORBA/union/discriminator(*rle-entity-4e*), #"two");
  check-equal("value = 'h'", CORBA/union/value(*rle-entity-4e*), 'h');
  check-equal("discriminator = #\"three\"", CORBA/union/discriminator(*rle-entity-4f*), #"three");
  check-equal("value = 222", CORBA/union/value(*rle-entity-4f*), 222);
end test;

define test union-make-rle-entity-5-test ()
  check-equal("discriminator = #\"one\"", CORBA/union/discriminator(*rle-entity-5a*), #"one");
  check-equal("value = 50", CORBA/union/value(*rle-entity-5a*), 50);
  check-equal("discriminator = #\"two\"", CORBA/union/discriminator(*rle-entity-5b*), #"two");
  check-equal("value = 'i'", CORBA/union/value(*rle-entity-5b*), 'i');
  check-equal("discriminator = #\"one\"", CORBA/union/discriminator(*rle-entity-5c*), #"one");
  check-equal("value = 55", CORBA/union/value(*rle-entity-5c*), 55);
  check-equal("discriminator = #\"two\"", CORBA/union/discriminator(*rle-entity-5d*), #"two");
  check-equal("value = 'j'", CORBA/union/value(*rle-entity-5d*), 'j');
  check-equal("discriminator = #\"three\"", CORBA/union/discriminator(*rle-entity-5e*), #"three");
  check-equal("value = 'k'", CORBA/union/value(*rle-entity-5e*), 'k');
end test;

define test union-any-test ()
  let rle-entity-1-any = as(CORBA/<any>, *rle-entity-1a*);
  check-true("Coerce *rle-entity-1a* to an any", instance?(rle-entity-1-any, CORBA/<any>));
  check-equal("Coerce any back to <RLE-entity-1>", *rle-entity-1a*, as(<RLE-entity-1>, rle-entity-1-any));

  let rle-entity-2-any = as(CORBA/<any>, *rle-entity-2a*);
  check-true("Coerce *rle-entity-2a* to an any", instance?(rle-entity-2-any, CORBA/<any>));
  check-equal("Coerce any back to <RLE-entity-2>", *rle-entity-2a*, as(<RLE-entity-2>, rle-entity-2-any));

  let rle-entity-3-any = as(CORBA/<any>, *rle-entity-3a*);
  check-true("Coerce *rle-entity-3a* to an any", instance?(rle-entity-3-any, CORBA/<any>));
  check-equal("Coerce any back to <RLE-entity-3>", *rle-entity-3a*, as(<RLE-entity-3>, rle-entity-3-any));

  let rle-entity-4-any = as(CORBA/<any>, *rle-entity-4a*);
  check-true("Coerce *rle-entity-4a* to an any", instance?(rle-entity-4-any, CORBA/<any>));
  check-equal("Coerce any back to <RLE-entity-4>", *rle-entity-4a*, as(<RLE-entity-4>, rle-entity-4-any));

  let rle-entity-5-any = as(CORBA/<any>, *rle-entity-5a*);
  check-true("Coerce *rle-entity-5a* to an any", instance?(rle-entity-5-any, CORBA/<any>));
  check-equal("Coerce any back to <RLE-entity-5>", *rle-entity-5a*, as(<RLE-entity-5>, rle-entity-5-any));
end test;

define test union-coercion-test ()
  check-equal("Coerce *rle-entity-1a* to CORBA/<short>", as(CORBA/<short>, *rle-entity-1a*), 5);
  check-equal("Coerce *rle-entity-1b* to CORBA/<char>", as(CORBA/<char>, *rle-entity-1b*), 'a');

  check-equal("Coerce *rle-entity-2a* to CORBA/<short>", as(CORBA/<short>, *rle-entity-2a*), 20);
  check-equal("Coerce *rle-entity-2b* to CORBA/<char>", as(CORBA/<char>, *rle-entity-2b*), 'c');

  check-equal("Coerce *rle-entity-3a* to CORBA/<short>", as(CORBA/<short>, *rle-entity-3a*), 30);
  check-equal("Coerce *rle-entity-3b* to CORBA/<char>", as(CORBA/<char>, *rle-entity-3b*), 'e');

  check-equal("Coerce *rle-entity-4a* to CORBA/<short>", as(CORBA/<short>, *rle-entity-4a*), 40);
  check-equal("Coerce *rle-entity-4b* to CORBA/<char>", as(CORBA/<char>, *rle-entity-4b*), 'g');
  check-equal("Coerce *rle-entity-4c* to CORBA/<unsigned-long>", as(CORBA/<unsigned-long>, *rle-entity-4c*), 111);

  check-equal("Coerce *rle-entity-5a* to CORBA/<short>", as(CORBA/<short>, *rle-entity-5a*), 50);
  check-equal("Coerce *rle-entity-5b* to CORBA/<char>", as(CORBA/<char>, *rle-entity-5b*), 'i');
end test;

define test union-attribute-test ()
  let UnionTest = get-UnionTest-reference();

  let union = make(<RLE-entity-1>, length: 10);
  check("RLE_entity_1 attribute setter", UnionTest/rle-entity-1-attribute-setter, union, UnionTest);
  check-false("RLE_entity_1 attribute check", UnionTest/check-rle-entity-1-attribute(UnionTest));
  check-equal("RLE_entity_1 attribute getter", UnionTest/rle-entity-1-attribute(UnionTest), union);

  let union = make(<RLE-entity-2>, character: 'h');
  check("RLE_entity_2 attribute setter", UnionTest/rle-entity-2-attribute-setter, union, UnionTest);
  check-false("RLE_entity_2 attribute check", UnionTest/check-rle-entity-2-attribute(UnionTest));
  check-equal("RLE_entity_2 attribute getter", UnionTest/rle-entity-2-attribute(UnionTest), union);

  let union = make(<RLE-entity-3>, length: 20);
  check("RLE_entity_3 attribute setter", UnionTest/rle-entity-3-attribute-setter, union, UnionTest);
  check-false("RLE_entity_3 attribute check", UnionTest/check-rle-entity-3-attribute(UnionTest));
  check-equal("RLE_entity_3 attribute getter", UnionTest/rle-entity-3-attribute(UnionTest), union);

  let union = make(<RLE-entity-4>, character: 'i');
  check("RLE_entity_4 attribute setter", UnionTest/rle-entity-4-attribute-setter, union, UnionTest);
  check-false("RLE_entity_4 attribute check", UnionTest/check-rle-entity-4-attribute(UnionTest));
  check-equal("RLE_entity_4 attribute getter", UnionTest/rle-entity-4-attribute(UnionTest), union);

  let union = make(<RLE-entity-5>, length: 30);
  check("RLE_entity_5 attribute setter", UnionTest/rle-entity-5-attribute-setter, union, UnionTest);
  check-false("RLE_entity_5 attribute check", UnionTest/check-rle-entity-5-attribute(UnionTest));
  check-equal("RLE_entity_5 attribute getter", UnionTest/rle-entity-5-attribute(UnionTest), union);
end test;

define test union-rle-entity-1-operation-test ()
  let UnionTest = get-UnionTest-reference();

  let (i, ii, iii) = UnionTest/rle-entity-1-operation(UnionTest, *rle-entity-1a*, *rle-entity-1b*);
  check-equal("rle-entity-1-operation first result = 1a", i, *rle-entity-1a*);
  check-equal("rle-entity-1-operation second result = 1b", ii, *rle-entity-1b*);
  check-equal("rle-entity-1-operation third result = 1a", iii, *rle-entity-1a*);

//  Comment out due to bug 4147
//  let (i, ii, iii) = UnionTest/rle-entity-1-operation(UnionTest, *rle-entity-1c*, *rle-entity-1d*);
//  check-equal("rle-entity-1-operation first result = 1c", i, *rle-entity-1c*);
//  check-equal("rle-entity-1-operation second result = 1d", ii, *rle-entity-1d*);
//  check-equal("rle-entity-1-operation third result = 1c", iii, *rle-entity-1c*);

  let (i, ii, iii) = UnionTest/rle-entity-1-operation(UnionTest, *rle-entity-1e*, *rle-entity-1e*);
  check-equal("rle-entity-1-operation first result = 1e", i, *rle-entity-1e*);
  check-equal("rle-entity-1-operation second result = 1e", ii, *rle-entity-1e*);
  check-equal("rle-entity-1-operation third result = 1e", iii, *rle-entity-1e*);
end test;

define test union-rle-entity-2-operation-test ()
  let UnionTest = get-UnionTest-reference();

  let (i, ii, iii) = UnionTest/rle-entity-2-operation(UnionTest, *rle-entity-2a*, *rle-entity-2b*);
  check-equal("rle-entity-2-operation first result = 2a", i, *rle-entity-2a*);
  check-equal("rle-entity-2-operation second result = 2b", ii, *rle-entity-2b*);
  check-equal("rle-entity-2-operation third result = 2a", iii, *rle-entity-2a*);

  let (i, ii, iii) = UnionTest/rle-entity-2-operation(UnionTest, *rle-entity-2c*, *rle-entity-2d*);
  check-equal("rle-entity-2-operation first result = 2c", i, *rle-entity-2c*);
  check-equal("rle-entity-2-operation second result = 2d", ii, *rle-entity-2d*);
  check-equal("rle-entity-2-operation third result = 2c", iii, *rle-entity-2c*);
end test;

define test union-rle-entity-3-operation-test ()
  let UnionTest = get-UnionTest-reference();

  let (i, ii, iii) = UnionTest/rle-entity-3-operation(UnionTest, *rle-entity-3a*, *rle-entity-3b*);
  check-equal("rle-entity-3-operation first result = 3a", i, *rle-entity-3a*);
  check-equal("rle-entity-3-operation second result = 3b", ii, *rle-entity-3b*);
  check-equal("rle-entity-3-operation third result = 3a", iii, *rle-entity-3a*);

  let (i, ii, iii) = UnionTest/rle-entity-3-operation(UnionTest, *rle-entity-3c*, *rle-entity-3d*);
  check-equal("rle-entity-3-operation first result = 3c", i, *rle-entity-3c*);
  check-equal("rle-entity-3-operation second result = 3d", ii, *rle-entity-3d*);
  check-equal("rle-entity-3-operation third result = 3c", iii, *rle-entity-3c*);
end test;

define test union-rle-entity-4-operation-test ()
  let UnionTest = get-UnionTest-reference();

  let (i, ii, iii) = UnionTest/rle-entity-4-operation(UnionTest, *rle-entity-4a*, *rle-entity-4b*);
  check-equal("rle-entity-4-operation first result = 4a", i, *rle-entity-4a*);
  check-equal("rle-entity-4-operation second result = 4b", ii, *rle-entity-4b*);
  check-equal("rle-entity-4-operation third result = 4a", iii, *rle-entity-4a*);

  let (i, ii, iii) = UnionTest/rle-entity-4-operation(UnionTest, *rle-entity-4c*, *rle-entity-4d*);
  check-equal("rle-entity-4-operation first result = 4c", i, *rle-entity-4c*);
  check-equal("rle-entity-4-operation second result = 4d", ii, *rle-entity-4d*);
  check-equal("rle-entity-4-operation third result = 4c", iii, *rle-entity-4c*);

  let (i, ii, iii) = UnionTest/rle-entity-4-operation(UnionTest, *rle-entity-4e*, *rle-entity-4f*);
  check-equal("rle-entity-4-operation first result = 4e", i, *rle-entity-4e*);
  check-equal("rle-entity-4-operation second result = 4f", ii, *rle-entity-4f*);
  check-equal("rle-entity-4-operation third result = 4e", iii, *rle-entity-4e*);
end test;

define test union-rle-entity-5-operation-test ()
  let UnionTest = get-UnionTest-reference();

  let (i, ii, iii) = UnionTest/rle-entity-5-operation(UnionTest, *rle-entity-5a*, *rle-entity-5b*);
  check-equal("rle-entity-5-operation first result = 5a", i, *rle-entity-5a*);
  check-equal("rle-entity-5-operation second result = 5b", ii, *rle-entity-5b*);
  check-equal("rle-entity-5-operation third result = 5a", iii, *rle-entity-5a*);

  let (i, ii, iii) = UnionTest/rle-entity-5-operation(UnionTest, *rle-entity-5c*, *rle-entity-5d*);
  check-equal("rle-entity-5-operation first result = 5c", i, *rle-entity-5c*);
  check-equal("rle-entity-5-operation second result = 5d", ii, *rle-entity-5d*);
  check-equal("rle-entity-5-operation third result = 5c", iii, *rle-entity-5c*);

  let (i, ii, iii) = UnionTest/rle-entity-5-operation(UnionTest, *rle-entity-5e*, *rle-entity-5e*);
  check-equal("rle-entity-5-operation first result = 5e", i, *rle-entity-5e*);
  check-equal("rle-entity-5-operation second result = 5e", ii, *rle-entity-5e*);
  check-equal("rle-entity-5-operation third result = 5e", iii, *rle-entity-5e*);
end test;

define suite union-test-suite ()
  test union-make-rle-entity-1-test;
  test union-make-rle-entity-2-test;
  test union-make-rle-entity-3-test;
  test union-make-rle-entity-4-test;
  test union-make-rle-entity-5-test;
  test union-any-test;
  test union-coercion-test;
  test union-attribute-test;
  test union-rle-entity-1-operation-test;
  test union-rle-entity-2-operation-test;
  test union-rle-entity-3-operation-test;
  test union-rle-entity-4-operation-test;
  test union-rle-entity-5-operation-test;
end suite;

