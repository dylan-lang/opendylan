Module:    sequence-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *SequenceTest-ior-file* = "c:\\temp\\sequencetest.ior";

define method sequence-equal? (sequence-1 :: <sequence>, sequence-2 :: <sequence>)
 => (equal? :: <boolean>)
  if (size(sequence-1) = size(sequence-2))
    let equal? = #t;
    let limit = size(sequence-1);
    for (i from 0 below limit)
      equal? := (equal? & (sequence-1[i] = sequence-2[i]));
    end for;
    equal?;
  else
    #f;
  end if;
end method;

define test short-seq-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let SequenceTest = as(<SequenceTest>, corba/orb/file-to-object(orb, *SequenceTest-ior-file*));
  let seq = make(limited(CORBA/<sequence>, of: CORBA/<short>));
  for (i from 0 below 100)
    seq := add!(seq, i * 11);
  end for;

  // Attributes
  //
  check("sequence<short> attribute setter", SequenceTest/attribute-short-seq-setter, seq, SequenceTest);
  check-false("sequence<short> attribute check", SequenceTest/check-attribute-short-seq(SequenceTest));
  check-true("sequence<short> attribute getter", sequence-equal?(SequenceTest/attribute-short-seq(SequenceTest), seq));

  // Parameters & results
  //
  check-false("sequence<short> in parameter", sequencetest/in-parameter-short-seq(SequenceTest, seq));
  check-true("sequence<short> inout parameter", sequence-equal?(SequenceTest/inout-parameter-short-seq(SequenceTest, seq), seq));
  check-true("sequence<short> out parameter", sequence-equal?(SequenceTest/out-parameter-short-seq(SequenceTest), seq));
  check-true("sequence<short> result", sequence-equal?(SequenceTest/result-short-seq(SequenceTest), seq));

  // CORBA/<any> coercion

  check-true("Coerce sequence<short> to any", instance?(as(CORBA/<any>, seq), CORBA/<any>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let any = make(CORBA/<any>,
                 value: seq,
		 type: corba/orb/create-sequence-tc(orb,
                                                    0,
                                                    corba/$short-typecode));
  check-true("Coerce any to sequence<short>",
             sequence-equal?(as(limited(CORBA/<sequence>, of: CORBA/<short>), any),
                             seq));
end test;

define constant *numbers* = #["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"];

define sideways method \= (struct-1 :: <Structure>, struct-2 :: <Structure>)
 => (equal? :: <boolean>)
  (Structure/name(struct-1) = Structure/name(struct-2))
    & (Structure/info(struct-1) = Structure/info(struct-2))
end method;

define test struct-seq-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let SequenceTest = as(<SequenceTest>, corba/orb/file-to-object(orb, *SequenceTest-ior-file*));
  let seq = make(limited(CORBA/<sequence>, of: <Structure>));
  for (i :: <integer> from 0 below 10)
    seq := add!(seq, make(<Structure>, name: *numbers*[i], info: i));
  end for;

  // Attributes
  //
  check("sequence<Structure> attribute setter", SequenceTest/attribute-struct-seq-setter, seq, SequenceTest);
  check-false("sequence<Structure> attribute check", SequenceTest/check-attribute-struct-seq(SequenceTest));
  check-true("sequence<Structure> attribute getter", sequence-equal?(SequenceTest/attribute-struct-seq(SequenceTest), seq));

  // Parameters & results
  //
  check-false("sequence<Structure> in parameter", sequencetest/in-parameter-struct-seq(SequenceTest, seq));
  check-true("sequence<Structure> inout parameter", sequence-equal?(SequenceTest/inout-parameter-struct-seq(SequenceTest, seq), seq));
  check-true("sequence<Structure> out parameter", sequence-equal?(SequenceTest/out-parameter-struct-seq(SequenceTest), seq));
  check-true("sequence<Structure> result", sequence-equal?(SequenceTest/result-struct-seq(SequenceTest), seq));

  // CORBA/<any> coercion
  check-true("Coerce sequence<Structure> to any", instance?(as(CORBA/<any>, seq), CORBA/<any>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let any = make(CORBA/<any>,
                 value: seq,
		 type: corba/orb/create-sequence-tc(orb,
                                                    0,
                                                    class-typecode(<Structure>)));
  check-true("Coerce any to sequence<Structure>",
             sequence-equal?(as(limited(CORBA/<sequence>, of: <Structure>), any),
                             seq));
end test;

define test object-seq-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let SequenceTest = as(<SequenceTest>, corba/orb/file-to-object(orb, *SequenceTest-ior-file*));
  let seq = make(limited(CORBA/<sequence>, of: <TestObject>));
  for (i :: <integer> from 0 below 20)
    seq := add!(seq, SequenceTest/TestObject-factory(SequenceTest, i));
  end for;

  // Attributes
  //
  check("sequence<TestObject> attribute setter", SequenceTest/attribute-object-seq-setter, seq, SequenceTest);
  check-false("sequence<TestObject> attribute check", SequenceTest/check-attribute-object-seq(SequenceTest));
  check-true("sequence<TestObject> attribute getter", sequence-equal?(SequenceTest/attribute-object-seq(SequenceTest), seq));

  // Parameters & results
  //
  check-false("sequence<TestObject> in parameter", sequencetest/in-parameter-object-seq(SequenceTest, seq));
  check-true("sequence<TestObject> inout parameter", sequence-equal?(SequenceTest/inout-parameter-object-seq(SequenceTest, seq), seq));
  check-true("sequence<TestObject> out parameter", sequence-equal?(SequenceTest/out-parameter-object-seq(SequenceTest), seq));
  check-true("sequence<TestObject> result", sequence-equal?(SequenceTest/result-object-seq(SequenceTest), seq));

  // CORBA/<any> coercion
  check-true("Coerce sequence<TestObject> to any", instance?(as(CORBA/<any>, seq), CORBA/<any>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let any = make(CORBA/<any>,
                 value: seq,
		 type: corba/orb/create-sequence-tc(orb,
                                                    0,
                                                    class-typecode(<TestObject>)));
  check-true("Coerce any to sequence<TestObject>",
             sequence-equal?(as(limited(CORBA/<sequence>, of: <TestObject>), any),
                             seq));
end test;

define constant *short-name* = #['B', 'a', 't', 'm', 'a', 'n', ' ', 'a', 'n', 'd', ' ', 'R', 'o', 'b', 'i', 'n'];
define constant *long-name* = #['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

define test bounded-seq-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let SequenceTest = as(<SequenceTest>, corba/orb/file-to-object(orb, *SequenceTest-ior-file*));
  let short-name = make(SequenceTest/<BoundedString>);
  let long-name = make(SequenceTest/<BoundedString>);
  do(curry(add!, short-name), *short-name*);
  do(curry(add!, long-name), *long-name*);

  check-false("Set short name", SequenceTest/set-short-name(SequenceTest, short-name));
//  Put this back in when the runtime raises an error for sequences longer than the bound
//  check-condition("Set long name", <error>, SequenceTest/set-long-name(SequenceTest, long-name));
  let (name-size, name) = SequenceTest/get-name(SequenceTest);
  check-equal("Size returned by Get name", name-size, size(short-name));
  check-equal("Name returned by Get name", name, short-name);
  check-true("Get short name", sequence-equal?(SequenceTest/get-short-name(SequenceTest), short-name));
//  Put this back in when the runtime raises an error for sequences longer than the bound
//  check-condition("Get long name",  <error>, SequenceTest/get-long-name, SequenceTest);
  check-true("Reverse name",  sequence-equal?(SequenceTest/reverse-name(SequenceTest, short-name), reverse(short-name)));
end test;

define suite sequence-test-suite ()
  test short-seq-test;
  test struct-seq-test;
  test object-seq-test;
  test bounded-seq-test;
end suite;
