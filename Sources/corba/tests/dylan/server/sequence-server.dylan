Module:    sequence-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <SequenceTest-implementation> (<SequenceTest-servant>)
  slot SequenceTest-implementation-objects :: limited(CORBA/<sequence>, of: <TestObject>);
  slot SequenceTest/attribute-object-seq :: limited(CORBA/<sequence>, of: <TestObject>),
    setter: SequenceTest/%attribute-object-seq-setter;
  slot SequenceTest/attribute-struct-seq :: limited(CORBA/<sequence>, of: <Structure>);
  slot SequenceTest/attribute-short-seq :: limited(CORBA/<sequence>, of: CORBA/<short>);
end class;

define method SequenceTest/attribute-object-seq-setter
    (seq :: limited(CORBA/<sequence>, of: <TestObject>), object :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: <TestObject>))
  reset-implementation-objects(object, seq);
  SequenceTest/%attribute-object-seq(object) := seq;
end method;

define method reset-implementation-objects
    (object :: <SequenceTest-implementation>, seq :: limited(CORBA/<sequence>, of: <TestObject>))
  let total = size(SequenceTest-implementation-objects(object));
  let last-made = make(limited(CORBA/<sequence>, of: <TestObject>));
  // reset the seq of implementation objects to chronologically match references given setter
  for (i from total - size(seq) below total)
    last-made := add!(last-made, SequenceTest-implementation-objects(object)[i]);
  end for;
  SequenceTest-implementation-objects(object) := last-made;
end method;

define method initialize (object :: <SequenceTest-implementation>, #key)
  next-method();
  SequenceTest-implementation-objects(object) := make(limited(CORBA/<sequence>, of: <TestObject>));
end method;

define method SequenceTest/check-attribute-short-seq (SequenceTest :: <SequenceTest-implementation>)
 => ()
  let seq = SequenceTest/attribute-short-seq(SequenceTest);
  for (i from 0 below size(seq))
   let expected = i * 11;
    unless (seq[i] = expected)
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/in-parameter-short-seq (SequenceTest :: <SequenceTest-implementation>, inseq :: limited(CORBA/<sequence>, of: CORBA/<short>))
 => ()
  let seq = SequenceTest/attribute-short-seq(SequenceTest);
  for (i from 0 below size(seq))
    unless (seq[i] = inseq[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/inout-parameter-short-seq (SequenceTest :: <SequenceTest-implementation>, inseq :: limited(CORBA/<sequence>, of: CORBA/<short>))
 => (seq :: limited(CORBA/<sequence>, of: CORBA/<short>))
  let seq = SequenceTest/attribute-short-seq(SequenceTest);
  for (i from 0 below size(seq))
    unless (seq[i] = inseq[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
  seq;
end method;

define method SequenceTest/out-parameter-short-seq (SequenceTest :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: CORBA/<short>))
  SequenceTest/attribute-short-seq(SequenceTest);
end method;

define method SequenceTest/result-short-seq (SequenceTest :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: CORBA/<short>))
  SequenceTest/attribute-short-seq(SequenceTest);
end method;


define constant *numbers* = #["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"];

define method SequenceTest/check-attribute-struct-seq (SequenceTest :: <SequenceTest-implementation>)
 => ()
  let seq = SequenceTest/attribute-struct-seq(SequenceTest);
  for (i from 0 below size(seq))
    let expected = make(<Structure>, name: *numbers*[i], info: i);
    unless (seq[i] = expected)
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/in-parameter-struct-seq (SequenceTest :: <SequenceTest-implementation>, inseq :: limited(CORBA/<sequence>, of: <Structure>))
 => ()
  let seq = SequenceTest/attribute-struct-seq(SequenceTest);
  for (i from 0 below size(seq))
    unless (seq[i] = inseq[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/inout-parameter-struct-seq (SequenceTest :: <SequenceTest-implementation>, inseq :: limited(CORBA/<sequence>, of: <Structure>))
 => (seq :: limited(CORBA/<sequence>, of: <Structure>))
  let seq = SequenceTest/attribute-struct-seq(SequenceTest);
  for (i from 0 below size(seq))
    unless (seq[i] = inseq[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
  seq;
end method;

define method SequenceTest/out-parameter-struct-seq (SequenceTest :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: <Structure>))
  SequenceTest/attribute-struct-seq(SequenceTest);
end method;

define method SequenceTest/result-struct-seq (SequenceTest :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: <Structure>))
  SequenceTest/attribute-struct-seq(SequenceTest);
end method;


define class <TestObject-implementation> (<TestObject-servant>)
  slot TestObject/id :: CORBA/<long> = 0, init-keyword: id:;
  slot TestObject/ior :: CORBA/<string> = "";
end class;

define method make-TestObject (id :: CORBA/<long>)
 => (object :: <TestObject-implementation>, reference :: <TestObject>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let TestObject = make(<TestObject-implementation>, id: id);
  let objectid = portableserver/poa/activate-object(root-poa, TestObject);
  let TestObjectRef = portableserver/poa/servant-to-reference(root-poa, TestObject);
  TestObject.TestObject/ior := corba/orb/object-to-string(orb, TestObjectRef);
  values(TestObject, as(<TestObject>, TestObjectRef));
end method;

define method ior-string (object :: <TestObject>)
 => (ior :: CORBA/<string>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  corba/orb/object-to-string(orb, object);
end method;

define method ior-string (object :: <TestObject-implementation>)
 => (ior :: CORBA/<string>)
  TestObject/ior(object);
end method;

define sideways method \= (object-1 :: <TestObject>, object-2 :: <TestObject>)
 => (equal? :: <boolean>)
  ior-string(object-1) = ior-string(object-2);
end method;

define method SequenceTest/TestObject-factory (SequenceTest :: <SequenceTest-implementation>, id :: CORBA/<long>)
 => (TestObject :: <TestObject>)
  let (object, reference) = make-TestObject(id);
  add!(SequenceTest-implementation-objects(SequenceTest), object);
  reference;
end method;

define method SequenceTest/check-attribute-object-seq (SequenceTest :: <SequenceTest-implementation>)
 => ()
  let seq = SequenceTest/attribute-object-seq(SequenceTest);
  for (i from 0 below size(seq))
    let expected = SequenceTest-implementation-objects(SequenceTest)[i];
    unless (seq[i] = expected)
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/in-parameter-object-seq (SequenceTest :: <SequenceTest-implementation>, inseq :: limited(CORBA/<sequence>, of: <TestObject>))
  => ()
  let seq = SequenceTest/attribute-object-seq(SequenceTest);
  for (i from 0 below size(seq))
    unless (seq[i] = inseq[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/inout-parameter-object-seq (SequenceTest :: <SequenceTest-implementation>, inseq :: limited(CORBA/<sequence>, of: <TestObject>))
 => (seq :: limited(CORBA/<sequence>, of: <TestObject>))
  let seq = SequenceTest/attribute-object-seq(SequenceTest);
  for (i from 0 below size(seq))
    unless (seq[i] = inseq[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
  seq;
end method;

define method SequenceTest/out-parameter-object-seq (SequenceTest :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: <TestObject>))
  SequenceTest/attribute-object-seq(SequenceTest);
end method;

define method SequenceTest/result-object-seq (SequenceTest :: <SequenceTest-implementation>)
 => (seq :: limited(CORBA/<sequence>, of: <TestObject>))
  SequenceTest/attribute-object-seq(SequenceTest);
end method;


define constant *short-name* = #['B', 'a', 't', 'm', 'a', 'n', ' ', 'a', 'n', 'd', ' ', 'R', 'o', 'b', 'i', 'n'];
define constant *long-name* = #['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

define method SequenceTest/set-short-name (SequenceTest :: <SequenceTest-implementation>, name :: SequenceTest/<BoundedString>)
 => ()
  for (character in name, i from 0 below size(*short-name*))
    unless (character = *short-name*[i])
      error(make(SequenceTest/<failure>, index: i));
    end unless;
  end for;
end method;

define method SequenceTest/set-long-name (SequenceTest :: <SequenceTest-implementation>, name :: SequenceTest/<BoundedString>)
 => ()
end method;

define method SequenceTest/get-short-name (SequenceTest :: <SequenceTest-implementation>)
 => (name :: SequenceTest/<BoundedString>)
  let name = make(SequenceTest/<BoundedString>);
  do(curry(add!, name), *short-name*);
  name;
end method;

define method SequenceTest/get-long-name (SequenceTest :: <SequenceTest-implementation>)
 => (name :: SequenceTest/<BoundedString>)
  let name = make(SequenceTest/<BoundedString>);
  do(curry(add!, name), *long-name*);
  name;
end method;

define method SequenceTest/get-name (SequenceTest :: <SequenceTest-implementation>)
 => (size :: CORBA/<short>, name :: SequenceTest/<BoundedString>)
  let name = make(SequenceTest/<BoundedString>);
  do(curry(add!, name), *short-name*);
  values(size(name), name);
end method;

define method SequenceTest/reverse-name (SequenceTest :: <SequenceTest-implementation>,  name :: SequenceTest/<BoundedString>)
 => (name :: SequenceTest/<BoundedString>)
  reverse(name);
end method;


define constant *SequenceTest-ior-file* = "c:\\temp\\sequencetest.ior";

define method start-SequenceTest-server () => ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a SequenceTest object
  let SequenceTest = make(<SequenceTest-implementation>);

  // put it in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, SequenceTest);

  // create an ior string to pass to clients (via special file)
  let SequenceTestRef = portableserver/poa/servant-to-reference(root-poa, SequenceTest);
  corba/orb/object-to-file(orb, *SequenceTest-ior-file*, SequenceTestRef);

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);
end method;

register-server(start-sequencetest-server);
