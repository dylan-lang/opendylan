Module:    struct-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *struct-a* :: <StructureA>
 = make(<StructureA>, // a-short: -32768, a-short: 32768,
                      a-long: -40000, a-ulong: 40000,
                      a-octet: 128);
define variable *struct-b* :: <StructureB>
 = make(<StructureB>, b-boolean: #f);
define variable *struct-c* :: <StructureC>
 = make(<StructureC>, c-string: "Tinky Winky", c-struct: *struct-b*, c-char: 'A');
define variable *struct-d* :: <StructureD>
 = make(<StructureD>, d-float: 0.0, d-double: 1.0d0);

define class <structtest-implementation> (<structtest-servant>)
  slot structtest/struct-a :: <StructureA> = *struct-a*;
  slot structtest/struct-b :: <StructureB> = *struct-b*;
  slot structtest/struct-c :: <StructureC> = *struct-c*;
  slot structtest/struct-d :: <StructureD> = *struct-d*;
end class;

/*
define method structtest/get-a-short (structtest :: <structtest-implementation>)
 => (result :: CORBA/<short>)
  StructureA/a-short(structtest/struct-a(structtest));
end method;

define method structtest/get-a-ushort (structtest :: <structtest-implementation>)
 => (result :: CORBA/<unsigned-short>)
  StructureA/a-ushort(structtest/struct-a(structtest));
end method;
*/

define method structtest/get-a-long (structtest :: <structtest-implementation>)
 => (result :: CORBA/<long>)
  StructureA/a-long(structtest/struct-a(structtest));
end method;

define method structtest/get-a-ulong (structtest :: <structtest-implementation>)
 => (result :: CORBA/<unsigned-long>)
  StructureA/a-ulong(structtest/struct-a(structtest));
end method;

define method structtest/get-a-octet (structtest :: <structtest-implementation>)
 => (result :: CORBA/<octet>)
  StructureA/a-octet(structtest/struct-a(structtest));
end method;

define method structtest/get-b-boolean (structtest :: <structtest-implementation>)
 => (result :: CORBA/<boolean>)
  StructureB/b-boolean(structtest/struct-b(structtest));
end method;

define method structtest/get-c-string (structtest :: <structtest-implementation>)
 => (result :: CORBA/<string>)
  StructureC/c-string(structtest/struct-c(structtest));
end method;

define method structtest/get-c-struct (structtest :: <structtest-implementation>)
 => (result :: <StructureB>)
  StructureC/c-struct(structtest/struct-c(structtest));
end method;

define method structtest/get-c-char (structtest :: <structtest-implementation>)
 => (result :: CORBA/<char>)
  StructureC/c-char(structtest/struct-c(structtest));
end method;

define method structtest/get-d-float (structtest :: <structtest-implementation>)
 => (result :: CORBA/<float>)
  StructureD/d-float(structtest/struct-d(structtest));
end method;

define method structtest/get-d-double (structtest :: <structtest-implementation>)
 => (result :: CORBA/<double>)
  StructureD/d-double(structtest/struct-d(structtest));
end method;

define method structtest/in-parameter-a (structtest :: <structtest-implementation>, struct :: <StructureA>)
 => (result :: CORBA/<boolean>)
  (struct = structtest/struct-a(structtest));
//  (StructureA/a-long(structtest/struct-a(structtest)) = StructureA/a-long(struct))
//  & (StructureA/a-ulong(structtest/struct-a(structtest)) = StructureA/a-ulong(struct))
//  & (StructureA/a-octet(structtest/struct-a(structtest)) = StructureA/a-octet(struct));
end method;

define method structtest/in-parameter-b (structtest :: <structtest-implementation>, struct :: <StructureB>)
 => (result :: CORBA/<boolean>)
  (struct = structtest/struct-b(structtest));
//  (StructureB/b-boolean(structtest/struct-b(structtest)) = StructureB/a-boolean(struct));
end method;

define method structtest/in-parameter-c (structtest :: <structtest-implementation>, struct :: <StructureC>)
 => (result :: CORBA/<boolean>)
  (struct = structtest/struct-c(structtest));
end method;

define method structtest/in-parameter-d (structtest :: <structtest-implementation>, struct :: <StructureD>)
 => (result :: CORBA/<boolean>)
  (struct = structtest/struct-d(structtest));
end method;

define method structtest/result-a (structtest :: <structtest-implementation>)
 => (result :: <StructureA>)
  structtest/struct-a(structtest);
end method;

define method structtest/result-b (structtest :: <structtest-implementation>)
 => (result :: <StructureB>)
  structtest/struct-b(structtest);
end method;

define method structtest/result-c (structtest :: <structtest-implementation>)
 => (result :: <StructureC>)
  structtest/struct-c(structtest);
end method;

define method structtest/result-d (structtest :: <structtest-implementation>)
 => (result :: <StructureD>)
  structtest/struct-d(structtest);
end method;

define constant *structtest-ior-file* = "c:\\temp\\structtest.ior";

define method start-structtest-server () => ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a structtest object
  let structtest = make(<structtest-implementation>);

  // put it in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, structtest);

  // create an ior string to pass to clients (via special file)
  let structtestref = portableserver/poa/servant-to-reference(root-poa, structtest);
  corba/orb/object-to-file(orb, *structtest-ior-file*, structtestref);

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);

end method;

register-server(start-structtest-server);
