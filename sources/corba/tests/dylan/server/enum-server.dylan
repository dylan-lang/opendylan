Module:    enum-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <enumtest-implementation> (<enumtest-servant>)
  slot next-in-parameter :: <planet> = #"Mercury";
  slot next-result :: <planet> = #"Mercury";
end class;

define method enumtest/reset-in-parameter (enumtest :: <enumtest-implementation>)
 => ()
  enumtest.next-in-parameter := #"Mercury";
end method;

define method enumtest/in-parameter (enumtest :: <enumtest-implementation>, symbol :: <planet>)
 => (ok? :: CORBA/<boolean>)
  let ok? = (symbol = enumtest.next-in-parameter);
  enumtest.next-in-parameter := planet/successor(enumtest.next-in-parameter);
  ok?;
end method;

define method enumtest/reset-result (enumtest :: <enumtest-implementation>)
 => ()
  enumtest.next-result := #"Mercury";
end method;

define method enumtest/result (enumtest :: <enumtest-implementation>)
 => (symbol :: <planet>)
  let symbol = enumtest.next-result;
  enumtest.next-result := planet/successor(symbol);
  symbol;
end method;

define constant *enumtest-ior-file* = "c:\\temp\\enumtest.ior";

define method start-enumtest-server () => ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make an enumtest object
  let enumtest = make(<enumtest-implementation>);

  // put it in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, enumtest);

  // create an ior string to pass to clients (via special file)
  let enumtestref = portableserver/poa/servant-to-reference(root-poa, enumtest);
  corba/orb/object-to-file(orb, *enumtest-ior-file*, enumtestref);

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);

end method;

register-server(start-enumtest-server);
