Module:    tree-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <treetest-implementation> (<treetest-servant>)
end class;

define method treetest/depth (object :: <treetest-implementation>, tree :: <tree>)
 => (result :: corba/<short>)
  0;
end method;

define method treetest/identity (object :: <treetest-implementation>, tree :: <tree>)
 => (result :: <tree>)
  tree;
end method;

define method treetest/identityB (object :: <treetest-implementation>, tree :: <treeB>)
 => (result :: <treeB>)
  tree;
end method;

define method treetest/identityU (object :: <treetest-implementation>, tree :: <treeU>)
 => (result :: <treeU>)
  tree;
end method;

define method treetest/extract-tree (object :: <treetest-implementation>, wrapper :: <tree-wrapper>)
 => (result :: <tree>)
  wrapper.tree-wrapper/real-tree;
end method;

define constant *treetest-ior-file* = "c:\\temp\\treetest.ior";

define method start-treetest-server () => ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a treetest object
  let treetest = make(<treetest-implementation>);

  // put it in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, treetest);

  // create an ior string to pass to clients (via special file)
  let treetestref = portableserver/poa/servant-to-reference(root-poa, treetest);
  corba/orb/object-to-file(orb, *treetest-ior-file*, treetestref);

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);

end method;

register-server(start-treetest-server);
