Module:   corba-hello-world-server
Synopsis: Distributed Hello World
Author:   Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $hello-world-ior-file = "c:\\temp\\hello.ior";

define class <world-implementation> (<world-servant>)
end class;

define method world/hello (world :: <world-implementation>)
    => (hello :: <string>)
  "Hello World!"
end method;

define method main () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let impl = make(<world-implementation>);
  let world = portableserver/poa/servant-to-reference(poa, impl);
  corba/orb/object-to-file(orb, $hello-world-ior-file, world);
  let manager = portableserver/poa/the-poamanager(poa);
  portableserver/poamanager/activate(manager);
  corba/orb/run(orb);
end method main;

begin
  main();
end;
