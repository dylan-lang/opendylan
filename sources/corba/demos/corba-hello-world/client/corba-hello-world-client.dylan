Module:   corba-hello-world-client
Synopsis: Distributed Hello World
Author:   Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $hello-world-ior-file = "c:\\temp\\hello.ior";

define method main () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let world = as(<world>, corba/orb/file-to-object(orb, $hello-world-ior-file));
  format-out("%s\n", world/hello(world));
end method main;

begin
  main();
end;
