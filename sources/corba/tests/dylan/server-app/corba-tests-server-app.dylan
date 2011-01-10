Module: corba-tests-server-app
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main ()
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // Change orb-tests-server's port so that it doesn't clash with orb-tests-app's.
  let port = orb-service-port(orb);
  when (port) // when fixed
    orb-service-port(orb) := port + 1;
  end when;

  run-servers();

  corba/orb/run(orb);
end method;

main();

