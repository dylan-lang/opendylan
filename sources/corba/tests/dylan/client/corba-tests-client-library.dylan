Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-tests-client
  use dylan;
  use generic-arithmetic;
  use big-integers;
  use dylan-orb;
  use io;
  use testworks;
  use corba-tests-utilities;
  use corba-tests-protocol;
  use corba-tests-stubs;
  use corba-tests-skeletons;
  export
    corba-tests-client;
end library corba-tests-client;
