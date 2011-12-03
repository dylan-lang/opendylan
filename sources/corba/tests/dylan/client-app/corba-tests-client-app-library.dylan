Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-tests-client-app
  use dylan;
  use io;
  use corba-tests-client;
  use corba-tests-server;    // For server co-location testing
  use testworks;
end library; 

define module corba-tests-client-app
  use dylan;
  use format-out;
  use corba-tests-client;
  use testworks;
end module;
