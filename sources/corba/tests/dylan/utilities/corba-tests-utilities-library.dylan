Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-tests-utilities
  use functional-dylan;
  export corba-tests-utilities;
end library;

define module corba-tests-utilities
  use functional-dylan;
  export
    run-servers,
    register-server,
    unregister-server;
end module;
