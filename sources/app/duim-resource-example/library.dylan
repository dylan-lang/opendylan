Module:    dylan-user
Synopsis:  Example using windows resources in DUIM
Author:    Roman Budzianowski, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-resource-example
  use common-dylan;
  use io;
  use system;
  use win32-user;		//---*** Just for debugging
  use win32-resource-database;	//---*** Just for debugging

  use duim;
end library duim-examples;

define module duim-resource-example
  use common-dylan;
  use format;
  use format-out;
  use operating-system;
  use win32-user;		//---*** Just for debugging
  use resource-database;	//---*** Just for debugging

  use duim-internals;
end module duim-examples;
