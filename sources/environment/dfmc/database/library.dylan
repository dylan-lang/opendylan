Module:    Dylan-User
Synopsis:  DFM compiler database
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-environment-database
  use environment-protocols;
  use io, import: { print };
  use dfmc-browser-support;
  use user-projects;
  use dfmc-conditions;	//---*** for the warning classes

  export dfmc-environment-database;
end library dfmc-environment-database;
