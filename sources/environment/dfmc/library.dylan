Module:    Dylan-User
Synopsis:  DFMC environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-environment
  use common-dylan;

  use environment-protocols;
  use dfmc-environment-projects;
  use dfmc-environment-database;
  use dfmc-environment-application;

  export dfmc-environment;
end library dfmc-environment;
