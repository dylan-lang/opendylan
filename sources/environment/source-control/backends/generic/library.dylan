Module:    Dylan-User
Synopsis:  Environment-Generic Source Control System Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library source-control-generic-backend
  use common-dylan;
  use system;
  use commands;

  use duim;

  use source-control-manager;

  export source-control-generic-backend;
end library source-control-generic-backend;
