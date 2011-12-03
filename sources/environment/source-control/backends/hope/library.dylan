Module:    Dylan-User
Synopsis:  Environment-Hope Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library source-control-hope-backend
  use common-dylan;
  use system;
  use commands;
  use io;

  use source-control-manager;

  export source-control-hope-backend;
end library source-control-hope-backend;
