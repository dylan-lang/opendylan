Module:    Dylan-User
Synopsis:  Environment-Source Control Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library source-control-manager
  use functional-dylan;
  use system;
  use commands;
  use release-info;

  export source-control-manager,
	 source-control-manager-internals;
end library source-control-manager;
