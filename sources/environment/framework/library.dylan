Module:    Dylan-User
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-framework
  use functional-dylan;
  use system;

  use duim-core;

  use environment-protocols;
  use environment-manager;

  export environment-framework;
end library environment-framework;
