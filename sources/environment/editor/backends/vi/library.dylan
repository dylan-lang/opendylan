Module:    Dylan-User
Synopsis:  Environment-Vi Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library editor-vi-backend
  use functional-dylan;
  use commands;

  use editor-manager;
  use editor-exe-backend;

  export editor-vi-backend;
end library editor-vi-backend;
