Module:    Dylan-User
Synopsis:  Environment-Exe Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library editor-exe-backend
  use functional-dylan;
  use system;
  use commands;

  use editor-manager;

  export editor-exe-backend;
end library editor-exe-backend;
