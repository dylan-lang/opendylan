Module:    Dylan-User
Synopsis:  Environment-Deuce Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library editor-deuce-backend
  use functional-dylan;
  use commands;
  use system;

  use duim;
  use deuce;
  use duim-deuce;

  use editor-manager;
  use environment-protocols;
  use environment-manager;
  use environment-tools;
  use environment-deuce;

  export editor-deuce-backend;
end library editor-deuce-backend;
