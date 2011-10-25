Module:    Dylan-User
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-deuce
  use functional-dylan;
  use system;

  use duim-core;

  use deuce;
  use duim-deuce;

  use environment-protocols;
  use environment-framework;
  use environment-manager;
  use editor-manager;
  use source-control-manager;
  use environment-tools;

  use commands;
  use environment-commands;

//  use dfmc-shell;

  export environment-deuce;
end library environment-deuce;
