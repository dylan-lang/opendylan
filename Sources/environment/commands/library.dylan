Module:    Dylan-User
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-commands
  use functional-dylan;
  use system;
  use io;
  use commands;

  use environment-protocols;
  use environment-manager;
  use environment-reports;
  use source-control-manager;

  use dfmc-environment-projects;

  export command-lines,
         environment-commands;
end library environment-commands;
