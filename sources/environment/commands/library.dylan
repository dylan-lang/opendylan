Module:    Dylan-User
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-commands
  use dylan;
  use common-dylan;
  use system;
  use io;
  use commands;

  use build-system;
  use environment-protocols;
  use environment-manager;
  use environment-reports;
  use release-info;
  use source-control-manager;

  use dfmc-environment-projects;

  export command-lines,
         environment-commands;
end library environment-commands;
