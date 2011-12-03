Module:    Dylan-User
Synopsis:  The application commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-application-commands
  use common-dylan;
  use system;
  use io;
  use commands;

  use environment-protocols;
  use environment-manager;
  use environment-reports;
  use environment-commands;

  use dfmc-environment;
  use dfmc-environment-projects;
  use dfmc-environment-application;  // For application callbacks

  export environment-application-commands;
end library environment-application-commands;
