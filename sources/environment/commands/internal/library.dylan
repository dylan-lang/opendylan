Module:    Dylan-User
Synopsis:  The internal-only commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-internal-commands
  use functional-dylan;
  use system;
  use io;

  use commands;
  use environment-protocols;
  use environment-commands;

  use projects;
  use registry-projects;
  use user-projects;

  use dood;
  use build-system;
  use dfmc-browser-support;

  export environment-internal-commands;
end library environment-internal-commands;
