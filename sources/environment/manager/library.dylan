Module:    Dylan-User
Synopsis:  Environment Manager
Author:    Andy Armstrong, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-manager
  use dylan;
  use system;
  use channels;

  use environment-protocols;

  export environment-manager;
  export environment-command-calling;
  // export environment-commands;
  export asynchronous-results;
end library environment-manager;
