Module:       dylan-user
Synopsis:     Sockets Tests Server
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sockets-tests-server
  use functional-dylan;
  use network;
  use io;
  use system;

  // Add any more module exports here.
  export sockets-tests-server;
end library sockets-tests-server;
