Module:       dylan-user
Synopsis:     Sockets Tests Client
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sockets-tests-client
  use functional-dylan;
  use testworks;
  use network;
  use system;
  use io;

  // Add any more module exports here.
  export sockets-tests-client;
end library sockets-tests-client;
