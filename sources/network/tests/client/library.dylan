Module:       dylan-user
Synopsis:     Sockets Tests Client
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sockets-tests-client
  use common-dylan;
  use testworks;
  use network;
  use system;
  use io;

  export sockets-tests-client;
end library sockets-tests-client;

define module sockets-tests-client
  use common-dylan;
  use simple-format, import: { format-to-string };
  use testworks;
  use threads;
  use sockets;
  use streams;
  use date;
end module sockets-tests-client;
