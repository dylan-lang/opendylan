Module:       dylan-user
Author:       Toby Weinberg, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library echo-server
  use common-dylan;
  use io;
  use network;
  export echo-server;
end library;

define module echo-server
  use dylan;
  use streams;
  use simple-format;
  use threads;
  use sockets;
end module;
