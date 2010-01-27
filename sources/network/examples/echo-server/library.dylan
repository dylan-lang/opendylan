Module:       dylan-user
Author:       Toby Weinberg, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
  use simple-io;
  use threads;
  use sockets;
end module;
