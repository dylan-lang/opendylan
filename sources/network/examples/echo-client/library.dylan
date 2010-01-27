Module:       dylan-user
Author:       Toby Weinberg, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library echo-client
  use common-dylan;
  use io;
  use network;
  export echo-client;
end library;

define module echo-client
  use dylan;
  use streams;
  use standard-io;
  use simple-io;
  use sockets;
end module;
