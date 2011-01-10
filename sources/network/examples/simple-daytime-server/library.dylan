Module:       dylan-user
Author:       Toby Weinberg, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library daytime-server
  use functional-dylan;
  use io;
  use system;
  use network;
  export daytime-server;
end library;

define module daytime-server
  use functional-dylan;
  use streams;
  use standard-io;
  use format;
  use format-out;
  use date;
  use sockets;
end module;
