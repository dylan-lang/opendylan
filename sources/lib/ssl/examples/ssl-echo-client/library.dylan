Module:       dylan-user
Author:       Toby Weinberg, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library ssl-echo-client
  use common-dylan;
  use io;
  use network;
  use ssl-network;
  export ssl-echo-client;
end library;

define module ssl-echo-client
  use dylan;
  use streams;
  use standard-io;
  use format-out;
  use sockets;
end module;
