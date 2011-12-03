Module:   dylan-user
Synopsis: Distributed Hello World
Author:   Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-hello-world-client
  use common-dylan;
  use io;
  use dylan-orb;
  use hello-stubs;

  // Add any more module exports here.
  export corba-hello-world-client;
end library corba-hello-world-client;
