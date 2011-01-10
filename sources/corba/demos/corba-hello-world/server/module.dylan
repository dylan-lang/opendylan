Module:   dylan-user
Synopsis: Distributed Hello World
Author:   Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module corba-hello-world-server
  use byte-vector;
  use finalization;
  use functional-dylan;
  use machine-words;
  use simple-random;
  use transcendentals;
  use format;
  use format-out;
  use print;
  use standard-io;
  use streams;
  use dylan-orb;
  use hello-skeletons;

  // Add binding exports here.

end module corba-hello-world-server;
