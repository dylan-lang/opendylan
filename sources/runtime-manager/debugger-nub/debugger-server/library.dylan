Module:    dylan-user
Synopsis:  Distributed Debugger Server
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library debugger-server
  use functional-dylan;
  use system;
  use io;
  use c-ffi;

  use duim;
  use win32-duim;

  use dylan-orb;
  use remote-nub-skeletons;

end library debugger-server;
