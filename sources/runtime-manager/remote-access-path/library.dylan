module:       dylan-user
synopsis:     Debugger remote access path library
author:       Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library remote-access-path
  use dylan;
  use common-dylan;
  use big-integers;
  use collections;
  use io;
  use system;
  use c-ffi;
  use access-path;
  use dylan-orb;
  use remote-nub-client;
end library;
