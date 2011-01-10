Module:    dylan-user
Synopsis:  Distributed Debugger Server
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module debugger-server
  use dylan-extensions,
     import: {<double-integer>};
  use threads;
  use byte-vector;
  use finalization;
  use functional-dylan;
  use machine-words;
  use simple-random;
  use transcendentals;
  use operating-system;
  use file-system;
  use date;
  use format;
  use format-out;
  use print;
  use standard-io;
  use streams;
  use c-ffi;
  use duim;
  use win32-duim;
  use dylan-orb;
  use remote-nub-skeletons;


end module debugger-server;
