Module:        dylan-user
Synopsis:      The CORBA Debugger Nub of the Remote Debugger
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module remote-nub
  use dylan-extensions,
     import: {<double-integer>};
  use threads;
  use byte-vector;
  use finalization;
  use common-dylan;
  use machine-words;
  use simple-random;
  use transcendentals;
  use operating-system;
  use file-system;
  use format;
  use format-out;
  use print;
  use standard-io;
  use streams;
  use c-ffi;
  use dylan-orb;
  use remote-nub-skeletons;
  use remote-nub-stubs;


end module remote-nub;
