Module:       Dylan-User
Synopsis:     Standalone wrapper for DUIM-Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library standalone-deuce
  use common-dylan;
  use collections;
  use io;
  use system;

  //---*** For the mail sending hack
  // use network;
  // use smtp-client;

  use duim;
  use win32-duim;
  use deuce;
  use duim-deuce;

  export standalone-deuce,
         standalone-deuce-internals;
end library standalone-deuce;
