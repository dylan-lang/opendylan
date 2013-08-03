Module:       Dylan-User
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-deuce
  use common-dylan;
  use collections;

  use io;

  use duim;
  use deuce;

  export duim-deuce,
         duim-deuce-internals;
end library duim-deuce;
