Module:       Dylan-User
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deuce
  use functional-dylan;
  use collections;
  use io;
  use system;

  export deuce-commands,
	 deuce,
	 deuce-internals;
end library deuce;
