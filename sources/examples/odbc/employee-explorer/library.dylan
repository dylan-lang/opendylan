Module:   dylan-user
Synopsis: An employee tree explorer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library employee-explorer
  use functional-dylan;
  use io;
  use collections;
  use duim;
  use win32-duim;
  use sql-odbc;

  export employee-explorer;
end library employee-explorer;
