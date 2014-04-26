Module:   dylan-user
Synopsis: An employee tree explorer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module employee-explorer
  use streams;
  use standard-io;
  use print;
  use format-out;
  use format;
  use duim;
  use win32-duim, import: { <win32-icon> };
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use common-dylan;
  use simple-random;
  use sql-odbc, exclude: { command-function };
end module employee-explorer;
