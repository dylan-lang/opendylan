Module:   dylan-user
Synopsis: A simple sql query viewer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library select-viewer
  use common-dylan;
  use io;
  use collections;
  use sql-odbc;
  use duim;

  export select-viewer;
end library select-viewer;
