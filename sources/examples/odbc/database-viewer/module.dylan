Module:       Dylan-User
Author:       Andy Armstrong, Keith Playford
Synopsis:     A simple database viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module database-viewer
  use common-dylan;
  use threads;
  use simple-io;
  use sql-odbc,
    exclude: { command-function };
  use duim;

  export <database-viewer>;
end module database-viewer;
