Module:       Dylan-User
Author:       Andy Armstrong, Keith Playford
Synopsis:     A simple database viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module database-viewer
  use common-dylan;
  use threads;
  use simple-format;
  use sql-odbc,
    exclude: { command-function };
  use duim;

  export <database-viewer>;
end module database-viewer;
