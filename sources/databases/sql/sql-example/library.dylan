Module: Dylan-User
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sql-example
  use dylan;
  use common-dylan;
  use sql-odbc;
  use io;

  export sql-example;
end library;

define module sql-example
  use dylan;
  use common-dylan;
  use sql-odbc;
  use format-out;
end module;
