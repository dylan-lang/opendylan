Module: Dylan-User
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sql-example
  use functional-dylan;
  use sql-odbc;
  use io;

  export sql-example;
end library;

define module sql-example
  use functional-dylan;
  use sql-odbc;
  use format-out;
end module;
