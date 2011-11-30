Module: dylan-user
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sql-odbc-test
  use functional-dylan;
  use memory-manager;
  use sql-odbc; 

  use io;
  use c-ffi;
  use system;
  use generic-arithmetic;
  use big-integers;

  use testworks;

  export sql-odbc-test;
end library;

define module sql-odbc-test-include
create
 *datasource-name*,
 *user-name*,
 *user-password*,
 *the-dbms*,
 *detect-null-column*,
 *do-introspection*,
 *dbms-class*,
 *dbms-class-name*,
 *dbms-user-class*,
 *dbms-database-class*,
 *dbms-sql-statement-class*;
end module;

  

define module sql-odbc-test-bignums
  use generic-arithmetic-common-dylan;
  use memory-manager;
  use threads;
  use sql-odbc;
  use finalization;

  use format-out;
  use format;
  use c-ffi;
  use date;
  use operating-system;

  use testworks;
  use sql-odbc-test-include;

  export big-integer-test-suite;
end module;

define module sql-odbc-test
  use functional-dylan;
  use memory-manager;
  use threads;
  use sql-odbc;
  use finalization;

  use format-out;
  use format;
  use c-ffi;
  use date;
  use operating-system;

  use testworks;
  use sql-odbc-test-include;
  use sql-odbc-test-bignums;

end module;

