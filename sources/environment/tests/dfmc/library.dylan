Module:    Dylan-User
Synopsis:  DFMC Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-environment-test-suite
  use functional-dylan;
  use system;

  use source-records;

  use testworks;

  use environment-protocols;
  use environment-commands;
  use environment-application-commands;
  use environment-test-suite;

  export dfmc-environment-test-suite;
end library dfmc-environment-test-suite;
