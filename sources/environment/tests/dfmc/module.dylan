Module:    Dylan-User
Synopsis:  DFMC Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-test-suite
  use functional-dylan;
  use simple-format;
  use locators;

  use source-records;
  use operating-system;

  use testworks;

  use environment-protocols,
    exclude: { application-filename,
	       application-arguments };
  use environment-commands;
  use environment-test-suite;

  export dfmc-environment-suite;
end module dfmc-environment-test-suite;
