Module:    dylan-user
Synopsis:  Locators library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library locators-test-suite
  use functional-dylan;
  use testworks;
  use testworks-specs;

  use locators;

  export locators-test-suite;
end library locators-test-suite;

define module locators-test-suite
  use functional-dylan;
  use simple-format;
  use testworks;
  use testworks-specs;

  use locators;
  //---*** Hopefully we can get rid of these
  use locator-internals;

  export locators-test-suite;
end module locators-test-suite;
