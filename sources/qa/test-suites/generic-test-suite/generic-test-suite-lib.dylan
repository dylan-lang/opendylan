Module:    dylan-user
Author:    Shri Amit
Synopsis:  A wrapper suite around all platform independent test-suites
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library generic-test-suite
  use common-dylan;
  use testworks;
  use libraries-test-suite;
  use deuce-test-suite;
  use environment-test-suite;
  use duim-test-suite;

  export generic-test-suite
end;

define module generic-test-suite
  use common-dylan;
  use testworks;
  use libraries-test-suite;
  use deuce-test-suite;
  use environment-test-suite;
  use duim-test-suite;

  export generic-test-suite
end;
