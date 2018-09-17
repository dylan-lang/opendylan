Module:    dylan-user
Author:    Shri Amit, Andy Armstrong
Synopsis:  A wrapper suite around the test-suites for various dylan libraries
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library libraries-test-suite
  use common-dylan;
  use testworks;
  use dylan-test-suite;
  use common-dylan-test-suite;
  use collections-test-suite;
  use system-test-suite;
  use io-test-suite;
  use testworks-test-suite;

  export libraries-test-suite
end library libraries-test-suite;

define module libraries-test-suite
  use common-dylan;
  use testworks;
  use dylan-test-suite;
  use common-dylan-test-suite;
  use collections-test-suite;
  use system-test-suite;
  use io-test-suite;
  use testworks-test-suite;

  export libraries-test-suite
end module libraries-test-suite;
