Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  An application library for test-suite common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library common-dylan-test-suite-app
  use dylan-test-suite;
  use common-dylan-test-suite;
  use testworks;
end library common-dylan-test-suite-app;

define module common-dylan-test-suite-app
  use dylan-test-suite;
  use common-dylan-test-suite;
  use testworks;
end module common-dylan-test-suite-app;
