Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  An application library for the deuce-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deuce-test-suite-app
  use deuce-test-suite;
  use testworks;
end library deuce-test-suite-app;

define module deuce-test-suite-app
  use deuce-test-suite;
  use testworks;
end deuce-test-suite-app;
