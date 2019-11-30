Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Settings tests

define test test-<settings> ()
  //---*** Fill this in...
end test;

define test test-<system-settings> ()
  //---*** Fill this in...
end test;

define test test-<site-settings> ()
  //---*** Fill this in...
end test;

define test test-<site-software-settings> ()
  //---*** Fill this in...
end test;

define test test-<local-settings> ()
  //---*** Fill this in...
end test;

define test test-<local-software-settings> ()
  //---*** Fill this in...
end test;

define test test-<local-hardware-settings> ()
  //---*** Fill this in...
end test;

define test test-<default-user-settings> ()
  //---*** Fill this in...
end test;

define test test-<default-user-software-settings> ()
  //---*** Fill this in...
end test;

define test test-<current-user-settings> ()
  //---*** Fill this in...
end test;

define test test-<current-user-software-settings> ()
  //---*** Fill this in...
end test;

define suite settings-test-suite ()
  test test-<settings>;
  test test-<system-settings>;
  test test-<site-settings>;
  test test-<site-software-settings>;
  test test-<local-settings>;
  test test-<local-software-settings>;
  test test-<local-hardware-settings>;
  test test-<default-user-settings>;
  test test-<default-user-software-settings>;
  test test-<current-user-settings>;
  test test-<current-user-software-settings>;
end;
