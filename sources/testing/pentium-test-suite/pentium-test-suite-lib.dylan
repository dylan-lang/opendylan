Module:    dylan-user
Author:    Shri Amit
Synopsis:  A wrapper suite for test-suites that are win32 pentium back end specific
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library pentium-test-suite
  use common-dylan;
  use testworks;

  // Include all the pc specific test-suites
  use win32-test-suite;

  // Include all the pc HARP back end specific test-suites
  use threads-test-suite;

  export pentium-test-suite
end;

define module pentium-test-suite
  use common-dylan;
  use testworks;
  use win32-test-suite;
  use threads-test-suite;

  export pentium-test-suite
end;
