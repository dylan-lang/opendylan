Module:    dylan-user
Author:    Shri Amit
Synopsis:  A wrapper suite for test-suites that are win32 specific
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-test-suite
  use functional-dylan;
  use testworks;
  use generic-test-suite;
  use c-ffi-test;

  export win32-test-suite
end;

define module win32-test-suite
  use functional-dylan;
  use testworks;
  use generic-test-suite;
  use c-ffi-test;

  export win32-test-suite
end;
