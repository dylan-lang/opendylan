Module:       dylan-user
Summary:      A test suite to test the testworks harness
Author:       Andy Armstrong, James Kirsch, Shri Amit
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library testworks-test-suite
  use dylan;
  use testworks;

  export testworks-test-suite;
end library testworks-test-suite;

define module testworks-test-suite
  use dylan;
  use testworks;

  export testworks-test-suite,
         \with-debugging;
end module testworks-test-suite;
