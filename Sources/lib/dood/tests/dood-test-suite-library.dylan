module:    dylan-user
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dood-test-suite
  use functional-dylan;
  use io;
  use dood;
  use testworks;
  use random;
  export dood-test-suite;
end library;

define module dood-test-suite
  use functional-dylan;
  use streams;
  use standard-io;
  use format;
  use format-out;
  use dood;
  use testworks;
  use random;
  export dood-test-suite;
end module;

// eof
