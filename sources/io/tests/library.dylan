Module:       dylan-user
Synopsis:     IO library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library io-test-suite
  use common-dylan;
  use system;
  use io;

  use testworks;
  use testworks-specs;

  use common-dylan-test-suite;  // For stream testing protocol

  export io-test-suite;
end library io-test-suite;

define module io-test-suite
  use common-dylan;
  use simple-random;
  use threads;
  use date;
  use operating-system;
  use file-system;
  use locators;

  use streams;
  use streams-internals;
  use print;
  use print-internals;
  use format;

  use testworks;
  use testworks-specs;

  use common-dylan-test-suite,  // For stream testing protocol
    exclude: { make-test-instance };

  // IO test suite
  export io-test-suite;
end module io-test-suite;
