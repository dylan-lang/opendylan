Module:       dylan-user
Synopsis:     System library test suite
Author:              Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library system-test-suite
  use common-dylan;
  // TODO(cgay): https://github.com/dylan-lang/opendylan/issues/1237
  use common-dylan-test-suite;        // For generic stream testing
  use io;
  use system;
  use testworks;

  export system-test-suite;
end library system-test-suite;

define module system-test-suite
  use common-dylan;
  use common-dylan-test-suite,
    import: { make-stream-tests-of-size,
              register-stream-class-info,
              test-stream-class };
  use simple-random;
  use threads;
  use format;
  use streams;
  use streams-internals;
  use testworks;

  // System modules to test
  use date;
  use operating-system;
  use file-system;
  use locators;
  use locators-internals;
  use settings;
  use settings-internals;
  use simple-xml;

  export system-test-suite;
end module system-test-suite;
