Module:       dylan-user
Synopsis:     System library test suite
Author:              Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library system-test-suite
  use common-dylan;
  use io;
  use system;

  use common-dylan-test-suite;        // For generic stream testing

  use testworks;
  use testworks-specs;

  export system-test-suite;
end library system-test-suite;

define module system-test-suite
  use common-dylan;
  use simple-random;
  use threads;
  use format;
  use streams;
  use streams-internals;

  use date;
  use operating-system;
  use file-system;
  use locators;
  use settings;
  use settings-internals;
  use simple-xml;

  // TODO(cgay): get rid of this because it causes all the common-dylan tests
  // to be registered, and also slows compilation. Whatever's being used from
  // this needs to be factored out into a library that can be shared and has no
  // tests in it.
  use common-dylan-test-suite,
    import: { make-stream-tests-of-size,
              register-stream-class-info };

  use testworks;
  use testworks-specs;

  export system-test-suite;
end module system-test-suite;
