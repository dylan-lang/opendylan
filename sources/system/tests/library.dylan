Module:       dylan-user
Synopsis:     System library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library system-test-suite
  use common-dylan;
  use io;
  use system;

  use common-dylan-test-suite;	// For generic stream testing

  use testworks;
  use testworks-specs;

  export system-test-suite;
end library system-test-suite;

define module system-test-suite
  use common-dylan;
  use simple-random;
  use threads;
  use streams;
  use streams-internals;

  use date;
  use operating-system;
  use file-system;
  use locators;
  use settings;
  use settings-internals;
  use simple-xml;

  use common-dylan-test-suite;	// For generic stream testing

  use testworks;
  use testworks-specs;

  export system-test-suite;
end module system-test-suite;
