Module:       dylan-user
Synopsis:     IO library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library io-test-suite
  use common-dylan,
    import: { byte-vector, common-dylan, machine-words, simple-random, threads };
  use common-dylan-test-utilities;  // For stream testing protocol
  use io;
  use strings;
  use system;
  use testworks;

  export io-test-suite;
end library;

define module io-test-suite
  use byte-vector,
    import: { copy-bytes };
  use common-dylan;
  use simple-random;
  use threads;
  use date;
  use operating-system;
  use file-system;
  use locators;
  use machine-words;
  use strings;

  use streams;
  use streams-internals;
  use print;
  use print-internals;
  use format;
  use standard-io;

  use testworks;

  use common-dylan-test-utilities,
    exclude: { make-test-instance };

  // IO test suite
  export io-test-suite;
end module;
