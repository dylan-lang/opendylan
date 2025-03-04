Module:    Dylan-User
Synopsis:  DFMC Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-test-suite
  use common-dylan;
  use simple-format, exclude: { format-to-string };
  use streams;
  use format;
  use standard-io;
  use progress-stream;
  use locators;
  use file-system;
  use threads;

  use source-records;
  use operating-system,
    exclude: { run-application };

  use testworks;

  use channels;
  use regular-expressions,
    import: { compile-regex, regex-position };
  use dylan-orb;

  use access-path, import: { debugger-message };

  use environment-protocols,
    exclude: { application-filename,
               application-arguments };
  use dfmc-application;
  use environment-test-suite;

  export dfmc-environment-suite;
end module dfmc-environment-test-suite;
