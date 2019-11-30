Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test test-bug-3276 ()
  check-equal("as(<locator>, \"\\\") produces a directory, not a UNC locator",
              as(<string>, as(<microsoft-directory-locator>, "\\")),
              "\\")
end test;

define test test-url-parsing-bug ()
  let url = "http://localhost:7020/foo";
  check-equal(format-to-string("as(<url>, %s)", url),
              as(<url>, url),
              make(<file-url>,
                   name: "foo",
                   directory: make(<directory-url>,
                                   path: #[],
                                   server: make(<http-server>,
                                                host: "localhost",
                                                port: 7020))))
end test;

define suite system-regressions-test-suite ()
  test test-bug-3276;
  test test-url-parsing-bug;
end suite;
