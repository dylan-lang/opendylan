Module: test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite com-suite ()
  test com-istream-test;
end suite com-suite;

define method run-suite ()
  OLE-initialize(); // Do we need this?
  run-test-application(com-suite);
end run-suite;

run-suite();

