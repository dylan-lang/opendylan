Module:    dfmc-environment-test-suite
Synopsis:  DFMC Environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DFMC environment suite

define suite dfmc-environment-browsing-suite
    (setup-function:   open-test-projects,
     cleanup-function: close-test-projects)
  suite environment-suite;
  suite projects-suite;
  suite names-suite;
end suite dfmc-environment-browsing-suite;

define suite dfmc-environment-suite ()
  suite dfmc-environment-browsing-suite;
  suite dfmc-environment-debugging-suite;
end suite;
