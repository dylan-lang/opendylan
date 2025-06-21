Module:    environment-test-suite
Synopsis:  Environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment suite

define suite environment-suite ()
  suite environment-protocols-suite;
  //---*** andrewa: this needs a DUIM backend to work
  // suite environment-framework-suite;
  //---*** andrewa: the editor tests aren't automatic
  // suite suite-test-frontend; // editor-common testing
end suite environment-suite;
