Module:    Dylan-User
Synopsis:  Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-test-suite
  use environment-protocols;
  use release-info;
  //---*** andrewa: this needs a DUIM backend to work
  // use environment-framework;
  //---*** andrewa: the editor tests aren't automatic
  // use test-editor-manager-common;
  use duim-core;

  use testworks;

  export environment-test-suite;
end library environment-test-suite;
