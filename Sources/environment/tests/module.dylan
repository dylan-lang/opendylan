Module:    Dylan-User
Synopsis:  Environment Tests
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-test-suite
  use environment-imports;
  use environment-protocols;
  //---*** andrewa: this needs a DUIM backend to work
  // use environment-framework;
  //---*** The editor manager tests aren't automatic
  // use test-editor-manager-common;
  use duim;

  use testworks;

  export environment-suite;
end module environment-test-suite;
