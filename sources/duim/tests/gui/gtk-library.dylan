Module:    Dylan-User
Author:    Andy Armstrong, Shri Amit
Synopsis:  An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-duim-gui-test-suite
  use functional-dylan;
  use duim-core;
  use duim-extended-geometry;
  use duim-recording;
  use gtk-duim;

  export duim-gui-test-suite
end library gtk-duim-gui-test-suite;
