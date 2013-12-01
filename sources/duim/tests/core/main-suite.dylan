Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Define the main DUIM suite

define suite duim-suite ()
  suite duim-test-suite;
  suite duim-graphics-suite;
  suite duim-geometry-suite;
  suite duim-regions-suite;
  suite duim-transforms-suite;
  suite duim-colors-suite;
  suite duim-layouts-suite;
  suite duim-frames-suite;
  suite duim-gadgets-suite;
  suite duim-menus-suite;
  suite duim-dialogs-suite;
  suite duim-events-suite;
  suite duim-gestures-suite;
  suite duim-commands-suite;
end suite duim-suite;
