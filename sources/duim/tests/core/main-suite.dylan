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
  // suite postscript-duim-suite;	---*** remove PostScript for Kansas
end suite duim-suite;


/// run-all-tests is a simple entry point

define method run-all-tests 
    (#key debug? = *debug?*,
          progress-function = null-progress-function)
 => (result :: <result>)
  perform-suite(duim-suite, 
                debug?: debug?,
                progress-function: progress-function)
end method run-all-tests;

//--- We can't do this, because the combined test suite applications
//--- also run the test suite, so this gets run twice!
// run-all-tests();
