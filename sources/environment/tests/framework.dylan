Module:    environment-test-suite
Synopsis:  Environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// History tests

/*---*** This needs a DUIM backend to work...
define class <test-history-frame> (<frame-history-mixin>, <simple-frame>)
end class <test-history-frame>;

define test environment-history-test ()
  let frame = make(<test-history-frame>);
  check-true("History frame has no initial history",
	     empty?(frame-history(frame)));
  check-true("History frame has back and forward disabled",
	     ~command-enabled?(frame-select-previous-object, frame)
	       & ~command-enabled?(frame-select-next-object, frame));
  frame-add-to-history(frame, 1);
  check-true("History frame accepts new object",
	     begin
	       let history = frame-history(frame);
	       size(history) == 1 & history[0] = 1
	     end);
  frame-add-to-history(frame, 2);
  check-true("History frame accepts a second object",
	     begin
	       let history = frame-history(frame);
	       size(history) == 2 & history[0] = 2
	     end);
  frame-add-to-history(frame, 3);
  frame-select-previous-object(frame);
  check-true("History frame can move to previous object",
	     begin
	       let history = frame-history(frame);
	       size(history) == 2 & history[0] = 2
	     end);
  check-true("History frame has back and forward enabled",
	     command-enabled?(frame-select-previous-object, frame)
	       & command-enabled?(frame-select-next-object, frame));
  frame-remove-from-history(frame, 1);
  check-true("History frame can remove first object",
	     begin
	       let history = frame-history(frame);
	       size(history) == 1 & history[0] = 2
	     end);
end test environment-history-test;


/// Environment framework suite

define suite environment-framework-suite ()
  test environment-history-test;
end suite environment-framework-suite;
*/
