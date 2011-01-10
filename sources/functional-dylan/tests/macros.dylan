Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macro testing

define functional-extensions macro-test assert-test ()
  check-condition("Assert signals error on #f", <error>, assert(#f, "Failed"));
  check-false("Assert doesn't signal error on #t", assert(#t, "Failed"));
  check-false("Assert doesn't signal error on 10", assert(10, "Failed"));
end macro-test assert-test;

define functional-extensions macro-test debug-assert-test ()
  check-condition("Assert signals error on #f", <error>, debug-assert(#f, "Failed"));
  check-false("Assert doesn't signal error on #t", debug-assert(#t, "Failed"));
  check-false("Assert doesn't signal error on 10", debug-assert(10, "Failed"));
end macro-test debug-assert-test;

define functional-extensions macro-test iterate-test ()
  check-equal("iterate macro computes factorial 5",
	      iterate recurse (x = 5)
		if (x < 2) x else x * recurse(x - 1) end
	      end,
              120)
end macro-test iterate-test;

define table $test-table :: <object-table>
  = { 0 => #"zero",
      1 => #"one",
      2 => #"two" };

define functional-extensions macro-test table-definer-test ()
  check-true("define table produces correct table",
	     size($test-table) = 3
	       & $test-table[0] == #"zero"
	       & $test-table[1] == #"one"
	       & $test-table[2] == #"two")
end macro-test table-definer-test;

define functional-extensions macro-test timing-test ()
  check-true("timing macro returns two integer values",
	     begin
	       let (seconds, microseconds)
		 = timing ()
		     for (i from 0 to 200) end
	           end;
	       instance?(seconds, <integer>)
	         & instance?(microseconds, <integer>)
	     end)
end macro-test timing-test;

define functional-extensions macro-test when-test ()
  check-equal("when (#t) 10 end returns 10",
              when (#t) 10 end, 10);
  check-false("when (#f) 10 end returns #f",
              when (#f) 10 end);
end macro-test when-test;

