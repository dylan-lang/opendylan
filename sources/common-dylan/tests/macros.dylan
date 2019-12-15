Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macro testing

define test test-assert ()
  check-condition("Assert signals error on #f", <error>, assert(#f, "Failed"));
  check-false("Assert doesn't signal error on #t", assert(#t, "Failed"));
  check-false("Assert doesn't signal error on 10", assert(10, "Failed"));
end test;

define test test-debug-assert ()
  check-condition("Assert signals error on #f", <error>, debug-assert(#f, "Failed"));
  check-false("Assert doesn't signal error on #t", debug-assert(#t, "Failed"));
  check-false("Assert doesn't signal error on 10", debug-assert(10, "Failed"));
end test;

define test test-iterate ()
  check-equal("iterate macro computes factorial 5",
              iterate recurse (x = 5)
                if (x < 2) x else x * recurse(x - 1) end
              end,
              120)
end test;

define table $test-table :: <object-table>
  = { 0 => #"zero",
      1 => #"one",
      2 => #"two" };

define test test-define-table ()
  check-true("define table produces correct table",
             size($test-table) = 3
               & $test-table[0] == #"zero"
               & $test-table[1] == #"one"
               & $test-table[2] == #"two")
end test;

define test test-timing ()
  check-true("timing macro returns two integer values",
             begin
               let (seconds, microseconds)
                 = timing ()
                     for (i from 0 to 200) end
                   end;
               instance?(seconds, <integer>)
                 & instance?(microseconds, <integer>)
             end)
end test;

define test test-profiling ()
  check-true("profiling macro returns two integer values",
             begin
               let true? = #f;
               profiling (cpu-time-seconds, cpu-time-microseconds)
                 for (i from 0 to 200) end
               results
                 true?
                   := instance?(cpu-time-seconds, <integer>)
                        & instance?(cpu-time-microseconds, <integer>)
               end;
               true?
             end)
end test;

define test test-when ()
  check-equal("when (#t) 10 end returns 10",
              when (#t) 10 end, 10);
  check-false("when (#f) 10 end returns #f",
              when (#f) 10 end);
end test;

define suite common-dylan-macros-test-suite ()
  test test-assert;
  test test-debug-assert;
  test test-iterate;
  test test-define-table;
  test test-timing;
  test test-profiling;
  test test-when;
end;
