Module: common-dylan-test-suite
Synopsis: Tests for simple-profiling:common-dylan


define test test-start-profiling ()
  //---*** Fill this in...
end test;

define test test-start-profiling-type ()
  //---*** Fill this in...
end test;

define test test-stop-profiling ()
  //---*** Fill this in...
end test;

define test test-stop-profiling-type ()
  //---*** Fill this in...
end test;

define test test-profiling-type-result ()
  //---*** Fill this in...
end test;

define test test-<profiling-state> ()
  //---*** Fill this in...
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

define suite simple-profiling-test-suite ()
  test test-start-profiling;
  test test-start-profiling-type;
  test test-stop-profiling;
  test test-stop-profiling-type;
  test test-profiling-type-result;
  test test-profiling;
  test test-<profiling-state>;
  test test-timing;
end suite;
