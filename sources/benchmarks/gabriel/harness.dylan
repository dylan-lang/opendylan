Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: A very simple harness for the gabriel benchmarks
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Example:
//   define benchmark ctak = ctak-top-level-function;
// defines a new benchmark.

define variable *all-benchmarks* = #();

define macro benchmark-definer
  { define benchmark ?bname:name = ?top-level-fun:expression }
  =>
  { begin
      *all-benchmarks* := remove!(*all-benchmarks*, ?"bname",
                                  test: method(elem, value)
                                          first(elem) = value
                                        end);
      // efficiency not necessary here.  reverse twice to add to end.
      *all-benchmarks* := reverse(add!(reverse(*all-benchmarks*),
				       list(?"bname", ?top-level-fun)));
    end; }
end macro benchmark-definer;

define function run-all-benchmarks () => ()
  for (bench in *all-benchmarks*)
    run-benchmark(first(bench), second(bench));
  end for;
end function run-all-benchmarks;

// Convert seconds and microseconds to a string "s.uuu".
define function format-time
    (seconds :: <integer>, microseconds :: <integer>)
 => (time :: <string>)
  let (ms, rem) = round/(microseconds, 1000);
  if (ms >= 1000)  // > 999500 ms
    seconds := seconds + 1;
    ms := 0;
  end if;
  format-to-string("%d.%s", seconds, integer-to-string(ms, size: 3, fill: '0'));
end function format-time;

define function run-benchmark (name, function) => ()
  block ()
    format-out("%s...", name);
    let (secs, usecs) = timing ()
                          function();
                        end;
    format-out("took %s seconds.  (%d seconds, %d microseconds)\n",
                                   format-time(secs, usecs), secs, usecs);
//  exception (e :: <error>)
//    format-out("%s got an error: %s\n", name, e);
  end block;
end function run-benchmark;


