Module:    inefficient
Synopsis:  An inefficient example program.
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This example is good for showing the use of "timing", but not
// much good for general profiling, as it's too small and runs too fast.

define class <item> (<object>)
  slot value :: <integer>, required-init-keyword: value:;
end class <item>;

// (5) 1300ms !
define sealed domain make(subclass(<item>));
define sealed domain initialize(<item>);


define function loop (n :: <integer>) // (3) 8250ms
//  let accumulator :: <sequence> = #(); // (1) 10000ms
  let accumulator :: <list> = #(); // (2) 9000ms
  for (i :: <integer> from 0 below n) // (4) 8100ms
    accumulator := add!(accumulator, make(<item>, value: i));
  end;
end function loop;

define method time-loop (n :: <integer>) => ()
  let (secs, msecs) = timing () loop (1000) end;
  format-out("Timed loop(%d): %ds, %dms", n, secs, msecs);
end method time-loop;

define method main () => ()
  time-loop(100);
end method main;

begin
  main();
end;
