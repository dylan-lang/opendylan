Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class == <machine-word>) => (instance :: <machine-word>);
  make(<machine-word>, value: 1729)
end method make-test-instance;

define test test-<machine-word> ()
  //---*** Fill this in...
end test;


/// Variables

define test test-$machine-word-size ()
  //---** What can we do here?
end test;

define test test-$maximum-signed-machine-word ()
  //---** What can we do here?
end test;

define test test-$minimum-signed-machine-word ()
  //---** What can we do here?
end test;

define test test-$maximum-unsigned-machine-word ()
  //---** What can we do here?
end test;

define test test-$minimum-unsigned-machine-word ()
  //---** What can we do here?
end test;

define test test-as-unsigned ()
  //---** What can we do here?
end test;


/// Basic and signed single word operations

define test test-%logior ()
  //---*** Fill this in...
end test;

define test test-%logxor ()
  //---*** Fill this in...
end test;

define test test-%logand ()
  //---*** Fill this in...
end test;

define test test-%lognot ()
  //---*** Fill this in...
end test;

define test test-%logbit? ()
  //---*** Fill this in...
end test;

define test test-%count-low-zeros ()
  check-equal("%count-low-zeros of <integer> 0",
              $machine-word-size, %count-low-zeros(0));
  check-equal("%count-low-zeros of <machine-word> 0",
              $machine-word-size, %count-low-zeros(as(<machine-word>, 0)));
  check-equal("%count-low-zeros of <integer> -1",
              0, %count-low-zeros(-1));
  check-equal("%count-low-zeros of <machine-word> -1",
              0, as(<machine-word>, %count-low-zeros(-1)));
  check-equal("%count-low-zeros of <integer> 4096",
              12, %count-low-zeros(4096));
  check-equal("%count-low-zeros of <machine-word> 4096",
              12, %count-low-zeros(as(<machine-word>, 4096)));
end test;

define test test-%count-high-zeros ()
  check-equal("%count-high-zeros of <integer> 0",
              $machine-word-size, %count-high-zeros(0));
  check-equal("%count-high-zeros of <machine-word> 0",
              $machine-word-size, %count-high-zeros(as(<machine-word>, 0)));
  check-equal("%count-high-zeros of <integer> -1",
              0, %count-high-zeros(-1));
  check-equal("%count-high-zeros of <machine-word> -1",
              0, as(<machine-word>, %count-high-zeros(-1)));
  check-equal("%count-high-zeros of <integer> 4096",
              $machine-word-size - 13,
              %count-high-zeros(4096));
  check-equal("%count-high-zeros of <machine-word> 4096",
              $machine-word-size - 13,
              %count-high-zeros(as(<machine-word>, 4096)));
end test;

define test test-%count-ones ()
  check-equal("%count-ones of <integer> 0",
              0, %count-ones(0));
  check-equal("%count-ones of <machine-word> 0",
              0, %count-ones(as(<machine-word>, 0)));
  check-equal("%count-ones of <integer> -1",
              $machine-word-size, %count-ones(-1));
  check-equal("%count-ones of <machine-word> -1",
              $machine-word-size, %count-ones(as(<machine-word>, -1)));
  check-equal("%count-ones of <integer> 4096",
              1, %count-ones(4096));
  check-equal("%count-ones of <machine-word> 4096",
              1, %count-ones(as(<machine-word>, 4096)));
end test;

define test test-%+ ()
  //---*** Fill this in...
end test;

define test test-%- ()
  //---*** Fill this in...
end test;

define test test-%* ()
  //---*** Fill this in...
end test;

define test test-%floor/ ()
  //---*** Fill this in...
end test;

define test test-%ceiling/ ()
  //---*** Fill this in...
end test;

define test test-%round/ ()
  //---*** Fill this in...
end test;

define test test-%truncate/ ()
  //---*** Fill this in...
end test;

define test test-%divide ()
  //---*** Fill this in...
end test;

define test test-%negative ()
  //---*** Fill this in...
end test;

define test test-%abs ()
  //---*** Fill this in...
end test;

define test test-%shift-left ()
  //---*** Fill this in...
end test;

define test test-%shift-right ()
  //---*** Fill this in...
end test;


/// Overflow signalling operations

define test test-so%+ ()
  //---*** Fill this in...
end test;

define test test-so%- ()
  //---*** Fill this in...
end test;

define test test-so%* ()
  //---*** Fill this in...
end test;

define test test-so%negative ()
  //---*** Fill this in...
end test;

define test test-so%abs ()
  //---*** Fill this in...
end test;

define test test-so%shift-left ()
  //---*** Fill this in...
end test;


/// Signed double word operations

define test test-d%floor/ ()
  //---*** Fill this in...
end test;

define test test-d%ceiling/ ()
  //---*** Fill this in...
end test;

define test test-d%round/ ()
  //---*** Fill this in...
end test;

define test test-d%truncate/ ()
  //---*** Fill this in...
end test;

define test test-d%divide ()
  //---*** Fill this in...
end test;


/// Unsigned single word operations

define test test-u%+ ()
  //---*** Fill this in...
end test;

define test test-u%- ()
  //---*** Fill this in...
end test;

define test test-u%* ()
  //---*** Fill this in...
end test;

define test test-u%divide ()
  //---*** Fill this in...
end test;

define test test-u%rotate-left ()
  //---*** Fill this in...
end test;

define test test-u%rotate-right ()
  //---*** Fill this in...
end test;

define test test-u%shift-left ()
  //---*** Fill this in...
end test;

define test test-u%shift-right ()
  //---*** Fill this in...
end test;

define test test-u%< ()
  //---*** Fill this in...
end test;


/// Unsigned double word operations

define test test-ud%divide ()
  //---*** Fill this in...
end test;

define test test-ud%shift-left ()
  //---*** Fill this in...
end test;

define test test-ud%shift-right ()
  //---*** Fill this in...
end test;

define suite machine-words-test-suite ()
  test test-<machine-word>;
  test test-$machine-word-size;
  test test-$maximum-signed-machine-word;
  test test-$minimum-signed-machine-word;
  test test-$maximum-unsigned-machine-word;
  test test-$minimum-unsigned-machine-word;
  test test-as-unsigned;
  test test-%logior;
  test test-%logxor;
  test test-%logand;
  test test-%lognot;
  test test-%logbit?;
  test test-%count-low-zeros;
  test test-%count-high-zeros;
  test test-%count-ones;
  test test-%+;
  test test-%-;
  test test-%*;
  test test-%floor/;
  test test-%ceiling/;
  test test-%round/;
  test test-%truncate/;
  test test-%divide;
  test test-%negative;
  test test-%abs;
  test test-%shift-left;
  test test-%shift-right;
  test test-so%+;
  test test-so%-;
  test test-so%*;
  test test-so%negative;
  test test-so%abs;
  test test-so%shift-left;
  test test-d%floor/;
  test test-d%ceiling/;
  test test-d%round/;
  test test-d%truncate/;
  test test-d%divide;
  test test-u%+;
  test test-u%-;
  test test-u%*;
  test test-u%divide;
  test test-u%rotate-left;
  test test-u%rotate-right;
  test test-u%shift-left;
  test test-u%shift-right;
  test test-u%<;
  test test-ud%divide;
  test test-ud%shift-left;
  test test-ud%shift-right;
end suite;
