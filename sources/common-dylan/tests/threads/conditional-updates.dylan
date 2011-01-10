Module:       common-dylan-test-suite
Synopsis:     Test conditional-update!
Author:       Tony Mann, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Testing conditional-update!

define locked variable *locked-counter* = 0;

define method debug-warn-on-fail ()
  let count = *locked-counter*;
  format-l("Information: a conditional-update missed\n");
  #f;
end method;

define method debug-locked-counter ()
  let count = *locked-counter*;
  format-l("%d - new value from %s\n", 
           count, current-thread().thread-name);
  count;
end method;

define method increment-value 
   (x :: <integer>) => (res :: <integer>)
  thread-yield(); // make this more interesting
  x + 1;
end method;

define method decrement-value 
   (x :: <integer>) => (res :: <integer>)
  thread-yield(); // make this more interesting
  x - 1;
end method;


define method cond-inc-count ()
  while (conditional-update!(old = *locked-counter*)
           old.increment-value;
           success #f
           failure debug-warn-on-fail(); #t
         end conditional-update!)
  end while;
  debug-locked-counter();
end method;

define method cond-dec-count ()
  while (conditional-update!(old = *locked-counter*)
           old.decrement-value;
           success #f
           failure debug-warn-on-fail(); #t
         end conditional-update!)
  end while;
  debug-locked-counter();
end method;


/* These versions spuriously compile incorrectly


define method cond-inc-count ()
  while (conditional-update!(old = *locked-counter*)
           thread-yield(); // make this more interesting
           old + 1;
           success #f
           failure debug-warn-on-fail(); #t
         end conditional-update!)
  end while;
  debug-locked-counter();
end method;

define method cond-dec-count ()
  while (conditional-update!(old = *locked-counter*)
           thread-yield(); // make this more interesting
           old - 1;
           success #f
           failure debug-warn-on-fail(); #t
         end conditional-update!)
  end while;
  debug-locked-counter();
end method;

*/

define test conditional-updates
      ()

  let decrementers = 2;
  let incrementers = 2;
  let tries = 10;
  let vec = make(<vector>, size: (decrementers + incrementers));
  *locked-counter* := 0;

  // first make a whole load of decrement threads
  for (i from 0 below decrementers)
    let name = concatenate("Cond-decrementer ", integer-to-string(i));
    vec[i] := make(<thread>, name: name, function:
         method ()
           for (j from 1 to tries)
             sleep(2.0);
             cond-dec-count();
           end for;
           format-l("%s finished\n", name);
         end);
  end for;

  for (i from 0 below incrementers)
    let name = concatenate("Cond-incrementer ", integer-to-string(i));
    vec[i + 2] := make(<thread>, name: name, function:
         method ()
           for (j from 1 to tries)
             sleep(1.0);
             cond-inc-count();
           end for;
           format-l("%s finished\n", name);
         end);
  end for;

  for (i from 0 below (decrementers + incrementers))
    join-thread(vec[i]);
  end for;

  check-true("*locked-counter* is zero", *locked-counter* == 0);
end test;


define suite conditional-updates-suite ()
  test conditional-updates;
end suite;
