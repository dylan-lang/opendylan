Module:    gctest
Synopsis:  A multi-threaded test of the GC
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *table* = make(<table>, weak: #"value");

define constant $vector-size = 1000;

define constant *lock* = make(<lock>);


define method report-progress (x :: <integer>, thread :: <string>) => ()
  let (div, rem) = truncate/(x, 1000);
  if (rem = 0)
    with-lock (*lock*)
      format-out("Thread %s at %d iterations\n", thread, x);
    end with-lock;
  end if;
end method;


define method make-big-obj () => (vec :: <vector>)
  // make a 1Kbyte vector
  let vec = make(<vector>, size: 250);
  vec[0] := #"magic";
  vec;
end method;

define method test-big-obj (obj :: <vector>) => ()
  unless (obj[0] == #"magic")
    error("illegal object found");
  end;
end method;


define method bash-one
    (vec :: <vector>, veci :: <integer>, tabi :: <integer>) => (nexti :: <integer>)
  let obj = make-big-obj();
  let nexti = if (veci >= ($vector-size - 1)) 0 else veci + 1 end if;
  *table*[tabi] := obj;
  vec[nexti] := obj;
  nexti;
end method;

define method bash-it 
    (start :: <integer>, num :: <integer>, thread :: <string>) => ()
  let vec = make(<vector>, size: $vector-size);
  let i :: <integer> = start;
  for (count from 0 below num)
    i := bash-one(vec, i, count + start);
    report-progress(count, thread);
  end for;
end method;

define method test-it (start :: <integer>, num :: <integer>, thread :: <string>) => ()
  // Provoke some table accesses
  for (i from 0 below num)
    let entry = element(*table*, i + start, default: #f);
    if (entry)
      test-big-obj(entry);
    end if;
    report-progress(i, thread);
  end for;
end method;

define method main () => ()
  // want each thread to allocate about 50Mbytes
  let obj-count = 50000;
  let this-obj = make-big-obj();
  let thr1 = make(<thread>,
                  function: method () bash-it(0, obj-count, "Th1") end);
  let thr2 = make(<thread>,
                  function: method () bash-it(10000, obj-count, "Th2") end);
  sleep(1);
  test-it(0, obj-count, "Mas");
  join-thread(thr1);
  join-thread(thr2);
  test-big-obj(this-obj);
  let tabsize = *table*.size;
  format-out("There are %= entries remaining in the table\n", tabsize);
  if (tabsize >= obj-count)
    format-out("Looks like weak tables work poorly\n");
  else
    format-out("Looks like everything works OK\n");
  end if;
end method main;

begin
  main();
end;
