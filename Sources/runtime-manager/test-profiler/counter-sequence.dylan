module:    test-profiler
synopsis:  Implement <counter-sequence> type and associated methods
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <counter-sequence> = <simple-vector>;


// Sort a counter sequence so that the highest counts come first. Counters
// with equal counts are sorted alphabetically by name.
//
define method sort-counters (counter-sequence :: <counter-sequence>)
                         => (sorted-counter-sequence :: <counter-sequence>)

  local method lt (counter-1 :: <counter>, counter-2 :: <counter>)
    if (counter-1.value = counter-2.value)
      (counter-1.name < counter-2.name)
    else
      (counter-2.value < counter-1.value)
    end if
  end method;

  sort (counter-sequence, test: lt)
end method;


// List the counter names and values from a sequence on standard output.
// Also, calculate and display the percentage contribution each counter
// makes to the total count for the sequence.
//
define method print-counters (counter-sequence :: <counter-sequence>) => ()
  let total = 0;
  for (counter in counter-sequence)
    total := total + value (counter);
  end for;
  for (counter in counter-sequence)
    let (quotient, remainder) = truncate/ (value (counter) * 100, total);
    let percentage = quotient;
    format-out (format-to-string ("%10d (%2d) : %s\n",
                                  value (counter),
                                  percentage,
                                  name (counter)));
  end for;
  format-out (format-to-string ("\n%10d       : Total\n\n", total));
end method;
