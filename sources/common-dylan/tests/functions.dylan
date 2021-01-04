Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// TODO(cgay): move to common-dylan-benchmarks library
define benchmark benchmark-split ()
  local
    // Would be nice to provide this separator function in common-dylan, or to
    // provide an easy way to make a separator function from
    // strings/whitespace? or from a set of elements or...
    method find-whitespace
        (big :: <string>, bpos :: <integer>, epos :: <integer>)
     => (bpos :: false-or(<integer>), _end :: false-or(<integer>))
      iterate loop (pos = bpos, start = #f)
        if (member?(big[pos], " \t"))
          loop(pos + 1, pos)
        elseif (start)
          values(start, pos)
        end                 // else values(#f, #f)
      end
    end method;
  benchmark-repeat(iterations: 1000)
    split("The quick brown fox jumps over the lazy dog.", find-whitespace);
  end;
end benchmark;
