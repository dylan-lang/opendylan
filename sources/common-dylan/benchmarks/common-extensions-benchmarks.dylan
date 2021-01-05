Module:       common-dylan-benchmarks
Synopsis:     Benchmarks for common-extensions:common-dylan
License:      See License.txt in this distribution for details.


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
