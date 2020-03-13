Module: gabriel-benchmarks
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
//  This file contains a recursive as well as an iterative test.
define method create-n (n :: <integer>)
  for (n :: <integer> from n to 0 by -1, a = #() then pair(#f, a))
  finally
    a
  end for
end method create-n;

define variable div2-l = create-n(200);

define method iterative-div2 (l)
  for (l = l then tail(tail(l)),
       a = #() then pair(head(l), a),
       until: empty?(l))
  finally
    a
  end for
end method iterative-div2;

define method recursive-div2 (l)
  if (empty?(l))
    #()
  else
    pair(head(l), recursive-div2(tail(tail(l))))
  end if;
end method recursive-div2;

define method div2-test-1 (l)
  for (i from 300 to 0 by -1)
    iterative-div2(l);
    iterative-div2(l);
    iterative-div2(l);
    iterative-div2(l);
  end for;
end method div2-test-1;

define method div2-test-2 (l)
  for (i :: <integer> from 300 to 0 by -1)
    recursive-div2(l);
    recursive-div2(l);
    recursive-div2(l);
    recursive-div2(l);
  end for;
end method div2-test-2;

define benchmark div2-test-1-benchmark ()
  benchmark-repeat (iterations: 200)
    div2-test-1(div2-l);
  end;
end benchmark;

define benchmark div2-test-2-benchmark ()
  benchmark-repeat (iterations: 200)
    div2-test-2(div2-l);
  end;
end benchmark;
