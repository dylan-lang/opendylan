Module:   gabriel-benchmarks
Synopsis: Gabriel benchmarks in Dylan
Author:   Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  DERIV -- This is the Common Lisp version of a symbolic derivative benchmark
//  written by Vaughn Pratt.  It uses a simple subset of Lisp and does a lot of
//  CONSing.
define method deriv-aux (a)
  list(#"/", deriv(a), a);
end method deriv-aux;

define method deriv (a)
  if (~instance?(a, <list>))
    if (a == #"x")
      1
    else
      0
    end if
  elseif (head(a) == #"+")
    pair(#"+", map(deriv, tail(a)))
  elseif (head(a) == #"-")
    pair(#"-", map(deriv, tail(a)))
  elseif (head(a) == #"*")
    list(#"*", a, pair(#"+", map(deriv-aux, tail(a))))
  elseif (head(a) == #"/")
    list(#"-", list(#"/", deriv(second(a)), third(a)),
         list(#"/", second(a),
              list(#"*", third(a), third(a), deriv(third(a)))))
  else
    #"error"
  end if
end method deriv;

define method deriv-run ()
  for (i from 0 below 1000)
    // runs it 5000 times
    deriv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"),
            #(#"*", #"b", #"x"), 5));
    deriv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"),
            #(#"*", #"b", #"x"), 5));
    deriv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"),
            #(#"*", #"b", #"x"), 5));
    deriv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"),
            #(#"*", #"b", #"x"), 5));
    deriv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"),
            #(#"*", #"b", #"x"), 5));
  end for;
end method deriv-run;

define benchmark deriv-benchmark ()
  benchmark-repeat (iterations: 60)
    deriv-run()
  end;
end benchmark;
