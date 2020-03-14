Module: gabriel-benchmarks
Synopsis: STAK benchmark, converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  STAK -- The TAKeuchi function with special variables instead of parameter
//  passing.
define variable *stak-x* :: <integer> = 0;
define variable *stak-y* :: <integer> = 0;
define variable *stak-z* :: <integer> = 0;

define method stak-aux ()
  if (~ (*stak-y* < *stak-x*))
    *stak-z*
  else
    let (in-x, in-y, in-z) = values(*stak-x*, *stak-y*, *stak-z*);
    let x = dynamic-bind (*stak-x* = in-x - 1,
                          *stak-y* = in-y,
                          *stak-z* = in-z)
              stak-aux();
            end dynamic-bind;
    let y = dynamic-bind (*stak-x* = in-y - 1,
                          *stak-y* = in-z,
                          *stak-z* = in-x)
              stak-aux();
            end dynamic-bind;
    let z = dynamic-bind (*stak-x* = in-z - 1,
                          *stak-y* = in-x,
                          *stak-z* = in-y)
              stak-aux();
            end dynamic-bind;
    dynamic-bind(*stak-x* = x,
                 *stak-y* = y,
                 *stak-z* = z)
      stak-aux()
    end dynamic-bind
  end if
end method stak-aux;

define function stak (x, y, z)
  dynamic-bind(*stak-x* = x,
               *stak-y* = y,
               *stak-z* = z)
    stak-aux()
  end dynamic-bind
end;

define benchmark stak-benchmark ()
  let result
    = benchmark-repeat (iterations: 200)
        stak(18, 12, 6)
      end;
  assert-equal(7, result);
end benchmark;
