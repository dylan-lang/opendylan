Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: TAKL benchmark, converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TAKL -- The TAKeuchi function using lists as counters.

define function listn (n :: <integer>)
  if (n == 0)
    #()
  else
    pair(n, listn(n - 1))
  end if
end function listn;

define variable *L18* = listn(18);
define variable *L12* = listn(12);
define variable *L6* = listn(6);

define function mas (x, y, z)
  if (~shorter?(y, x))
    z
  else
    mas(mas(tail(x), y, z),
	mas(tail(y), z, x),
	mas(tail(z), x, y))
  end if
end function mas;

define function shorter? (x, y)
  y ~== #() & (x == #() | shorter?(tail(x), tail(y)))
end function shorter?;

define function testtakl ()
  mas(*l18*, *l12*, *l6*)
end function testtakl;

define benchmark takl = testtakl;

