module:    harp-instructions
Synopsis:  Spread functions for the major instruction types.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Some standard spread functions for Dylan HARP

define spread-function none (backend, op, fn, ins)
  fn(backend, op);
end;

define spread-function du (backend, op, fn, ins)
  fn(backend, op, du-def(1), du-uze(1));
end;

define spread-function duu (backend, op, fn, ins)
  fn(backend, op, duu-def(1), duu-uze(1), duu-uze(2));
end;

define spread-function duuu (backend, op, fn, ins)
  fn(backend, op, duuu-def(1), duuu-uze(1), duuu-uze(2), duuu-uze(3));
end;

define spread-function duuuu (backend, op, fn, ins)
  fn(backend, op, duuuu-def(1), duuuu-uze(1), duuuu-uze(2), duuuu-uze(3), duuuu-uze(4));
end;

define spread-function uuuu (backend, op, fn, ins)
  fn(backend, op, uuuu-uze(1), uuuu-uze(2), uuuu-uze(3), uuuu-uze(4));
end;

define spread-function uuuuu (backend, op, fn, ins)
  fn(backend, op, uuuuu-uze(1), uuuuu-uze(2), uuuuu-uze(3), uuuuu-uze(4), uuuuu-uze(5));
end;

define spread-function uuuuuu (backend, op, fn, ins)
  fn(backend, op, uuuuuu-uze(1), uuuuuu-uze(2), uuuuuu-uze(3), uuuuuu-uze(4), uuuuuu-uze(5), uuuuuu-uze(6));
end;

define spread-function ddu (backend, op, fn, ins)
  fn(backend, op, ddu-def(1), ddu-def(2), ddu-uze(1));
end;

define spread-function dduu (backend, op, fn, ins)
  fn(backend, op, dduu-def(1), dduu-def(2), dduu-uze(1), dduu-uze(2));
end;

define spread-function dduuu (backend, op, fn, ins)
  fn(backend, op, dduuu-def(1), dduuu-def(2), dduuu-uze(1), dduuu-uze(2), dduuu-uze(3));
end;

define spread-function tu (backend, op, fn, ins, #key sv-ins: sv) 
  fn(backend, op, ins-tag(sv, ins), tu-uze(1));
end;

define spread-function tuu (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), tuu-uze(1), tuu-uze(2));
end;

define spread-function td (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), td-def(1));
end;

define spread-function tdu (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), tdu-def(1), tdu-uze(1));
end;

define spread-function tduu (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), tduu-def(1), tduu-uze(1), tduu-uze(2));
end;

define spread-function tddu (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), tddu-def(1), tddu-def(2), tddu-uze(1));
end;

define spread-function tdduu (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), tdduu-def(1), tdduu-def(2), tdduu-uze(1), tdduu-uze(2));
end;

define spread-function u (backend, op, fn, ins)
  fn(backend, op, u-uze(1));
end;

define spread-function uu (backend, op, fn, ins)
  fn(backend, op, uu-uze(1), uu-uze(2));
end;

define spread-function uuu (backend, op, fn, ins)
  fn(backend, op, uuu-uze(1), uuu-uze(2), uuu-uze(3));
end;

define spread-function d (backend, op, fn, ins)
  fn(backend, op, d-def(1));
end;

define spread-function tuuu (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins), tuuu-uze(1), tuuu-uze(2), tuuu-uze(3));
end;

define spread-function t (backend, op, fn, ins, #key sv-ins: sv)
  fn(backend, op, ins-tag(sv, ins));
end;

define ddu spread-function t-pop (backend, op, fn, ins)
  fn(backend, op, ddu-def(2), ddu-uze(1));
end;

//// Any other useful functions for ops ...


//// MJS 19Nov93: the overflow functions must clash the destination with the
//// sources since they assume that the sources are valid in the overflow case
//// Note that some backends define their own functions which call this one.

define duu register-function default-overflow-function-clashes (backend, ins)
  list(list(duu-def(1), duu-uze(1)),
       list(duu-def(1), duu-uze(2)))
end;


define dduu register-function default-double-overflow-function-clashes (backend, ins)
  list(list(dduu-def(1), dduu-uze(1)),
       list(dduu-def(1), dduu-uze(2)),
       list(dduu-def(2), dduu-uze(1)),
       list(dduu-def(2), dduu-uze(2)))
end;

