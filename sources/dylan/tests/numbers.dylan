Module:       dylan-test-suite
Synopsis:     Numerics testing
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test test-<number> ()
    test-number-class(<number>, abstract?: #t);
end;

define test test-<complex> ()
    test-number-class(<complex>, abstract?: #t);
end;

define test test-<real> ()
    test-number-class(<real>, abstract?: #t);
end;

define test test-<float> ()
    test-number-class(<float>, abstract?: #t);
end;

define test test-<single-float> ()
    test-number-class(<single-float>, abstract?: #f);
end;

define test test-<double-float> ()
    test-number-class(<double-float>, abstract?: #f);
end;

define test test-<extended-float> ()
    // Note that <extended-float> == <double-float> in OD so this repeats the
    // above tests.
    test-number-class(<extended-float>, abstract?: #f);
end;

define test test-<rational> ()
    test-number-class(<rational>, abstract?: #t);
end;

define test test-<integer> ()
    test-number-class(<integer>, abstract?: #f);
end;

define suite dylan-numerics-test-suite ()
  test test-<number>;
  test test-<complex>;
  test test-<real>;
  test test-<float>;
  test test-<single-float>;
  test test-<double-float>;
  test test-<extended-float>;
  test test-<rational>;
  test test-<integer>;
end;
