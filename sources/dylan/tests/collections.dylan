Module:       dylan-test-suite
Synopsis:     Collection tests
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test test-<collection> ()
    test-collection-class(<collection>);
end;

define test test-<explicit-key-collection> ()
    test-collection-class(<explicit-key-collection>);
end;

define test test-<sequence> ()
    test-collection-class(<sequence>);
end;

define test test-<mutable-collection> ()
    test-collection-class(<mutable-collection>);
end;

define test test-<mutable-explicit-key-collection> ()
    test-collection-class(<mutable-explicit-key-collection>);
end;

define test test-<mutable-sequence> ()
    test-collection-class(<mutable-sequence>);
end;

define test test-<stretchy-collection> ()
    test-collection-class(<stretchy-collection>);
end;

define test test-<array>
    (expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295")
  test-collection-class(<array>, instantiable?: #t);
end;

define test test-<vector> ()
    test-collection-class(<vector>, instantiable?: #t);
end;

define test test-<simple-vector> ()
    test-collection-class(<simple-vector>, instantiable?: #t);
end;

define test test-<simple-object-vector>
    (expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295")
  test-collection-class(<simple-object-vector>, instantiable?: #t);
end;

define test test-<stretchy-vector>
    (expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295")
  test-collection-class(<stretchy-vector>, instantiable?: #t);
end;

define test test-<deque> ()
    test-collection-class(<deque>, instantiable?: #t);
end;

define test test-<list> ()
    test-collection-class(<list>, instantiable?: #t);
end;

define test test-<pair> ()
    test-collection-class(<pair>, instantiable?: #t);
end;

define test test-<empty-list> ()
    test-collection-class(<empty-list>, instantiable?: #t);
end;

define test test-<range> ()
    test-collection-class(<range>, instantiable?: #t);
end;

define test test-<string> ()
    test-collection-class(<string>, instantiable?: #t);
end;

define test test-<byte-string> ()
    test-collection-class(<byte-string>, instantiable?: #t);
end;

define test test-<table> ()
    test-collection-class(<table>, instantiable?: #t);
end;

define test test-<object-table> ()
    test-collection-class(<object-table>, instantiable?: #t);
end;

define suite dylan-collections-test-suite ()
  test test-<collection>;
  test test-<explicit-key-collection>;
  test test-<sequence>;
  test test-<mutable-collection>;
  test test-<mutable-explicit-key-collection>;
  test test-<mutable-sequence>;
  test test-<stretchy-collection>;
  test test-<array>;
  test test-<vector>;
  test test-<simple-vector>;
  test test-<simple-object-vector>;
  test test-<stretchy-vector>;
  test test-<deque>;
  test test-<list>;
  test test-<pair>;
  test test-<empty-list>;
  test test-<range>;
  test test-<string>;
  test test-<byte-string>;
  test test-<table>;
  test test-<object-table>;
end suite dylan-collections-test-suite;
