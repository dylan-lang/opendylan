Module:       collections-test-suite
Author:       Keith Dennison
Synopsis:     Tests for counting bits in bit-vectors
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test bit-count-empty-vector ()
  let empty-vector = make(<bit-vector>, size: 0);
  check-equal("bit-count(empty-vector) = 0", bit-count(empty-vector), 0);
  check-equal("bit-count(empty-vector, bit-value: 0) = 0",
    bit-count(empty-vector, bit-value: 0), 0);
  check-equal("bit-count(empty-vector, bit-value: 1) = 0",
    bit-count(empty-vector, bit-value: 1), 0);
end test;


define test bit-count-tiny-vector ()
  let vector = make(<bit-vector>, size: $tiny-size);
  let bits = list(1, 4, 6, 7, 9, 11, 13);
  set-bits(vector, bits);
  check-equal("Count bits in tiny vector", bit-count(vector), size(bits));
  check-equal("Count zero bits in tiny vector",
    bit-count(vector, bit-value: 0), $tiny-size - size(bits));
  check-equal("Count one bits in tiny vector",
    bit-count(vector, bit-value: 1), size(bits));
end test;


define test bit-count-huge-vector ()
  let vector = make(<bit-vector>, size: $huge-size);
  let bits = list(2, 3, 4, 8, 10, 21, 22, 29, 33, 38, 74, 94, 100, 109, 111);
  set-bits(vector, bits);
  check-equal("Count bits in huge vector", bit-count(vector), size(bits));
  check-equal("Count zero bits in huge vector",
    bit-count(vector, bit-value: 0), $huge-size - size(bits));
  check-equal("Count one bits in huge vector",
    bit-count(vector, bit-value: 1), size(bits));
end test;

define test bit-count-multiple-word-sized-vector ()
  let vector = make(<bit-vector>, size: $multiple-word-size);
  let bits = list(0, 1, 3, 5, 6, 7, 20, 29, 33, 38, 74, 75, 94);
  set-bits(vector, bits);
  check-equal("Count bits in multiple-word sized vector",
    bit-count(vector), size(bits));
  check-equal("Count zero bits in multiple-word sized vector",
    bit-count(vector, bit-value: 0), $multiple-word-size - size(bits));
  check-equal("Count one bits in multiple-word sized vector",
    bit-count(vector, bit-value: 1), size(bits));
end test;

define suite bit-count-suite (description: "Test bit-count")
  test bit-count-empty-vector;
  test bit-count-tiny-vector;
  test bit-count-huge-vector;
  test bit-count-multiple-word-sized-vector;
end suite;
