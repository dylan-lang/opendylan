Module:       collections-test-suite
Synopsis:     Test bit-vector-not function
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test copy-sequence-tiny-vector ()
  let bits = list(2, 3, 5, 7, 11, 13);
  let vector = make(<bit-vector>, size: $tiny-size);
  set-bits(vector, bits);

  // Copy whole thing
  let new-vector = copy-sequence(vector);
  bit-vector-consistency-checks("Copy whole tiny vector",
    new-vector, $tiny-size, bits);

  // Copy from start to limit
  let new-vector = copy-sequence(vector, end: 8);
  bit-vector-consistency-checks("Copy tiny vector below bit 8",
    new-vector, 8, list(2, 3, 5, 7));

  // Copy from limit to end
  let new-vector = copy-sequence(vector, start: 5);
  bit-vector-consistency-checks("Copy tiny vector from bit 5",
    new-vector, $tiny-size - 5, list(0, 2, 6, 8));

  // Copy with both limits inside
  let new-vector = copy-sequence(vector, start: 4, end: 11);
  bit-vector-consistency-checks("Copy tiny vector between bits 4 and 11",
    new-vector, 7, list(1, 3));
end test;

define test copy-sequence-huge-vector ()
  let bits
    = list(2, 3, 5, 7, 11, 13, 17, 19, 27, 44, 56, 78, 99, 105, 111, 116);
  let vector = make(<bit-vector>, size: $huge-size);
  set-bits(vector, bits);

  let new-vector = copy-sequence(vector);
  bit-vector-consistency-checks("Copy whole huge vector",
    new-vector, $huge-size, bits);

  let new-vector = copy-sequence(vector, end: 100);
  bit-vector-consistency-checks("Copy huge vector below bit 100",
    new-vector, 100, list(2, 3, 5, 7, 11, 13, 17, 19, 27, 44, 56, 78, 99));

  let new-vector = copy-sequence(vector, end: 96);
  bit-vector-consistency-checks("Copy huge vector below bit 96",
    new-vector, 96, list(2, 3, 5, 7, 11, 13, 17, 19, 27, 44, 56, 78));

  let new-vector = copy-sequence(vector, start: 67);
  bit-vector-consistency-checks("Copy huge vector from bit 67",
    new-vector, $huge-size - 67, list(11, 32, 38, 44, 49));

  let new-vector = copy-sequence(vector, start: 32);
  bit-vector-consistency-checks("Copy huge vector from bit 32",
    new-vector, $huge-size - 32, list(12, 24, 46, 67, 73, 79, 84));

  let new-vector = copy-sequence(vector, start: 17, end: 99);
  bit-vector-consistency-checks("Copy huge vector between bits 17 and 99",
    new-vector, 82, list(0, 2, 10, 27, 39, 61));
end test;


define suite copy-sequence-suite()
  test copy-sequence-tiny-vector;
  test copy-sequence-huge-vector;
end suite;
