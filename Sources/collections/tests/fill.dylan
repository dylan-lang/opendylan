Module:       collections-test-suite
Synopsis:     Test fill! for bit-vectors
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define test fill-empty-vector ()
  let vector = make(<bit-vector>, size: 0);
  fill!(vector, start: 0, end: 1);
end test;
*/

define test fill-tiny-vector ()
  let vector :: <bit-vector> = make(<bit-vector>, size: $tiny-size);
  let bits = list(0, 10);
  set-bits(vector, bits);

  check-equal("fill!(vector, 1, start: 6, end: 9)",
    fill!(vector, 1, start: 6, end: 9), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start:6, end: 9)",
    vector, $tiny-size, list(0, 6, 7, 8, 10));

  check-equal("fill!(vector, 1, start: 4)",
    fill!(vector, 1, start: 4), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start: 4",
    vector, $tiny-size, list(0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13));

  check-equal("fill!(vector, 0, end: 7)",
    fill!(vector, 0, end: 7), vector);
  bit-vector-consistency-checks("fill!(vector, 0, end: 7)",
    vector, $tiny-size, list(7, 8, 9, 10, 11, 12, 13));

  check-equal("fill!(vector, 0, start: 0, end: $tiny-size)",
    fill!(vector, 0, start: 0, end: $tiny-size), vector);
  bit-vector-consistency-checks("fill!(vector, 0, start: 0, end: $tiny-size)",
    vector, $tiny-size, #"all-zeros");

  check-equal("fill!(vector, 1, start: 0, end: 8)",
    fill!(vector, 1, start: 0, end: 8), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start: 0, end: 8)",
    vector, $tiny-size, list(0, 1, 2, 3, 4, 5, 6, 7));

  check-equal("fill!(vector, 1, start: 6, end: $tiny-size)",
    fill!(vector, 1, start: 6, end: $tiny-size), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start: 6, end: $tiny-size)",
    vector, $tiny-size, #"all-ones");

  check-equal("fill!(vector, 0)",
    fill!(vector, 0), vector);
  bit-vector-consistency-checks("fill!(vector, 0)",
    vector, $tiny-size, #"all-zeros");
end test;


define test fill-huge-vector ()
  let vector :: <bit-vector> = make(<bit-vector>, size: $huge-size);
  let bits = list(0, 5, 7, 23, 57, 99, 100, 101, 116);
  set-bits(vector, bits);

  check-equal("fill!(vector, 1, start: 28, end: 35)",
    fill!(vector, 1, start: 28, end: 35), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start: 28, end: 35)",
    vector, $huge-size, list(0, 5, 7, 23, 28, 29, 30, 31, 32, 33, 34, 57,
                             99, 100, 101, 116));

  check-equal("fill!(vector, 0, start: 32)",
    fill!(vector, 0, start: 32), vector);
  bit-vector-consistency-checks("fill!(vector, 0, start: 32",
    vector, $huge-size, list(0, 5, 7, 23, 28, 29, 30, 31));

  check-equal("fill!(vector, 1, end: 32)",
    fill!(vector, 1, end: 32), vector);
  bit-vector-consistency-checks("fill!(vector, 1, end: 32)",
    vector, $huge-size, list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                             14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                             26, 27, 28, 29, 30, 31));

  check-equal("fill!(vector, 0, start: 0, end: $huge-size)",
    fill!(vector, 0, start: 0, end: $huge-size), vector);
  bit-vector-consistency-checks("fill!(vector, 0, start: 0, end: $huge-size)",
    vector, $huge-size, #"all-zeros");

  check-equal("fill!(vector, 1, start: 0, end: 35)",
    fill!(vector, 1, start: 0, end: 35), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start: 0, end: 35)",
    vector, $huge-size, list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                             14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                             26, 27, 28, 29, 30, 31, 32, 33, 34));

  check-equal("fill!(vector, 1, start: 35, end: $huge-size)",
    fill!(vector, 1, start: 35, end: $huge-size), vector);
  bit-vector-consistency-checks("fill!(vector, 1, start: 35, end: $huge-size)",
    vector, $huge-size, #"all-ones");

  check-equal("fill!(vector, 0)",
    fill!(vector, 0), vector);
  bit-vector-consistency-checks("fill!(vector, 0)",
    vector, $huge-size, #"all-zeros");
end test;


define suite fill-suite (description: "Tests for fill!")
  test fill-tiny-vector;
  test fill-huge-vector;
end suite;
