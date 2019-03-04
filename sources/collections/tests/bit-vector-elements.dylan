Module:       collections-test-suite
Synopsis:     Tests for bit-vector element setters and getters
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test bit-vector-elements-tiny-vector ()
  let vector1 = make(<bit-vector>, size: $tiny-size);
  let vector2 = make(<bit-vector>, size: $tiny-size, fill: 0);
  let vector3 = make(<bit-vector>, size: $tiny-size, fill: 1);

  bit-vector-consistency-checks("Make tiny vector1",
    vector1, $tiny-size, #"all-zeros");
  bit-vector-consistency-checks("Make tiny vector2",
    vector2, $tiny-size, #"all-zeros");
  bit-vector-consistency-checks("Make tiny vector3",
    vector3, $tiny-size, #"all-ones");

  check-equal("Bit 0 of vector1 to 1", element-setter(1, vector1, 0), 1);
  bit-vector-consistency-checks("Only bit 0 set",
    vector1, $tiny-size, list(0));

  check-equal("Bit 3 of vector1 to 1", (element(vector1, 3) := 1), 1);
  bit-vector-consistency-checks("Only bits 0 and 3 set",
    vector1, $tiny-size, list(0, 3));

  check-equal("Bit 2 of vector1 to 1", (vector1[2] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 set",
    vector1, $tiny-size, list(0, 2, 3));

  check-equal("Bit 0 of vector1 to 1 again", (vector1[0] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 set",
    vector1, $tiny-size, list(0, 2, 3));

  check-equal("Bit 1 of vector1 to 0", (element(vector1, 1) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 set",
    vector1, $tiny-size, list(0, 2, 3));

  check-equal("Bit 3 of vector1 to 1 again", (element(vector1, 3) := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 set",
    vector1, $tiny-size, list(0, 2, 3));

  check-equal("Bit 2 of vector1 to 1 again",
    element-setter(1, vector1, 2), 1);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 set",
    vector1, $tiny-size, list(0, 2, 3));

  // vector3

  check-equal("Bit 0 of vector3 to 0", element-setter(0, vector3, 0), 0);
  bit-vector-consistency-checks("Only bit 0 unset",
    vector3, $tiny-size, list(1, 2, 3));

  check-equal("Bit 3 of vector3 to 0", (element(vector3, 3) := 0), 0);
  bit-vector-consistency-checks("Only bits 0 and 3 unset",
    vector3, $tiny-size, list(1, 2));

  check-equal("Bit 2 of vector3 to 0", (vector3[2] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 unset",
    vector3, $tiny-size, list(1));

  check-equal("Bit 0 of vector3 to 0 again", (element(vector3, 0) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 2 and 3 unset",
    vector3, $tiny-size, list(1));

  check-equal("Bit 1 of vector3 to 0 again", element-setter(0, vector3, 1), 0);
  bit-vector-consistency-checks("All bits unset",
    vector3, $tiny-size, list());

  check-equal("Bit 2 of vector3 to 1", element-setter(1, vector3, 2), 1);
  bit-vector-consistency-checks("Only bits 0, 1, 3 unset",
    vector3, $tiny-size, list(2));

  check-equal("Bit 0 of vector3 to 0 again", (vector3[0] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 1, 3 unset",
    vector3, $tiny-size, list(2));

end test;

define test bit-vector-elements-small-vector ()
  let vector1 = make(<bit-vector>, size: $small-size);
  let vector2 = make(<bit-vector>, size: $small-size, fill: 0);
  let vector3 = make(<bit-vector>, size: $small-size, fill: 1);

  bit-vector-consistency-checks("Make small vector1",
    vector1, $small-size, #"all-zeros");
  bit-vector-consistency-checks("Make small vector2",
    vector2, $small-size, #"all-zeros");
  bit-vector-consistency-checks("Make small vector3",
    vector3, $small-size, #"all-ones");

  check-equal("Bit 0 of vector1 to 1", element-setter(1, vector1, 0), 1);
  bit-vector-consistency-checks("Only bit 0 set",
    vector1, $small-size, list(0));

  check-equal("Bit 10 of vector1 to 1", (element(vector1, 10) := 1), 1);
  bit-vector-consistency-checks("Only bits 0 and 10 set",
    vector1, $small-size, list(0, 10));

  check-equal("Bit 7 of vector1 to 1", (vector1[7] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 10 and 7 set",
    vector1, $small-size, list(0, 10, 7));


  check-equal("Bit 0 of vector1 to 1 again", (vector1[0] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $small-size, list(0, 7, 10));

  check-equal("Bit 2 of vector1 to 0", (element(vector1, 2) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $small-size, list(0, 7, 10));

  check-equal("Bit 7 of vector1 to 1 again", (element(vector1, 7) := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $small-size, list(0, 7, 10));

  check-equal("Bit 10 of vector1 to 1 again",
    element-setter(1, vector1, 10), 1);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $small-size, list(0, 7, 10));


  check-equal("Bit 0 of vector3 to 0", element-setter(0, vector3, 0), 0);
  bit-vector-consistency-checks("Only bit 0 unset",
    vector3, $small-size, list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13));

  check-equal("Bit 10 of vector3 to 0", (element(vector3, 10) := 0), 0);
  bit-vector-consistency-checks("Only bits 0 and 10 unset",
    vector3, $small-size, list(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 4 of vector3 to 0", (vector3[4] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $small-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));


  check-equal("Bit 0 of vector3 to 0 again", (element(vector3, 0) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $small-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 4 of vector3 to 0 again", element-setter(0, vector3, 4), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $small-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 5 of vector3 to 1", element-setter(1, vector3, 5), 1);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $small-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 10 of vector1 to 0 again", (vector3[10] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $small-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));
end test;

define test bit-vector-elements-huge-vector ()
  let vector1 = make(<bit-vector>, size: $huge-size);
  let vector2 = make(<bit-vector>, size: $huge-size, fill: 0);
  let vector3 = make(<bit-vector>, size: $huge-size, fill: 1);

  bit-vector-consistency-checks("Make huge vector1",
    vector1, $huge-size, #"all-zeros");
  bit-vector-consistency-checks("Make huge vector2",
    vector2, $huge-size, #"all-zeros");
  bit-vector-consistency-checks("Make huge vector3",
    vector3, $huge-size, #"all-ones");

  check-equal("Bit 0 of vector1 to 1", element-setter(1, vector1, 0), 1);
  bit-vector-consistency-checks("Only bit 0 set",
    vector1, $huge-size, list(0));

  check-equal("Bit 99 of vector1 to 1", (element(vector1, 99) := 1), 1);
  bit-vector-consistency-checks("Only bits 0 and 99 set",
    vector1, $huge-size, list(0, 99));

  check-equal("Bit 20 of vector1 to 1", (vector1[20] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 99 set",
    vector1, $huge-size, list(0, 20, 99));


  check-equal("Bit 0 of vector1 to 1 again", (vector1[0] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 99 set",
    vector1, $huge-size, list(0, 20, 99));

  check-equal("Bit 45 of vector1 to 0", (element(vector1, 45) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 20 and 99 set",
    vector1, $huge-size, list(0, 20, 99));

  check-equal("Bit 20 of vector1 to 1 again", (element(vector1, 20) := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 99 set",
    vector1, $huge-size, list(0, 20, 99));

  check-equal("Bit 99 of vector1 to 1 again",
    element-setter(1, vector1, 99), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 99 set",
    vector1, $huge-size, list(0, 20, 99));


  check-equal("Bit 0 of vector3 to 0", element-setter(0, vector3, 0), 0);
  bit-vector-consistency-checks("Only bit 0 unset",
    vector3, $huge-size, compute-not-bits(list(0), $huge-size));

  check-equal("Bit 99 of vector3 to 0", (element(vector3, 99) := 0), 0);
  bit-vector-consistency-checks("Only bits 0 and 99 unset",
    vector3, $huge-size, compute-not-bits(list(0, 99), $huge-size));

  check-equal("Bit 101 of vector3 to 0", (vector3[101] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 99 and 101 unset",
    vector3, $huge-size, compute-not-bits(list(0, 99, 101), $huge-size));

  check-equal("Bit 0 of vector3 to 0 again", (element(vector3, 0) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 99 and 101 unset",
    vector3, $huge-size, compute-not-bits(list(0, 99, 101), $huge-size));

  check-equal("Bit 101 of vector3 to 0 again", element-setter(0, vector3, 101), 0);
  bit-vector-consistency-checks("Only bits 0, 99 and 101 unset",
    vector3, $huge-size, compute-not-bits(list(0, 99, 101), $huge-size));

  check-equal("Bit 5 of vector3 to 1", element-setter(1, vector3, 5), 1);
  bit-vector-consistency-checks("Only bits 0, 99 and 101 unset",
    vector3, $huge-size, compute-not-bits(list(0, 99, 101), $huge-size));

  check-equal("Bit 99 of vector1 to 0 again", (vector3[99] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 99 and 101 unset",
    vector3, $huge-size, compute-not-bits(list(0, 99, 101), $huge-size));
end test;

define test bit-vector-elements-multiple-word-vector ()
  let vector1 = make(<bit-vector>, size: $multiple-word-size);
  let vector2 = make(<bit-vector>, size: $multiple-word-size, fill: 0);
  let vector3 = make(<bit-vector>, size: $multiple-word-size, fill: 1);

  bit-vector-consistency-checks("Make multiple-word vector1",
    vector1, $multiple-word-size, #"all-zeros");
  bit-vector-consistency-checks("Make multiple-word vector2",
    vector2, $multiple-word-size, #"all-zeros");
  bit-vector-consistency-checks("Make multiple-word vector3",
    vector3, $multiple-word-size, #"all-ones");

  check-equal("Bit 0 of vector1 to 1", element-setter(1, vector1, 0), 1);
  bit-vector-consistency-checks("Only bit 0 set",
    vector1, $multiple-word-size, list(0));

  check-equal("Bit 57 of vector1 to 1", (element(vector1, 57) := 1), 1);
  bit-vector-consistency-checks("Only bits 0 and 57 set",
    vector1, $multiple-word-size, list(0, 57));

  check-equal("Bit 20 of vector1 to 1", (vector1[20] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 57 set",
    vector1, $multiple-word-size, list(0, 20, 57));


  check-equal("Bit 0 of vector1 to 1 again", (vector1[0] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 57 set",
    vector1, $multiple-word-size, list(0, 20, 57));

  check-equal("Bit 45 of vector1 to 0", (element(vector1, 45) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 20 and 57 set",
    vector1, $multiple-word-size, list(0, 20, 57));

  check-equal("Bit 20 of vector1 to 1 again", (element(vector1, 20) := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 57 set",
    vector1, $multiple-word-size, list(0, 20, 57));

  check-equal("Bit 57 of vector1 to 1 again",
    element-setter(1, vector1, 57), 1);
  bit-vector-consistency-checks("Only bits 0, 20 and 57 set",
    vector1, $multiple-word-size, list(0, 20, 57));


  check-equal("Bit 0 of vector3 to 0", element-setter(0, vector3, 0), 0);
  bit-vector-consistency-checks("Only bit 0 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0), $multiple-word-size));

  check-equal("Bit 57 of vector3 to 0", (element(vector3, 57) := 0), 0);
  bit-vector-consistency-checks("Only bits 0 and 57 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0, 57), $multiple-word-size));

  check-equal("Bit 23 of vector3 to 0", (vector3[23] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 57 and 23 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0, 57, 23), $multiple-word-size));

  check-equal("Bit 0 of vector3 to 0 again", (element(vector3, 0) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 57 and 23 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0, 57, 23), $multiple-word-size));

  check-equal("Bit 23 of vector3 to 0 again", element-setter(0, vector3, 23), 0);
  bit-vector-consistency-checks("Only bits 0, 57 and 23 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0, 57, 23), $multiple-word-size));

  check-equal("Bit 5 of vector3 to 1", element-setter(1, vector3, 5), 1);
  bit-vector-consistency-checks("Only bits 0, 57 and 23 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0, 57, 23), $multiple-word-size));

  check-equal("Bit 57 of vector1 to 0 again", (vector3[57] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 57 and 23 unset",
    vector3, $multiple-word-size, compute-not-bits(list(0, 57, 23), $multiple-word-size));
end test;

define suite bit-vector-elements-suite ()
  test bit-vector-elements-tiny-vector;
  test bit-vector-elements-small-vector;
  test bit-vector-elements-huge-vector;
  test bit-vector-elements-multiple-word-vector;
end suite;
