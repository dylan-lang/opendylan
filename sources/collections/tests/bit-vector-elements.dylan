Module:       collections-test-suite
Synopsis:     Tests for bit-vector element setters and getters
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test bit-vector-elements-tiny-vector
    (description: "Tests for small bit-vectors")

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

  check-equal("Bit 10 of vector1 to 1", (element(vector1, 10) := 1), 1);
  bit-vector-consistency-checks("Only bits 0 and 10 set",
    vector1, $tiny-size, list(0, 10));

  check-equal("Bit 7 of vector1 to 1", (vector1[7] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 10 and 7 set",
    vector1, $tiny-size, list(0, 10, 7));


  check-equal("Bit 0 of vector1 to 1 again", (vector1[0] := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $tiny-size, list(0, 7, 10));

  check-equal("Bit 2 of vector1 to 0", (element(vector1, 2) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $tiny-size, list(0, 7, 10));

  check-equal("Bit 7 of vector1 to 1 again", (element(vector1, 7) := 1), 1);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $tiny-size, list(0, 7, 10));

  check-equal("Bit 10 of vector1 to 1 again",
    element-setter(1, vector1, 10), 1);
  bit-vector-consistency-checks("Only bits 0, 7 and 10 set",
    vector1, $tiny-size, list(0, 7, 10));


  check-equal("Bit 0 of vector3 to 0", element-setter(0, vector3, 0), 0);
  bit-vector-consistency-checks("Only bit 0 unset",
    vector3, $tiny-size, list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13));

  check-equal("Bit 10 of vector3 to 0", (element(vector3, 10) := 0), 0);
  bit-vector-consistency-checks("Only bits 0 and 10 unset",
    vector3, $tiny-size, list(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 4 of vector3 to 0", (vector3[4] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $tiny-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));


  check-equal("Bit 0 of vector3 to 0 again", (element(vector3, 0) := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $tiny-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 4 of vector3 to 0 again", element-setter(0, vector3, 4), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $tiny-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 5 of vector3 to 1", element-setter(1, vector3, 5), 1);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $tiny-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

  check-equal("Bit 10 of vector1 to 0 again", (vector3[10] := 0), 0);
  bit-vector-consistency-checks("Only bits 0, 4 and 10 unset",
    vector3, $tiny-size, list(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13));

end test;


define suite bit-vector-elements-suite
    (description: "Tests for element setters and getters")
  test bit-vector-elements-tiny-vector;
end suite;
