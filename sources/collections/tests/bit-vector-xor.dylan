Module:       collections-test-suite
Synopsis:     Test bit-vector-xor function
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method test-xor-for-all-pad-values
    (prefix :: <string>, vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     bits00, bits01, bits10, bits11)
 => ()
  let (result, pad) = bit-vector-xor(vector1, vector2);
  check-equal(concatenate(prefix, ", pad1=0, pad2=0: result pad"), pad, 0);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=0, pad2=0"),
    result, $tiny-size, bits00);

  let (result, pad) = bit-vector-xor(vector1, vector2, pad1: 0, pad2: 1);
  check-equal(concatenate(prefix, ", pad1=0, pad2=0: result pad"), pad, 1);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=0, pad2=1"),
    result, $tiny-size, bits01);

  let (result, pad) = bit-vector-xor(vector1, vector2, pad1: 1, pad2: 0);
  check-equal(concatenate(prefix, ", pad1=1, pad2=0: result pad"), pad, 1);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=1, pad2=0"),
    result, $tiny-size, bits10);

  let (result, pad) = bit-vector-xor(vector1, vector2, pad1: 1, pad2: 1);
  check-equal(concatenate(prefix, ", pad1=1, pad2=1: result pad"), pad, 0);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=1, pad2=1"),
    result, $tiny-size, bits11);
end method;


define test bit-vector-xor-tiny-vector
    (description: "Test bit-vector-andc2 with tiny sized bit-vectors")

  let vector1 = make(<bit-vector>, size: $tiny-size);
  let vector2 = make(<bit-vector>, size: $tiny-size);
  let vector3 = make(<bit-vector>, size: $tiny-size, fill: 1);
  let vector4 = make(<bit-vector>, size: $tiny-size, fill: 1);

  let bits5 = list(2, 3, 5, 7, 11, 13);
  let not-bits5 = list(0, 1, 4, 6, 8, 9, 10, 12);
  let vector5 = make(<bit-vector>, size: $tiny-size);
  set-bits(vector5, bits5);

  let bits6 = list(1, 3, 6, 7, 9, 10, 12, 13);
  let not-bits6 = list(0, 2, 4, 5, 8, 11);
  let vector6 = make(<bit-vector>, size: $tiny-size, fill: 1);
  unset-bits(vector6, not-bits6);

  let bits5-xor-bits6
    = intersection(union(bits5, bits6),
                   compute-not-bits(intersection(bits5, bits6), $tiny-size));
  let bits6-xor-bits5 = bits5-xor-bits6;

  // Xor two all-zero vectors together with different pad values
  //
  test-xor-for-all-pad-values("Xor two all-zero vectors",
    vector1, vector2,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // Xor two all-one vectors together with different pad values
  //
  test-xor-for-all-pad-values("Xor two all-one vectors",
    vector3, vector4,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // Xor an all-zero and all-one vector together with different pad values
  //
  test-xor-for-all-pad-values("Xor an all-one and all-zero vector",
    vector1, vector3,
    #"all-ones", #"all-ones", #"all-ones", #"all-ones");
  test-xor-for-all-pad-values("Xor an all-zero and all-one vector",
    vector3, vector1,
    #"all-ones", #"all-ones", #"all-ones", #"all-ones");

  // Xor an all-zero vector with a mixed vector and different pad values
  //
  test-xor-for-all-pad-values("Xor an all-zero and mixed vector",
    vector1, vector5,
    bits5, bits5, bits5, bits5);
  test-xor-for-all-pad-values("Xor a mixed and all-zero vector",
    vector5, vector1,
    bits5, bits5, bits5, bits5);

  // Xor an all-one vector with a mixed vector and different pad values
  //
  test-xor-for-all-pad-values("Xor an all-one and mixed vector",
    vector3, vector5,
      not-bits5, not-bits5, not-bits5, not-bits5);
  test-xor-for-all-pad-values("Xor a mixed and all-one vector",
    vector5, vector3,
      not-bits5, not-bits5, not-bits5, not-bits5);

  // Xor two mixed vectors together with different pad values
  //
  test-xor-for-all-pad-values("Xor two vectors",
    vector5, vector6,
    bits5-xor-bits6, bits5-xor-bits6, bits5-xor-bits6, bits5-xor-bits6);
  test-xor-for-all-pad-values("Xor two vectors again, arguments reversed",
    vector6, vector5,
    bits6-xor-bits5, bits6-xor-bits5, bits6-xor-bits5, bits6-xor-bits5);

end test;


define suite bit-vector-xor-suite ()
  test bit-vector-xor-tiny-vector;
end suite;
