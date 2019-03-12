Module:       collections-test-suite
Synopsis:     Test bit-vector-and function
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method test-and-for-all-pad-values
  (prefix :: <string>, size :: <integer>,
     vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     bits00, bits01, bits10, bits11)
 => ()
  let (result, pad) = bit-vector-and(vector1, vector2);
  check-equal(concatenate(prefix, ", pad1=0, pad2=0: result pad"), pad, 0);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=0, pad2=0"),
    result, size, bits00);

  let (result, pad) = bit-vector-and(vector1, vector2, pad1: 0, pad2: 1);
  check-equal(concatenate(prefix, ", pad1=0, pad2=0: result pad"), pad, 0);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=0, pad2=1"),
    result, size, bits01);

  let (result, pad) = bit-vector-and(vector1, vector2, pad1: 1, pad2: 0);
  check-equal(concatenate(prefix, ", pad1=1, pad2=0: result pad"), pad, 0);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=1, pad2=0"),
    result, size, bits10);

  let (result, pad) = bit-vector-and(vector1, vector2, pad1: 1, pad2: 1);
  check-equal(concatenate(prefix, ", pad1=1, pad2=1: result pad"), pad, 1);
  bit-vector-consistency-checks(concatenate(prefix, ", pad1=1, pad2=1"),
    result, size, bits11);
end method;


define test bit-vector-and-tiny-vector ()
  let vector1 = make(<bit-vector>, size: $tiny-size);
  let vector2 = make(<bit-vector>, size: $tiny-size);
  let vector3 = make(<bit-vector>, size: $tiny-size, fill: 1);
  let vector4 = make(<bit-vector>, size: $tiny-size, fill: 1);

  let bits5 = list(2, 3);
  let not-bits5 = list(0, 1);
  let vector5 = make(<bit-vector>, size: $tiny-size);
  set-bits(vector5, bits5);

  let bits6 = list(1, 3);
  let not-bits6 = list(0, 2);
  let vector6 = make(<bit-vector>, size: $tiny-size, fill: 1);
  unset-bits(vector6, not-bits6);

  let bits5-and-bits6 = intersection(bits5, bits6);
  let bits6-and-bits5 = bits5-and-bits6;

  // And two all-zero vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-zero vectors",
    $tiny-size, vector1, vector2,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And two all-one vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-one vectors",
    $tiny-size, vector3, vector4,
    #"all-ones", #"all-ones", #"all-ones", #"all-ones");

  // And an all-zero and all-one vector together with different pad values
  //
  test-and-for-all-pad-values("And an all-one and all-zero vector",
    $tiny-size, vector1, vector3,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And an all-zero and all-one vector",
    $tiny-size, vector3, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-zero vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-zero and mixed vector",
    $tiny-size, vector1, vector5,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And a mixed and all-zero vector",
    $tiny-size, vector5, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-one vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-one and mixed vector",
    $tiny-size, vector3, vector5,
    bits5, bits5, bits5, bits5);
  test-and-for-all-pad-values("And a mixed and all-one vector",
    $tiny-size, vector5, vector3,
    bits5, bits5, bits5, bits5);

  // And two mixed vectors together with different pad values
  //
  test-and-for-all-pad-values("And two vectors",
    $tiny-size, vector5, vector6,
    bits5-and-bits6, bits5-and-bits6, bits5-and-bits6, bits5-and-bits6);
  test-and-for-all-pad-values("And two vectors again, arguments reversed",
    $tiny-size, vector6, vector5,
    bits6-and-bits5, bits6-and-bits5, bits6-and-bits5, bits6-and-bits5);

end test;

define test bit-vector-and-small-vector ()
  let vector1 = make(<bit-vector>, size: $small-size);
  let vector2 = make(<bit-vector>, size: $small-size);
  let vector3 = make(<bit-vector>, size: $small-size, fill: 1);
  let vector4 = make(<bit-vector>, size: $small-size, fill: 1);

  let bits5 = list(2, 3, 5, 7, 11, 13);
  let not-bits5 = list(0, 1, 4, 6, 8, 9, 10, 12);
  let vector5 = make(<bit-vector>, size: $small-size);
  set-bits(vector5, bits5);

  let bits6 = list(1, 3, 6, 7, 9, 10, 12, 13);
  let not-bits6 = list(0, 2, 4, 5, 8, 11);
  let vector6 = make(<bit-vector>, size: $small-size, fill: 1);
  unset-bits(vector6, not-bits6);

  let bits5-and-bits6 = intersection(bits5, bits6);
  let bits6-and-bits5 = bits5-and-bits6;

  // And two all-zero vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-zero vectors",
    $small-size, vector1, vector2,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And two all-one vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-one vectors",
    $small-size, vector3, vector4,
    #"all-ones", #"all-ones", #"all-ones", #"all-ones");

  // And an all-zero and all-one vector together with different pad values
  //
  test-and-for-all-pad-values("And an all-one and all-zero vector",
    $small-size, vector1, vector3,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And an all-zero and all-one vector",
    $small-size, vector3, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-zero vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-zero and mixed vector",
    $small-size, vector1, vector5,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And a mixed and all-zero vector",
    $small-size, vector5, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-one vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-one and mixed vector",
    $small-size, vector3, vector5,
    bits5, bits5, bits5, bits5);
  test-and-for-all-pad-values("And a mixed and all-one vector",
    $small-size, vector5, vector3,
    bits5, bits5, bits5, bits5);

  // And two mixed vectors together with different pad values
  //
  test-and-for-all-pad-values("And two vectors",
    $small-size, vector5, vector6,
    bits5-and-bits6, bits5-and-bits6, bits5-and-bits6, bits5-and-bits6);
  test-and-for-all-pad-values("And two vectors again, arguments reversed",
    $small-size, vector6, vector5,
    bits6-and-bits5, bits6-and-bits5, bits6-and-bits5, bits6-and-bits5);

end test;

define test bit-vector-and-huge-vector ()
  let vector1 = make(<bit-vector>, size: $huge-size);
  let vector2 = make(<bit-vector>, size: $huge-size);
  let vector3 = make(<bit-vector>, size: $huge-size, fill: 1);
  let vector4 = make(<bit-vector>, size: $huge-size, fill: 1);

  let bits5 = random-elements($huge-size, seed: 186);
  let not-bits5 = compute-not-bits(bits5, $huge-size);
  let vector5 = make(<bit-vector>, size: $huge-size);
  set-bits(vector5, bits5);

  let bits6 = random-elements($huge-size, seed: 191);
  let not-bits6 = compute-not-bits(bits6, $huge-size);
  let vector6 = make(<bit-vector>, size: $huge-size, fill: 1);
  unset-bits(vector6, not-bits6);

  let bits5-and-bits6 = intersection(bits5, bits6);
  let bits6-and-bits5 = bits5-and-bits6;

  // And two all-zero vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-zero vectors",
    $huge-size, vector1, vector2,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And two all-one vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-one vectors",
    $huge-size, vector3, vector4,
    #"all-ones", #"all-ones", #"all-ones", #"all-ones");

  // And an all-zero and all-one vector together with different pad values
  //
  test-and-for-all-pad-values("And an all-one and all-zero vector",
    $huge-size, vector1, vector3,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And an all-zero and all-one vector",
    $huge-size, vector3, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-zero vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-zero and mixed vector",
    $huge-size, vector1, vector5,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And a mixed and all-zero vector",
    $huge-size, vector5, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-one vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-one and mixed vector",
    $huge-size, vector3, vector5,
    bits5, bits5, bits5, bits5);
  test-and-for-all-pad-values("And a mixed and all-one vector",
    $huge-size, vector5, vector3,
    bits5, bits5, bits5, bits5);

  // And two mixed vectors together with different pad values
  //
  test-and-for-all-pad-values("And two vectors",
    $huge-size, vector5, vector6,
    bits5-and-bits6, bits5-and-bits6, bits5-and-bits6, bits5-and-bits6);
  test-and-for-all-pad-values("And two vectors again, arguments reversed",
    $huge-size, vector6, vector5,
    bits6-and-bits5, bits6-and-bits5, bits6-and-bits5, bits6-and-bits5);

end test;

define test bit-vector-and-multiple-word-vector ()
  let vector1 = make(<bit-vector>, size: $multiple-word-size);
  let vector2 = make(<bit-vector>, size: $multiple-word-size);
  let vector3 = make(<bit-vector>, size: $multiple-word-size, fill: 1);
  let vector4 = make(<bit-vector>, size: $multiple-word-size, fill: 1);

  let bits5 = random-elements($multiple-word-size, seed: 257);
  let not-bits5 = compute-not-bits(bits5, $multiple-word-size);
  let vector5 = make(<bit-vector>, size: $multiple-word-size);
  set-bits(vector5, bits5);

  let bits6 = random-elements($multiple-word-size, seed: 262);
  let not-bits6 = compute-not-bits(bits6, $multiple-word-size);
  let vector6 = make(<bit-vector>, size: $multiple-word-size, fill: 1);
  unset-bits(vector6, not-bits6);

  let bits5-and-bits6 = intersection(bits5, bits6);
  let bits6-and-bits5 = bits5-and-bits6;

  // And two all-zero vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-zero vectors",
    $multiple-word-size, vector1, vector2,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And two all-one vectors together with different pad values
  //
  test-and-for-all-pad-values("And two all-one vectors",
    $multiple-word-size, vector3, vector4,
    #"all-ones", #"all-ones", #"all-ones", #"all-ones");

  // And an all-zero and all-one vector together with different pad values
  //
  test-and-for-all-pad-values("And an all-one and all-zero vector",
    $multiple-word-size, vector1, vector3,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And an all-zero and all-one vector",
    $multiple-word-size, vector3, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-zero vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-zero and mixed vector",
    $multiple-word-size, vector1, vector5,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");
  test-and-for-all-pad-values("And a mixed and all-zero vector",
    $multiple-word-size, vector5, vector1,
    #"all-zeros", #"all-zeros", #"all-zeros", #"all-zeros");

  // And an all-one vector with a mixed vector and different pad values
  //
  test-and-for-all-pad-values("And an all-one and mixed vector",
    $multiple-word-size, vector3, vector5,
    bits5, bits5, bits5, bits5);
  test-and-for-all-pad-values("And a mixed and all-one vector",
    $multiple-word-size, vector5, vector3,
    bits5, bits5, bits5, bits5);

  // And two mixed vectors together with different pad values
  //
  test-and-for-all-pad-values("And two vectors",
    $multiple-word-size, vector5, vector6,
    bits5-and-bits6, bits5-and-bits6, bits5-and-bits6, bits5-and-bits6);
  test-and-for-all-pad-values("And two vectors again, arguments reversed",
    $multiple-word-size, vector6, vector5,
    bits6-and-bits5, bits6-and-bits5, bits6-and-bits5, bits6-and-bits5);

end test;

define suite bit-vector-and-suite ()
  test bit-vector-and-tiny-vector;
  test bit-vector-and-small-vector;
  test bit-vector-and-huge-vector;
  test bit-vector-and-multiple-word-vector;
end suite;
