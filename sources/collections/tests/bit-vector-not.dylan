Module:       collections-test-suite
Synopsis:     Test bit-vector-not function
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test bit-vector-not-empty-vector ()
  let vector = make(<bit-vector>, size: 0);

  let (result, result-pad) = bit-vector-not(vector, pad: 0);
  check-equal("Empty vector with pad 0: size(bit-vector-not(vector)) is 0",
    size(result), 0);
  check-equal("Empty vector with pad 0, bit-vector-not(vector) pad is 1",
    result-pad, 1);

  let (result, result-pad) = bit-vector-not(vector, pad: 1);
  check-equal("Empty vector with pad 1, size(bit-vector-not(vector) is 0",
    size(result), 0);
  check-equal("Empty vector with pad 1, bit-vector-not(vector) pad is 0",
    result-pad, 0);
end test;


define test bit-vector-not!-empty-vector ()
  let vector = make(<bit-vector>, size: 0);

  let (result, result-pad) = bit-vector-not!(vector, pad: 0);
  check-equal("Empty vector with pad 0: bit-vector-not!(vector) == vector",
    result, vector);
  check-equal("Empty vector with pad 0: size(bit-vector-not!(vector)) = 0",
    size(result), 0);
  check-equal("Empty vector with pad 0: bit-vector-not!(vector), pad is 1",
    result-pad, 1);

  let (result, result-pad) = bit-vector-not!(vector, pad: 1);
  check-equal("Empty vector with pad 1: bit-vector-not!(vector) == vector",
    result, vector);
  check-equal("Empty vector with pad 1: size(bit-vector-not!(vector)) = 0",
    size(result), 0);
  check-equal("Empty vector with pad 1: bit-vector-not!(vector) pad is 0",
    result-pad, 0);
end test;

define method bit-vector-not-sized-vector(size :: <integer>)
  let vector1 :: <bit-vector> = make(<bit-vector>, size: size);
  let vector2 :: <bit-vector> = make(<bit-vector>, size: size, fill: 1);
  let vector3 :: <bit-vector> = make(<bit-vector>, size: size);

  let vector3-elements = random-elements(size);
  let not-vector3-elements = compute-not-bits(vector3-elements, size);
  set-bits(vector3, vector3-elements);

  // bit-vector-not of an all-zero vector
  //
  let (result, result-pad) = bit-vector-not(vector1);
  bit-vector-checks(result, result-pad,
    "bit-vector-not of an all-zero vector with default pad",
    size, #"all-ones", 1);

  let (result, result-pad) = bit-vector-not(vector1, pad: 1);
  bit-vector-checks(result, result-pad,
    "bit-vector-not of an all-zero vector with pad = 1",
    size, #"all-ones", 0);

  // bit-vector-not of an all-one vector
  //
  let (result, result-pad) = bit-vector-not(vector2, pad: 0);
  bit-vector-checks(result, result-pad,
    "bit-vector-not of an all-one vector with pad = 0",
    size, #"all-zeros", 1);

  let (result, result-pad) = bit-vector-not(vector2, pad: 1);
  bit-vector-checks(result, result-pad,
    "bit-vector-not of an all-one vector with pad = 1",
    size, #"all-zeros", 0);

  // bit-vector-not of a vector
  //
  let (result, result-pad) = bit-vector-not(vector3);
  bit-vector-checks(result, result-pad,
    "bit-vector-not of a vector with default pad",
    size, not-vector3-elements, 1);

  let (result, result-pad) = bit-vector-not(vector3, pad: 1);
  bit-vector-checks(result, result-pad,
    "bit-vector-not of a vector with pad = 1",
    size, not-vector3-elements, 0);
end method;

define method bit-vector-not!-sized-vector(size :: <integer>)
  let vector1 :: <bit-vector> = make(<bit-vector>, size: size);
  let vector2 :: <bit-vector> = make(<bit-vector>, size: size);
  let vector3 :: <bit-vector> = make(<bit-vector>, size: size);

  let vector3-elements = random-elements(size);
  let not-vector3-elements = compute-not-bits(vector3-elements, size);
  set-bits(vector3, vector3-elements);

  // bit-vector-not! of an all-zero vector
  //
  let (result, result-pad) = bit-vector-not!(vector1);
  check-equal("With default pad, bit-vector-not!(vector1) == vector1",
    result, vector1);
  bit-vector-checks(result, result-pad,
    "bit-vector-not! of an all-zero vector with default pad",
    size, #"all-ones", 1);

  let (result, result-pad) = bit-vector-not!(vector2, pad: 1);
  check-equal("With pad = 1, bit-vector-not!(vector2) == vector2",
    result, vector2);
  bit-vector-checks(result, result-pad,
    "bit-vector-not! of an all-zero vector with pad = 1",
    size, #"all-ones", 0);

  // bit-vector-not of an all-one vector
  //
  let (result, result-pad) = bit-vector-not!(vector1, pad: 0);
  check-equal("With pad = 0, bit-vector-not!(vector1) == vector1",
    result, vector1);
  bit-vector-checks(result, result-pad,
    "bit-vector-not! of an all-one vector with pad = 0",
    size, #"all-zeros", 1);

  let (result, result-pad) = bit-vector-not!(vector2, pad: 1);
  check-equal("With pad = 1, bit-vector-not!(vector2) == vector2",
    result, vector2);
  bit-vector-checks(result, result-pad,
    "bit-vector-not! of an all-one vector with pad = 1",
    size, #"all-zeros", 0);

  // bit-vector-not of a vector
  //
  let (result, result-pad) = bit-vector-not!(vector3);
  check-equal("With default pad, bit-vector-not!(vector3) == vector3",
    result, vector3);
  bit-vector-checks(result, result-pad,
    "bit-vector-not! of a vector with default pad",
    size, not-vector3-elements, 1);

  let (result, result-pad) = bit-vector-not!(vector3, pad: 1);
  check-equal("With pad = 1, bit-vector-not!(vector3) == vector3",
    result, vector3);
  bit-vector-checks(result, result-pad,
    "bit-vector-not! of a vector with pad = 1",
    size, vector3-elements, 0);
end method;

define test bit-vector-not-tiny-vector ()
    bit-vector-not-sized-vector($tiny-size);
end test;

define test bit-vector-not!-tiny-vector ()
    bit-vector-not!-sized-vector($tiny-size);
end test;

define test bit-vector-not-small-vector ()
    bit-vector-not-sized-vector($tiny-size);
end test;

define test bit-vector-not!-small-vector ()
    bit-vector-not!-sized-vector($small-size);
end test;

define test bit-vector-not-huge-vector ()
    bit-vector-not-sized-vector($huge-size);
end test;

define test bit-vector-not!-huge-vector ()
    bit-vector-not!-sized-vector($huge-size);
end test;

define test bit-vector-not-multiple-word-sized-vector ()
    bit-vector-not-sized-vector($multiple-word-size);
end test;

define test bit-vector-not!-multiple-word-sized-vector ()
    bit-vector-not!-sized-vector($multiple-word-size);
end test;

define suite bit-vector-not-suite ()
  test bit-vector-not-empty-vector;
  test bit-vector-not!-empty-vector;
  test bit-vector-not-tiny-vector;
  test bit-vector-not!-tiny-vector;
  test bit-vector-not-small-vector;
  test bit-vector-not!-small-vector;
  test bit-vector-not-huge-vector;
  test bit-vector-not!-huge-vector;
  test bit-vector-not-multiple-word-sized-vector;
  test bit-vector-not!-multiple-word-sized-vector;
end suite;
