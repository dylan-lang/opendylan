Module:       collections-test-suite
Synopsis:     Test suite for collections library
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite bit-vector-test-suite ()
  suite bit-vector-elements-suite;
  suite fill-suite;
  suite copy-sequence-suite;
  suite bit-vector-and-suite;
  suite bit-vector-andc2-suite;
  suite bit-vector-or-suite;
  suite bit-vector-xor-suite;
  suite bit-vector-not-suite;
  suite bit-count-suite;
end suite bit-vector-test-suite;

define suite bit-set-test-suite ()
  test bit-set-equals;
  test bit-set-member;
  test bit-set-add;
  test bit-set-add!;
  test bit-set-remove;
  test bit-set-remove!;
  test bit-set-infinite;
  test bit-set-empty;
  test bit-set-size;
  test bit-set-union;
  test bit-set-intersection;
  test bit-set-difference;
  test bit-set-complement;
  test bit-set-union!;
  test bit-set-intersection!;
  test bit-set-difference!;
  test bit-set-complement!;
  test bit-set-copy;
  test bit-set-force-empty;
  test bit-set-force-universal;
  test bit-set-iteration;
  test bit-set-laws;
end suite bit-set-test-suite;

define test bug-4351 ()
  check-no-errors("remove-all-keys! has a method on <set>",
                  remove-all-keys!(make(<set>)));
end test bug-4351;

define suite collections-test-suite ()
  suite bit-vector-test-suite;
  suite bit-set-test-suite;
  test bug-4351;
end suite collections-test-suite;
