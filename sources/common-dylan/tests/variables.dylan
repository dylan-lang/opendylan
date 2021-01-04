Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// TODO(cgay): these belong in io-test-suite

define test test-*standard-input* ()
  //---*** Fill this in!
end test;

define test test-*standard-output* ()
  //---*** Fill this in!
end test;

define test test-*standard-error* ()
  //---*** Fill this in!
end test;

define suite common-dylan-variables-test-suite ()
  test test-*standard-input*;
  test test-*standard-output*;
  test test-*standard-error*;
end suite;
