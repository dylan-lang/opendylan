Module: variable-search-tests
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test locate-variable-test ()
  let (binding, module, library) = locate-variable(<function>);
  assert-equal(binding, "<function>");
  assert-equal(module, "dylan");
  assert-equal(library, "dylan");

  let (binding, module, library) = locate-variable(<random>);
  assert-equal(binding, "<random>");
  assert-equal(module, "simple-random");
  assert-equal(library, "common-dylan");
end test;

define test variable-value-test ()
  let function-binding = variable-value("<function>", "dylan", "dylan");
  assert-equal(function-binding, <function>);

  let random-binding = variable-value("<random>", "simple-random", "common-dylan");
  assert-equal(random-binding, <random>);
end test;

define suite variable-search-tests-suite ()
  test locate-variable-test;
  test variable-value-test;
end suite;

run-test-application(variable-search-tests-suite);
