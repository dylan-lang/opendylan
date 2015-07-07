Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define test variable-name-test ()
  let f = read-fragment("x");
  assert-true(instance?(f, <variable-name-fragment>));
  assert-equal(f.fragment-name, #"x");
end test variable-name-test;

define test not-unary-operator-test ()
  let f = read-fragment("~x");
  assert-true(instance?(f, <unary-operator-call-fragment>));
  assert-equal(f.fragment-function.fragment-name, #"~");
  assert-equal(f.fragment-arguments.size, 1);
  let f-arg = f.fragment-arguments.first;
  assert-true(instance?(f-arg, <variable-name-fragment>));
  assert-equal(f-arg.fragment-name, #"x");
end test not-unary-operator-test;

define test negative-unary-operator-test ()
  let f = read-fragment("-x");
  assert-true(instance?(f, <unary-operator-call-fragment>));
  // The function name is transformed during reading into
  // "negative". See initialize on <unary-operator-call-fragment>.
  assert-equal(f.fragment-function.fragment-name, #"negative");
  assert-equal(f.fragment-arguments.size, 1);
  let f-arg = f.fragment-arguments.first;
  assert-true(instance?(f-arg, <variable-name-fragment>));
  assert-equal(f-arg.fragment-name, #"x");
end test negative-unary-operator-test;

define test escaped-name-test ()
  let f = read-fragment("\\+");
  assert-true(instance?(f, <escaped-name-fragment>));
  assert-equal(f.fragment-name, #"+");
end test escaped-name-test;

define suite expressions-test-suite ()
  test variable-name-test;
  test not-unary-operator-test;
  test negative-unary-operator-test;
  test escaped-name-test;
end suite expressions-test-suite;
