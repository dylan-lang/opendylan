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

define function verify-binary-operator
    (code :: <string>, operator :: <symbol>)
 => ()
  let f = read-fragment(code);
  assert-true(instance?(f, <binary-operator-call-fragment>));
  assert-equal(f.fragment-function.fragment-name, operator);
  assert-equal(f.fragment-arguments.size, 2);
  let f-x-arg = f.fragment-arguments.first;
  assert-true(instance?(f-x-arg, <variable-name-fragment>));
  assert-equal(f-x-arg.fragment-name, #"x");
  let f-1-arg = f.fragment-arguments.second;
  verify-literal(f-1-arg, 1, <integer-fragment>);
end function verify-binary-operator;

define test binary-+-test ()
  verify-binary-operator("x + 1", #"+");
end test binary-+-test;

define test binary---test ()
  verify-binary-operator("x - 1", #"-");
end test binary---test;

define test binary-*-test ()
  verify-binary-operator("x * 1", #"*");
end test binary-*-test;

define test binary-/-test ()
  verify-binary-operator("x / 1", #"/");
end test binary-/-test;

define  test binary-^-test ()
  verify-binary-operator("x ^ 1", #"^");
end test binary-^-test;

define test binary-=-test ()
  verify-binary-operator("x = 1", #"=");
end test binary-=-test;

define test binary-==-test ()
  verify-binary-operator("x == 1", #"==");
end test binary-==-test;

define test binary-~=-test ()
  verify-binary-operator("x ~= 1", #"~=");
end test binary-~=-test;

define test binary-~==-test ()
  verify-binary-operator("x ~== 1", #"~==");
end test binary-~==-test;

define test binary-<-test ()
  verify-binary-operator("x < 1", #"<");
end test binary-<-test;

define test binary-<=-test ()
  verify-binary-operator("x <= 1", #"<=");
end test binary-<=-test;

define test binary->-test ()
  verify-binary-operator("x > 1", #">");
end test binary->-test;

define test binary->=-test ()
  verify-binary-operator("x >= 1", #">=");
end test binary->=-test;

define test escaped-name-test ()
  let f = read-fragment("\\+");
  assert-true(instance?(f, <escaped-name-fragment>));
  assert-equal(f.fragment-name, #"+");
end test escaped-name-test;

define suite expressions-test-suite ()
  test variable-name-test;
  test not-unary-operator-test;
  test negative-unary-operator-test;
  test binary-+-test;
  test binary---test;
  test binary-*-test;
  test binary-/-test;
  test binary-^-test;
  test binary-=-test;
  test binary-==-test;
  test binary-~=-test;
  test binary-~==-test;
  test binary-<-test;
  test binary-<=-test;
  test binary->-test;
  test binary->=-test;
  // This doesn't test &, | and := yet.
  test escaped-name-test;
end suite expressions-test-suite;
