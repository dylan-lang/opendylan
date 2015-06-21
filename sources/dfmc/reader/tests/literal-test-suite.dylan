Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define function verify-literal
    (f :: <fragment>, value, required-class)
 => ()
  assert-equal(f.fragment-value, value);
  assert-true(instance?(f, required-class));
end function verify-literal;

define test boolean-literal-test ()
  let t = read-fragment("#t");
  verify-literal(t, #t, <true-fragment>);

  let f = read-fragment("#f");
  verify-literal(f, #f, <false-fragment>);
end test boolean-literal-test;

define test character-literal-test ()
  let f = read-fragment("'n'");
  verify-literal(f, 'n', <character-fragment>);

  let f = read-fragment("'\\\\'");
  assert-equal(f.fragment-value, '\\');

  let f = read-fragment("'\\n'");
  assert-equal(f.fragment-value, '\n');

  let f = read-fragment("'\\<01>'");
  assert-equal(as(<integer>, f.fragment-value), 1);
end test character-literal-test;

define test integer-literal-test ()
  let f = read-fragment("123");
  verify-literal(f, 123, <integer-fragment>);
end test integer-literal-test;

define test ratio-literal-test ()
  assert-signals(<ratios-not-supported>, read-fragment("1/2"));
end test ratio-literal-test;

define test string-literal-test ()
  let f = read-fragment("\"abc\"");
  verify-literal(f, "abc", <string-fragment>);
end test string-literal-test;

define test symbol-literal-test ()
  let kw = read-fragment("test:");
  verify-literal(kw, #"test", <keyword-syntax-symbol-fragment>);

  let sym = read-fragment("#\"hello world\"");
  verify-literal(sym, #"hello world", <symbol-syntax-symbol-fragment>);
end test symbol-literal-test;

define suite literal-test-suite ()
  test boolean-literal-test;
  test character-literal-test;
  test integer-literal-test;
  test ratio-literal-test;
  test string-literal-test;
  test symbol-literal-test;
end suite literal-test-suite;
