Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define function get-token-as-string
    (source :: <string>) => (token :: <string>, builder :: <fragment-builder>)
  let contents = as(<byte-vector>, source);
  let fragment = get-token(make-lexer(source));
  fragment.fragment-source-location.source-location-string
end function;

define test lex-integer-test ()
  let (tok, kind) = get-token-as-string("123,abc");
  assert-equal(tok, "123");
end test lex-integer-test;

define test lex-string-test ()
  assert-equal(get-token-as-string("\"abc\"...."), "\"abc\"");
end test lex-string-test;

define test lex-multi-line-string-test ()
//  assert-equal(get-token-as-string("\"\"\"abc\"\"\"...."), "\"\"\"abc\"\"\"",
//               "basic triple-quoted string");
  assert-equal("\"\"\"ab\nc\"\"\"", get-token-as-string("\"\"\"ab\nc\"\"\"...."),
               "embedded newline");
  // assert-equal(get-token-as-string("\"\"\"ab\"\nc\"\"\""), "\"\"\"ab\"\nc\"\"\"",
  //              "embedded double quote before newline");
  // assert-equal(get-token-as-string("\"\"\"ab\"\"\nc\"\"\""), "\"\"\"ab\"\"\nc\"\"\"",
  //              "two embedded double quotes before newline");
  // assert-equal(get-token-as-string("\"\"\"ab\"\n \"\nc\"\"\""), "\"\"\"ab\"\n \"\nc\"\"\"",
  //              "embedded double quotes on different lines");
end test lex-multi-line-string-test;

define test test-otherwise-transition ()
  expect-condition(<error>,
                   make-transition-table(vector(#(#"otherwise" . #"bar"),
                                                #(#"otherwise" . #"baz"))),
                   "only one otherwise clause is allowed");

  expect-condition(<error>,
                   make-transition-table(vector(pair($full-character-set, #"orange"),
                                                #(#"otherwise" . #"pear"))),
                   "otherwise had no effect");

  let transitions
    = make-transition-table(vector(#('t' . #"tiger"),
                                   #('j' . #"jaguar"),
                                   #(#"otherwise" . #"lion")));
  // spot checks...
  expect-false(transitions[0],
               "\\0 is not part of the valid Dylan source code character set");
  expect-equal(#"lion", transitions[as(<integer>, '\t')]);
  expect-equal(#"lion", transitions[128]);
  expect-equal(#"lion", transitions[255]);
  expect-equal(#"tiger", transitions[as(<integer>, 't')]);
  expect-equal(#"jaguar", transitions[as(<integer>, 'j')]);
end test;

define test test-end-of-file-handling ()
  assert-instance?(<eof-marker>, get-token(make-lexer("  ")));
  assert-instance?(<eof-marker>, get-token(make-lexer("")));
  // This is a special case because according to the state machine the leading / in /* is
  // an accepting state. There is now code in the lexer to prevent that.
  assert-signals(<invalid-token>, get-token(make-lexer("/* ")));
end test;

define suite dfmc-reader-test-suite ()
  suite literal-test-suite;
  suite comments-test-suite;
  suite expressions-test-suite;
  test lex-integer-test;
  test lex-string-test;
  test lex-multi-line-string-test;
  test test-otherwise-transition;
  test test-end-of-file-handling;
end suite;
