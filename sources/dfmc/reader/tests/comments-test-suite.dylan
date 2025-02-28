Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define test line-comment-test ()
  let f = read-fragment("// ignore\n1");
  verify-literal(f, 1, <integer-fragment>);
end test line-comment-test;

define test multi-line-comment-test ()
  let f = read-fragment("/* ignore */1/* ignore again */");
  verify-literal(f, 1, <integer-fragment>);
end test multi-line-comment-test;

define test nested-multi-line-comment-test ()
  let f = read-fragment("/* ignore /* here too */ */1");
  verify-literal(f, 1, <integer-fragment>);
end test nested-multi-line-comment-test;

define test test-delimited-comment-with-nested-line-comment ()
  let f = get-token(make-lexer("/*\n// */\n*/2"));
  assert-equal(2, f.fragment-value,
               "delimited comment with \"// */\" in it");

  let f = get-token(make-lexer("/*\n// /* \n*/2"));
  assert-equal(2, f.fragment-value,
               "delimited comment with \"// /*\" in it");
end test;

define test test-delimited-comment-with-embedded-slash ()
  // This case showed up during attempted boostrapping. A slash anywhere in the comment
  // exited the delimited-string state machine because there was no #"otherwise" clause.
  let fragment = read-fragment("/* /4 */ define constant c = 16;");
  assert-instance?(<list-definition-fragment>, fragment);
end test;

define test test-delimited-comment-all-stars ()
  assert-instance?(<integer-fragment>, read-fragment("/**/ 2"),
                   "even number of stars");
  assert-instance?(<integer-fragment>, read-fragment("/***/ 3"),
                   "odd number of stars");
end test;

define suite comments-test-suite ()
  test line-comment-test;
  test multi-line-comment-test;
  test nested-multi-line-comment-test;
  test test-delimited-comment-with-nested-line-comment;
  test test-delimited-comment-with-embedded-slash;
  test test-delimited-comment-all-stars;
end suite comments-test-suite;
