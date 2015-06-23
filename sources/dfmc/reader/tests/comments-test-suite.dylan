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

define suite comments-test-suite ()
  test line-comment-test;
  test multi-line-comment-test;
  test nested-multi-line-comment-test;
end suite comments-test-suite;
