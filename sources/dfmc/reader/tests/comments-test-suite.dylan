Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define test test-line-comment ()
  let f = read-fragment("// ignore\n1");
  verify-literal(f, 1, <integer-fragment>);
end test;

define test test-multi-line-comment ()
  let f = read-fragment("/* ignore */1/* ignore again */");
  verify-literal(f, 1, <integer-fragment>);
end test;

define test test-nested-multi-line-comment ()
  let f = read-fragment("/* ignore /* here too */ */1");
  verify-literal(f, 1, <integer-fragment>);
end test;

define suite comments-test-suite ()
  test test-line-comment;
  test test-multi-line-comment;
  test test-nested-multi-line-comment;
end suite;
