Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define not-inline function verify-fragment-is-99 (text)
  let f = read-fragment(text);
  assert-true(f, format-to-string("no fragment was read from %=", text));
  verify-literal(f, 99, <integer-fragment>);
end function;

define test test-line-comment ()
  verify-fragment-is-99("// ignore\n99");
end test;

define test test-multi-line-comment ()
  verify-fragment-is-99("/* ignore */99/* ignore again */");
end test;

define test test-nested-multi-line-comment ()
  verify-fragment-is-99("/* ignore /* here too */ */99");
end test;

// DRM: "A single-line comment may appear within a delimited comment;
// occurrences of slash-star or star-slash within the single line comment are
// ignored."

define test test-delimited-comment-with-nested-line-comment ()
  assert-false(read-fragment("/* // */99"));
  verify-fragment-is-99("/* // */\n*/99");   // 1st */ should be ignored
  verify-fragment-is-99("/* // /*\n*/99");   // 2nd /* should be ignored
end test;

define test test-delimited-comment-with-string-nested-comments ()
  verify-fragment-is-99("/* \"*/\" */99");   // Ignore */ inside string.
  verify-fragment-is-99("/* \"/*\" */99");   // Ignore /* inside string.
  verify-fragment-is-99("/* \"//\" */99");   // Ignore // inside string.
  verify-fragment-is-99("/* \"  \n*/99");    // Incomplete string ended by newline.
end test;

define suite comments-test-suite ()
  test test-line-comment;
  test test-multi-line-comment;
  test test-nested-multi-line-comment;
  test test-delimited-comment-with-nested-line-comment;
  test test-delimited-comment-with-string-nested-comments;
end suite;
