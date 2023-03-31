Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define function get-token-as-string
    (source :: <string>, start :: <integer>) => (token :: <string>, kind)
  let contents = as(<byte-vector>, source);
  let (kind, bpos, bline, bcol, epos, eline, ecol, unexpected-eof?, current-line, line-start)
    = get-token-1($initial-state, contents, start, contents.size, 0, 0);
  values(as(<string>, copy-sequence(contents, start: bpos, end: epos)),
         kind)
end function get-token-as-string;

define test lex-integer-test ()
  let (tok, kind) = get-token-as-string("123,abc", 0);
  assert-equal(tok, "123");
end test lex-integer-test;

define test lex-string-test ()
  assert-equal(get-token-as-string("\"abc\"....", 0), "\"abc\"");
end test lex-string-test;

define test lex-multi-line-string-test ()
  assert-equal(get-token-as-string("\"\"\"abc\"\"\"....", 0), "\"\"\"abc\"\"\"",
               "basic triple-quoted string");
  assert-equal(get-token-as-string("\"\"\"ab\nc\"\"\"....", 0), "\"\"\"ab\nc\"\"\"",
               "embedded newline");
  assert-equal(get-token-as-string("\"\"\"ab\"\nc\"\"\"", 0), "\"\"\"ab\"\nc\"\"\"",
               "embedded double quote before newline");
  assert-equal(get-token-as-string("\"\"\"ab\"\"\nc\"\"\"", 0), "\"\"\"ab\"\"\nc\"\"\"",
               "two embedded double quotes before newline");
  assert-equal(get-token-as-string("\"\"\"ab\"\n \"\nc\"\"\"", 0), "\"\"\"ab\"\n \"\nc\"\"\"",
               "embedded double quotes on different lines");
end test lex-multi-line-string-test;

define test skip-multi-line-comment-test ()
  local method skip (source, bpos, expected-epos, expected-lines-skipped,
                     expected-line-start, name)
      let (epos, lines-skipped, line-start)
        = skip-multi-line-comment(as(<byte-vector>, source), source.size, bpos);
      assert-equal(epos, expected-epos, name);
      assert-equal(lines-skipped, expected-lines-skipped, name);
      assert-equal(line-start, expected-line-start, name);
    end;
  // Note that "/*" has already been read when skip-multi-line-comment is
  // called by the lexer.
  skip(" */", 0, 3, 0, #f, "simple case");
  skip("/*/**/*/xx", 2, 8, 0, #f, "nested /* */ comment");
  skip("/*\n*/abc", 2, 5, 1, 3, "nested newline");
  skip("/*a\n //abc\n*/ ", 2, 13, 2, 11, "nested // comment");
  skip("/*..\n// /*\n*/", 2, 13, 2, 11, "nested // with /* in it");
end test;

define suite dfmc-reader-test-suite ()
  suite literal-test-suite;
  suite comments-test-suite;
  suite expressions-test-suite;
  test skip-multi-line-comment-test;
  test lex-integer-test;
  test lex-string-test;
  test lex-multi-line-string-test;
end suite;

