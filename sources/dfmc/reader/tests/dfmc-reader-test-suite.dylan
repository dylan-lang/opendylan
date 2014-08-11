Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

// define function get-token-as-string
//     (source :: <string>) => (token :: <string>, kind)
//   local method do-nothing(#rest _) end;
//   let contents = as(<byte-vector>, source);
//   let (pos, result-kind, result-start, result-end, unexpected-eof, lnum, lstart)
//     = get-token-1($initial-state, contents, do-nothing, do-nothing);
//   as(<string>, copy-sequence(contents, end: result-end))
// end function get-token-as-string;

// define test lex-integer-test ()
//   assert-equal(get-token-as-string("123,abc"), "123");
// end;

// define test lex-multi-line-string-test ()
//   assert-equal(get-token-as-string("\"\"\"abc\"\"\"...."), "abc");
// end test lex-multi-line-string-test;

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
end test skip-multi-line-comment-test;

define suite dfmc-reader-test-suite ()
  suite literal-test-suite;
  suite comments-test-suite;
  suite expressions-test-suite;
  test skip-multi-line-comment-test;
  // test lex-integer-test;
  // test lex-multi-line-string-test;
end suite dfmc-reader-test-suite;
