Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

// TODO: this can just use get-token and i think there's already a function to get the
// fragment string.
define function get-token-as-string
    (source :: <string>) => (token :: <string>, builder :: <fragment-builder>)
  let contents = as(<byte-vector>, source);
  let lexer = make-lexer(source);
  let (builder, bpos, bline, bcol, epos, eline, ecol, unexpected-eof?, line-start)
    = get-token-1(lexer);
  values(as(<string>, copy-sequence(contents, start: bpos, end: epos)),
         builder)
end function get-token-as-string;

define test lex-integer-test ()
  let (tok, kind) = get-token-as-string("123,abc");
  assert-equal(tok, "123");
end test lex-integer-test;

define test lex-string-test ()
  assert-equal(get-token-as-string("\"abc\"...."), "\"abc\"");
end test lex-string-test;

define test lex-multi-line-string-test ()
  assert-equal(get-token-as-string("\"\"\"abc\"\"\"...."), "\"\"\"abc\"\"\"",
               "basic triple-quoted string");
  assert-equal(get-token-as-string("\"\"\"ab\nc\"\"\"...."), "\"\"\"ab\nc\"\"\"",
               "embedded newline");
  assert-equal(get-token-as-string("\"\"\"ab\"\nc\"\"\""), "\"\"\"ab\"\nc\"\"\"",
               "embedded double quote before newline");
  assert-equal(get-token-as-string("\"\"\"ab\"\"\nc\"\"\""), "\"\"\"ab\"\"\nc\"\"\"",
               "two embedded double quotes before newline");
  assert-equal(get-token-as-string("\"\"\"ab\"\n \"\nc\"\"\""), "\"\"\"ab\"\n \"\nc\"\"\"",
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

define test test-otherwise-transition ()
  expect-condition(<error>,
                   make-transition-table(vector(#(#"otherwise" . #"bar"),
                                                #(#"otherwise" . #"baz"))),
                   "> 1 otherwise clause disallowed");

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

define suite dfmc-reader-test-suite ()
  suite literal-test-suite;
  suite comments-test-suite;
  suite expressions-test-suite;
  test skip-multi-line-comment-test;
  test lex-integer-test;
  test lex-string-test;
  test lex-multi-line-string-test;
  test test-otherwise-transition;
end suite;

