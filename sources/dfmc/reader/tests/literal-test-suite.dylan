Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define function verify-literal
    (fragment, want-value, required-class) => ()
  assert-instance?(required-class, fragment,
                   format-to-string("verify-literal for value %=", want-value));
  if (instance?(fragment, required-class))
    assert-equal(want-value, fragment.fragment-value);
  end;
end function;

define function verify-presentation
    (f :: <fragment>, presentation :: <string>) => ()
  let stream = make(<string-stream>, direction: #"output");
  present-fragments(list(f), stream);
  assert-equal(stream.stream-contents, presentation,
               format-to-string("verify-presentation for %=", presentation));
end function;

define test binary-integer-literal-test ()
  verify-literal(read-fragment("#b10"), 2, <integer-fragment>);
  verify-literal(read-fragment("#b010"), 2, <integer-fragment>);
  verify-literal(read-fragment("#b1111_0000"), 240, <integer-fragment>);
  assert-false(read-fragment("#b10_"));
  assert-signals(<invalid-token>, read-fragment("#b_10"));
  assert-signals(<invalid-token>, read-fragment("#b2"));
  assert-signals(<invalid-token>, read-fragment("#b + 1"));
end test binary-integer-literal-test;

define test boolean-literal-test ()
  let t = read-fragment("#t");
  verify-literal(t, #t, <true-fragment>);
  verify-presentation(t, "#t");

  let f = read-fragment("#f");
  verify-literal(f, #f, <false-fragment>);
  verify-presentation(f, "#f");
end test boolean-literal-test;

define test character-literal-test ()
  let f = read-fragment("'n'");
  verify-literal(f, 'n', <character-fragment>);
  verify-presentation(f, "'n'");

  let f = read-fragment("'N'");
  assert-equal(f.fragment-value, 'N');
  verify-presentation(f, "'N'");

  let f = read-fragment("'\\\\'");
  assert-equal(f.fragment-value, '\\');
  verify-presentation(f, "'\\\\'");

  let f = read-fragment("'\\n'");
  assert-equal(f.fragment-value, '\n');
  verify-presentation(f, "'\\n'");

  let f = read-fragment("'\\<1>'");
  assert-equal(as(<integer>, f.fragment-value), 1);
  verify-presentation(f, "'\\<1>'");

  let f = read-fragment("'\\<01>'");
  assert-equal(as(<integer>, f.fragment-value), 1);
  verify-presentation(f, "'\\<1>'");

  let f = read-fragment("'\\<fF>'");
  assert-equal(as(<integer>, f.fragment-value), 255);
  verify-presentation(f, "'\\<FF>'");

  assert-signals(<invalid-token>, read-fragment("'21'"));
  assert-signals(<invalid-token>, read-fragment("'\\j'"));
  assert-signals(<invalid-token>, read-fragment("'\\<gg>'"));
  assert-signals(<character-code-too-large>, read-fragment("'\\<fff>'"));
end test character-literal-test;

define test decimal-integer-literal-test ()
  let f = read-fragment("123");
  verify-literal(f, 123, <integer-fragment>);
  verify-presentation(f, "123");

  let f = read-fragment("-456");
  verify-literal(f, -456, <integer-fragment>);
  verify-presentation(f, "-456");

  let f = read-fragment("+789");
  verify-literal(f, 789, <integer-fragment>);
  verify-presentation(f, "789");

  let f = read-fragment("1_000_000");
  verify-literal(f, 1000000, <integer-fragment>);
  verify-presentation(f, "1000000");

  let f = read-fragment("1_000_000");
  verify-literal(f, 1000000, <integer-fragment>);
  verify-presentation(f, "1000000");

  let f = read-fragment("1_2_3_4");
  verify-literal(f, 1234, <integer-fragment>);
  verify-presentation(f, "1234");

  assert-false(read-fragment("100_"));  // No trailing underscore.
  assert-false(read-fragment("1__00")); // No multi-underscore runs.
  assert-false(read-fragment("_100"));  // No leading underscore.
end test decimal-integer-literal-test;

define test float-literal-test ()
  verify-literal(read-fragment("3.0"),   3.0, <float-fragment>);
  verify-literal(read-fragment("-3.0"), -3.0, <float-fragment>);
  verify-literal(read-fragment("3e0"),   3.0, <float-fragment>);
  verify-literal(read-fragment("3.0e0"), 3.0, <float-fragment>);
  verify-literal(read-fragment("3.e0"),  3.0, <float-fragment>);

  verify-literal(read-fragment(".5"),   0.5, <float-fragment>);
  verify-literal(read-fragment("-.5"), -0.5, <float-fragment>);
  verify-literal(read-fragment("+.5"),  0.5, <float-fragment>);

  verify-literal(read-fragment("6."),   6.0, <float-fragment>);
  verify-literal(read-fragment("+6."),  6.0, <float-fragment>);
  verify-literal(read-fragment("-6."), -6.0, <float-fragment>);

  verify-literal(read-fragment("3.0s0"),   3.0, <float-fragment>);
  verify-literal(read-fragment("30.0s-1"), 3.0, <float-fragment>);
  verify-literal(read-fragment("3.0d0"),   3.0, <float-fragment>);
  verify-literal(read-fragment("-3.0d0"), -3.0, <float-fragment>);
  verify-literal(read-fragment("-3.0d3"), -3000.0, <float-fragment>);
  verify-literal(read-fragment("-3d3"),   -3000.0, <float-fragment>);

  // Underscores
  verify-literal(read-fragment("1_23.45"),     123.45, <float-fragment>);
  verify-literal(read-fragment("123.4_5"),     123.45, <float-fragment>);
  verify-literal(read-fragment("1_23.45e1_0"), 123.45e10, <float-fragment>);
  verify-literal(read-fragment("1_23.45e+1_0"), 123.45e10, <float-fragment>);
  verify-literal(read-fragment("1_23.45e-1_0"), 123.45e-10, <float-fragment>);
  verify-literal(read-fragment("1_23.45d1_0"), 123.45d10, <float-fragment>);

  // Underscore only allowed between decimal digits.
  assert-false(read-fragment("_123.45e10"));
  assert-false(read-fragment("123_.45e10"));
  assert-false(read-fragment("123._45e10"));
  assert-false(read-fragment("123.45_e10"));
  assert-false(read-fragment("123.45e_10"));
  assert-false(read-fragment("123.45e10_"));
  assert-false(read-fragment("+_123.45e10"));

  // Multiple underscores not allowed
  assert-false(read-fragment("1__23.45d10"));
  assert-false(read-fragment("123.45d1__0"));
  assert-false(read-fragment("123.4__5d10"));
end test float-literal-test;

define test hexadecimal-integer-literal-test ()
  verify-literal(read-fragment("#xff"), 255, <integer-fragment>);
  verify-literal(read-fragment("#xdead_beef"), 3735928559, <integer-fragment>);
  verify-literal(read-fragment("#xb_e_e_f"), 48879, <integer-fragment>);

  assert-signals(<invalid-token>, read-fragment("#x_beef"));  // No leading underscore.
  assert-false(read-fragment("#xbe__ef")); // No multi-underscore runs.
  assert-false(read-fragment("#xbeef_"));  // No trailing underscore.
  assert-signals(<invalid-token>, read-fragment("#xh"));
  assert-signals(<invalid-token>, read-fragment("#x + 1"));
end test hexadecimal-integer-literal-test;

define test list-literal-test ()
  let f = read-fragment("#()");
  verify-literal(f, #(), <proper-list-fragment>);
  verify-presentation(f, "#()");

  let f = read-fragment("#('a', 'b')");
  verify-literal(f, #('a', 'b'), <proper-list-fragment>);
  verify-presentation(f, "#('a', 'b')");

  let f = read-fragment("#(a:, #\"b\")");
  verify-presentation(f, "#(a:, #\"b\")");
end test list-literal-test;

define test octal-integer-literal-test ()
  verify-literal(read-fragment("#o70"), 56, <integer-fragment>);
  verify-literal(read-fragment("#o3_444"), 1828, <integer-fragment>);

  assert-signals(<invalid-token>, read-fragment("#o_70"));  // No leading underscore.
  assert-false(read-fragment("#o70_"));                     // No trailing underscore.
  assert-false(read-fragment("#o7__0")); // No multi-underscore runs.
  assert-signals(<invalid-token>, read-fragment("#o8"));
  assert-signals(<invalid-token>, read-fragment("#o + 1"));
end test octal-integer-literal-test;

define test pair-literal-test ()
  let f = read-fragment("#(1 . 2)");
  verify-literal(f, #(1 . 2), <improper-list-fragment>);
  verify-presentation(f, "#(1 . 2)");

  let f = read-fragment("#(a: . #\"b\")");
  verify-presentation(f, "#(a: . #\"b\")");
end test pair-literal-test;

define test ratio-literal-test ()
  assert-signals(<ratios-not-supported>, read-fragment("1/2"));
end test ratio-literal-test;

// Define #:string: syntax.
define function string-parser (s) s end;

define test string-literal-test ()
  verify-literal(read-fragment(#:string:{""}), "", <string-fragment>);
  verify-literal(read-fragment(#:string:{"abc"}), "abc", <string-fragment>);
  verify-literal(read-fragment(#:string:{"a\nc"}), "a\nc", <string-fragment>);

  let char = curry(as, <character>);
  // One of every escape sequence. "\a\b\e\f\n\r\t\0\'\"\\"
  verify-literal(read-fragment(#:string:{"\a\b\e\f\n\r\t\0\'\"\\"}),
                 map-as(<string>, char, #('\a', '\b', '\e', '\f', '\n', '\r',
                                          '\t', '\0', '\'', '\"', '\\')),
                 <string-fragment>);

  // Basic hex escaping.
  verify-literal(read-fragment(#:string:{"z\<9f>z"}),
                 map-as(<string>, char, #('z', #x9f, 'z')),
                 <string-fragment>);
  // We can't handle character codes > 255 yet, but the leading zeros shouldn't
  // confuse the reader.
  verify-literal(read-fragment(#:string:{"z\<009f>z"}),
                 map-as(<string>, char, #('z', #x9f, 'z')),
                 <string-fragment>);
  assert-signals(<invalid-token>,
                 read-fragment(#:string:{"\1<b>"}),
                 "invalid hex escape");
  assert-signals(<invalid-token>,
                 read-fragment("\"\n\""),
                 "Newline not allowed in non-multi-line string literal");
  assert-signals(<invalid-string-literal>,
                 read-fragment("\"\t\""),
                 "Tab not allowed in string literal");
end test;

// Note: one line as in one line of source code not as in having no newline characters.
define test test-triple-quoted-one-line-strings ()
  let frag0 = read-fragment(#:string:{""""""});
  expect-equal("", frag0.fragment-value);
  // Make sure the reader didn't stop at the first pair of double quotes...
  assert-equal(#:string:{""""""},
               source-location-string(fragment-source-location(frag0)),
               "entire empty string consumed");
  let frag1 = read-fragment(#:string:{"""abc"""});
  assert-equal("abc", frag1.fragment-value);
  let frag2 = read-fragment(#:string:{""" abc """});
  assert-equal(" abc ", frag2.fragment-value);
  let frag3 = read-fragment(#:string:{"""abc\ndef"""});
  assert-equal("abc\ndef", frag3.fragment-value);
end test;

define test test-multi-line-string-basics ()
  let frag1 = read-fragment(#:string:{
"""
abc
"""});
  assert-equal("abc", frag1.fragment-value);

  let frag2 = read-fragment(#:string:{
  """
  abc
  """});
  assert-equal("abc", frag2.fragment-value);

  let frag3 = read-fragment(#:string:{
"""
abc
def
"""});
  assert-equal("abc\ndef", frag3.fragment-value);

  let frag4 = read-fragment(#:string:{
  """
  abc
  def
  """});
  assert-equal("abc\ndef", frag4.fragment-value);

  let frag5 = read-fragment(#:string:{
"""



"""});
  assert-equal("\n\n\n", frag5.fragment-value);
end test;

define test test-multi-line-empty-strings ()
  let frag1 = read-fragment(#:string:{"""
"""});
  assert-equal("", frag1.fragment-value, "multi-line empty string, no prefix");
  let frag2 = read-fragment(#:string:{"""
                                      """});
  assert-equal("", frag2.fragment-value, "multi-line empty string, with prefix");
end test;

define test test-multi-line-string-with-blank-lines ()
  let frag1 = read-fragment(#:string:{"""

"""});
  assert-equal("\n", frag1.fragment-value, "blank line, no prefix");

  // Not using #:string: here to avoid having trailing whitespace, which could be removed
  // by editors.
  let frag2 = read-fragment("\"\"\"\n  def\n  \n  \"\"\"");
  assert-equal("def\n\n", frag2.fragment-value, "blank line with prefix");
end test;

define test test-multi-line-string-whitespace-relative-to-delimiters ()
  let frag1 = read-fragment(#:string:{"""
  abc
def
"""});
  assert-equal("  abc\ndef", frag1.fragment-value);
  let frag2 = read-fragment(#:string:{"""
    xxx
  yyy
  """});
  assert-equal("  xxx\nyyy", frag2.fragment-value);
end test;

define test test-multi-line-string-delimiter-rules ()
  // Whitespace following the opening """ and preceding the first \n should be ignored.
  // Since editors sometimes remove trailing whitespace we write this using escape
  // sequences instead of with #:string:.
  let frag1 = read-fragment("\"\"\"   \n  abc\n  \"\"\"");
  assert-equal("abc", frag1.fragment-value);
  assert-signals(<invalid-string-literal>,
                 read-fragment(#:string:{"""a  (only whitespace allowed after start delim)
abc
"""}),
                 "junk on first line");
  assert-signals(<invalid-string-literal>,
                 read-fragment(#:string:{"""
abc
xxx"""}),
                  "junk on last line");
  assert-signals(<invalid-string-literal>,
                 read-fragment(#:string:{"""
   abc
  xxx  (this line not indented enough)
   """}),
                 "prefix mismatch non-white");
end test;

define test test-multi-line-string-eol-handling ()
  // Check that CRLF and CR are converted to LF.
  let frag1 = read-fragment("\"\"\"\na\r\nc\n\"\"\"");
  assert-equal("a\nc", frag1.fragment-value);
  let frag2 = read-fragment("\"\"\"\na\rc\n\"\"\"");
  assert-equal("a\nc", frag2.fragment-value);
  let frag3 = read-fragment("\"\"\"\r\na\n\rc\r\n\"\"\"");
  assert-equal("a\n\nc", frag3.fragment-value);
end test;

define test test-multi-line-string-escaping ()
  let frag1 = read-fragment(#:string:{"""\a\b\e\f\n\r\t\0\'\"\\"""});
  // https://github.com/dylan-lang/opendylan/issues/425
  assert-equal(map-as(<string>, identity,
                      #('\a', '\b', '\e', '\f', '\n', '\r', '\t', '\0', '\'', '\"', '\\')),
               frag1.fragment-value,
               "one of each standard escape sequence");
  let char = curry(as, <character>);
  let frag2 = read-fragment(#:string:{"""z\<9f>z"""});
  assert-equal(map-as(<string>, char, #('z', #x9f, 'z')),
               frag2.fragment-value,
               "basic hex escaping");

  // We can't handle character codes > 255 yet, but the leading zeros shouldn't
  // confuse the reader.
  let frag3 = read-fragment(#:string:{"""z\<009f>z"""});
  assert-equal(map-as(<string>, char, #('z', #x9f, 'z')),
               frag3.fragment-value,
               "hex escape with leading zeros");

  assert-signals(<invalid-token>,
                 read-fragment(#:string:{"""\1<b>"""}),
                 "invalid escape sequence");
  let frag4 = read-fragment(#:string:{"""
  \nxxx
  """});
  assert-equal("\nxxx", frag4.fragment-value,
               "newline escape sequence, xxx longer than prefix");
  let frag5 = read-fragment(#:string:{"""
  \nxx
  """});
  assert-equal("\nxx", frag5.fragment-value,
               "newline escape sequence, xx same length as prefix");
  let frag6 = read-fragment(#:string:{"""
  \nx
  """});
  assert-equal("\nx", frag6.fragment-value,
               "Newline escape sequences, x shorter than prefix");
end test;

define test string-literal-raw-one-line-test ()
  let f = read-fragment(#:string:{#r""});
  verify-literal(f, "", <string-fragment>);
  let source = source-location-string(fragment-source-location(f));
  assert-equal(#:string:{#r""}, source);

  verify-literal(read-fragment(#:string:{#r"abc"}), "abc", <string-fragment>);
  verify-literal(read-fragment(#:string:{#r"a\c"}), "a\\c", <string-fragment>);

  // All escape codes ignored?  \ precedes the terminating double quote to
  // ensure that it is ignored. We replace the X after the fact, to avoid
  // confusing Emacs.
  let s = #:string:{#r"\a\b\e\f\n\r\t\0\'\\\<X"};
  s[s.size - 2] := '\\';
  verify-literal(read-fragment(s),
                 concatenate(#:string:{\a\b\e\f\n\r\t\0\'\\\<}, "\\"),
                 <string-fragment>);
end test;

define test string-literal-raw-multi-line-test ()
  let f = read-fragment(#:string:{#r""""""});
  verify-literal(f, "", <string-fragment>);
  let source = source-location-string(fragment-source-location(f));
  assert-equal(#:string:{#r""""""}, source);

  verify-literal(read-fragment(#:string:{#r"""abc"""}), "abc", <string-fragment>);
  verify-literal(read-fragment(#:string:{#r"""a\c"""}), "a\\c", <string-fragment>);
  verify-literal(read-fragment(#:string:{#r"""a""c"""}), "a\"\"c", <string-fragment>);

  // All escape codes ignored?  \ precedes the terminating double quotes to
  // ensure that it is ignored.
  let s = #:string:{#r"""\a\b\e\f\n\r\t\0\'\\\<X"""};
  s[s.size - 4] := '\\';        // to avoid confusing emacs
  verify-literal(read-fragment(s),
                 concatenate(#:string:{\a\b\e\f\n\r\t\0\'\\\<}, "\\"),
                 <string-fragment>);
end test;

define test symbol-literal-test ()
  let kw = read-fragment("test:");
  verify-literal(kw, #"test", <keyword-syntax-symbol-fragment>);
  verify-presentation(kw, "test:");

  let sym = read-fragment("#\"hello world\"");
  verify-literal(sym, #"hello world", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, "#\"hello world\"");

  // Basic multi-line syntax
  let sym = read-fragment(#:string:{#"""a"""});
  verify-literal(sym, #"a", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, #:string:{#"a"});

  // Literal Newline accepted and preserved?
  let sym = read-fragment("#\"\"\"\na\nb\n\"\"\"");
  verify-literal(sym, #"a\nb", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, "#\"a\nb\"");

  // CRLF -> LF?
  let sym = read-fragment("#\"\"\"\nc\r\nd\n\"\"\"");
  verify-literal(sym, #"c\nd", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, "#\"c\nd\"");

  // CR -> LF?
  let sym = read-fragment("#\"\"\"\ne\rf\n\"\"\"");
  verify-literal(sym, #"e\nf", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, "#\"e\nf\"");
end test symbol-literal-test;

define test vector-literal-test ()
  let f = read-fragment("#[]");
  verify-literal(f, #(), <vector-fragment>);
  verify-presentation(f, "#[]");

  let f = read-fragment("#[-1, 2]");
  verify-literal(f, #(-1, 2), <vector-fragment>);
  verify-presentation(f, "#[-1, 2]");

  let f = read-fragment("#[\"a\", b:]");
  verify-literal(f, #("a", #"b"), <vector-fragment>);
  verify-presentation(f, "#[\"a\", b:]");
end test vector-literal-test;

define test hash-literal-test ()
  // End delimiter is escaped so the hash literal is not terminated. This used
  // to crash the compiler, unless '}' appeared somewhere later in the source
  // record.
  let source = "#:foo:{\\}";
  assert-false(read-fragment(source));
end test;

define suite literal-test-suite ()
  test binary-integer-literal-test;
  test boolean-literal-test;
  test character-literal-test;
  test decimal-integer-literal-test;
  test float-literal-test;
  test hexadecimal-integer-literal-test;
  test list-literal-test;
  test octal-integer-literal-test;
  test pair-literal-test;
  test ratio-literal-test;
  test string-literal-raw-multi-line-test;
  test string-literal-raw-one-line-test;
  test string-literal-test;
  test symbol-literal-test;
  test vector-literal-test;
  test hash-literal-test;
  test test-multi-line-string-eol-handling;
  test test-multi-line-string-basics;
  test test-multi-line-string-escaping;
  test test-multi-line-string-delimiter-rules;
  test test-multi-line-string-whitespace-relative-to-delimiters;
  test test-multi-line-string-with-blank-lines;
  test test-multi-line-empty-strings;
end suite literal-test-suite;
