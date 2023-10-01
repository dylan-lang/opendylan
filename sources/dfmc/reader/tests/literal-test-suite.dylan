Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define function verify-literal
    (fragment, value, required-class) => ()
  assert-instance?(required-class, fragment,
                   format-to-string("verify-literal for value %=", value));
  if (instance?(fragment, required-class))
    assert-equal(fragment.fragment-value, value);
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
  // A one line string literal can't contain a literal Newline.
  assert-signals(<invalid-token>, read-fragment("\"\n\""));
  assert-signals(<invalid-token>, read-fragment(#:string:{"\1<b>"}));
end test;

// verify multi-line string
define function verify-mls
    (name, source, want)
  assert-no-errors(read-fragment(source), "%s - parses without error", name);
  let frag = read-fragment(source);
  assert-instance?(<string-fragment>, frag, "%s - is string fragment", name);
  assert-equal(frag.fragment-value, want, "%s - has expected value", name);
end function;

define test string-literal-one-line-test ()
  verify-mls("empty string", #:string:{""""""}, "");

  // Make sure the reader didn't stop at the first pair of double quotes...
  let empty-string-fragment = read-fragment(#:string:{""""""});
  assert-equal(#:string:{""""""},
               source-location-string(fragment-source-location(empty-string-fragment)),
               "entire empty string consumed");

  verify-mls("simple abc",
             #:string:{"""abc"""},  "abc");
  verify-mls("abc with spaces",
             #:string:{""" abc """},  " abc ");
end test;

define test string-literal-multi-line-test ()
  verify-mls("multi-line empty string, no prefix",
             #:string:{"""
"""},
             "");
  verify-mls("multi-line empty string, with prefix",
             #:string:{"""
"""},
             "");
  verify-mls("multi-line one blank line, no prefix",
             #:string:{"""

"""},
             "\n");
  verify-mls("leading whitespace relative to end delim retained",
             #:string:{"""
  abc
def
"""},
             "  abc\ndef");
  verify-mls("end delim to right of start delim",
             #:string:{"""
                           abc
                         def
                         """},
             "  abc\ndef");
  verify-mls("whitespace on first line ignored?", // 0x20 = space
             #:string:{"""\<20>\<20>
  abc
def
"""},
             "  abc\ndef");
  // The first blank line below is truly empty and the second one has only the prefix
  // (written as \<20> to avoid editors removing trailing whitespace).
  verify-mls("blank lines retained",
             #:string:{"""

   def
\<20>\<20>\<20>
   """},
             "\ndef\n");
  assert-signals(<error>,
                 read-fragment(#:string:{"""a  (only whitespace allowed after start delim)
abc
"""}),
                 "junk on first line");
  assert-signals(<error>,
                 read-fragment(#:string:{"""
abc
xxx"""}),
                  "junk on last line");
  assert-signals(<error>,
                 read-fragment(#:string:{"""
   abc
  xxx  (this line not indented enough)
   """}),
                 "prefix mismatch non-white");
  // Prefix should be "   " but one line has a literal tab in prefix.
  /* TODO: the literal tab causes a failure due (I presume) to
           https://github.com/dylan-lang/opendylan/issues/425
  check-condition("prefix mismatch whitespace",
                  <error>,
                  read-fragment("\"\"\"\n   aaa\n \t bbb\n   \"\"\""));
  */

  // Check that CRLF and CR are converted to LF.
  verify-mls("eol canonicalized 1",
             "\"\"\"\na\r\nc\n\"\"\"",
             "a\nc");
  verify-mls("eol canonicalized 2",
             "\"\"\"\na\rc\n\"\"\"",
             "a\nc");
  verify-mls("eol canonicalized 3",
             "\"\"\"\r\na\n\rc\r\n\"\"\"",
             "a\n\nc");

  let char = curry(as, <character>);
  verify-mls("all escape sequences",
             #:string:{"""\a\b\e\f\n\r\t\0\'\"\\"""},
             map-as(<string>, char,
                    #('\a', '\b', '\e', '\f', '\n', '\r', '\t', '\0', '\'', '\"', '\\')));
  verify-mls("basic hex escaping",
             #:string:{"""z\<9f>z"""},
             map-as(<string>, char, #('z', #x9f, 'z')));
  // We can't handle character codes > 255 yet, but the leading zeros shouldn't
  // confuse the reader.
  verify-mls("hex escape with leading zeros",
             #:string:{"""z\<009f>z"""},
             map-as(<string>, char, #('z', #x9f, 'z')));

  assert-signals(<invalid-token>,
                 read-fragment(#:string:{"""\1<b>"""}),
                 "invalid escape sequence");

  verify-mls("one line",
             #:string:{
"""
abc
"""},
                  "abc");
  verify-mls("one line with prefix",
             #:string:{
  """
  abc
  """},
             "abc");
  verify-mls("two lines",
             #:string:{
"""
abc
def
"""},
             "abc\ndef");
  verify-mls("two lines with prefix",
             #:string:{
  """
  abc
  def
  """},
             "abc\ndef");
  verify-mls("empty line at start",
             #:string:{
"""

abc
"""},
             "\nabc");
  verify-mls("two empty lines at start",
             #:string:{
"""


abc
"""},
             "\n\nabc");
  verify-mls("one empty line",
             #:string:{
"""

"""},
             "\n");
  verify-mls("one empty line with prefix",
             #:string:{
  """
\<20>\<20>
  """},
             "\n");
  verify-mls("empty line at end",
             #:string:{
"""
abc

"""},
             "abc\n");
  verify-mls("two empty lines at end",
             #:string:{
"""
abc


"""},
                  "abc\n\n");
  verify-mls("empty lines at start and end",
             #:string:{
"""

abc

"""},
             "\nabc\n");
  verify-mls("two empty lines",
             #:string:{
"""


"""},
             "\n\n");
  verify-mls("three empty lines",
             #:string:{
"""



"""},
             "\n\n\n");
  verify-mls("three empty lines at end",
             #:string:{
"""
abc



"""},
             "abc\n\n\n");
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
  // ensure that it is ignored. We replace the X after the fact, to avoid
  // confusing Emacs.
  let s = #:string:{#r"""\a\b\e\f\n\r\t\0\'\\\<X"""};
  s[s.size - 4] := '\\';
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
  let sym = read-fragment("#\"\"\"a\nb\"\"\"");
  verify-literal(sym, #"a\nb", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, "#\"a\nb\"");

  // CRLF -> LF?
  let sym = read-fragment("#\"\"\"c\r\nd\"\"\"");
  verify-literal(sym, #"c\nd", <symbol-syntax-symbol-fragment>);
  verify-presentation(sym, "#\"c\nd\"");

  // CR -> LF?
  let sym = read-fragment("#\"\"\"e\rf\"\"\"");
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
  test string-literal-multi-line-test;
  test string-literal-one-line-test;
  test string-literal-raw-multi-line-test;
  test string-literal-raw-one-line-test;
  test string-literal-test;
  test symbol-literal-test;
  test vector-literal-test;
  test hash-literal-test;
end suite literal-test-suite;
