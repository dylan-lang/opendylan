Module: common-dylan-test-suite
Synopsis: Tests for ../format.dylan


define test test-character-to-integer ()
  for (char in "0123456789",
       code from 0)
    assert-equal(code, character-to-integer(char),
                 format-to-string("code for %c is %d", char, code));
  end;
  for (char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
       code from 10)
    assert-equal(code, character-to-integer(char),
                 format-to-string("code for %c is %d", char, code));
    assert-equal(code, character-to-integer(as-lowercase(char)),
                 format-to-string("code for %c is %d", as-lowercase(char), code));
  end;
end test;

define test test-string-to-machine-word ()
  local method mword (int)
          as(<machine-word>, int)
        end;
  assert-signals(<error>, string-to-machine-word(""));
  assert-signals(<error>, string-to-machine-word("G"));

  let (value, epos) = string-to-machine-word("123G");
  assert-equal(mword(#x123), value);
  assert-equal(3, epos);

  assert-equal(mword(#x123), string-to-machine-word("G0123G", start: 1));
  assert-equal(mword(#x12), string-to-machine-word("G0123G", start: 1, end: 4));
  assert-equal(mword(#x9abcdef), string-to-machine-word("9abcdef"));
end test;

//---*** NOTE: The <double-float> results will have to be changed if
//---*** we ever implement a better printing algorithm to get more digits
define constant $float-string-mappings
  = #(#(0.0,           "0.0"),
      #(0.0d0,         "0.0d0"),
      #(1.0,           "1.0000000"),
      #(1.0d0,         "1.0000000d0"),
      #(10.0,          "10.000000"),
      #(10.0d0,        "10.000000d0"),
      #(100.0,         "100.00000"),
      #(100.0d0,       "100.00000d0"),
      #(123456789.0,   "1.2345679s8"),
      #(123456789.0d0, "1.2345678d8"),
      // Added for bug 
      #(1.0d5,         "100000.00d0"),
      #(1.0s6,         "1000000.0"),
      #(1.0d6,         "1000000.0d0"),
      #(1.0d7,         "1.0000000d7"));

define test test-float-to-string
    (expected-to-fail-reason:
       "To be determined. Need all tests to be non-failures for the"
       " test suites to be used in CI to detect regressions.")
  for (float-mapping in $float-string-mappings)
    let float  = float-mapping[0];
    let string = float-mapping[1];
    check-equal(format-to-string("float-to-string(%d)", float),
                float-to-string(float), string)
  end;
  //---*** NOTE: Our runtime should catch 0.0 / 0.0 and signal an invalid
  //---***       float operation error rather than generating a {NaN}.
  check-equal("float-to-string(0.0 / 0.0)",
              float-to-string(0.0 / 0.0),
              "{NaN}");
  check-equal("float-to-string(0.0d0 / 0.0d0)",
              float-to-string(0.0d0 / 0.0d0),
              "{NaN}d0");
  //---*** NOTE: When we implement floating point exception control,
  //---***       replace the above two checks with the following:
/*
  check-equal("float-to-string(0.0 / 0.0)",
              float-to-string(with-floating-exceptions-disabled ()
                                0.0 / 0.0
                              end),
              "{NaN}");
  check-equal("float-to-string(0.0d0 / 0.0d0)",
              float-to-string(with-floating-exceptions-disabled ()
                                0.0d0 / 0.0d0
                              end),
              "{NaN}d0");
  check-equal("float-to-string(1.0 / 0.0)",
              float-to-string(with-floating-exceptions-disabled ()
                                1.0 / 0.0
                              end),
              "+{infinity}");
  check-equal("float-to-string(1.0d0 / 0.0d0)",
              float-to-string(with-floating-exceptions-disabled ()
                                1.0d0 / 0.0d0
                              end),
              "+{infinity}d0");
  check-equal("float-to-string(-1.0 / 0.0)",
              float-to-string(with-floating-exceptions-disabled ()
                                -1.0 / 0.0
                              end),
              "-{infinity}");
  check-equal("float-to-string(-1.0d0 / 0.0d0)",
              float-to-string(with-floating-exceptions-disabled ()
                                -1.0d0 / 0.0d0
                              end),
              "-{infinity}d0");
*/
end test;

define constant $integer-string-mappings
  = #[#[0,     10,  "0"],
      #[1,     10,  "1"],
      #[9,     10,  "9"],
      #[1234,  10,  "1234"],
      #[10,    16,  "A"],
      #[-1,    10,  "-1"],
      #[-9,    10,  "-9"],
      #[-10,   10,  "-10"],
      #[-1234, 10,  "-1234"],
      #[-10,   16,  "-A"]];

define test test-integer-to-string ()
  for (integer-mapping in $integer-string-mappings)
    let integer = integer-mapping[0];
    let base    = integer-mapping[1];
    let string  = integer-mapping[2];
    check-equal(format-to-string("integer-to-string(%d)", integer),
                integer-to-string(integer, base: base), string)
  end;
  check-equal("integer-to-string(10, size: 6)",
              integer-to-string(10, size: 6),
              "000010");
  check-equal("integer-to-string(10, size: 6, fill: ' ')",
              integer-to-string(10, size: 6, fill: ' '),
              "    10");
  check-equal("integer-to-string(127, base: 2, size: 8)",
              integer-to-string(127, base: 2, size: 8),
              "01111111");
  check-equal("integer-to-string(100, base: 36) defaults to uppercase",
              integer-to-string(100, base: 36),
              "2S");
  check-equal("integer-to-string(100, base: 36, lowercase?: #t)",
              integer-to-string(100, base: 36, lowercase?: #t),
              "2s");
  check-no-errors("integer-to-string($minimum-integer)",
                  integer-to-string($minimum-integer));
  check-no-errors("integer-to-string($maximum-integer)",
                  integer-to-string($maximum-integer));
  check-condition("bad base (1) signals error",
                  <error>, integer-to-string(100, base: 1));
  check-condition("bad base (37) signals error",
                  <error>, integer-to-string(100, base: 37));
end test;

define test test-number-to-string ()
  //---*** Fill this in...
end test;


define constant $format-object-mappings
  = vector(vector(10, "10", "10"),
           vector('a', "a", "'a'"),
           vector('Z', "Z", "'Z'"),
           vector(#"symbol", "#\"symbol\""),
           vector(#"symbol", "#\"symbol\""),
           vector(#f, "#f"),
           vector(#t, "#t"),
           vector(<object>, "<object>", "{<class>: <object>}"),
           vector(find-key, "find-key", "{<incremental-generic-function>: find-key}"),
           vector("10", "10", "\"10\""));

define constant $format-complex-object-mappings
  = vector(vector(#(), "size 0"),
           vector(pair(1, 2), "1, 2"),
           vector(range(from: 0, to: 10), "0 to 10"),
           vector(range(from: 10, to: 1, by: -1), "10 to 1 by -1"),
           vector(range(from: 10, by: -1), "10 by -1"),
           vector(make(<array>, dimensions: #(2, 3)), "2 x 3"),
           vector(as(<vector>, #(1, 'a', "Hello")),
                  "1, 'a', \"Hello\""),
           vector(singleton(10), "10"),
           vector(type-union(<integer>, <string>),
                  "<integer>, <string>"),
           vector(type-union(singleton(#f), <string>),
                  "#f, <string>"));

define function test-print-name
    (object, pretty-name :: <string>, unique-name :: <string>)
 => ()
  check-equal(format-to-string("format-to-string(\"%%s\", %s)", unique-name),
              format-to-string("%s", object),
              pretty-name);
  check-equal(format-to-string("format-to-string(\"%%=\", %s)", unique-name),
              format-to-string("%=", object),
              unique-name);
end function test-print-name;

define generic test-print-1 () => ();
define generic test-print-2 ();
define generic test-print-3 (a :: <integer>) => num;
define generic test-print-4 (a :: <number>, #rest args) => (num :: <integer>);
define generic test-print-5 (a :: <string>, #key test) => (num :: <integer>, #rest vals);
define generic test-print-6 (a :: <string>, #key #all-keys) => (#rest vals :: <string>);
define generic test-print-7 (a :: subclass(<string>)) => ();
define generic test-print-8 (a :: false-or(<string>)) => ();
define generic test-print-9 (a :: type-union(<integer>, <float>)) => ();
define generic test-print-10 (a :: one-of(#"a", #"b")) => ();
define generic test-print-11
    (a :: limited(<integer>, min: 0), b :: limited(<integer>, max: 64))
 => (c :: limited(<integer>, min: 0, max: 64));
define generic test-print-12
    (a :: limited(<vector>, of: <float>),
     b :: limited(<vector>, of: <double-float>, size: 4))
 => (c :: false-or(limited(<array>, of: <float>, dimensions: #[2, 2])));

define method test-print-1 () => ()
end method test-print-1;

define method test-print-2 ()
end method test-print-2;

define method test-print-3 (a :: <integer>) => (num)
  #f
end method test-print-3;

define method test-print-4 (a :: <number>, #rest args) => (num :: <integer>)
  0
end method test-print-4;

define method test-print-5 (a :: <string>, #key test) => (num :: <integer>, #rest vals)
  0
end method test-print-5;

define method test-print-6 (a :: <string>, #key #all-keys) => (#rest vals :: <string>)
end method test-print-6;

define method test-print-7 (a :: subclass(<string>)) => ()
end method test-print-7;

define method test-print-8 (a :: false-or(<string>)) => ()
end method test-print-8;

define method test-print-9 (a :: type-union(<integer>, <float>)) => ()
end method test-print-9;

define method test-print-10 (a :: one-of(#"a", #"b")) => ()
end method test-print-10;

define method test-print-11
    (a :: limited(<integer>, min: 0), b :: limited(<integer>, max: 64))
 => (c :: limited(<integer>, min: 0, max: 64))
  0
end method test-print-11;

define method test-print-12
    (a :: limited(<vector>, of: <float>),
     b :: limited(<vector>, of: <double-float>, size: 4))
 => (c :: false-or(limited(<array>, of: <float>, dimensions: #[2, 2])))
  #f
end method test-print-12;

define constant $format-function-mappings
  = vector(vector(test-print-1,
                  "{<sealed-generic-function>: test-print-1}",
                  "{<simple-method>: ??? () => ()}"),
           vector(test-print-2,
                  "{<sealed-generic-function>: test-print-2}",
                  "{<simple-method>: ??? () => (#rest)}"),
           vector(test-print-3,
                  "{<sealed-generic-function>: test-print-3}",
                  "{<simple-method>: ??? (<integer>) => (<object>)}"),
           vector(test-print-4,
                  "{<sealed-generic-function>: test-print-4}",
                  "{<simple-method>: ??? (<number>, #rest) => (<integer>)}"),
           vector(test-print-5,
                  "{<sealed-generic-function>: test-print-5}",
                  "{<keyword-method>: ??? (<string>, #key test:) => (<integer>, #rest)}"),
           vector(test-print-6,
                  "{<sealed-generic-function>: test-print-6}",
                  "{<keyword-method>: ??? (<string>, #key #all-keys) => (#rest <string>)}"),
           vector(test-print-7,
                  "{<sealed-generic-function>: test-print-7}",
                  "{<simple-method>: ??? (subclass(<string>)) => ()}"),
           vector(test-print-8,
                  "{<sealed-generic-function>: test-print-8}",
                  "{<simple-method>: ??? (false-or(<string>)) => ()}"),
           vector(test-print-9,
                  "{<sealed-generic-function>: test-print-9}",
                  "{<simple-method>: ??? (type-union(<integer>, <float>)) => ()}"),
           vector(test-print-10,
                  "{<sealed-generic-function>: test-print-10}",
                  "{<simple-method>: ??? (one-of(#\"a\", #\"b\")) => ()}"),
           vector(test-print-11,
                  "{<sealed-generic-function>: test-print-11}",
                  "{<simple-method>: ??? (limited(<integer>, min: 0), limited(<integer>, max: 64)) => (limited(<integer>, min: 0, max: 64))}"),
           vector(test-print-12,
                  "{<sealed-generic-function>: test-print-12}",
                  "{<simple-method>: ??? (limited(<simple-vector>, of: <float>), limited(<simple-vector>, of: <double-float>, size: 4)) => (false-or(limited(<array>, of: <float>, dimensions: #[2, 2])))}"));

define test test-string-to-integer ()
  for (integer-mapping in $integer-string-mappings)
    let integer = integer-mapping[0];
    let base    = integer-mapping[1];
    let string  = integer-mapping[2];
    check-equal(format-to-string("string-to-integer(%s)", string),
                string-to-integer(string, base: base), integer)
  end;
  check-no-errors("string-to-integer of minimum integer",
                  string-to-integer(integer-to-string($minimum-integer)));
  check-no-errors("string-to-integer of maximum integer",
                  string-to-integer(integer-to-string($maximum-integer)));
end test;

define function format-object-tests
    () => ()
  for (mapping in $format-object-mappings)
    let object = mapping[0];
    let pretty-name = mapping[1];
    let unique-name = if (size(mapping) = 3) mapping[2] else pretty-name end;
    test-print-name(object, pretty-name, unique-name)
  end;
  for (mapping in $format-complex-object-mappings)
    let object = mapping[0];
    let class-name = format-to-string("%s", object-class(object));
    let unique-name = format-to-string("{%s: %s}", class-name, mapping[1]);
    test-print-name(object, unique-name, unique-name)
  end;
  let type = type-union(<string>, type-union(singleton(10), <character>));
  let class-name = format-to-string("%s", object-class(type));
  let expected-name
    = format-to-string("{%s: <string>, {%s: 10, <character>}}",
                       class-name, class-name);
  test-print-name(type, expected-name, expected-name)
end function;

define function format-function-tests
    () => ()
  for (mapping in $format-function-mappings)
    let gf = mapping[0];
    let gf-expected-text = mapping[1];
    check-equal(format-to-string("format-to-string(\"%%=\", %s)", gf-expected-text),
                format-to-string("%=", gf),
                gf-expected-text);
    let meth = generic-function-methods(gf).first;
    let meth-expected-text = mapping[2];
    check-equal(format-to-string("format-to-string(\"%%=\", %s)", meth-expected-text),
                format-to-string("%=", meth),
                meth-expected-text);
  end for;
end function;

define test test-format-to-string ()
  check-instance?("format-to-string returns a string",
                  <string>,
                  format-to-string("Hello"));
  check-condition("format-to-string crashes when missing an argument",
                  <error>, format-to-string("Hello %s"));
  check-condition("format-to-string crashes with argument of wrong type",
                  <error>, format-to-string("Hello %c", 10));
  check-condition("format-to-string crashes with invalid directive %z",
                  <error>, format-to-string("Hello %z", 10));
  check-equal("format-to-string(\"%d\", 10)",
              format-to-string("%d", 10),
              "10");
  check-equal("format-to-string(\"%b\", 7)",
              format-to-string("%b", 7),
              "111");
  check-equal("format-to-string(\"%o\", 16)",
              format-to-string("%o", 16),
              "20");
  check-equal("format-to-string(\"%x\", 257)",
              format-to-string("%x", 257),
              "101");
  check-equal("format-to-string(\"%c\", 'a')",
              format-to-string("%c", 'a'),
              "a");
  check-equal("format-to-string(\"%%\")",
              format-to-string("%%"),
              "%");
  format-object-tests();
  format-function-tests();
end test;

define suite common-dylan-format-test-suite ()
  test test-character-to-integer;
  test test-string-to-machine-word;
  test test-float-to-string;
  test test-integer-to-string;
  test test-number-to-string;
  test test-string-to-integer;
  test test-format-to-string;
end suite;
