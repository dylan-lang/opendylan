Module:       io-test-suite
Synopsis:     IO library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define print variable-test *print-length* ()
  //---*** Fill this in...
end variable-test *print-length*;

define print variable-test *print-level* ()
  //---*** Fill this in...
end variable-test *print-level*;

define print variable-test *print-circle?* ()
  //---*** Fill this in...
end variable-test *print-circle?*;

define print variable-test *print-pretty?* ()
  //---*** Fill this in...
end variable-test *print-pretty?*;

define print variable-test *print-escape?* ()
  //---*** Fill this in...
end variable-test *print-escape?*;


define print function-test print ()
  //---*** Fill this in...
end function-test print;

define print function-test print-object ()
  //---*** Fill this in...
end function-test print-object;

define print function-test print-to-string ()
  test-print-character();
  test-print-string();
  test-print-booleans();
  test-print-numbers();
  test-print-functions();
  test-print-types();
  test-print-miscellaneous();
  extend-print-object();
  test-print-variables();
end function-test print-to-string;

define print macro-test printing-object-test ()
  //---*** Fill this in...
end macro-test printing-object-test;


/// A simple printing harness

define class <test-class> (<object>)
  constant slot the-slot1 = "", init-keyword: the-slot1:;
  constant slot the-slot2 = "", init-keyword: the-slot2:;
end class <test-class>;

define method check-print
    (name :: <string>, object :: <object>, #rest args) => ()
  check-instance?(concatenate("print-to-string ", name, " returns string"),
                  <string>,
                  apply(print-to-string, object, args));
  check-equal(concatenate("print ", name, " matches print-to-string"),
              begin
                let stream
                  = make(<byte-string-stream>, direction: #"output");
                apply(print, object, stream, args);
                stream-contents(stream)
              end,
              apply(print-to-string, object, args))
end method check-print;


/// The print tests

define function test-print-character ()
  check-equal("a character",
              print-to-string('c'), "'c'");
  check-equal("an escape character",
              print-to-string('\n'), "'\\n'");
  // check-equal("a non-escape character",
  //               print-to-string('\q'), "'q'");
  check-equal("\\",
              print-to-string('\\'), "'\\\\'");
end function test-print-character;

define function test-print-string ()
  check-equal("a string",
              print-to-string("hello"), "\"hello\"");
  check-equal("a string with escape character",
              print-to-string("hello\nthere"), "\"hello\\nthere\"");
  // check-equal("a non-escape character",
  //               print-to-string("hi\qthere!"),
  //               "\"hiqthere!\"");
  // check-equal("a space",
  //               print-to-string("\ space"), "\" space\"");
  check-equal("\\ for strings",
              print-to-string("we\\know"), "\"we\\\\know\"");
end function test-print-string;

define function test-print-booleans ()
  check-equal("#t", print-to-string(#T), "#t");
  check-equal("#f", print-to-string(#F), "#f");
end function test-print-booleans;

define function test-print-numbers ()
  check-equal("integer", print-to-string(100), "100");
  check-equal("negative integer", print-to-string(-55), "-55");
end function test-print-numbers;

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

define function test-print-functions ()
  check-equal("function: test-print-1",
              print-to-string(test-print-1),
              "{generic function test-print-1 () => ()}");
  check-equal("function: test-print-2",
              print-to-string(test-print-2),
              "{generic function test-print-2 () => (#rest)}");
  check-equal("function: test-print-3",
              print-to-string(test-print-3),
              "{generic function test-print-3 (<integer>) => (<object>)}");
  check-equal("function: test-print-4",
              print-to-string(test-print-4),
              "{generic function test-print-4 (<number>, #rest) => (<integer>)}");
  check-equal("function: test-print-5",
              print-to-string(test-print-5),
              "{generic function test-print-5 (<string>, #key test:) => (<integer>, #rest)}");
  check-equal("function: test-print-6",
              print-to-string(test-print-6),
              "{generic function test-print-6 (<string>, #key #all-keys) => (#rest <string>)}");
  check-equal("function: test-print-7",
              print-to-string(test-print-7),
              "{generic function test-print-7 (subclass(<string>)) => ()}");
  check-equal("function: test-print-8",
              print-to-string(test-print-8),
              "{generic function test-print-8 (false-or(<string>)) => ()}");
  check-equal("function: test-print-9",
              print-to-string(test-print-9),
              "{generic function test-print-9 (type-union(<integer>, <float>)) => ()}");
  check-equal("function: test-print-10",
              print-to-string(test-print-10),
              "{generic function test-print-10 (one-of(#\"a\", #\"b\")) => ()}");
  check-equal("function: test-print-11",
              print-to-string(test-print-11),
              "{generic function test-print-11 (limited(<integer>, min: 0), limited(<integer>, max: 64)) => (limited(<integer>, min: 0, max: 64))}");
  check-equal("function: test-print-12",
              print-to-string(test-print-12),
              "{generic function test-print-12 (limited(<simple-vector>, of: <float>), limited(<simple-vector>, of: <double-float>, size: 4)) => (false-or(limited(<array>, of: <float>, dimensions: #[2, 2])))}");
  let m = generic-function-methods(test-print-1).first;
  check-equal("method: test-print-1", print-to-string(m), "{method () => ()}");
end function test-print-functions;

define constant $type-printing-tests
 = vector(
     vector("<object>", <object>, "{class <object>}"),
     vector("singleton(3)", singleton(3), "{Type singleton(3)}"),
     vector("false-or(<integer>)", false-or(<integer>),
            "{Type false-or(<integer>)}"),
     vector("false-or, manual", type-union(singleton(#f), <integer>),
            "{Type false-or(<integer>)}"),
     vector("false-or, manual 2", type-union(<integer>, singleton(#f)),
            "{Type false-or(<integer>)}"),
     vector("false-or(<integer>, <float>)", false-or(<integer>, <float>),
            "{Type false-or(<integer>, <float>)}"),
     vector("type-union", type-union(<integer>, <string>),
            "{Type type-union(<integer>, <string>)}"),
     vector("type-union nested", type-union(<integer>, <string>, <float>),
            "{Type type-union(<integer>, <string>, <float>)}"),
     vector("one-of", one-of(1, 2, "hello"),
            "{Type one-of(1, 2, \"hello\")}"),
     vector("one-of symbols", one-of(#"a", #"b"),
            "{Type one-of(#\"a\", #\"b\")}"),
     vector("limited-integer min", limited(<integer>, min: 0),
            "{Type limited(<integer>, min: 0)}"),
     vector("limited-integer max", limited(<integer>, max: 64),
            "{Type limited(<integer>, max: 64)}"),
     vector("limited-integer min-max", limited(<integer>, min: 0, max: 32),
            "{Type limited(<integer>, min: 0, max: 32)}"),
     vector("subclass", subclass(<string>),
            "{Type subclass(<string>)}")
   );

define function test-print-types ()
  for (type-test in $type-printing-tests)
    check-print(type-test[0], type-test[1]);
    check-equal(type-test[0], print-to-string(type-test[1]), type-test[2]);
  end for;
end function test-print-types;

define function test-print-miscellaneous ()
  check-print("class", <test-class>);
  check-print("make(class)", make(<test-class>));
  check-print("list", #(1, 2, 3));
  check-print("list", #[1, 2, 3]);
  check-print("function", add);
  check-print("range", range(from: 10, by: 2, to: 20));
  check-print("symbol", #"symbol");
end function test-print-miscellaneous;

// extend-print-object

define method print-object
    (test-class :: <test-class>, stream :: <stream>) => ()
  if (test-class.the-slot1 = "" &  test-class.the-slot2 = "")
    write(stream, "{empty <test-class>}");
  else
    printing-logical-block (stream, prefix: "<test-class>:")
      pprint-newline(#"mandatory", stream);
      pprint-indent(#"block", 2, stream);
      write(stream, "the-slot1: ");
      write(stream, test-class.the-slot1);
      pprint-newline(#"mandatory", stream);
      pprint-indent(#"block", 2, stream);
      write(stream, "the-slot2: ");
      write(stream, test-class.the-slot2);
    end;
  end if;
end method print-object;

define function extend-print-object ()
  let test-class = make(<test-class>, the-slot1: "cheese", the-slot2: "milk");
  check-equal("testing print-object for <test-class>",
              print-to-string(test-class),
              "<test-class>:\nthe-slot1: cheese\nthe-slot2: milk");
  check-equal("test empty <test-class>",
              print-to-string(make(<test-class>)),
              "{empty <test-class>}");
end function extend-print-object;

define function test-print-variables ()
  check-print("print circular list",
              begin
                let my-list = list(1, 2, 3);
                last(my-list) := my-list;
                my-list
              end,
              circle?: #t);
end function test-print-variables;
