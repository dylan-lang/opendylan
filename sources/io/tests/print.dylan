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

define function test-print-miscellaneous ()
  check-print("<object>", <object>);
  check-print("class", <test-class>);
  check-print("make(class)", make(<test-class>));
  check-print("list", #(1, 2, 3));
  check-print("list", #[1, 2, 3]);
  check-print("function", add);
  check-print("range", range(from: 10, by: 2, to: 20));
  check-print("symbol", #"symbol");

  // common types/specializers
  check-print("singleton(3)", singleton(3));
  check-print("false-or(<integer>)", false-or(<integer>));
  check-print("limited <integer>", limited(<integer>, min: 0, max: 255));
  check-print("type-union", type-union(<integer>, <string>));
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
