Module:    functional-dylan-test-suite
Synopsis:  Functional Objects extensions library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function test cases

define functional-extensions function-test concatenate! ()
  let my-list = #(3, 4);
  check("test concatenate! on a list", \=, concatenate!(my-list, #(5), #(6)),
	#(3, 4, 5, 6));
  check("concatenate! should have not affected my-list", \=, my-list, #(3, 4));
  let my-stretchy-vector = make(<stretchy-vector>);
  add!(my-stretchy-vector, 3);
  add!(my-stretchy-vector, 4);
  let my-stretchy-vector-afterwards = make(<stretchy-vector>);
  add!(my-stretchy-vector-afterwards, 3);
  add!(my-stretchy-vector-afterwards, 4);
  add!(my-stretchy-vector-afterwards, 5);
  add!(my-stretchy-vector-afterwards, 6);
  check("test concatenate! on a stretchy-vector", \=, 
	concatenate!(my-stretchy-vector, #(5, 6)),
	my-stretchy-vector-afterwards);
  check("concatenate! should have changed my-stretchy-vector",
	\=, my-stretchy-vector, my-stretchy-vector-afterwards);
end function-test concatenate!;

define constant $test-error-message = "Test Error";

define class <test-error> (<error>)
end class <test-error>;

define method condition-to-string
    (error :: <test-error>) => (string :: <byte-string>)
  $test-error-message
end method condition-to-string;

define functional-extensions function-test condition-to-string ()
  check-equal("condition-to-string of an error produces correct string",
              condition-to-string(make(<simple-error>, format-string: "Hello")),
              "Hello");
  check-true("condition-to-string of a type error produces a string",
             begin
               let error = make(<type-error>, value: 10, type: <class>);
               instance?(condition-to-string(error), <string>)
             end);
  check-equal("condition-to-string of an error with a condition-to-string method",
              condition-to-string(make(<test-error>)),
              $test-error-message)
end function-test condition-to-string;

define functional-extensions function-test debug-message ()
  check-false("debug-message doesn't crash", debug-message("Hello"));
  check-false("debug-message doesn't crash with incorrect format arguments",
              debug-message("Hello %s"));
end function-test debug-message;

define functional-extensions function-test difference ()
  //---*** Do all collections by using dylan-test-suite collection code
  let list1 = #(1, 2, 3);
  let list2 = #(3, 4, 5);
  check("test difference #1", \=, difference(list1, list2), #(1, 2));
  check("test difference #2", \=, difference(list2, list1), #(4, 5));
  check("test difference #3", \=, difference(list1, list1), #());
  check("test difference with \\>", \=, difference(list1, list2, test: \>),
        list1);
end function-test difference;

define functional-extensions function-test false-or ()
  let new-type = #f;
  check-true("False-or returns type",
             begin
               new-type := false-or(<string>);
               instance?(new-type, <type>)
             end);
  check-true(format-to-string("%s is false-or(<string>)", "abc"),
             instance?("abc", new-type));
  check-true("#f is false-or(<string>)",
             instance?(#f, new-type));
  check-false("#t is not false-or(<string>)",
              instance?(#t, new-type));
end function-test false-or;

define functional-extensions function-test find-element ()
  //---*** Do all collections by using dylan-test-suite collection code
  let list1 = #("oh", "we", "like", "sheep", "like");
  check("test find-element", \=, 
	find-element(list1, method (the-element) (the-element = "like") end),
	"like");
  check("test failure find-element", \=,
	find-element(list1, method (the-element) (the-element = "thermos") end),
	#f);
  check("test failure find-element with failure as symbol", \=,
	find-element(list1, method (the-element) (the-element = "thermos") end,  
		     failure: #"heckfire"), #"heckfire");
  check("test find-element with skip: 1", \=,
	find-element(list1, method (the-element) (the-element = "like") end,
		     skip: 1), "like");
  check("skip: is too big", \=,
	find-element(list1, method (the-element) (the-element = "like") end,
		     skip: 2), #f);
end function-test find-element;

define functional-extensions function-test found? ()
  check-false("found?(unfound()) == #f", found?(unfound()));
  check-true("found?(#f)", found?(#f));
  check-true("found?(#t)", found?(#t));
end function-test found?;

define functional-extensions function-test ignorable ()
  check-true("ignorable doesn't crash",
             begin
               ignorable(test-function);
               #t
             end)
end function-test ignorable;

define functional-extensions function-test ignore ()
  check-true("ignore doesn't crash",
             begin
               ignore(test-function);
               #t
             end)
end function-test ignore;

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

define functional-extensions function-test integer-to-string ()
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
  check-no-errors("integer-to-string($minimum-integer)",
		  integer-to-string($minimum-integer));
  check-no-errors("integer-to-string($maximum-integer)",
		  integer-to-string($maximum-integer));
end function-test integer-to-string;

define functional-extensions function-test unfound ()
  //--- Nothing since fully tested by unfound?
end function-test unfound;

define functional-extensions function-test unfound? ()
  check-true("unfound?(unfound())", unfound?(unfound()));
  check-false("unfound?(#f) == #f", unfound?(#f));
  check-false("unfound?(#t) == #f", unfound?(#t));
end function-test unfound?;

define functional-extensions function-test number-to-string ()
  //---*** Fill this in...
end function-test number-to-string;

define functional-extensions function-test one-of ()
  let new-type = #f;
  check-true("one-of returns type",
             begin
               new-type := one-of(#"one", #t);
               instance?(new-type, <type>)
             end);
  check-true(format-to-string("%s is one-of(%=, #t)", #"one", #"one"),
             instance?(#"one", new-type));
  check-true(format-to-string("#t is one-of(%=, #t)", #"one"),
             instance?(#t, new-type));
  check-false(format-to-string("#f is one-of(%=, #t)", #"one"),
              instance?(#f, new-type));
end function-test one-of;

define functional-extensions function-test position ()
  //---*** Do all collections by using dylan-test-suite collection code
  let list1 = #(1, 'a', 34.43, 'a', "done");
  check-equal("test position",
              position(list1, 'a'),
              1);
  check-equal("test position with skip of 2",
              position(list1, 'a', skip: 1),
              3);
  check-false("test position with wrong item",
              position(list1, 'w'));
  check-false("test posision with skip greater than existance", 
	      position(list1, 'a', skip: 2));
  check-equal("test position using test: \\<", 
              position(#(1, 2, 3, 4), 3, test: \<),
              3);
end function-test position;

define functional-extensions function-test split ()
  check-equal("split on empty string",
              split("", '/'),
              #[""]);
  check-equal("split on single character",
	      split("a", '/'),
	      #["a"]);
  check-equal("split on two characters",
	      split("a/b", '/'),
	      #["a", "b"]);
  check-equal("split on multiple single characters",
	      split("aXbXcXdXeXfXg", 'X'),
	      #["a", "b", "c", "d", "e", "f", "g"]);
  check-equal("split on single word",
	      split("hello", '/'),
	      #["hello"]);
  check-equal("split on two words",
	      split("hello/world", '/'),
	      #["hello", "world"]);
  check-equal("split on three words",
	      split("majorXminorXbuild", 'X'),
	      #["major", "minor", "build"]);
  check-equal("split on multiple words",
	      split("x=100&y=200&width=30&height=10", '&'),
	      #["x=100", "y=200", "width=30", "height=10"]);
  check-equal("split on single separator character",
	      split("/", '/'),
	      #["", ""]);
  check-equal("split on a/",
	      split("a/", '/'),
	      #["a", ""]);
  check-equal("split on /b",
	      split("/b", '/'),
	      #["", "b"]);
  check-equal("split with double separator",
	      split("majorXXbuild", 'X'),
	      #["major", "", "build"]);
  check-equal("split with spaces",
	      split("major, minor, build", ','),
	      #["major", " minor", " build"]);
  check-equal("split with spaces everywhere",
	      split(" major , minor , build ", ','),
	      #[" major ", " minor ", " build "]);
  check-equal("split with start",
	      split("http://www.dylan.com/first/second/third", '/', start: 21),
	      #["first", "second", "third"]);
  check-equal("split with end",
	      split("first/second/third/test.txt", '/', end: 18),
	      #["first", "second", "third"]);
  check-equal("split with start and end",
	      split("http://www.dylan.com/first/second/third", '.', start: 7, end: 20),
	      #["www", "dylan", "com"]);
end function-test split;

define functional-extensions function-test remove-all-keys! ()
  //---*** Do all collections by using dylan-test-suite collection code
end function-test remove-all-keys!;

define functional-extensions function-test string-to-integer ()
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
end function-test string-to-integer;

define functional-extensions function-test subclass ()
  let new-type = #f;
  check-true("subclass returns type",
             begin
               new-type := subclass(<string>);
               instance?(new-type, <type>)
             end);
  check-true(format-to-string("<string> is subclass(<string>)"),
             instance?(<string>, new-type));
  check-true(format-to-string("<byte-string> is subclass(<string>)"),
             instance?(<byte-string>, new-type));
  check-false(format-to-string("<object> is not subclass(<string>)"),
              instance?(<object>, new-type));
end function-test subclass;

define functional-extensions function-test supplied? ()
  check-false("supplied?(unsupplied())", supplied?(unsupplied()));
  check-true("supplied?(#f) == #f", supplied?(#f));
  check-true("supplied?(#t) == #f", supplied?(#t));
end function-test supplied?;

define functional-extensions function-test fill-table! ()
  let table = make(<table>);
  check-equal("fill-table(...) returns the table",
              fill-table!(table, #[0, "Zero", 1, "One"]),
              table);
  check-equal("table(...)[0] = \"Zero\"",
              table[0], "Zero");
  check-equal("table(...)[1] = \"One\"",
              table[1], "One");
end function-test fill-table!;

define functional-extensions function-test unsupplied ()
  //--- Nothing since fully tested by unsupplied?
end function-test unsupplied;

define functional-extensions function-test unsupplied? ()
  check-true("unsupplied?(unsupplied())", unsupplied?(unsupplied()));
  check-false("unsupplied?(#f) == #f", unsupplied?(#f));
  check-false("unsupplied?(#t) == #f", unsupplied?(#t));
end function-test unsupplied?;


/// simple-format module

define simple-format function-test format-out ()
  check-false("format-out doesn't crash", format-out("Hello"));
  check-condition("format-out crashes when missing an argument",
		  <error>, format-out("Hello %s"));
  check-condition("format-out crashes with argument of wrong type",
		  <error>, format-out("Hello %c", 10));
  check-condition("format-out crashes with invalid directive %z",
		  <error>, format-out("Hello %z", 10));
end function-test format-out;

define simple-format function-test format-to-string ()
  check-true("format-to-string returns a string",
	     instance?(format-to-string("Hello"), <string>));
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
  format-object-tests()
end function-test format-to-string;

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
end function format-object-tests;


/// simple-random tests

/*---*** andrewa: not used yet...
define method chi-square
    (N :: <integer>, range :: <integer>) => (chi-square :: <integer>)
  let f = make(<simple-object-vector>, size: range, fill: 0);
  for (i from 0 below N) 
    let rand = random(range);
    f[rand] := f[rand] + 1;
  end;
  let t = 0;
  for (i from 0 below range) t := t + f[i] * f[i] end;
  floor/(range * t, N) - N
end method chi-square;
*/

define simple-random function-test random ()
  // We should use chi-square somehow, but we don't want it to be slow.
  // Also, what value should it be returning?
  //---*** Fill this in...
end function-test random;

