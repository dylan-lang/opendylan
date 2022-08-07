Module: common-dylan-test-suite
Synopsis: Tests for common-extensions and some dylan-extensions definitions
          that don't naturally fit in other files.


define test test-$unsupplied ()
  //---** What can we do here?
end test;

define test test-$unfound ()
  //---** What can we do here?
end test;


define test test-<format-string-condition> ()
  //---*** Fill this in...
end test;

define test test-<simple-condition> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <stretchy-sequence>) => (object)
  make(<stretchy-sequence>)
end method make-test-instance;

define test test-<stretchy-sequence> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <stretchy-object-vector>) => (object)
  make(<stretchy-object-vector>)
end method make-test-instance;

define test test-<stretchy-object-vector> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <object-deque>) => (object)
  make(<object-deque>)
end method make-test-instance;

define test test-<object-deque> ()
  //---*** Fill this in...
end test;

define sideways method make-test-instance
    (class == <string-table>) => (object)
  make(<string-table>)
end method make-test-instance;

define test test-<string-table> ()
  //---*** Fill this in...
end test;

define test test-concatenate! ()
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
end test;

define constant $test-error-message = "Test Error";

define class <test-error> (<error>)
end class <test-error>;

define method condition-to-string
    (error :: <test-error>) => (string :: <byte-string>)
  $test-error-message
end method condition-to-string;

define test test-condition-to-string ()
  check-equal("condition-to-string of an error produces correct string",
              condition-to-string(make(<simple-error>, format-string: "Hello")),
              "Hello");
  check-instance?("condition-to-string of a type error produces a string",
                  <string>,
                  begin
                    let error = make(<type-error>, value: 10, type: <class>);
                    condition-to-string(error)
                  end);
  check-equal("condition-to-string of an error with a condition-to-string method",
              condition-to-string(make(<test-error>)),
              $test-error-message)
end test;

define test test-debug-message ()
  check-false("debug-message doesn't crash", debug-message("Hello"));
  check-false("debug-message doesn't crash with incorrect format arguments",
              debug-message("Hello %s"));
end test;

define test test-difference ()
  //---*** Do all collections by using dylan-test-suite collection code
  let list1 = #(1, 2, 3);
  let list2 = #(3, 4, 5);
  check("test difference #1", \=, difference(list1, list2), #(1, 2));
  check("test difference #2", \=, difference(list2, list1), #(4, 5));
  check("test difference #3", \=, difference(list1, list1), #());
  check("test difference with \\>", \=, difference(list1, list2, test: \>),
        list1);
end test;

define test test-false-or ()
  let new-type = #f;
  check-instance?("False-or returns type",
                  <type>, new-type := false-or(<string>));
  check-instance?(format-to-string("%s is false-or(<string>)", "abc"),
                  new-type, "abc");
  check-instance?("#f is false-or(<string>)",
                  new-type, #f);
  check-false("#t is not false-or(<string>)",
              instance?(#t, new-type));
end test;

define test test-find-element ()
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
end test;

/*
Commenting these two out for now because they cause serious warnings. We want
to be able to run libraries-test-suite from the GitHub CI and have serious
warnings cause a failure. --cgay 2022

define test test-ignorable ()
  assert-signals(<error>, ignorable(this-is-undefined),
                 "ignorable crashes on undefined variables");
end;

define test test-ignore ()
  assert-signals(<error>, ignore(this-is-undefined),
                 "ignore crashes on undefined variables");
end;
*/

define test test-one-of ()
  let new-type = #f;
  check-instance?("one-of returns type",
                  <type>,
                  new-type := one-of(#"one", #t));
  check-instance?(format-to-string("%s is one-of(%=, #t)", #"one", #"one"),
                  new-type, #"one");
  check-instance?(format-to-string("#t is one-of(%=, #t)", #"one"),
                  new-type, #t);
  check-false(format-to-string("#f is one-of(%=, #t)", #"one"),
              instance?(#f, new-type));
end test;

define test test-position ()
  //---*** Do all collections by using dylan-test-suite collection code
  for (sequence in #[#(1, 'a', 34.43, 'a', "done"),
                     #[1, 'a', 34.43, 'a', "done"],
                     "xaxad"])
    check-equal("test position",
                position(sequence, 'a'),
                1);
    check-equal("test position with skip of 1",
                position(sequence, 'a', skip: 1),
                3);
    check-false("test position with wrong item",
                position(sequence, 'w'));
    check-false("test position with skip greater than existence",
                position(sequence, 'a', skip: 2));

    check-equal("test position with start at first",
                position(sequence, 'a', start: 1),
                1);
    check-equal("test position with start beyond first",
                position(sequence, 'a', start: 2),
                3);
    check-false("test position with end",
                position(sequence, 'a', end: 1));
    check-false("test position with skip and end",
                position(sequence, 'a', end: 3, skip: 1));
  end for;
  check-equal("test position using test: \\<",
              position(#(1, 2, 3, 4), 3, test: \<),
              3);
end test;

define test test-split ()
  // a character separator should act the same as a string separator that
  // contains only that character...
  for (separator in #('/', "/"))
    local method fmt (name)
            format-to-string("%s, sep = %=", name, separator);
          end;
    check-equal(fmt("split empty string"),
                split("", separator),
                #[""]);
    check-equal(fmt("split single character"),
                split("a", separator),
                #["a"]);
    check-equal(fmt("split two characters"),
                split("a/b", separator),
                #["a", "b"]);
    check-equal(fmt("split multiple single characters"),
                split("a/b/c/d/e/f/g", separator),
                #["a", "b", "c", "d", "e", "f", "g"]);
    check-equal(fmt("split single word"),
                split("hello", separator),
                #["hello"]);
    check-equal(fmt("split two words"),
                split("hello/world", separator),
                #["hello", "world"]);
    check-equal(fmt("split three words"),
                split("major/minor/build", separator),
                #["major", "minor", "build"]);
    check-equal(fmt("split multiple words"),
                split("x=100/y=200/width=30/height=10", separator),
                #["x=100", "y=200", "width=30", "height=10"]);
    check-equal(fmt("split only the separator character"),
                split("/", separator),
                #["", ""]);
    check-equal(fmt("split a/"),
                split("a/", separator),
                #["a", ""]);
    check-equal(fmt("split /b"),
                split("/b", separator),
                #["", "b"]);
    check-equal(fmt("split with double separator"),
                split("major//build", separator),
                #["major", "", "build"]);
    check-equal(fmt("split with spaces"),
                split(" major / minor / build ", separator),
                #[" major ", " minor ", " build "]);
    check-equal(fmt("split with start"),
                split("123456789/123456789", separator, start: 1),
                #["23456789", "123456789"]);
    check-equal(fmt("split with end"),
                split("012/456789", separator, end: 8),
                #["012", "4567"]);
    check-equal(fmt("split with start and end"),
                split("012/456789", separator, start: 2, end: 8),
                #["2", "4567"]);
    check-equal(fmt("split with count"),
                split("1/2/3/4", separator, count: 2),
                #["1", "2/3/4"]);
    check-equal(fmt("split with count and start"),
                split("1/2/3/4", separator, count: 2, start: 2),
                #["2", "3/4"]);
    check-equal(fmt("split with count and end"),
                split("1/2/3/4", separator, count: 2, end: 5),
                #["1", "2/3"]);
    check-equal(fmt("split with count, start, and end"),
                split("1/2/3/4", separator, count: 2, start: 2, end: 5),
                #["2", "3"]);
    check-equal(fmt("split with count = 1"),
                split("a/b/c/d", separator, count: 1),
                #["a/b/c/d"]);
    check-equal(fmt("split with remove-if-empty?: #t"),
                split("/a/b/", separator, remove-if-empty?: #t),
                #["a", "b"]);
    check-equal(fmt("split with remove-if-empty?: #t and separator only"),
                split("/", separator, remove-if-empty?: #t),
                #[]);
    check-equal(fmt("split with remove-if-empty?: #t and separators only"),
                split("///", separator, remove-if-empty?: #t),
                #[]);
    check-equal(fmt("split with remove-if-empty?: #t and start, only separator"),
                split("a/", separator, start: 1, remove-if-empty?: #t),
                #[]);
    check-equal(fmt("split with remove-if-empty?: #t and start"),
                split("/a", separator, start: 1, remove-if-empty?: #t),
                #["a"]);
    check-equal(fmt("split with remove-if-empty?: #t and end, only separator"),
                split("/a", separator, end: 1, remove-if-empty?: #t),
                #[]);
    check-equal(fmt("split with remove-if-empty?: #t and end"),
                split("a/", separator, end: 1, remove-if-empty?: #t),
                #["a"]);
    check-equal(fmt("split with remove-if-empty?: #t and count"),
                split("/a/b", separator, count: 1, remove-if-empty?: #t),
                #["a/b"]);
    check-equal(fmt("split with remove-if-empty?: #t and count, only separator character"),
                split("/", separator, count: 1, remove-if-empty?: #t),
                #[]);
    check-equal(fmt("split with test"),
                split("a/", separator, test: \~==),
                #["", "/"]);
    check-equal(fmt("split with separator not found"),
                split("abc", "x"),
                #["abc"]);
  end for;

  check-condition("split with empty separator signals?",
                  <error>,
                  split("abc", ""));
  check-condition("split with splitter that returns same indices signals?",
                  <error>,
                  split("abc", method (_, bpos, _) values(bpos, bpos) end));
  check-equal("split with separator crossing start:",
              split("xxx one xxx two xxx", "xxx", start: 1),
              #["xx one ", " two ", ""]);
  check-equal("split with separator crossing end:",
              split("xxx one xxx two xxx", "xxx", end: 17),
              #["", " one ", " two x"]);
  check-equal("split with separator crossing start: and end:",
              split("xxx one xxx two xxx", "xxx", start: 1, end: 17),
              #["xx one ", " two x"]);
end test;

define test test-join ()
  let abc = #("a", "b", "c");
  for (separator in #("blah", #[1], #(1)),
       expected in #("", #[], #()))
    check-equal("join empty sequence return type",
                join(#[], separator),
                expected);
  end;
  check-equal("basic join",
              "a, b, c",
              join(abc, ", "));
  check-equal("join of one element",
              "singleton",
              join(#("singleton"), " "));
  check-equal("join with conjunction",
              "a, b and c",
              join(abc, ", ",
                   conjunction: " and "));
  check-equal("join with key",
              "1, 2, 3",
              join(#(1, 2, 3), ", ",
                   key: integer-to-string));
  check-equal("join with conjunction and key",
              "1, 2 and 3",
              join(#(1, 2, 3), ", ",
                   conjunction: " and ",
                   key: integer-to-string));
end test;

define test test-remove-all-keys! ()
  //---*** Do all collections by using dylan-test-suite collection code
end test;

define test test-subclass ()
  let new-type = #f;
  check-instance?("subclass returns type",
                  <type>,
                  new-type := subclass(<string>));
  check-instance?(format-to-string("<string> is subclass(<string>)"),
                  new-type, <string>);
  check-instance?(format-to-string("<byte-string> is subclass(<string>)"),
                  new-type, <byte-string>);
  check-false(format-to-string("<object> is not subclass(<string>)"),
              instance?(<object>, new-type));
end test;

define test test-fill-table! ()
  let table = make(<table>);
  check-equal("fill-table(...) returns the table",
              fill-table!(table, #[0, "Zero", 1, "One"]),
              table);
  check-equal("table(...)[0] = \"Zero\"",
              table[0], "Zero");
  check-equal("table(...)[1] = \"One\"",
              table[1], "One");
end test;

// Application startup handling

define test test-application-name ()
  check-instance?("application-name returns #f or a string",
                  false-or(<string>), application-name());
end test;

define test test-application-filename ()
  let filename = application-filename();
  check-true("application-filename returns #f or a valid, existing file name",
             ~filename | file-exists?(filename));
end test;

define test test-application-arguments ()
  check-instance?("application-arguments returns a sequence",
                  <sequence>, application-arguments());
end test;

define test test-tokenize-command-line ()
  //---*** Fill this in...
end test;

define test test-exit-application ()
  //---*** Fill this in...
end test;

define test test-register-application-exit-function ()
  //---*** Fill this in...
end test;

define test test-unfound ()
  //---*** Fill this in...
end test;

define test test-unfound? ()
  check-true("unfound?($unfound)", unfound?($unfound));
  check-false("unfound?(#f) == #f", unfound?(#f));
  check-false("unfound?(#t) == #f", unfound?(#t));
end test;

define test test-found? ()
  check-false("found?($unfound) is false", found?($unfound));
  check-true("found?(#f)", found?(#f));
  check-true("found?(#t)", found?(#t));
end test;

define test test-unsupplied ()
  //---*** Fill this in...
end test;

define test test-unsupplied? ()
  check-true("unsupplied?($unsupplied)", unsupplied?($unsupplied));
  check-false("unsupplied?(#f) == #f", unsupplied?(#f));
  check-false("unsupplied?(#t) == #f", unsupplied?(#t));
end test;

define test test-supplied? ()
  //---*** Fill this in...
end test;

define test test-true? ()
  //---*** Fill this in...
end test;

define test test-false? ()
  //---*** Fill this in...
end test;


/// simple-format module

define test test-format-out ()
  check-no-errors("format-out doesn't crash", format-out("Hello"));
  check-condition("format-out crashes when missing an argument",
                  <error>, format-out("Hello %s"));
  check-condition("format-out crashes with argument of wrong type",
                  <error>, format-out("Hello %c", 10));
  check-condition("format-out crashes with invalid directive %z",
                  <error>, format-out("Hello %z", 10));
end test;


/// finalization tests

define test test-drain-finalization-queue ()
  //---*** Fill this in...
end test;

define test test-finalize ()
  //---*** Fill this in...
end test;

define test test-finalize-when-unreachable ()
  //---*** Fill this in...
end test;

define test test-automatic-finalization-enabled?-setter ()
  //---*** Fill this in...
end test;

define test test-automatic-finalization-enabled? ()
  //---*** Fill this in...
end test;

define suite common-extensions-test-suite ()
  test test-$unfound;
  test test-$unsupplied;
  test test-<format-string-condition>;
  test test-<object-deque>;
  test test-<simple-condition>;
  test test-<stretchy-object-vector>;
  test test-<stretchy-sequence>;
  test test-<string-table>;
  test test-concatenate!;
  test test-condition-to-string;
  test test-debug-message;
  test test-difference;
  test test-false-or;
  test test-find-element;
  //test test-ignorable;
  //test test-ignore;
  test test-one-of;
  test test-position;
  test test-split;
  test test-join;
  test test-remove-all-keys!;
  test test-subclass;
  test test-fill-table!;
  test test-application-name;
  test test-application-filename;
  test test-application-arguments;
  test test-tokenize-command-line;
  test test-exit-application;
  test test-register-application-exit-function;
  test test-unfound;
  test test-unfound?;
  test test-found?;
  test test-unsupplied;
  test test-unsupplied?;
  test test-supplied?;
  test test-true?;
  test test-false?;
  test test-format-out;
  test test-drain-finalization-queue;
  test test-finalize;
  test test-finalize-when-unreachable;
  test test-automatic-finalization-enabled?-setter;
  test test-automatic-finalization-enabled?;
end suite;
