Module:    c-ffi-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---------
/// C string tests

/// This just calls the c-callable function in tests-aux
define c-function c-string-value
  parameter str :: <C-string>;
  parameter offset :: <C-size-t>;
  result r :: <c-character>;
  c-name: "c_string_value";
end;

/// This just calls the c-callable function in tests-aux
define c-function c-string-value-setter
  parameter new :: <C-character>;
  parameter str :: <C-string>;
  parameter offset :: <C-size-t>;
  result r :: <c-character>;
  c-name: "c_string_value_setter";
end;

/// check-a-c-string
/// This set of tests modifies the string and accesses it through a
/// foreign function, ans via element to make sure that every view of it
/// is correct.
define function check-a-c-string (name-string, a-string, seed) => ();
  for (i from 0 below size(a-string))
    let this-char = as(<character>, as(<integer>, seed) + i);
    check-equal(concatenate(name-string,
                            " c-string get value from alias before"),
                c-string-value(a-string, i),
                a-string[i]);
    check-equal(concatenate(name-string,
                            " c-string set value"),
                c-string-value(a-string, i) := this-char,
                this-char);
    check-equal(concatenate(name-string,
                            " c-string set value"),
                c-string-value(a-string, i),
                this-char);
    check-equal(concatenate(name-string,
                            " c-string get value alias"),
                a-string[i],
                this-char);
    check-equal(concatenate(name-string,
                            " c-string get value from alias"),
                c-string-value(a-string, i),
                a-string[i]);
  end;
end;



/// c-string-test
/// make sure that <C-string>s behave as strings and collections, and that
/// views onto a <byte-string> as a <c-string> from a foreign function
/// all correspond.
define test c-string-test ()
  let a-string = shallow-copy("This is a test string");
  check-a-c-string("<byte-string>", a-string, 'A');
  let c-string = #f;
  check-true("Create a C-string with map-as",
             instance?(c-string := map-as(<c-string>, identity, a-string),
                       <C-string>));
  check-equal("size works on c-strings", size(a-string), size(c-string));
  for (item in a-string,
      index from 0 below size(a-string))
    check-equal("elements of c-string", item, c-string[index]);
  end;
  for (item in c-string,
      index from 0 below size(c-string))
    check-equal("elements of c-string forward-iteration",
                item, a-string[index]);
  end;
  check-a-c-string("<C-string>", c-string, 'B');
end;

//
// -------------
// tests for bug 393

define function main-bug-393 ()
  for (i from 0 below 200)
    let buf = make(<c-string>, size: 10 + i);
    destroy(buf);
  end for;
  #t;
end main-bug-393;

define test bug-393 ()
  check-true("bug-393 try it", main-bug-393());
end;
