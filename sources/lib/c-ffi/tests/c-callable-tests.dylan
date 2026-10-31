Module:    c-ffi-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-function callback-tests
  c-name: "callback_tests";
end c-function;

define variable *callback-flag* = #f;

define sealed method a-function () => ();
  *callback-flag* := #"a-function";
end;

/// called from the C tests
define C-callable-wrapper simple-callable of a-function
  c-name: "simple_callable";
end;
ignore(simple-callable);

define C-callable-wrapper dylan-check of
    method
        (description :: <string>, want :: <ffi-integer>, got :: <ffi-integer>)
     => ()
      check-equal(description, want, got);
    end
  parameter description :: <C-string>;
  parameter want :: <C-long>;
  parameter got :: <C-long>;
  c-name: "dylan_check";
end;
ignore(dylan-check);

/// called from C
/// Be very careful to get correspondence right with C code.
define c-callable-wrapper more-interesting-callable of
    method (p1 :: <integer>, p2 :: <integer>, p4 :: <integer>,
            p5 :: <integer>, p6 ::  <minimal*>) => (r, p3, p4, p5)
      check-equal("slot value of struct passed into dylan", p6.slot-1, 171717);
      check-equal("parameter 1 passed into interesting callable", 1, p1);
      check-equal("parameter 2 passed into interesting callable", 2, p2);
      check-equal("parameter 4 passed into interesting callable", 4, p4);
      check-equal("parameter 5 passed into interesting callable", 5, p5);
      p6.slot-1 := #xfffff00;
      values(p4, p1, p2, p4)
    end
  input parameter p1 :: <C-unsigned-long>;
  parameter p2 :: <C-unsigned-char>;
  output parameter p3 :: <c-unsigned-char*>;
  input output parameter p4 :: <C-unsigned-char*>;
  input output parameter p5 :: <C-unsigned-char*>;
  parameter p6 :: <minimal*>;
  result r :: <C-unsigned-char>;
  c-name: "more_interesting_callable";
end;
ignore(more-interesting-callable);

/// signed integer identity function callable from C
define c-callable-wrapper dylan-int-identity of identity
  parameter j :: <C-raw-signed-long>;
  result j :: <C-raw-signed-long>;
  c-name: "dylan_int_identity"
end;
ignore(dylan-int-identity);

define function always-one ()
  as(<machine-word>, 1)
end;

define c-callable-wrapper dylan-always-one of always-one
  result j :: <C-raw-signed-long>;
  c-name: "dylan_always_one"
end;
ignore(dylan-always-one);

define test c-callable-test ()
  *callback-flag* := #f;

  callback-tests();

  check-equal("Callback flag set by a-function",
              #"a-function", *callback-flag*);

  *callback-flag* := #f;
end test;
