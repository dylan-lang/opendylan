Module:       warnings-test-suite
Synopsis:     A test suite for compiler warnings
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function test-function
    (x :: <integer>, y :: <string>) => ()
  x
end function test-function;

define function too-few-arguments
    () => ()
  test-function()
end function too-few-arguments;

define function too-many-arguments
    () => ()
  test-function(10, "Hello", "Invalid extra argument")
end function too-many-arguments;

define function incorrect-argument-type
    () => ()
  test-function("Hello", "World")
end function incorrect-argument-type;

define function duplicate-arguments
    (x, x) => (x)
  x
end function duplicate-arguments;

define function duplicate-keywords
    (#key x, x) => ()
  x
end function duplicate-keywords;

ignore(too-few-arguments,
       too-many-arguments,
       incorrect-argument-type,
       duplicate-arguments,
       duplicate-keywords);


/// Keywords

define function test-keywords
    (x, #key a :: <integer>, b)
  values(x, a, b)
end function test-keywords;

define function incorrect-keywords
    () => ()
  test-keywords(10, no-such-keyword: 20)
end function incorrect-keywords;

define function incorrect-keyword-types
    () => ()
  test-keywords(10, a: "Hello")
end function incorrect-keyword-types;

ignore(incorrect-keywords,
       incorrect-keyword-types);


/// Generic functions

define generic no-method-generic (x) => (y);

define generic no-method-exported-generic (x) => (y);

define function call-generic-with-no-methods
    (x) => ()
  no-method-generic(x)
end function call-generic-with-no-methods;

define open generic open-but-not-exported-generic
    (x) => ();

define method open-but-not-exported-generic
    (x) => ()
  ignore(x);
  values()
end method open-but-not-exported-generic;

define generic incongruent-generic-function
    (x :: <collection>, #key x) => (value :: <string>);

define method incongruent-generic-function
    (x :: <integer>, #key x) => (value :: <string>)
  "Incongruent argument type"
end method incongruent-generic-function;

define method incongruent-generic-function
    (x :: <collection>, #key x) => (value :: <integer>)
  10
end method incongruent-generic-function;

define method incongruent-generic-function
    (x :: <collection>) => (value :: <string>)
  "missing keywords"
end method incongruent-generic-function;

ignore(call-generic-with-no-methods,
       open-but-not-exported-generic,
       incongruent-generic-function);
