module:    optimizations-test-suite
Synopsis:  A test suite for compiler optimizations
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function rest-function (#rest rest) end;

define function key-function (#key a = 10, b = 11) end;

define function rest-key-function (#key a = 10, b = 11) end;

define function required+rest-function (x, y, #rest rest) end;

define function required+key-function (x, y, #key a = 10, b = 11) end;

define function required+rest-key-function (x, y, #key a = 10, b = 11) end;

define method optional-arguments-tests ()
  rest-function();
  rest-function(1, 2, 3);
  key-function();
  key-function(a: 1);
  key-function(b: 2);
  key-function(a: 1, b: 2);
  key-function(b: 2, a: 1);
  rest-key-function();
  rest-key-function(a: 1);
  rest-key-function(b: 2);
  rest-key-function(a: 1, b: 2);
  rest-key-function(b: 2, a: 1);
  required+rest-function("1", "2");
  required+rest-function("1", "2", 1, 2, 3);
  required+key-function("1", "2");
  required+key-function("1", "2", a: 1);
  required+key-function("1", "2", b: 2);
  required+key-function("1", "2", a: 1, b: 2);
  required+key-function("1", "2", b: 2, a: 1);
  required+rest-key-function("1", "2");
  required+rest-key-function("1", "2", a: 1);
  required+rest-key-function("1", "2", b: 2);
  required+rest-key-function("1", "2", a: 1, b: 2);
  required+rest-key-function("1", "2", b: 2, a: 1);
end method;

optional-arguments-tests();
