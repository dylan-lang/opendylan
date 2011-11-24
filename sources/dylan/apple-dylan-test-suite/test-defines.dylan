Module:    apple-dylan-test-suite
Filename:  test-defines.dylan
Summary:   Apple Dylan test suite, test-defines
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

define test defines (description: "")
  check("", instance?, deque-instance, <function>);
  check("", instance?, <dtest-test-class>, <class>);
  check-equal("", test-variable, 99);
end test defines;

// Design note 17 "Define like Bind"

define constant dlb-1 :: <integer> = 10;

define test define-like-bind-1
  (description: "Type declarations are allowed in define")
  check-equal("", dlb-1, 10);
end test define-like-bind-1;

// This should have raised a condition, should not be able
// to set! dlb-1 once it has been declared an integer
//
define test define-like-bind-1a
  (description: "enforce declarations")
  check-condition("", <type-error>,  dlb-1 := "not an integer");
end test define-like-bind-1a;

define constant (dlb-2a, dlb-2b, dlb-2c) = values(1, 2, 3);

define test define-like-bind-2 (description: "multiple-values define")
  check-equal("", dlb-2a, 1); 
  check-equal("", dlb-2b, 2);
  check-equal("", dlb-2c, 3);
end test define-like-bind-2;

define constant (dlb-3a, dlb-3b, dlb-3c) = values(1);

define test define-like-bind-3 (description: "multivalues default to #f")
  check-equal("", dlb-3a, 1);
  check-equal("", dlb-3b, #f);
  check-equal("", dlb-3c, #f);
end test define-like-bind-3;

define constant (dlb-4a, dlb-4b, #rest dlb-4r) = values(1, 2, 3, 4, 5, 6, 7);

define test define-like-bind-4 (description: "multivalues with #rest")
  check-equal("", dlb-4a, 1);
  check-equal("", dlb-4b, 2);
  check("", instance?, dlb-4r, <sequence>);
  // KJP: instance -> instance?
  check-equal("", as(<list>, dlb-4r), #(3, 4, 5, 6, 7));
end test define-like-bind-4;

define constant (dlb-5a :: <integer>, dlb-5b :: <integer>)
  = values(1, 2, 3, 4, 5, 6, 7);

define test define-like-bind-5 (description: "discard extra values")
  check-equal("", dlb-5a, 1);
  check-equal("", dlb-5b, 2);
end test define-like-bind-5;
 
define suite test-defines-suite ()
  test defines;
  test define-like-bind-1;
  test define-like-bind-1a;
  test define-like-bind-2;
  test define-like-bind-3;
  test define-like-bind-4;
  test define-like-bind-5;
end suite;


