module:    optimizations-test-suite
Synopsis:  A test suite for compiler optimizations
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic static-function (x);

define method static-function (x :: <integer>) end;

define method static-function (x :: <list>) end;

define open generic open-function (x);

define method open-function (x :: <integer>) end;

define method open-function (x :: <list>) end;

define method dispatch-tests ()
  static-function(1);
  static-function(#());
  open-function(1);
  open-function(#());
end method;

dispatch-tests();
