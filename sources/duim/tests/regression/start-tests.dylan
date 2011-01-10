Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  A regression test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main
    (arguments :: <sequence>) => (status-code :: <integer>)
  start-tests();
  0
end method main;

main(#[]);
