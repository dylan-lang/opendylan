Module:    win32-test-suite
Filename:  win32-test-suite.dylan
Author:    Shri Amit(amit)
Synopsis:  This mother suite is a wrapper around win32 specific test-suites
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite win32-test-suite ()
    suite generic-test-suite;
    suite c-ffi-test
end; 
