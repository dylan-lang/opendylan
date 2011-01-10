Module:    pentium-test-suite
Filename:  pentium-test-suite.dylan
Author:    Shri Amit(amit)
Synopsis:  A wrapper suite for test-suites that are win32 pentium back end specific
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite pentium-test-suite ()
    suite win32-test-suite;
    suite threads-test-suite
end; 
