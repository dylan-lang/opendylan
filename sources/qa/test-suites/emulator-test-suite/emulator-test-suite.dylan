Module:    emulator-test-suite
Filename:  emulator-test-suite.dylan
Author:    Shri Amit(amit)
Synopsis:  A wrapper suite around all emulator supported test-suites
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite emulator-test-suite ()
  suite generic-test-suite;
  suite threads-test-suite
end; 
