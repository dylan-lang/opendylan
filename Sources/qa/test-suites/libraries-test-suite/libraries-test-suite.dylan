Module:    libraries-test-suite
Filename:  libraries-test-suite.dylan
Author:    Shri Amit(amit)
Synopsis:  A wrapper suite around the test-suites for various dylan libraries
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite libraries-test-suite ()
  suite dylan-test-suite;
  suite common-dylan-test-suite;
  suite functional-dylan-test-suite;
  suite collections-test-suite;
  suite system-test-suite;
  suite io-test-suite;
  suite testworks-test-suite;
end suite libraries-test-suite; 
