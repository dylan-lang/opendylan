Module:    generic-test-suite
Filename:  generic-test-suite.dylan
Author:    Shri Amit(amit)
Synopsis:  A wrapper suite around all platform independent test-suites
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite generic-test-suite ()
  suite libraries-test-suite;
  suite deuce-test-suite;
  suite environment-suite;
  suite duim-suite
end suite generic-test-suite; 
