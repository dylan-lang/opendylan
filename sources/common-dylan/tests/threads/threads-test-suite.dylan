Module:       common-dylan-test-suite
Synopsis:     Define the threads test suite
Author:       Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite threads-test-suite()
  suite conditional-updates-suite;
  suite thread-variables-suite;
  suite simple-locks-suite;
  suite recursive-locks-suite;
  suite semaphores-suite;
  suite notifications-suite;
  suite threads-suite;
end suite threads-test-suite;
