Module:    common-dylan-test-suite-app
Author:    Andy Armstrong
Synopsis:  An application library for test-suite common-dylan-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite common-dylan-test-suite-app ()
  suite dylan-test-suite;
  suite common-dylan-test-suite;
end suite common-dylan-test-suite-app;

run-test-application(common-dylan-test-suite-app);
