Module:    corba-tests-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite corba-test-suite ()
  suite iiop-tests;
  suite request-tests;
  suite context-tests;
  suite typecode-tests;
end suite corba-test-suite;
