Module:    dylan-user
Synopsis:  An application for use by the environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-test-application
  use common-dylan;
  use threads;
  use duim;

  use environment-test-library;

end module environment-test-application;
