Module:    dylan-user
Synopsis:  A library to be used by the environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-test-library
  use common-dylan;
  use threads;
  use duim;

  export <test-object>;

  export no-such-definition;
end module environment-test-library;

define module internal-module
  use common-dylan;

  export no-such-definition;
end module internal-module;
