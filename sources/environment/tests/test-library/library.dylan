Module:    dylan-user
Synopsis:  A library to be used by the environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-test-library
  use common-dylan;
  use duim;

  export environment-test-library;

  export no-such-module;
end library environment-test-library;
