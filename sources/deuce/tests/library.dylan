Module:       Dylan-User
Synopsis:     Test suite for the Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deuce-test-suite
  use common-dylan;
  use deuce;

  use io;
  use testworks;

  export deuce-test-suite;
end library deuce-test-suite;

define module deuce-test-suite
  use common-dylan,
    exclude: { position, position-if, format-to-string };
  use format;
  use threads;
  use deuce-internals;

  use streams,
    rename: { <buffer> => streams/<buffer> };
  use testworks;

  export deuce-test-suite;
end module deuce-test-suite;
