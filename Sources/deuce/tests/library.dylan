Module:       Dylan-User
Synopsis:     Test suite for the Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deuce-test-suite
  use functional-dylan;
  use deuce;

  use streams;
  use testworks;

  export deuce-test-suite;
end library deuce-test-suite;

define module deuce-test-suite
  use functional-dylan,
    exclude: { position, position-if };
  use simple-format;
  use threads;
  use deuce-internals;

  use streams,
    rename: { <buffer> => streams/<buffer> };
  use testworks;

  export deuce-test-suite;
end module deuce-test-suite;
