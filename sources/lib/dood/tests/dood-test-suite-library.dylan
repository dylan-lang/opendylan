module:    dylan-user
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dood-test-suite
  use common-dylan;
  use io;
  use dood;
  use testworks;
  use common-dylan;
  export dood-test-suite;
end library;

define module dood-test-suite
  use common-dylan, exclude: { format-to-string };
  use streams;
  use standard-io;
  use format;
  use format-out;
  use dood;
  use testworks;
  use simple-random;
  export dood-test-suite;
end module;
