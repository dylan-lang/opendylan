module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library socket-test-suite
  use functional-dylan;
  use testworks;
  use byte-vector;

  use socket;

  export socket-test-suite;
end library socket-test-suite;

define module socket-test-suite
  use functional-dylan;
  use testworks;
  use byte-vector;

  use inet-address;
  use socket;

  export socket-test-suite;
end module socket-test-suite;

