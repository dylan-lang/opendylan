Module:       collections-test-suite
Synopsis:     Test suite for collections library
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library collections-test-suite
  use functional-dylan;
  use testworks;
  use testworks-specs;
  use collections;

  export collections-test-suite;
end library collections-test-suite;

define module collections-test-suite
  use functional-dylan;
  use simple-format;
  use testworks;
  use testworks-specs;

  use byte-vector;
  use bit-vector;
  use bit-set;
  use collectors;
  use plists;
  use set;
  use table-extensions;

  export collections-test-suite;
end module collections-test-suite;
