Module:    dylan-user
Synopsis:  Functional Objects extensions library test suite
Author:	   James Kirsch, Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library functional-dylan-test-suite
  use functional-dylan;
  use testworks;
  use testworks-specs;
  use dylan-test-suite;

  export functional-dylan-test-suite;
end library functional-dylan-test-suite;

define module functional-dylan-test-suite
  use functional-dylan;
  use simple-format;
  use simple-random;
  use testworks;
  use testworks-specs;
  use dylan-test-suite;		// to get collection testing

  export functional-dylan-test-suite;
end module functional-dylan-test-suite;
