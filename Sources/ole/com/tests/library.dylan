Module:    Dylan-User
Synopsis:  Tests for the COM library
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library com-test
  use functional-dylan;
  use com;
  use testworks;
end library com-test;

define module test
  use functional-dylan;
  use simple-format;
  use streams-protocol;
  use com;
  use testworks;
end module test;


