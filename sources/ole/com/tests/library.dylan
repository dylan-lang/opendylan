Module:    Dylan-User
Synopsis:  Tests for the COM library
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library com-test
  use common-dylan;
  use com;
  use testworks;
end library com-test;

define module test
  use common-dylan;
  use streams-protocol;
  use com;
  use testworks;
end module test;


