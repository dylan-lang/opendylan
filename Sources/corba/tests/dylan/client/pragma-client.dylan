Module:    pragma-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test repository-id-test ()
  check-equal("Repository ID for ::M1::T1", corba/typecode/id(class-typecode(M1/<T1>)), "IDL:M1/T1:1.0");
  check-equal("Repository ID for ::M1::T2", corba/typecode/id(class-typecode(M1/<T2>)), "DCE:d62207:3");
  check-equal("Repository ID for ::M2::M3::T3", corba/typecode/id(class-typecode(M2/M3/<T3>)), "IDL:P2/T3:1.0");
  check-equal("Repository ID for ::M2::T4", corba/typecode/id(class-typecode(M2/<T4>)), "IDL:P1/M2/T4:2.4");
  check-equal("Repository ID for ::M4::M3::T3", corba/typecode/id(class-typecode(M4/M3/<T3>)), "IDL:P2/T3:1.1");
  check-equal("Repository ID for ::M4::T4", corba/typecode/id(class-typecode(M4/<T4>)), "IDL:P1/M2/T4:2.5");
end test;

define suite pragma-test-suite ()
  test repository-id-test;
end suite;
