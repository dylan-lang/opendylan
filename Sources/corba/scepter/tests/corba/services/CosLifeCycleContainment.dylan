Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosLifeCycleContainment ()
  check("", test-idl-file, *corba-services-files*, "CosLifeCycleContainment");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosLifeCycleContainment",
"//  CosLifeCycleContainment Module, p 6-42 CORBAservices,\n"
"//  Life Cycle Service V1.0, 3/94\n"
"\n"
"#include <Containment.idl>\n"
"#include <CompoundLifeCycle.idl>\n"
"\n"
"module CosLifeCycleContainment {\n"
"\n"
"\tinterface Relationship : \n"
"\t\t\tCosCompoundLifeCycle::Relationship,\n"
"\t\t\tCosContainment::Relationship {};\n"
"\n"
"\tinterface ContainsRole : \n"
"\t\t\tCosCompoundLifeCycle::Role,\n"
"\t\t\tCosContainment::ContainsRole {};\n"
"\n"
"\tinterface ContainedInRole : \n"
"\t\t\tCosCompoundLifeCycle::Role,\n"
"\t\t\tCosContainment::ContainedInRole {};\n"
"};\n"
"\n");
