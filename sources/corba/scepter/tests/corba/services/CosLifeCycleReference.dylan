Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosLifeCycleReference ()
  check("", test-idl-file, *corba-services-files*, "CosLifeCycleReference");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosLifeCycleReference",
"// CosLifeCycleReference Module, p 6-44 CORBAservices,\n"
"// Life Cycle Service V1.0, 3/94\n"
"\n"
"#include <Reference.idl>\n"
"#include <CompoundLifeCycle.idl>\n"
"\n"
"module CosLifeCycleReference {\n"
"\n"
"\tinterface Relationship : \n"
"\t\t\tCosCompoundLifeCycle::Relationship,\n"
"\t\t\tCosReference::Relationship {};\n"
"\n"
"\tinterface ReferencesRole : \n"
"\t\t\tCosCompoundLifeCycle::Role,\n"
"\t\t\tCosReference::ReferencesRole {};\n"
"\n"
"\tinterface ReferencedByRole : \n"
"\t\t\tCosCompoundLifeCycle::Role,\n"
"\t\t\tCosReference::ReferencedByRole {};\n"
"};\n"
"\n");
