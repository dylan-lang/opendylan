Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosExternalizationContainment ()
  check("", test-idl-file, *corba-services-files*, "CosExternalizationContainment");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosExternalizationContainment",
"//  CosExternalizationContainment Module, p 8-26 CORBAservices,\n"
"// Externalization Service V1.0, 3/94\n"
"\n"
"#include <Containment.idl>\n"
"#include <CompoundExternalization.idl>\n"
"\n"
"module CosExternalizationContainment {\n"
"\n"
"\tinterface Relationship : \n"
"\t\t\tCosCompoundExternalization::Relationship,\n"
"\t\t\tCosContainment::Relationship {};\n"
"\n"
"\tinterface ContainsRole : \n"
"\t\t\tCosCompoundExternalization::Role,\n"
"\t\t\tCosContainment::ContainsRole {};\n"
"\n"
"\tinterface ContainedInRole : \n"
"\t\t\tCosCompoundExternalization::Role,\n"
"\t\t\tCosContainment::ContainedInRole {};\n"
"};\n"
"\n"
"\n");
