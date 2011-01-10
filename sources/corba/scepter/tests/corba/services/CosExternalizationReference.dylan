Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosExternalizationReference ()
  check("", test-idl-file, *corba-services-files*, "CosExternalizationReference");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosExternalizationReference",
"// CosExternalizationReference Module, p 8-28 CORBAservices, \n"
"// Externalization Service V1.0, 3/94\n"
"\n"
"#include <Reference.idl>\n"
"#include <CompoundExternalization.idl>\n"
"\n"
"module CosExternalizationReference {\n"
"\n"
"\tinterface Relationship : \n"
"\t\t\tCosCompoundExternalization::Relationship,\n"
"\t\t\tCosReference::Relationship {};\n"
"\n"
"\tinterface ReferencesRole : \n"
"\t\t\tCosCompoundExternalization::Role,\n"
"\t\t\tCosReference::ReferencesRole {};\n"
"\n"
"\tinterface ReferencedByRole : \n"
"\t\t\tCosCompoundExternalization::Role,\n"
"\t\t\tCosReference::ReferencedByRole {};\n"
"};\n"
"\n");
