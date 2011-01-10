Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-Reference ()
  check("", test-idl-file, *corba-services-files*, "Reference");
end test;

add-idl-file!(
  *corba-services-files*,
  "Reference",
"// CosReference Module, p 9-50 CORBAservices,\n"
"//  Relationship Service V1.0, 3/94\n"
"\n"
"#ifndef COSREFERENCE\n"
"#define COSREFERENCE\n"
"#include <Graphs.idl>\n"
"\n"
"module CosReference {\n"
"\n"
"\tinterface Relationship : \n"
"\t\t\tCosRelationships::Relationship {};\n"
"\n"
"\tinterface ReferencesRole : CosGraphs::Role {};\n"
"\n"
"\tinterface ReferencedByRole : CosGraphs::Role {};\n"
"\n"
"};\n"
"#endif\n"
"\n");
