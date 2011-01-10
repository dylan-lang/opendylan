Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-Containment ()
  check("", test-idl-file, *corba-services-files*, "Containment");
end test;

add-idl-file!(
  *corba-services-files*,
  "Containment",
"// CosContainment Module, p 9- 48 CORBAservices, Relationship\n"
"// Service V1.0, 3/94\n"
"\n"
"#ifndef COSCONTAINMENT\n"
"#define COSCONTAINMENT\n"
"#include <Graphs.idl>\n"
"\n"
"module CosContainment {\n"
"\n"
" \tinterface Relationship :\n"
" \t\t\tCosRelationships::Relationship {};\n"
"\n"
" \tinterface ContainsRole : CosGraphs::Role {};\n"
"\n"
" \tinterface ContainedInRole : CosGraphs::Role {};\n"
"\n"
"};\n"
"#endif\n"
"\n");
