Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-ObjectIdentity ()
  check("", test-idl-file, *corba-services-files*, "ObjectIdentity");
end test;

add-idl-file!(
  *corba-services-files*,
  "ObjectIdentity",
"// CosObjectIdentity Module, p 9-19 CORBAservices, Relationship\n"
"// Service V1.0, 3/94\n"
"\n"
"#ifndef COSOBJECTIDENTITY\n"
"#define COSOBJECTIDENTITY\n"
"module CosObjectIdentity {\n"
"\n"
"\ttypedef unsigned long ObjectIdentifier;\n"
"\n"
"\tinterface IdentifiableObject {\n"
"\t\t\treadonly attribute ObjectIdentifier constant_random_id;\n"
"\t\t\tboolean is_identical (\n"
"\t\t\t\tin IdentifiableObject other_object);\n"
"\t};\n"
"\n"
"};\n"
"#endif\n"
"\n"
"\n");
