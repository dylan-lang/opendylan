Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistencePO ()
  check("", test-idl-file, *corba-services-files*, "CosPersistencePO");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistencePO",
"// CosPersistencePO Module, p 5-12 CORBAservices,\n"
"// Persistent Object Service V1.0, 3/94\n"
"\n"
"#include \"CosPersistencePDS.idl\"\n"
"// CosPersistencePDS.idl #includes CosPersistencePID.idl\n"
"\n"
"module CosPersistencePO {\n"
"\n"
"\tinterface PO {\n"
"\t\tattribute CosPersistencePID::PID p;\n"
"\t\tCosPersistencePDS::PDS connect (\n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid disconnect (in CosPersistencePID::PID p);\n"
"\t\tvoid store (in CosPersistencePID::PID p);\n"
"\t\tvoid restore (in CosPersistencePID::PID p);\n"
"\t\tvoid delete (in CosPersistencePID::PID p);\n"
"\t};\n"
"\n"
"\tinterface SD {\n"
"\t\tvoid pre_store();\n"
"\t\tvoid post_restore();\n"
"\t};\n"
"};\n"
"\n");
