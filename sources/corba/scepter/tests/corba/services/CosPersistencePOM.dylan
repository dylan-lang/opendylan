Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistencePOM ()
  check("", test-idl-file, *corba-services-files*, "CosPersistencePOM");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistencePOM",
"// CosPersistencePOM Module, p 5-15 CORBAservices,\n"
"// Persistent Object Service V1.0, 3/94\n"
"\n"
"#include \"CosPersistencePDS.idl\"\n"
"\n"
"// CosPersistencePDS.idl #includes CosPersistencePID.idl\n"
"\n"
"module CosPersistencePOM {\n"
"\n"
"\tinterface Object;\n"
"\tinterface POM {\n"
"\t\tCosPersistencePDS::PDS connect (\n"
"\t\t\tin Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid disconnect (\n"
"\t\t\tin Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid store (\n"
"\t\t\tin Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid restore (\n"
"\t\t\tin Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid delete (\n"
"\t\t\tin Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t};\n"
"};\n"
"\n");
