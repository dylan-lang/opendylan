Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistencePDS ()
  check("", test-idl-file, *corba-services-files*, "CosPersistencePDS");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistencePDS",
"// CosPersistencePDS Module, p 5-20 CORBAservices, \n"
"// Persistent Object Service V1.0, 3/94\n"
"\n"
"#include \"CosPersistencePID.idl\"\n"
"\n"
"module CosPersistencePDS {\n"
"\n"
"\tinterface Object;\n"
"\tinterface PDS {\n"
"\t\tPDS \tconnect (in Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid disconnect (in Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid store (in Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid restore (in Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t\tvoid delete (in Object obj, \n"
"\t\t\tin CosPersistencePID::PID p);\n"
"\t};\n"
"};\n"
"\n");
