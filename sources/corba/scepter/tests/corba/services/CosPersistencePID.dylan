Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistencePID ()
  check("", test-idl-file, *corba-services-files*, "CosPersistencePID");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistencePID",
"module CosPersistencePID {\n"
"\tinterface PID {\n"
"\t\tattribute string datastore_type;\n"
"\t\tstring get_PIDString(); };\n"
"};\n"
"\n");
