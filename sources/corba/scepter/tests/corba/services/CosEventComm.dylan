Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosEventComm ()
  check("", test-idl-file, *corba-services-files*, "CosEventComm");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosEventComm",
"// CosEventComm Module, CORBAservices p 4-8, Event Service V1.0 3/94\n"
"\n"
"#ifndef COSEVENTCOMM\n"
"#define COSEVENTCOMM\n"
"module CosEventComm {\n"
"\n"
"\texception Disconnected{};\n"
"\n"
"\tinterface PushConsumer {\n"
"\t\tvoid push (in any data) raises(Disconnected);\n"
"\t\tvoid disconnect_push_consumer(); \n"
"\t};\n"
"\n"
"    interface PushSupplier {\n"
"\t\tvoid disconnect_push_supplier();\n"
"\t};\n"
"\n"
"\tinterface PullSupplier {\n"
"\t\tany pull () raises(Disconnected);\n"
"\t\tany try_pull (out boolean has_event) \n"
"\t\t\traises(Disconnected);\n"
"\t\tvoid\t disconnect_pull_supplier(); \n"
"\t};\n"
"\n"
"\tinterface PullConsumer {\n"
"\t\tvoid disconnect_pull_consumer();\n"
"\t};\n"
"\n"
"};\n"
"#endif\n"
"\n"
"\n");
