Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosTypedEventComm ()
  check("", test-idl-file, *corba-services-files*, "CosTypedEventComm");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosTypedEventComm",
"// CosTyped Event Module, p 4-22 CORBAservices, Event Service\n"
"// V1.0, 3/94\n"
"\n"
"#include \"CosEventComm.idl\" \n"
"\n"
"module CosTypedEventComm {\n"
"\n"
"\tinterface TypedPushConsumer : CosEventComm::PushConsumer {\n"
"\t\tObject get_typed_consumer(); \n"
"\t};\n"
"\n"
"\tinterface TypedPullSupplier : CosEventComm::PullSupplier { \t\t \n"
"\t\tObject get_typed_supplier();\n"
"\t};\n"
"\n"
"}; \n"
"\n");
