Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosEventChannel ()
  check("", test-idl-file, *corba-services-files*, "CosEventChannel");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosEventChannel",
"// CosEventChannelAdmin Module, p 4-15 CORBAservices, Event\n"
" // Service V1.0, 3/94\n"
"\n"
"#ifndef COSEVENTCHANNELADMIN\n"
"#define COSEVENTCHANNELADMIN\n"
"#include \"CosEventComm.idl\"\n"
"\n"
"module CosEventChannelAdmin {\n"
"\n"
" \texception AlreadyConnected {};\n"
"\texception TypeError {};\n"
"\n"
"\tinterface ProxyPushConsumer: CosEventComm::PushConsumer {\n"
"\t\tvoid connect_push_supplier(\n"
"\t\t\t\tin CosEventComm::PushSupplier push_supplier)\n"
"\t\t\traises(AlreadyConnected);\n"
"\t};\n"
"\n"
"\tinterface ProxyPullSupplier: CosEventComm::PullSupplier {\n"
"\t\tvoid connect_pull_consumer(\n"
"\t\t\t\tin CosEventComm::PullConsumer pull_consumer)\n"
"\t\t\traises(AlreadyConnected);\n"
"\t};\n"
"\n"
"\tinterface ProxyPullConsumer: CosEventComm::PullConsumer {\n"
"\t\tvoid connect_pull_supplier(\n"
"\t\t\t\tin CosEventComm::PullSupplier pull_supplier)\n"
"\t\t\traises(AlreadyConnected,TypeError);\n"
"\t};\n"
"\n"
"\tinterface ProxyPushSupplier: CosEventComm::PushSupplier {\n"
"\t\tvoid connect_push_consumer(\n"
"\t\t\t\tin CosEventComm::PushConsumer push_consumer)\n"
"\t\t\traises(AlreadyConnected, TypeError);\n"
"\t};\n"
"\n"
"\n"
"\tinterface ConsumerAdmin {\n"
"\t\tProxyPushSupplier obtain_push_supplier();\n"
"\t\tProxyPullSupplier obtain_pull_supplier();\n"
"\t};\n"
"\n"
"\tinterface SupplierAdmin {\n"
"\t\tProxyPushConsumer obtain_push_consumer();\n"
"\t\tProxyPullConsumer obtain_pull_consumer();\n"
"\t};\n"
"\n"
"\tinterface EventChannel {\n"
"\t\tConsumerAdmin for_consumers();\n"
"\t\tSupplierAdmin for_suppliers();\n"
"\t\tvoid destroy();\n"
"\t};\n"
"\n"
"};\n"
"#endif\n"
"\n");
