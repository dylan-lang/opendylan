Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosTypedEventChannel ()
  check("", test-idl-file, *corba-services-files*, "CosTypedEventChannel");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosTypedEventChannel",
"// CosTypedEventChannelAdmin Module, p 4- 25 CORBAservices,\n"
"// Event Service V1.0, 3/94\n"
"\n"
"#include \"CosEventChannel.idl\" \n"
"#include \"CosTypedEventComm.idl\"\n"
"module CosTypedEventChannelAdmin { \t \n"
"\texception InterfaceNotSupported {}; \n"
"\texception NoSuchImplementation {}; \n"
"\ttypedef string Key; \n"
"\n"
"\tinterface TypedProxyPushConsumer : \n"
"\t\t\tCosEventChannelAdmin::ProxyPushConsumer, \n"
"\t\t\tCosTypedEventComm::TypedPushConsumer  { }; \n"
"\n"
"\tinterface TypedProxyPullSupplier :\n"
" \t\t\tCosEventChannelAdmin::ProxyPullSupplier, \n"
"\t\t\tCosTypedEventComm::TypedPullSupplier { }; \n"
"\n"
"\tinterface TypedSupplierAdmin : \n"
"\t\t\tCosEventChannelAdmin::SupplierAdmin { \n"
"\t\tTypedProxyPushConsumer obtain_typed_push_consumer(\n"
" \t\t\t\tin Key supported_interface) \t\t    \n"
"\t\t\traises(InterfaceNotSupported); \n"
"\t\tCosEventChannelAdmin::ProxyPullConsumer obtain_typed_pull_consumer ( \n"
"\t\t\t\tin Key uses_interface) \t\t   \n"
"\t\t\t raises(NoSuchImplementation); \n"
"\t}; \n"
"\n"
"\tinterface TypedConsumerAdmin : \n"
"\t\t\tCosEventChannelAdmin::ConsumerAdmin { \n"
"\t\tTypedProxyPullSupplier obtain_typed_pull_supplier( \n"
"\t\t\t\tin Key supported_interface) \t\t    \n"
"\t\t\traises (InterfaceNotSupported); \n"
"\t\tCosEventChannelAdmin::ProxyPushSupplier obtain_typed_push_supplier( \n"
"\t\t\t\tin Key uses_interface) \t\t    \n"
"\t\t\traises(NoSuchImplementation); \n"
"\t}; \n"
"\n"
"\tinterface TypedEventChannel { \n"
"\t\tTypedConsumerAdmin for_consumers(); \n"
"\t\tTypedSupplierAdmin for_suppliers();                \n"
"\t\tvoid destroy (); \n"
"\t}; \n"
"};\n"
"\n");
