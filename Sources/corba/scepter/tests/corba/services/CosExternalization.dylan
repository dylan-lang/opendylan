Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosExternalization ()
  check("", test-idl-file, *corba-services-files*, "CosExternalization");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosExternalization",
"// CosExternalization Module, 8-12 CORBAservices,\n"
"// Externalization Service V1.0, 3/94\n"
"\n"
"\n"
"#include <LifeCycle.idl>\n"
"#include <Stream.idl>\n"
"module CosExternalization {\n"
"\t\texception InvalidFileNameError{};\n"
"\t\texception ContextAlreadyRegistered{};\n"
"\t\tinterface Stream: CosLifeCycle::LifeCycleObject{\n"
"\t\t\tvoid externalize(\n"
"\t\t\t\t\tin CosStream::Streamable theObject);\n"
"\t\t\tCosStream::Streamable internalize( \n"
"\t\t\t\t\tin CosLifeCycle::FactoryFinder there)\n"
"\t\t\t\traises( CosLifeCycle::NoFactory,\n"
"\t\t\t\t\tCosStream::StreamDataFormatError );\n"
"\t\t\tvoid begin_context()\n"
"\t\t\t\traises( ContextAlreadyRegistered);\n"
"\t\t\tvoid end_context(); \n"
"\t\t\tvoid flush();\n"
"\t\t};\n"
"\t\tinterface StreamFactory {\n"
"\t\t\tStream create();\n"
"\t\t};\n"
"\t\tinterface FileStreamFactory {\n"
"\t\t\tStream create(\n"
"\t\t\t\t\tin string theFileName) \n"
"\t\t\t\traises( InvalidFileNameError );\n"
"\t\t};\n"
"};\n"
"\n");
