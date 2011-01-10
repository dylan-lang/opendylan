Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-TSInteroperation ()
  check("", test-idl-file, *corba-services-files*, "TSInteroperation");
end test;

add-idl-file!(
  *corba-services-files*,
  "TSInteroperation",
"\n"
"#include \"CosTransactions.idl\"\n"
"// PIDL for CosTSInteroperation Module, p 10-59 \n"
"// CORBAservices, Transaction Service V1.0, 3/94\n"
"module CosTSInteroperation { // PIDL\n"
"\tstruct otid_t {\n"
"\t\tlong formatID; /*format identifier. 0 is OSI TP */\n"
"\t\tlong bequal_length;\n"
"\t\tsequence <octet> tid;\n"
"\t};\n"
"\tstruct TransIdentity {\n"
"\t\tCosTransactions::Coordinator coordinator;\n"
"\t\tCosTransactions::Terminator terminator;\n"
"\t\totid_t otid;\n"
"\t};\n"
"\tstruct PropagationContext {\n"
"\t\tunsigned long timeout;\n"
"\t\tTransIdentity current;\n"
"\t\tsequence <TransIdentity> parents;\n"
"\t\tany implementation_specific_data;\n"
"\t};\n"
"};\n"
"\n"
"\n");
