Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-TSPortability ()
  check("", test-idl-file, *corba-services-files*, "TSPortability");
end test;

add-idl-file!(
  *corba-services-files*,
  "TSPortability",
"// PIDL for CosTSPortability Module, p 10-63 \n"
"// CORBAservices, Transaction Service V1.0, 3/94\n"
"\n"
"#include \"CORBA.idl\"\n"
"#include \"TSInteroperation.idl\"\n"
"\n"
"module CosTSPortability { // PIDL\n"
"\ttypedef long ReqId;\n"
"\n"
"\tinterface Sender {\n"
"\t\tvoid sending_request(in ReqId id,\n"
"\t\t\tout CosTSInteroperation::PropagationContext ctx);\n"
"\t\tvoid received_reply(in ReqId id,\n"
"\t\t\tin CosTSInteroperation::PropagationContext ctx, \n"
"\t\t\tin CORBA::Environment env);\n"
"\t};\n"
"\n"
"\tinterface Receiver {\n"
"\t\tvoid received_request(in ReqId id,\n"
"\t\t\tin CosTSInteroperation::PropagationContext ctx);\n"
"\t\tvoid sending_reply(in ReqId id,\n"
"\t\t\tout CosTSInteroperation::PropagationContext ctx);\n"
"\t};\n"
"};\n"
"\n");
