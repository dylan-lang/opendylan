Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-core-idl-DCE_CIOP ()
  check("", test-idl-file, *corba-core-files*, "DCE_CIOP");
end test;

add-idl-file!(
  *corba-core-files*,
  "DCE_CIOP",
"\n"
"// Jason\n"
"#include \"IOP.idl\"\n"
"\n"
"// #include \"CORBA.idl\"\n"
"//\n"
"// Jason: hack presence of CORBA module to avoid\n"
"// parsing that monster more than once in the test\n"
"// suite\n"
"\n"
"module CORBA {\n"
"  interface Principal;\n"
"};\n"
"\n"
"// DCE CIOP module described in CORBA V2, 7-95 chap 13\n"
"// IDL for DCE CIOP module: CORBA V2, 7-95  p 13-2\n"
"module DCE_CIOP {\n"
"\tstruct InvokeRequestHeader {\n"
"\t\tboolean byte_order;\n"
"\t\tIOP::ServiceContextList service_context;\n"
"\t\tsequence <octet> object_key;\n"
"\t\tstring endpoint_id;\n"
"\t\tstring operation;\n"
"\t\tCORBA::Principal principal;\n"
"\t\tsequence <string> client_context;\n"
"\n"
"\t\t// in and inout parameters follow\n"
"\t};\n"
"\tenum InvokeResponseStatus {\n"
"\t\tINVOKE_NO_EXCEPTION,\n"
"\t\tINVOKE_USER_EXCEPTION,\n"
"\t\tINVOKE_SYSTEM_EXCEPTION,\n"
"\t\tINVOKE_LOCATION_FORWARD,\n"
"\t\tINVOKE_TRY_AGAIN\n"
"\t};\n"
"\n"
"\tstruct InvokeResponseHeader {\n"
"\t\tboolean byte_order;\n"
"\t\tIOP::ServiceContextList service_context;\n"
"\t\tInvokeResponseStatus status;\n"
"\n"
"\t\t// if status = INVOKE_NO_EXCEPTION,\n"
"\t\t// result then inouts and outs follow\n"
"\n"
"\t\t// if status = INVOKE_USER_EXCEPTION or\n"
"\t\t// INVOKE_SYSTEM_EXCEPTION, an exception follows\n"
"\n"
"\t\t// if status = INVOKE_LOCATION_FORWARD, an \n"
"\t\t// IOP::MultipleComponentsProfile follows\n"
"\t};\n"
"\n"
"\tstruct LocateRequestHeader {\n"
"\t\tboolean byte_order;\n"
"\t\tsequence <octet> object_key;\n"
"\t\tstring endpoint_id;\n"
"\t\tstring operation;\n"
"\n"
"\t\t// no body follows\n"
"\t};\n"
"\n"
"};\n"
"\n");
