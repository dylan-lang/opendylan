Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-core-idl-IIOP ()
  check("", test-idl-file, *corba-core-files*, "IIOP");
end test;

add-idl-file!(
  *corba-core-files*,
  "IIOP",
"//  IIOP module described in CORBA V2, 7-95 chap 12\n"
"// Complete IDL for IIOP module: CORBA V2, 7-95 p 12-31\n"
"module IIOP {\t\t\t\t\t\t\t\t\t\t\t\t\t// IDL\n"
"\tstruct Version {\n"
"\t\tchar\t\t\tmajor;\n"
"\t\tchar\t\t\tminor;\n"
"\t};\n"
"\n"
"\tstruct ProfileBody {\n"
"\t\tVersion\t\t\t\tiiop_version;\n"
"\t\tstring\t\t\t\thost;\n"
"\t\tunsigned short\t\t\t\tport;\n"
"\t\tsequence <octet>\t\t\t\tobject_key;\n"
" \t};\n"
"};\n"
"\n");
