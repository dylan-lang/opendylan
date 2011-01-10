Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test orbix-demo-idl-any_demo-test1 ()
  check("", test-idl-file, *orbix-demo-files*, "any_demo-test1");
end test;

add-idl-file!(
  *orbix-demo-files*,
  "any_demo-test1",
"// IDL file for the testAny demo code.  The IDL interface 'testAny' defines\n"
"// two operations.  op1 takes a parameter of type 'any';  op2 has an out\n"
"// parameter of type 'any'.\n"
" \n"
"\n"
"// define a struct so that we can pass a sequence of structs for the 'any'\n"
"// parameter.\n"
"\n"
"struct structS  {\n"
"   long i;\n"
"   float f;\n"
"};\n"
"\n"
"\n"
"// define a typedef to force sequence tc_ code generation\n"
"//\n"
"typedef sequence<structS> seqStruct;\n"
"\n"
"\n"
"interface testAny {\n"
"    void op1 (in any x);\n"
"\n"
"    void op2 (out any x);\n"
"};\n"
"\n");
