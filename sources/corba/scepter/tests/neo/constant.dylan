Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-constant ()
  check("", test-idl-file, *neo-files*, "constant");
end test;

add-idl-file!(
  *neo-files*,
  "constant",
"const short l = 4;\n"
"\n"
"enum N { zero, un , deux, trois };\n"
"\n"
"exception E {\n"
"\tshort cause ;\n"
"\t};\n"
"\n"
"module M {\n"
"\tconst short l = 4; \n"
"\n"
"\tenum N { zero, un , deux, trois };\n"
"\n"
"\texception E {\n"
"        short cause ;\n"
"        };\n"
"\t\n"
"\tinterface A {\n"
"\t        const short l = 4; \n"
" \n"
"\t        enum N { zero, un , deux, trois };\n"
" \n"
"       \t\t exception E {\n"
"        \tshort cause ;\n"
"        \t};\n"
"\tN foo(in N x, out N y, inout N z);\n"
"\t};\n"
"};\n"
"\n"
"\n");
