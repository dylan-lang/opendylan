Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-inherit ()
  check("", test-idl-file, *neo-files*, "inherit");
end test;

add-idl-file!(
  *neo-files*,
  "inherit",
"interface one {\n"
"    void op_one();\n"
"};\n"
"interface two : one {\n"
"    void op_two();\n"
"};\n"
"interface three : one {\n"
"    void op_three();\n"
"};\n"
"interface four : two, three {\n"
"    void op_four();\n"
"};\n"
"\n"
"interface five : four, three, two {\n"
"    void op_five();\n"
"};\n"
"\n"
"\n");
