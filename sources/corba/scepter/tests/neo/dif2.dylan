Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-dif2 ()
  check("", test-idl-file, *neo-files*, "dif2");
end test;

add-idl-file!(
  *neo-files*,
  "dif2",
"/* A test interface for DIF1 */\n"
"\n"
"interface ex1\n"
"{\n"
"\tvoid op0();\n"
"\tfloat op1(in float f, out double d, inout long l);\n"
"\tdouble op2(in double d, out long l, inout short s);\n"
"\tlong op3(in long l, out short s, inout unsigned long ul);\n"
"\tshort op4(in short s, out unsigned long ul, inout unsigned short us);\n"
"\tunsigned long op5(inout unsigned long ul, in unsigned short us, out char c, in boolean b);\n"
"\tunsigned short op6(out unsigned short us, inout char c);\n"
"};\n"
"\n"
"//interface ex3;\n"
"\n"
"interface ex2 : ex1 \n"
"{\n"
"\t// operations continued from ex1\n"
"\tchar op7(in char c, out boolean b, inout octet o);\n"
"\tboolean op8(out boolean b, in octet o);\n"
"\toctet op9(inout octet o, out string s, in string <10> ls, inout long l);\n"
"\tstring op10(in string s, out string <10> ls);\n"
"\tstring <10> op11(inout string <10> ls, out long l);\n"
"\tlong op12(in long l);\n"
"//\tex3 op13(Environment *ev);\n"
"};\n"
"\n"
"interface ex3 : ex1\n"
"{\n"
"\t// attributes\n"
"\treadonly attribute float float_attribute;\n"
"\tattribute double double_attribute;\n"
"\treadonly attribute long long_attribute;\n"
"\tattribute short short_attribute;\n"
"\treadonly attribute unsigned long unsigned_long_attribute;\n"
"\n"
"};\n"
"\n"
"interface ex4 : ex2, ex3\n"
"{\n"
"\tattribute unsigned short unsigned_short_attribute;\n"
"\treadonly attribute char char_attribute;\n"
"\tattribute boolean boolean_attribute;\n"
"\treadonly attribute octet octet_attribute;\n"
"\tattribute string string_attribute;\n"
"\treadonly attribute string <10> limited_string_attribute;\n"
"\treadonly attribute ex4 an_object_attribute;\n"
"};\n"
"\n");
