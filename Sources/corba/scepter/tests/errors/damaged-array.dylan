Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test damaged-idl-array ()
  check("", test-damaged-idl-file, *damaged-files*, "damaged-array");
end test;

add-idl-file!(
  *damaged-files*,
  "damaged-array",
"\n"
"typedef sequence<long,10> vec10;\n"
"typedef char str22[22][2];\n"
"typedef boolean boolarray[3][40];\n"
"\n"
"\n"
"struct A {\n"
"    long\ta1;\n"
"    char\ta2[17];\n"
"    char\ta3[10][2];\n"
"//    vec10\ta3;\n"
"};\n"
"\n"
"struct B {\n"
"    long\tb1;\n"
"    A\t\tb2;\n"
"};\n"
"\n"
"struct C {\n"
"    long\tc1;\n"
"    long\tc2;\n"
"};\n"
"\n"
"typedef A A_matrix[2][3][4];\n"
"typedef B B_matrix[2][3][4];\n"
"typedef C C_matrix[2][3][4];\n"
"\n"
"typedef A_matrix A_rmatrix[2][3][4][5];\n"
"\n"
"interface A_i {\n"
"    A\t\top(in A a1, inout A a2, out A a3);\n"
"    str22\top2(in str22 a1, inout str22 a2, out str22 a3);\n"
"    str22       op3(in short a1[3][4], inout short a2[3][4], out short a3[4][4]);\n"
"    A_matrix\top4(in A a1, inout A a2, out A a3);\n"
"    attribute str22 attribute_array;\n"
"    void foo(in any x[3], out any y[4], inout any z[3]);\n"
"};\n"
"\n"
"\n");

