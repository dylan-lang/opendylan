Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-primtypes ()
  check("", test-idl-file, *neo-files*, "primtypes");
end test;

add-idl-file!(
  *neo-files*,
  "primtypes",
"interface ex1\n"
"{\n"
"    attribute  long l1;\n"
"    long\tm1(in long a);\n"
"    ex1\t\tmtypes(\n"
"\tin long a1, inout long a2, out   long a3,\n"
"\tin unsigned long b1, inout unsigned long b2, out unsigned long b3,\n"
"\tin short c1,inout short c2, out short c3,\n"
"\tin unsigned short d1, inout unsigned short d2, out unsigned short d3,\n"
"\tin float e1, inout float e2, out float e3,\n"
"\tin double f1, inout double f2, out double f3,\n"
"\tin char g1,  inout char g2, out char g3\n"
"    );\n"
"\t\t   \n"
"};\n"
"\n"
"interface ex2 : ex1 \n"
"{\n"
"    readonly attribute long l2;\n"
"\n"
"    long\tm2(in long a, out long b);\n"
"    void\tmv();\n"
"};\n"
"\n");
