Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-struct ()
  check("", test-idl-file, *neo-files*, "struct");
end test;

add-idl-file!(
  *neo-files*,
  "struct",
"struct bar {\n"
"\tlong\tb1;\n"
"\tlong\tb2;\n"
"\tstring<20> b3;\n"
"};\n"
"\n"
"struct bar2 {\n"
"\tlong\tb3;\n"
"\tbar\tb4;\n"
"};\n"
"\n"
"struct bar3 {\n"
"    long b1;\n"
"    long b2;\n"
"    long b3;\n"
"};\n"
"\n"
"struct bar4 {\n"
"    long b1;\n"
"    bar2 b2;\n"
"    long b3;\n"
"    bar3 b4;\n"
"    long b5;\n"
"};\n"
"\n"
"\n"
"interface foo {\n"
"\tlong\top(in bar a, out bar2 b);\n"
"};\n"
"interface foo2 : foo {\n"
"    bar\top2(in bar2 a);\n"
"    long op3(in long a, out char b);\n"
"};\n"
"\n"
"\n"
"typedef\t\tunsigned short\tCoord;\n"
"\n"
"struct CellPos {\n"
"    Coord\trow;      \n"
"    Coord\tcol;\n"
"};\n"
"\n"
"struct CellPos2 {\n"
"\tunsigned short\trow;\n"
"\tunsigned short\tcol;\n"
"};\n"
"\n"
"struct CellPos3 {\n"
"\tCoord\trow;\n"
"\tCoord\tcol;\n"
"\tunsigned short col2;\n"
"};\n"
"\n");
