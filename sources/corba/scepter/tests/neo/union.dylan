Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-union ()
  check("", test-idl-file, *neo-files*, "union");
end test;

add-idl-file!(
  *neo-files*,
  "union",
"struct Bar {\n"
"    long\tb1;\n"
"    char\tb2;\n"
"};\n"
"\n"
"union Foo switch (long) {\n"
"case 1: long x;\n"
"case 2: Bar y;\n"
"default: char z;\n"
"};\n"
"\n"
"\n"
"union Foo2 switch (char) {\n"
"  case 'a': long x;\n"
"  case 'b': Foo y;\n"
"};\n"
"\n"
"interface a {\n"
"\tstruct astruct {\n"
"\t\tFoo2\ta1;\n"
"\t\tFoo\ta2;\n"
"\t\tBar\ta3;\n"
"\t};\n"
"\tFoo2\topA(in astruct b);\n"
"};\n"
"\n"
"\n");
