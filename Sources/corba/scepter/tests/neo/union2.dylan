Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-union2 ()
  check("", test-idl-file, *neo-files*, "union2", regression?: #t);
end test;

add-idl-file!(
  *neo-files*,
  "union2",
"enum FooDisc {\n"
"\tFOO1,\n"
"\tFOO2\n"
"};\n"
"union Foo switch (FooDisc) {\n"
"case FOO1: long x;\n"
"case FOO2: string y;\n"
"default: char z;\n"
"};\n"
"\n"
"\n"
"struct Bar {\n"
"    enum BarDisc {\n"
"\tBAR1,\n"
"\tBAR2\n"
"    } kind;\n"
"    union Foo switch (BarDisc) {\n"
"      case BAR1: long x;\n"
"      case BAR2: long y;\n"
"    } addr;\n"
"};\n"
"\n", regression:
"enum FooDisc {FOO1, FOO2};\n"
"union Foo switch (FooDisc) {\n"
"  case FOO1: long x;\n"
"  case FOO2: string y;\n"
"  default: char z;\n"
"  };\n"
"struct Bar {\n"
"  enum BarDisc {BAR1, BAR2};\n"
"  BarDisc kind;\n"
"  union Foo switch (BarDisc) {\n"
"    case BAR1: long x;\n"
"    case BAR2: long y;\n"
"    };\n"
"  Foo addr;\n"
"  };\n");
