Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-module ()
  check("", test-idl-file, *neo-files*, "module");
end test;

add-idl-file!(
  *neo-files*,
  "module",
"module mod1\n"
"{\n"
"    module mod2\n"
"    {\n"
"\tinterface ex1\n"
"\t{\n"
"\t    attribute long\tl1;\n"
"\t    readonly attribute long l2;\n"
"\n"
"\t    long\tm1(in long a);\n"
"\t};\n"
"\n"
"\tinterface ex2 : ex1\n"
"\t{\n"
"\t    attribute long\tl3;\n"
"\n"
"\t    long\tm2(in long a, out long b);\n"
"\t};\n"
"    };\n"
"};\n"
"\n");
