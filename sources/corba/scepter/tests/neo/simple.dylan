Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-simple ()
  check("", test-idl-file, *neo-files*, "simple");
end test;

add-idl-file!(
  *neo-files*,
  "simple",
"typedef sequence<long,10> vec10;\n"
"\n"
"exception simple_error {\n"
"    long reason;\n"
"};\n"
"\n"
"interface simple {\n"
"\tlong op(in long a, out simple b, inout long c) raises (simple_error);\n"
"};\n"
"\n"
"interface simple2 : simple {\n"
"    exception simple2_error {\n"
"\tlong\treason2;\n"
"    };\n"
"    long op2(in long a, in simple b, out simple2 c) raises (simple2_error);\n"
"};\n"
"\n");
