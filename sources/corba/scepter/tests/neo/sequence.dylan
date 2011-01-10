Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-sequence ()
  check("", test-idl-file, *neo-files*, "sequence");
end test;

add-idl-file!(
  *neo-files*,
  "sequence",
"typedef sequence<long,10> vec10;\n"
"typedef sequence<char,20> c20;\n"
"typedef sequence<c20,23> cc23;\n"
"typedef sequence<char,24> c24;\n"
"typedef sequence< sequence<long,10>, 10> c30;\n"
"struct TProgram {\n"
"    string\t\tpath;\n"
"    sequence<string>\targv;\n"
"    sequence<string>\tenvp;\n"
"};\n"
"\n"
"struct foo {\n"
"    long\tf1;\n"
"    char\tf2;\n"
"};\n"
"\n"
"typedef foo\t\t\tfoo_ar[20];\n"
"typedef sequence<foo> \t\tf_unbounded;\n"
"typedef sequence<foo_ar>\tf_ar_unbounded;\n"
"\n"
"interface TestSeq {\n"
"    attribute\tf_unbounded\tfattr;\n"
"    typedef sequence<long,10> also_vec10;\n"
"    vec10 op(in also_vec10 a, out vec10 b, inout sequence <long,10> c);\n"
"};\n"
"\n");
