Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-simple2 ()
  check("", test-idl-file, *neo-files*, "simple2");
end test;

add-idl-file!(
  *neo-files*,
  "simple2",
"#include \"simple.idl\"\n"
"\n"
"\n"
"interface simple3 : simple2 {\n"
"    exception OVERFLOW {\n"
"\tlong\ta;\n"
"\tstring\tmsg;\n"
"    };\n"
"    exception UNDERFLOW {\n"
"\tlong\tb;\n"
"\tstring\tmsg;\n"
"    };\n"
"    exception BADNUM {\n"
"\tvec10\tc;\n"
"    };\n"
"    exception USERERROR { \n"
"\tany reason;\n"
"    };\n"
"    long\top3(in long a, out simple3 b, out simple2 c, out simple d)\n"
"                      raises (OVERFLOW, UNDERFLOW, BADNUM);\n"
"    any\t\top4(in any x, inout any y, out any z)\n"
"               raises(UNDERFLOW);\n"
"\n"
"    oneway void\top5()\n"
"               raises(USERERROR);\n"
"};\n"
"\n");
