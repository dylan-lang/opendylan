Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test orbix-demo-idl-grid_tie-grid ()
  check("", test-idl-file, *orbix-demo-files*, "grid_tie-grid");
end test;

add-idl-file!(
  *orbix-demo-files*,
  "grid_tie-grid",
"// grid.idl\n"
"\n"
"// IDL definition of a 2-D grid (in row major order):\n"
"\n"
"interface grid {\n"
"\treadonly attribute short height;  // height of the grid\n"
"\treadonly attribute short width;   // width of the grid\n"
"\n"
"\t// IDL operations\n"
"\n"
"\t// set the element [n,m] of the grid, to value:\n"
"\tvoid set(in short n, in short m, in long value);\n"
"\n"
"\t// return element [n,m] of the grid:\n"
"\tlong get(in short n, in short m);\n"
"};\n"
"\n");
