Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test orbix-demo-idl-grid_iiop-grid ()
  check("", test-idl-file, *orbix-demo-files*, "grid_iiop-grid");
end test;

add-idl-file!(
  *orbix-demo-files*,
  "grid_iiop-grid",
"// grid.idl\n"
"\n"
"// IDL defintion of a 2-D grid (in row major order):\n"
"\n"
"interface grid {\n"
"\treadonly attribute short height;  // height of the grid\n"
"\treadonly attribute short width;   // width of the grid\n"
"\n"
"\t// set the element [n,m] of the grid, to value:\n"
"\tvoid set(in short n, in short m, in long value);\n"
"\n"
"\t// return element [n,m] of the grid:\n"
"\tlong get(in short n, in short m);\n"
"};\n"
"\n");
