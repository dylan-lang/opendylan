Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test orbix-demo-idl-filters-filter ()
  check("", test-idl-file, *orbix-demo-files*, "filters-filter");
end test;

add-idl-file!(
  *orbix-demo-files*,
  "filters-filter",
"// a simple IDL interface: inc.   Objects of this interface \n"
"// provide an operation 'increment' which takes an unsigned long value\n"
"// and returns one greater than the value passed in.\n"
"// \n"
"// Operation 'filterOut' is provided only so that the 'preProcess'\n"
"// per-process filter can test for calls to this operation and\n"
"// show how to raise an exception.\n"
"\n"
"interface inc {\n"
"    unsigned long increment (in unsigned long vin);\n"
"\n"
"    void filterOut ();\n"
"};\n"
"\n");
