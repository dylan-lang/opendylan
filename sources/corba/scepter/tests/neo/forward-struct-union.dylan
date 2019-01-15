Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test neo-idl-forward-struct-union ()
  check("", test-idl-file, *neo-files*, "forward-struct-union");
end test;

add-idl-file!(
  *neo-files*,
  "forward-struct-union",
"// Author:    Keith Dennison\n"
"// Copyright: Copyright (c) 1998.  Functional Objects, Inc.  All rights reserved.\n"
"\n"
"struct Tree;\n"
"typedef sequence<Tree> seqTree;\n"
"\n"
"struct Tree {\n"
"  string label;\n"
"  seqTree children;\n"
"};\n"
"typedef Tree TreeB;\n"
"\n"
"struct Tree_wrapper {\n"
"  Tree real_tree;\n"
"};\n"
"\n"
"union TreeU;\n"
"typedef sequence<TreeU> seqTreeU;\n"
"enum Dir { EAST, WEST };\n"
"union TreeU switch (Dir) {\n"
"  case EAST: string label;\n"
"  case WEST:  seqTreeU children;\n"
"};\n"
"\n"
"interface TreeTest {\n"
"  short depth (in Tree t);\n"
"  Tree  identity (in Tree t);\n"
"  TreeB identityB (in TreeB t);\n"
"  TreeU identityU (in TreeU t);\n"
"  Tree  extract_Tree (in Tree_wrapper t);\n"
"};\n");
