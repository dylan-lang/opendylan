Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CORBA ()
  check("", test-idl-file, *corba-services-files*, "CORBA");
end test;

add-idl-file!(
  *corba-services-files*,
  "CORBA",
"// Jason: stub CORBA module to satisfy service modules without\n"
"// having to have the whole goddam thing in here.\n"
"\n"
"#ifndef CORBADEF\n"
"#define CORBADEF\n"
"module CORBA {\n"
"\n"
"  interface InterfaceDef;\n"
"  interface Environment;\n"
"\n"
"};\n"
"#endif\n"
"\n");


