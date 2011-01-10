Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-LifeCycle ()
  check("", test-idl-file, *corba-services-files*, "LifeCycle");
end test;

add-idl-file!(
  *corba-services-files*,
  "LifeCycle",
"// CosLifeCycle Module, p 6-10 CORBAservices, LifeCycle Service V1.0, 3/94\n"
"\n"
"#ifndef COSLIFECYCLE\n"
"#define COSLIFECYCLE\n"
"#include \"Naming.idl\"\n"
"\n"
"module CosLifeCycle{\n"
"\n"
"\ttypedef CosNaming::Name Key; \n"
"\ttypedef Object Factory;\n"
"\ttypedef sequence <Factory> Factories;\n"
"\tstruct NVP {\t \n"
"\t\tCosNaming::Istring\t name; \n"
"\t\tany \tvalue; \n"
"\t};\n"
"        typedef NVP NameValuePair;\n"
"\ttypedef sequence <NameValuePair> Criteria;\n"
"\n"
"\texception NoFactory {\n"
"\t\tKey search_key;\n"
"\t};\n"
"\texception NotCopyable { string reason; };\n"
"\texception NotMovable { string reason; };\n"
"\texception NotRemovable { string reason; };\n"
"\texception InvalidCriteria{ \n"
"\t\tCriteria invalid_criteria;\n"
"\t};\n"
"\texception CannotMeetCriteria {\n"
"\t\tCriteria unmet_criteria;\n"
"\t};\n"
"\n"
"\n"
"\tinterface FactoryFinder {\n"
"\t\tFactories find_factories(in Key factory_key)\n"
"\t\t\traises(NoFactory);\n"
"\t};\n"
"\n"
"\tinterface LifeCycleObject {\n"
"\t\tLifeCycleObject copy(in FactoryFinder there, \n"
"\t\t\t\tin Criteria the_criteria)\n"
"\t\t\traises(NoFactory, NotCopyable, InvalidCriteria,\n"
"\t\t\t\t CannotMeetCriteria);\n"
"\t\tvoid move(in FactoryFinder there, \n"
"\t\t\t\tin Criteria the_criteria)\n"
"\t\t\traises(NoFactory, NotMovable, InvalidCriteria,\n"
"\t\t\t\t CannotMeetCriteria);\n"
"\t\tvoid remove()\n"
"\t\t\traises(NotRemovable);\n"
"\t};\n"
"\n"
"\tinterface GenericFactory {\n"
"\t\tboolean supports(in Key k);\n"
"\t\tObject create_object(\n"
"\t\t\t\tin Key \t\t\tk, \n"
"\t\t\t\tin Criteria\t\t the_criteria) \n"
"\t\t\traises (NoFactory, InvalidCriteria, \n"
"\t\t\t\tCannotMeetCriteria);\n"
"\t};\n"
"};\n"
"#endif\n"
"\n");
