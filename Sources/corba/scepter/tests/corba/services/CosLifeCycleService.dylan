Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosLifeCycleService ()
  check("", test-idl-file, *corba-services-files*, "CosLifeCycleService");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosLifeCycleService",
"// LifeCycleService Module, p 6- 55 CORBAservices, Life Cycle\n"
"// Service V1.0, 3/94\n"
"\n"
"#include \"LifeCycle.idl\" \n"
"\n"
"module LifeCycleService {\n"
"\n"
"\ttypedef sequence <CosLifeCycle::NameValuePair> PolicyList;\n"
"\ttypedef sequence <CosLifeCycle::Key> Keys;\n"
"\ttypedef sequence <CosLifeCycle::NameValuePair> PropertyList; \n"
"\ttypedef sequence <CosNaming::NameComponent> NameComponents;\n"
"\n"
"\tinterface LifeCycleServiceAdmin {\n"
"\n"
"\t\tattribute PolicyList policies;\n"
"\n"
"\t\tvoid bind_generic_factory(\n"
"\t\t\t\tin CosLifeCycle::GenericFactory gf,\n"
"\t\t\t\tin CosNaming::NameComponent name,\n"
"\t\t\t\tin Keys key_set,\n"
"\t\t\t\tin PropertyList other_properties)\n"
"\t\t\traises (CosNaming::NamingContext::AlreadyBound, CosNaming::NamingContext::InvalidName);\n"
"\n"
"\t\tvoid unbind_generic_factory(\n"
"\t\t\t\tin CosNaming::NameComponent name)\n"
"\t\t\traises (CosNaming::NamingContext::NotFound, CosNaming::NamingContext::InvalidName);\n"
"\n"
"\t\tCosLifeCycle::GenericFactory resolve_generic_factory(\n"
"\t\t\t\tin CosNaming::NameComponent name)\n"
"\t\t\traises (CosNaming::NamingContext::NotFound, CosNaming::NamingContext::InvalidName);\n"
"\n"
"\t\tNameComponents list_generic_factories();\n"
"\n"
"\t\tboolean match_service (in CosLifeCycle::GenericFactory f);\n"
"\n"
"\t\tstring get_hint();\n"
"\n"
"\t\tvoid get_link_properties(\n"
"\t\t\t\tin CosNaming::NameComponent name,\n"
"\t\t\t\tout Keys key_set,\n"
"\t\t\t\tout PropertyList other_properties)\n"
"\t\t\traises (CosNaming::NamingContext::NotFound, CosNaming::NamingContext::InvalidName);\n"
"                 };\n"
"};\n"
"\n");
