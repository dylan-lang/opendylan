Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CompoundExternalization ()
  check("", test-idl-file, *corba-services-files*, "CompoundExternalization");
end test;

add-idl-file!(
  *corba-services-files*,
  "CompoundExternalization",
"// CosCompoundExternalization Module, p 8-20 CORBAservices,\n"
"// Externalization Service V1.0, 3/94\n"
"\n"
"#ifndef COSCOMPOUNDEXTERNALIZATION\n"
"#define COSCOMPOUNDEXTERNALIZATION\n"
"#include <Graphs.idl>\n"
"#include <LifeCycle.idl>\n"
"\n"
"// #include <Stream.idl>\n"
"//\n"
"// GROSS HACK: Stream and CompoundExternalization try to include each other\n"
"// so put forward defined stubs in instead. Unfortunately, this doesn't work\n"
"// for Streamable, which is subclassed in CompoundExternalization. So have\n"
"// to put real interface in (with empty body) until this is sorted out.\n"
"// \n"
"module CosStream {\n"
"  interface Streamable {};\n"
"  interface StreamIO;\n"
"};\n"
"\n"
"module CosCompoundExternalization {\n"
"\tinterface Node;\n"
"\tinterface Role;\n"
"\tinterface Relationship;\n"
"\tinterface PropagationCriteriaFactory;\n"
"\n"
"\tstruct RelationshipHandle {\n"
"\t\t\tRelationship theRelationship;\n"
"\t\t\tCosObjectIdentity::ObjectIdentifier constantRandomId;\n"
"\t};\n"
"\n"
"\tinterface Node : CosGraphs::Node, CosStream::Streamable{\n"
"\t\t\tvoid externalize_node (in CosStream::StreamIO sio);\n"
"\t\t\tvoid internalize_node (in CosStream::StreamIO sio,\n"
"\t\t\t\t\t\tin CosLifeCycle::FactoryFinder there,\n"
"\t\t\t\t\t\tout Roles rolesOfNode)\n"
"\t\t\t\traises (CosLifeCycle::NoFactory);\n"
"\t\t};\n"
"\n"
"\tinterface Role : CosGraphs::Role {\n"
"\t\t\tvoid externalize_role (in CosStream::StreamIO sio); \n"
"\t\t\tvoid internalize_role (in CosStream::StreamIO sio); \n"
"\t\t\tCosGraphs::PropagationValue externalize_propagation (\n"
"\t\t\t\t\t\tin RelationshipHandle rel,\n"
"\t\t\t\t\t\tin CosRelationships::RoleName toRoleName,\n"
"\t\t\t\t\t\tout boolean sameForAll);\n"
"\t\t};\n"
"\n"
"\tinterface Relationship : \n"
"\t\t\t\tCosRelationships::Relationship {\n"
"\t\t\tvoid externalize_relationship (\n"
"\t\t\t\t\t\tin CosStream::StreamIO sio);\n"
"\t\t\tvoid internalize_relationship(\n"
"\t\t\t\t\t\tin CosStream::StreamIO sio,\n"
"\t\t\t\t\t\tin CosGraphs::NamedRoles newRoles);\n"
"\t\t\tCosGraphs::PropagationValue externalize_propagation (\n"
"\t\t\t\t\t\tin CosRelationships::RoleName fromRoleName,\n"
"\t\t\t\t\t\tin CosRelationships::RoleName toRoleName,\n"
"\t\t\t\t\t\tout boolean sameForAll);\n"
"\t};\n"
"\n"
"\tinterface PropagationCriteriaFactory {\n"
"\t\t\tCosGraphs::TraversalCriteria create_for_externalize( );\n"
"\t};\n"
"\n"
"};\n"
"#endif\n"
"\n");
