Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistencePDS_DA ()
  check("", test-idl-file, *corba-services-files*, "CosPersistencePDS_DA");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistencePDS_DA",
"// CosPersistencePDS_DA Module, p 5-22 CORBAservices,\n"
"// Persistent Object Service, V1.0, 3/94\n"
"\n"
"#include \"CosPersistencePDS.idl\"\n"
"// CosPersistencePDS.idl #includes CosPersistencePID.idl\n"
"\n"
"module CosPersistencePDS_DA {\n"
"\n"
"\ttypedef string DAObjectID;\n"
"\n"
"\tinterface PID_DA : CosPersistencePID::PID {\n"
"\t\tattribute DAObjectID oid;\n"
"\t};\n"
"\n"
"\tinterface DAObject {\n"
"\t\tboolean dado_same(in DAObject d);\n"
"\t\tDAObjectID dado_oid();\n"
"\t\tPID_DA dado_pid();\n"
"\t\tvoid dado_remove();\n"
"\t\tvoid dado_free();\n"
"\t};\n"
"\n"
"\tinterface DAObjectFactory {\n"
"\t\tDAObject create();\n"
"\t};\n"
"\n"
"\tinterface DAObjectFactoryFinder {\n"
"\t\tDAObjectFactory find_factory(in string key);\n"
"\t};\n"
"\n"
"\tinterface PDS_DA : CosPersistencePDS::PDS {\n"
"\t\tDAObject get_data();\n"
"\t\tvoid set_data(in DAObject new_data);\n"
"\t\tDAObject lookup(in DAObjectID id);\n"
"\t\tPID_DA get_pid();\n"
"\t\tPID_DA get_object_pid(in DAObject dao);\n"
"\t\tDAObjectFactoryFinder data_factories();\n"
"\t};\n"
"\n"
"\ttypedef sequence<string> AttributeNames;\n"
"\tinterface DynamicAttributeAccess {\n"
"\t\tAttributeNames attribute_names();\n"
"\t\tany attribute_get(in string name);\n"
"\t\tvoid attribute_set(in string name, in any value);\n"
"\t};\n"
"\n"
"\ttypedef string ClusterID;\n"
"\ttypedef sequence<ClusterID> ClusterIDs;\n"
"\tinterface PDS_ClusteredDA : PDS_DA{\n"
"\t\tClusterID cluster_id();\n"
"\t\tstring cluster_kind();\n"
"\t\tClusterIDs clusters_of();\n"
"\t\tPDS_ClusteredDA create_cluster(in string kind);\n"
"\t\tPDS_ClusteredDA open_cluster(in ClusterID cluster);\n"
"\t\tPDS_ClusteredDA copy_cluster(\n"
"\t\t\tin PDS_DA source);\n"
"\t};\n"
"};\n"
"\n");
