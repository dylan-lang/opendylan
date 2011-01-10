Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-CosPersistenceDDO ()
  check("", test-idl-file, *corba-services-files*, "CosPersistenceDDO");
end test;

add-idl-file!(
  *corba-services-files*,
  "CosPersistenceDDO",
"// CosPersistenceDDO Module, p 5-32 CORBAservices, Persistent Object Service V1.0, 3/94\n"
"\n"
"#include \"CosPersistencePID.idl\"\n"
"module CosPersistenceDDO {\n"
"\n"
"\tinterface DDO {\n"
"\t\tattribute string object_type;\n"
"\t\tattribute CosPersistencePID::PID p;\n"
"\t\tshort add_data();\n"
"\t\tshort add_data_property (in short data_id);\n"
"\t\tshort get_data_count();\n"
"\t\tshort get_data_property_count (in short data_id);\n"
"\t\tvoid get_data_property (in short data_id,\n"
"\t\t\tin short property_id,\n"
"\t\t\tout string property_name,\n"
"\t\t\tout any property_value);\n"
"\t\tvoid set_data_property (in short data_id,\n"
"\t\t\tin short property_id,\n"
"\t\t\tin string property_name,\n"
"\t\t\tin any property_value);\n"
"\t\tvoid get_data (in short data_id,\n"
"\t\t\tout string data_name,\n"
"\t\t\tout any data_value);\n"
"\t\tvoid set_data (in short data_id,\n"
"\t\t\tin string data_name,\n"
"\t\t\tin any data_value);\n"
"\t};\n"
"};\n"
"\n");
