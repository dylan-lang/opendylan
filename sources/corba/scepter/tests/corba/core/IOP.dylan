Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-core-idl-IOP ()
  check("", test-idl-file, *corba-core-files*, "IOP");
end test;

add-idl-file!(
  *corba-core-files*,
  "IOP",
"// IOP module described in chap 10 CORBA V2, 7-95 \n"
"module IOP{ // IDL\n"
"\t//\n"
"\t// Standard Protocol Profile tag values \n"
"\t// \n"
"\ttypedef unsigned long\t\t\t\t\t\t\t\t\tProfileId;\n"
"\t\tconst ProfileId\t\t\t\t\t\t\t\t\tTAG_INTERNET_IOP = 0;\n"
"\t\tconst ProfileId\t\t\t\t\t\t\t\t\tTAG_MULTIPLE_COMPONENTS = 1;\n"
"\n"
"\tstruct TaggedProfile {\n"
"\t\tProfileId\t\t\t\t\t\t\t\ttag;\n"
"\t\tsequence <octet>\t\t\t\t\t\t\t\tprofile_data;\n"
"\t};\n"
"\n"
"\t//\n"
"\t// an Interoperable Object Reference is a sequence of\n"
"\t// object-specific protocol profiles, plus a type ID.\n"
"\t//\n"
"\tstruct IOR {\n"
"\t\tstring\t\t\t\t\t\t\t\ttype_id;\n"
"\t\tsequence <TaggedProfile>\t\t\t\t\t\t\t\tprofiles;\n"
"\t};\n"
"\n"
"\t//\n"
"\t// Standard way of representing multicomponent profiles.\n"
"\t// This would be encapsulated in a TaggedProfile.\n"
"\t//\n"
"\ttypedef unsigned long ComponentId;\n"
"\tstruct TaggedComponent {\n"
"\t\tComponentId\t\t\t\t\t\t\t\t\ttag;\n"
"\t\tsequence <octet>\t\t\t\t\t\t\t\tcomponent_data;\n"
"\t};\n"
"\ttypedef sequence <TaggedComponent> \t\t\t\t\t\t\t\t\t\tMultipleComponentProfile;\n"
"\n"
"\n"
"typedef unsigned long\t\t\t\t\t\tServiceID;\n"
"\n"
"\tstruct ServiceContext {\n"
" \t\tServiceID\t\t\t\t\tcontext_id;\n"
" \t\tsequence <octet>\t\t\t\t\tcontext_data;\n"
" \t};\n"
"\ttypedef sequence <ServiceContext>\t\t\t\t\t\t\t\t\tServiceContextList;\n"
"\n"
"\tconst ServiceID\t\t\t\t\t\t\t\t\tTransactionService = 0;\n"
"\n"
"\tconst ComponentId TAG_OBJECT_KEY = 10;\n"
"\tconst ComponentId TAG_ENDPOINT_ID = 11;\n"
"\tconst ComponentId TAG_LOCATION_POLICY = 12;\n"
"\tconst octet LOCATE_NEVER = 0;\n"
"\tconst octet LOCATE_OBJECT = 1;\n"
"\tconst octet LOCATE_OPERATION = 2;\n"
"\tconst octet LOCATE_ALWAYS = 3;\n"
"};\n"
"\n"
"\n");
