Module: corba-tests-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite typecode-tests ()
  test typecode-kind-tests;
  test typecode-equal-tests;
  test typecode-id-tests;
  test typecode-name-tests;
  test typecode-member-count-tests;
  test typecode-member-name-tests;
  test typecode-member-type-tests;
  test typecode-member-label-tests;
  test typecode-discriminator-type-tests;
  test typecode-default-index-tests;
  test typecode-length-tests;
  test typecode-content-type-tests;
end suite;

define test typecode-kind-tests ()
  check("<tckind> type", instance?, #"tk-short", corba/<tckind>);
  check-equal("short typecode kind", CORBA/TypeCode/kind(corba/$short-typecode), #"tk-short");
  check-equal("long typecode kind", CORBA/TypeCode/kind(corba/$long-typecode), #"tk-long");
  check-equal("unsigned-short typecode kind", CORBA/TypeCode/kind(corba/$unsigned-short-typecode), #"tk-ushort");
  check-equal("unsigned-long typecode kind", CORBA/TypeCode/kind(corba/$unsigned-long-typecode), #"tk-ulong");
  check-equal("float typecode kind", CORBA/TypeCode/kind(corba/$float-typecode), #"tk-float");
  check-equal("double typecode kind", CORBA/TypeCode/kind(corba/$double-typecode), #"tk-double");
  check-equal("boolean typecode kind", CORBA/TypeCode/kind(corba/$boolean-typecode), #"tk-boolean");
  check-equal("char typecode kind", CORBA/TypeCode/kind(corba/$char-typecode), #"tk-char");
  check-equal("octet typecode kind", CORBA/TypeCode/kind(corba/$octet-typecode), #"tk-octet");
  check-equal("any typecode kind", CORBA/TypeCode/kind(corba/$any-typecode), #"tk-any");
  check-equal("typecode typecode kind", CORBA/TypeCode/kind(corba/$typecode-typecode), #"tk-typecode");
  check-equal("principal typecode kind", CORBA/TypeCode/kind(corba/$principal-typecode), #"tk-principal");
  check-equal("string typecode kind", CORBA/TypeCode/kind(corba/$string-typecode), #"tk-string");
  check-equal("struct typecode kind", CORBA/TypeCode/kind(class-typecode(<structure>)), #"tk-struct");
  check-equal("union typecode kind", CORBA/TypeCode/kind(class-typecode(<RLE-entity-1>)), #"tk-union");
  check-equal("enum typecode kind", CORBA/TypeCode/kind(class-typecode(<planet>)), #"tk-enum");
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  check-equal("sequence typecode kind",
	      CORBA/TypeCode/kind(corba/orb/create-sequence-tc(orb, 10, corba/$string-typecode)),
	      #"tk-sequence");
  check-equal("array typecode kind",
	      CORBA/TypeCode/kind(corba/orb/create-array-tc(orb, 100, corba/$unsigned-long-typecode)),
	      #"tk-array");
  check-equal("alias typecode kind",
	      CORBA/TypeCode/kind(corba/orb/create-alias-tc(orb, "LOCAL:alias", "structure", class-typecode(<structure>))),
	      #"tk-alias");
  check-equal("except typecode kind", CORBA/TypeCode/kind(class-typecode(corba/<unknown>)), #"tk-except");
end test;

define test typecode-equal-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  check("primitive typecode equal", CORBA/TypeCode/equal, corba/$string-typecode, corba/$string-typecode);
  check("complex typecode equal",
	CORBA/TypeCode/equal,
	corba/orb/create-sequence-tc(orb, 10, corba/$string-typecode),
	corba/orb/create-sequence-tc(orb, 10, corba/$string-typecode));
end test;

define test typecode-id-tests ()
  check-equal("typecode id",
	      CORBA/TypeCode/id(class-typecode(<structure>)),
	      "IDL:Structure:1.0");
  check-condition("typecode id badkind", CORBA/TypeCode/<BadKind>, CORBA/TypeCode/id(corba/$short-typecode));
end test;

define test typecode-name-tests ()
  check-equal("typecode name",
	      CORBA/TypeCode/name(class-typecode(<structure>)),
	      "Structure");
  check-condition("typecode name badkind",
		  CORBA/TypeCode/<BadKind>,
		  CORBA/TypeCode/name(corba/$short-typecode));
end test;

define test typecode-member-count-tests ()
  check-equal("typecode member count",
	      CORBA/TypeCode/member-count(class-typecode(<structure>)),
	      2);
  check-condition("typecode member count badkind",
		  CORBA/TypeCode/<BadKind>,
		  CORBA/TypeCode/member-count(corba/$short-typecode));
end test;  

define test typecode-member-name-tests ()
  check-equal("typecode member name",
	      CORBA/TypeCode/member-name(class-typecode(<structure>), 0),
	      "name");
  check-condition("typecode member name bounds",
		  CORBA/TypeCode/<Bounds>,
		  CORBA/TypeCode/member-name(class-typecode(<structure>), 10));
  check-condition("typecode member name badkind",
		  CORBA/TypeCode/<BadKind>,
		  CORBA/TypeCode/member-name(corba/$short-typecode, 0));
end test;  

define test typecode-member-type-tests ()
  check-equal("typecode member type",
	      CORBA/TypeCode/member-type(class-typecode(<structure>), 0),
	      corba/$string-typecode);
  check-condition("typecode member type bounds",
		  CORBA/TypeCode/<Bounds>,
		  CORBA/TypeCode/member-type(class-typecode(<structure>), 10));
  check-condition("typecode member type badkind",
		  CORBA/TypeCode/<BadKind>,
		  CORBA/TypeCode/member-type(corba/$short-typecode, 0));
end test;  

define test typecode-member-label-tests ()
  check-equal("typecode member label",
	      CORBA/Typecode/member-label(class-typecode(<RLE-Entity-1>), 0),
	      as(corba/<any>, 0));
  check-condition("typecode member label bounds",
		  CORBA/Typecode/<Bounds>,
		  CORBA/Typecode/member-label(class-typecode(<RLE-Entity-1>), 10));
  check-condition("typecode member label badkind",
		  CORBA/Typecode/<BadKind>,
		  CORBA/Typecode/member-label(corba/$short-typecode, 0));
end test;  

define test typecode-discriminator-type-tests ()
  check-equal("typecode discriminator type",
	      CORBA/TypeCode/discriminator-type(class-typecode(<RLE-Entity-1>)),
	      corba/$short-typecode);
  check-condition("typecode discriminator type badkind",
		  CORBA/Typecode/<BadKind>,
		  CORBA/Typecode/discriminator-type(corba/$short-typecode));
end test;  

define test typecode-default-index-tests ()
  check-equal("typecode default index",
	      CORBA/TypeCode/default-index(class-typecode(<RLE-Entity-1>)),
	      -1);
  check-condition("typecode default index badkind",
		  CORBA/Typecode/<BadKind>,
		  CORBA/Typecode/default-index(corba/$short-typecode));
end test;  

define test typecode-length-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  check-equal("typecode length",
	      CORBA/TypeCode/length(corba/orb/create-sequence-tc(orb, 10, corba/$string-typecode)),
	      10);
  check-condition("typecode length badkind",
		  CORBA/Typecode/<BadKind>,
		  CORBA/Typecode/length(corba/$short-typecode));
end test;  

define test typecode-content-type-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  check-equal("typecode content type",
	      CORBA/TypeCode/content-type(corba/orb/create-sequence-tc(orb, 10, corba/$string-typecode)),
	      corba/$string-typecode);
  check-condition("typecode content type badkind",
		  CORBA/Typecode/<BadKind>,
		  CORBA/Typecode/content-type(corba/$short-typecode));
end test;  

