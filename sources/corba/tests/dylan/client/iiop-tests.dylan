Module: corba-tests-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** float formats
/// ---*** performance

define suite iiop-tests ()
  suite little-endian-iiop-tests;
  suite big-endian-iiop-tests;
  suite single-stream-iiop-tests;
end suite;

define suite little-endian-iiop-tests (setup-function: method ()
							 architecture-little-endian?() := #t
						       end method)
  suite common-iiop-tests;
end suite;

define suite big-endian-iiop-tests (setup-function: method ()
						      architecture-little-endian?() := #f
						    end method)
  suite common-iiop-tests;
end suite;

define suite single-stream-iiop-tests (setup-function: method ()
							 architecture-little-endian?() := #t;
							 *single-marshalling-stream*
							   := make(<marshalling-stream>, inner-stream: #f);
						       end method,
				       cleanup-function: method ()
							   *single-marshalling-stream* := #f
							 end method)
  suite common-iiop-tests;
end suite;

define suite common-iiop-tests ()
  suite basic-iiop-tests;
  suite constructed-iiop-tests;
  suite typecode-iiop-tests;
  suite any-iiop-tests;  
//  suite object-iiop-tests;
//  suite ior-parsing-tests;
//  suite error-iiop-tests;
end suite;

define suite basic-iiop-tests ()
  test short-test;
  test long-test;
  test unsigned-short-test;
  test unsigned-long-test;
  test float-test;
  test double-test;
  test boolean-test;
  test char-test;
  test octet-test;
  test string-test;
end suite;

define suite constructed-iiop-tests ()
  test enum-test;
  test sequence-test;
  test array-test;
  test struct-test;
  test exception-test;
  test union-test;
  test indirection-test;
end suite;

define suite typecode-iiop-tests ()
  test empty-typecode-tests;
  test complex-typecode-tests;
end suite;

define suite any-iiop-tests ()
  test basic-any-tests;
  test constructed-any-tests;
end suite;

//define suite object-iiop-tests ()
//  test basic-object-tests;
//end suite;

define test short-test ()
  check-marshalling("short1", corba/$short-typecode, 0);
  check-marshalling("short2", corba/$short-typecode, 1);
  check-marshalling("short3", corba/$short-typecode, -1);
  check-marshalling("short4", corba/$short-typecode, 255);
  check-marshalling("short5", corba/$short-typecode, -255);
  check-marshalling("short6", corba/$short-typecode, 256);
  check-marshalling("short7", corba/$short-typecode, -256);
  check-marshalling("short8", corba/$short-typecode, 32767);
  check-marshalling("short9", corba/$short-typecode, -32768);
end test;

define test long-test ()
  check-marshalling("long1", corba/$long-typecode, 0);
  check-marshalling("long2", corba/$long-typecode, 1);
  check-marshalling("long3", corba/$long-typecode, -1);
  check-marshalling("long4", corba/$long-typecode, 255);
  check-marshalling("long5", corba/$long-typecode, -255);
  check-marshalling("long6", corba/$long-typecode, 256);
  check-marshalling("long7", corba/$long-typecode, -256);
  check-marshalling("long8", corba/$long-typecode, 32768);
  check-marshalling("long9", corba/$long-typecode, -32768);
  check-marshalling("long10", corba/$long-typecode, (2 ^ 31) - 1);
  check-marshalling("long11", corba/$long-typecode, -(2 ^ 31));
end test;

define test unsigned-short-test ()
  check-marshalling("ushort1", corba/$unsigned-short-typecode, 0);
  check-marshalling("ushort2", corba/$unsigned-short-typecode, 1);
  check-marshalling("ushort3", corba/$unsigned-short-typecode, 255);
  check-marshalling("ushort4", corba/$unsigned-short-typecode, 256);
  check-marshalling("ushort5", corba/$unsigned-short-typecode, 32767);
  check-marshalling("ushort6", corba/$unsigned-short-typecode, 65535);
end test;

define test unsigned-long-test ()
  check-marshalling("ulong1", corba/$unsigned-long-typecode, 0);
  check-marshalling("ulong2", corba/$unsigned-long-typecode, 1);
  check-marshalling("ulong3", corba/$unsigned-long-typecode, 255);
  check-marshalling("ulong4", corba/$unsigned-long-typecode, 256);
  check-marshalling("ulong5", corba/$unsigned-long-typecode, 32768);
  check-marshalling("ulong6", corba/$unsigned-long-typecode, (2 ^ 32) - 1);
end test;

define test float-test ()
  check-marshalling("float1", corba/$float-typecode, 0.0);
  check-marshalling("float2", corba/$float-typecode, 1.0);
  check-marshalling("float3", corba/$float-typecode, -1.0);
  check-marshalling("float4", corba/$float-typecode, 3.33333);
  check-marshalling("float5", corba/$float-typecode, -3.33333);
  check-marshalling("float6", corba/$float-typecode, 0.5);
  check-marshalling("float7", corba/$float-typecode, -0.5);
  check-marshalling("float8", corba/$float-typecode, 1.0e10);
  check-marshalling("float9", corba/$float-typecode, 1.0e-10);
end test;

define test double-test ()
  check-marshalling("double1", corba/$double-typecode, 0.0d0);
  check-marshalling("double2", corba/$double-typecode, 1.0d0);
  check-marshalling("double3", corba/$double-typecode, -1.0d0);
  check-marshalling("double4", corba/$double-typecode, 3.33333d0);
  check-marshalling("double5", corba/$double-typecode, -3.33333d0);
  check-marshalling("double6", corba/$double-typecode, 0.5d0);
  check-marshalling("double7", corba/$double-typecode, -0.5d0);
  check-marshalling("double8", corba/$double-typecode, 1.0d10);
  check-marshalling("double9", corba/$double-typecode, 1.0d-10);
  check-marshalling("double8", corba/$double-typecode, 1.0d100);
  check-marshalling("double9", corba/$double-typecode, 1.0d-100);
end test;

define test boolean-test ()
  check-marshalling("boolean1", corba/$boolean-typecode, #f);
  check-marshalling("boolean2", corba/$boolean-typecode, #t);
end test;

define test char-test ()
  check-marshalling("char1", corba/$char-typecode, 'a');
  check-marshalling("char2", corba/$char-typecode, 'z');
  check-marshalling("char3", corba/$char-typecode, '\'');
  check-marshalling("char3", corba/$char-typecode, '\t');
  check-marshalling("char4", corba/$char-typecode, '\<00>');
  check-marshalling("char5", corba/$char-typecode, '\<ff>');
end test;

define test octet-test ()
  check-marshalling("octet1", corba/$octet-typecode, 0);
  check-marshalling("octet2", corba/$octet-typecode, 1);
  check-marshalling("octet3", corba/$octet-typecode, 255);
end test;

define test string-test ()
  check-marshalling("string1", corba/$string-typecode, "");
  check-marshalling("string2", corba/$string-typecode, "a");
  check-marshalling("string3", corba/$string-typecode, "hello world");
end test;

define test enum-test ()
  check-marshalling("enum1", class-typecode(corba/<completion-status>), #"completed-yes");
  check-marshalling("enum2", class-typecode(corba/<completion-status>), #"completed-no");
  check-marshalling("enum3", class-typecode(corba/<completion-status>), #"completed-maybe");
  check-marshalling("enum4", class-typecode(corba/<exception-type>), #"no-exception");
  check-marshalling("enum5", class-typecode(corba/<exception-type>), #"user-exception");
  check-marshalling("enum6", class-typecode(corba/<exception-type>), #"system-exception");
  check-marshalling("enum7", class-typecode(<planet>), #"Mercury");
  check-marshalling("enum8", class-typecode(<planet>), #"Venus");
  check-marshalling("enum9", class-typecode(<planet>), #"Earth");
  check-marshalling("enum10", class-typecode(<planet>), #"Mars");
  check-marshalling("enum11", class-typecode(<planet>), #"Jupiter");
  check-marshalling("enum12", class-typecode(<planet>), #"Saturn");
  check-marshalling("enum13", class-typecode(<planet>), #"Uranus");
  check-marshalling("enum13", class-typecode(<planet>), #"Neptune");
  check-marshalling("enum13", class-typecode(<planet>), #"Pluto");
end test;

define test sequence-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let type1 = corba/orb/create-sequence-tc(orb, 0, corba/$short-typecode);
  let seq1 = make(corba/<sequence>);
  check-marshalling("sequence1", type1, seq1);
  add!(seq1, 0);
  check-marshalling("sequence2", type1, seq1);
  add!(seq1, 0);
  add!(seq1, 0);
  check-marshalling("sequence3", type1, seq1);
  let type2 = corba/orb/create-sequence-tc(orb, 0, corba/$string-typecode);
  let seq2 = make(corba/<sequence>);
  add!(seq2, "hello");
  add!(seq2, "world");
  check-marshalling("sequence4", type2, seq2);
  let type3 = corba/orb/create-sequence-tc(orb, 0, type1);
  let seq3 = make(corba/<sequence>);
  add!(seq3, seq1);
  check-marshalling("sequence5", type3, seq3);
end test;

define test array-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let type1 = corba/orb/create-array-tc(orb, 2, corba/$long-typecode);
  let arr1 = make(corba/<array>, dimensions: list(2), fill: 1);
  check-marshalling("array1", type1, arr1);
  let type2 = corba/orb/create-array-tc(orb, 3, type1);
  let arr2 = make(corba/<array>, dimensions: list(2, 3), fill: 2);
  check-marshalling("array2", type2, arr2);
  let type3 = corba/orb/create-array-tc(orb, 4, type2);
  let arr3 = make(corba/<array>, dimensions: list(2, 3, 4), fill: 3);
  check-marshalling("array3", type3, arr3);
  let type4 = corba/orb/create-array-tc(orb, 5, type3);
  let arr4 = make(corba/<array>, dimensions: list(2, 3, 4, 5), fill: 4);
  check-marshalling("array4", type4, arr4);
end test;

define test struct-test ()
  let type1 = class-typecode(<structure>);
  let struct1 = make(<structure>, name: "foo", info: 1);
  check-marshalling("struct1",
		    type1,
		    struct1,
		    coerce: curry(as, <structure>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let type2 = corba/orb/create-sequence-tc(orb, 0, type1);
  let struct2 = make(corba/<sequence>);
  add!(struct2, struct1);
  check-marshalling("struct2",
		    type2,
		    struct2,
		    coerce: method (seq)
			      map(method (x)
				    as(<structure>, x);
				  end method,
				  seq)
			    end method)
end test;

define test exception-test ()
  let type1 = class-typecode(corba/<unknown>);
  let excep1 = make(corba/<unknown>, minor: 99, completed: #"completed-maybe");
  check-marshalling("exception1",
		    type1,
		    excep1,
		    coerce: curry(as, corba/<unknown>));
end test;

define test union-test ()
  let type1 = class-typecode(<RLE-Entity-1>);
  let union1 = make(<RLE-Entity-1>, discriminator: 2, value: 'a');
  check-marshalling("union1",
		    type1,
		    union1,
		    coerce: curry(as, <RLE-Entity-1>));
  let union2 = make(<RLE-Entity-1>, discriminator: 0, value: 5);
  check-marshalling("union2",
		    type1,
		    union2,
		    coerce: curry(as, <RLE-Entity-1>));
  let union3 = make(<RLE-Entity-1>, length: 10);
  check-marshalling("union3",
		    type1,
		    union3,
		    coerce: curry(as, <RLE-Entity-1>));
  let union4 = make(<RLE-Entity-1>, character: 'a');
  check-marshalling("union4",
		    type1,
		    union4,
		    coerce: curry(as, <RLE-Entity-1>));
end test;

define test indirection-test ()
  check-marshalling("indirection1", class-typecode(<tree>), $a-tree, test: tree-equal?);
end test;

define test empty-typecode-tests ()
  check-marshalling("short-typecode", corba/$typecode-typecode, corba/$short-typecode);
  check-marshalling("long-typecode", corba/$typecode-typecode, corba/$long-typecode);
  check-marshalling("unsigned-short-typecode", corba/$typecode-typecode, corba/$unsigned-short-typecode);
  check-marshalling("unsigned-long-typecode", corba/$typecode-typecode, corba/$unsigned-long-typecode);
  check-marshalling("float-typecode", corba/$typecode-typecode, corba/$float-typecode);
  check-marshalling("double-typecode", corba/$typecode-typecode, corba/$double-typecode);
  check-marshalling("boolean-typecode", corba/$typecode-typecode, corba/$boolean-typecode);
  check-marshalling("char-typecode", corba/$typecode-typecode, corba/$char-typecode);
  check-marshalling("any-typecode", corba/$typecode-typecode, corba/$any-typecode);
  check-marshalling("typecode-typecode", corba/$typecode-typecode, corba/$typecode-typecode);
  check-marshalling("principal-typecode", corba/$typecode-typecode, corba/$principal-typecode);
  check-marshalling("string-typecode", corba/$typecode-typecode, corba/$string-typecode);
end test;

define test complex-typecode-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  check-marshalling("object-typecode",
		    corba/$typecode-typecode,
                    corba/orb/create-interface-tc(orb, "LOCAL:obj-ref-example", "example"));
  check-marshalling("struct-typecode",
		    corba/$typecode-typecode,
		    class-typecode(<structure>));
  check-marshalling("union-typecode",
		    corba/$typecode-typecode,
		    class-typecode(<RLE-Entity-1>));
  check-marshalling("enum-typecode",
		    corba/$typecode-typecode,
		    class-typecode(<planet>));
  check-marshalling("string-typecode",
		    corba/$typecode-typecode,
		    corba/orb/create-string-tc(orb, 10));
  check-marshalling("sequence-typecode",
		    corba/$typecode-typecode,
                    corba/orb/create-sequence-tc(orb, 10, corba/$string-typecode));
  check-marshalling("array-typecode",
		    corba/$typecode-typecode,
		    corba/orb/create-array-tc(orb, 100, corba/$unsigned-long-typecode));
  check-marshalling("alias-typecode",
		    corba/$typecode-typecode,
                    corba/orb/create-alias-tc(orb, "LOCAL:alias", "structure", class-typecode(<structure>)));
  check-marshalling("exception-typecode",
		    corba/$typecode-typecode,
		    class-typecode(corba/<unknown>));
//  check-marshalling("indirection-typecode",
//		    corba/$typecode-typecode,
//		    make(<indirection-typecode>, nesting: 0));
end test;

define test basic-any-tests ()
  check-marshalling("any1", corba/$any-typecode, make(corba/<any>, type: corba/$short-typecode, value: -17));
  check-marshalling("any2", corba/$any-typecode, make(corba/<any>, type: corba/$long-typecode, value: -30000));
  check-marshalling("any3", corba/$any-typecode, make(corba/<any>, type: corba/$unsigned-short-typecode, value: 13));
  check-marshalling("any4", corba/$any-typecode, make(corba/<any>, type: corba/$unsigned-long-typecode, value: 40000));
  check-marshalling("any5", corba/$any-typecode, make(corba/<any>, type: corba/$float-typecode, value: 3.1415));
  check-marshalling("any6", corba/$any-typecode, make(corba/<any>, type: corba/$double-typecode, value: 2.71828d0));
  check-marshalling("any7", corba/$any-typecode, make(corba/<any>, type: corba/$boolean-typecode, value: #t));
  check-marshalling("any8", corba/$any-typecode, make(corba/<any>, type: corba/$char-typecode, value: 'J'));
  check-marshalling("any9", corba/$any-typecode, make(corba/<any>, type: corba/$octet-typecode, value: #o007));
  check-marshalling("any10", corba/$any-typecode, make(corba/<any>, type: corba/$string-typecode, value: "hello world"));
end test;

define test constructed-any-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let type1 = class-typecode(corba/<completion-status>);
  let enum1 = #"completed-maybe";
  check-marshalling("any11", corba/$any-typecode, make(corba/<any>, type: type1, value: enum1));
  let type2 = corba/orb/create-sequence-tc(orb, 0, corba/$long-typecode);
  let seq2 = as(corba/<sequence>, #[999]);
  check-marshalling("any12", corba/$any-typecode, make(corba/<any>, type: type2, value: seq2));
  let type3 = corba/orb/create-array-tc(orb, 1, corba/$char-typecode);
  let arr3 = begin
	       let arr = make(corba/<array>, dimensions: #(1));
	       arr[0] := 'j';
	       arr
	     end;
  check-marshalling("any13", corba/$any-typecode, make(corba/<any>, type: type3, value: arr3));
  let type4 = class-typecode(<structure>);
  let struct4 = make(<structure>, name: "bar", info: 2);
  check-marshalling("any14",
		    corba/$any-typecode,
		    make(corba/<any>, type: type4, value: struct4),
		    test: method (any1, any2)
			    as(<structure>, any1) = as(<structure>, any2)
			  end method);
  let type5 = class-typecode(corba/<unknown>);
  let excep5 = make(corba/<unknown>, minor: 99, completed: #"completed-maybe");
  check-marshalling("any15",
		    corba/$any-typecode,
		    make(corba/<any>, type: type5, value: excep5),
		    test: method (any1, any2)
			    as(corba/<unknown>, any1) = as(corba/<unknown>, any2)
			  end method);
  let type6 = class-typecode(<RLE-Entity-1>);
  let union6 = make(<RLE-Entity-1>, discriminator: 2, value: 'a');
  check-marshalling("any16",
		    corba/$any-typecode,
		    make(corba/<any>, type: type6, value: union6),
		    test: method (any1, any2)
			    as(<RLE-Entity-1>, any1) = as(<RLE-Entity-1>, any2)
			  end method);
end test;

