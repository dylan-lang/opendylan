Module:    any-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $AnyTest-ior-file = "c:\\temp\\AnyTest.ior";

define method get-AnyTest-reference ()
 => (reference :: <AnyTest>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  as(<AnyTest>, corba/orb/file-to-object(orb, $AnyTest-ior-file));
end method;

define test any-short-test ()
  let AnyTest = get-AnyTest-reference();
  let any = make(CORBA/<any>, value: 0, type: corba/$short-typecode);
  check("Any attribute setter", AnyTest/any-attribute-setter, any, AnyTest);
  check-false("Check any attribute", AnyTest/check-any-attribute(AnyTest));
  check-equal("Any attribute getter", AnyTest/any-attribute(AnyTest), any);

  let (i, ii, iii) = AnyTest/any-operation(AnyTest, any, any);
  check-equal("Any operation first result", i, any);
  check-equal("Any operation second result", ii, any);
  check-equal("Any operation third result", iii, any);
end test;

define constant <tree-sequence> = limited(corba/<sequence>, of: <Tree>);

define constant $tree =
  make(<Tree>,
       label: "1",
       children: begin
		   let seq = make(<tree-sequence>);
		   add!(seq, make(<Tree>,
				  label: "2",
				  children: begin
					      let seq = make(<tree-sequence>);
					      add!(seq, make(<tree>,
							     label: "3",
							     children: make(<tree-sequence>)));
					      add!(seq, make(<tree>,
							     label: "4",
							     children: make(<tree-sequence>)));
					      seq
					    end));

		   add!(seq, make(<Tree>,
				  label: "5",
				  children: begin
					      let seq = make(<tree-sequence>);
					      add!(seq, make(<Tree>,
							     label: "6",
							     children: make(<tree-sequence>)));
					      add!(seq, make(<Tree>,
							     label: "7",
							     children: make(<tree-sequence>)));
					      seq
					    end));

		   seq
		 end);

define test any-tree-test ()
  let AnyTest = get-AnyTest-reference();
  let any = as(CORBA/<any>, $tree);
  check("Any tree attribute setter", AnyTest/any-tree-attribute-setter, any, AnyTest);
  check-false("Check any tree attribute", AnyTest/check-any-tree-attribute(AnyTest));
  check-equal("Any tree attribute getter", AnyTest/any-tree-attribute(AnyTest), any);
end test;

define suite any-test-suite ()
  test any-short-test;
//  test any-tree-test;
end suite;
