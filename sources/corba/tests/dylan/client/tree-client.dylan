Module:    tree-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *treetest-ior-file* = "c:\\temp\\treetest.ior";

define constant <tree-sequence> = limited(corba/<sequence>, of: <tree>);

define constant $a-tree =
  make(<tree>,
       label: "1",
       children: begin
		   let seq = make(<tree-sequence>);
		   add!(seq, make(<tree>,
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

		   add!(seq, make(<tree>,
				  label: "5",
				  children: begin
					      let seq = make(<tree-sequence>);
					      add!(seq, make(<tree>,
							     label: "6",
							     children: make(<tree-sequence>)));
					      add!(seq, make(<tree>,
							     label: "7",
							     children: make(<tree-sequence>)));
					      seq
					    end));

		   seq
		 end);

define method tree-equal? (tree1, tree2)
  tree1 := as(<tree>, tree1);
  tree2 := as(<tree>, tree2);
  (tree/label(tree1) = tree/label(tree2))
    & every?(tree-equal?, tree/children(tree1), tree/children(tree2))
end method;

define constant $u-tree =
  make(<treeU>,
       discriminator: #t,
       value: begin
		let seq = make(corba/<sequence>);
		add!(seq, make(<treeU>,
			       discriminator: #t,
			       value: begin
					let seq = make(corba/<sequence>);
					add!(seq, make(<treeU>,
						       discriminator: #f,
						       value: "3"));
					add!(seq, make(<treeU>,
						       discriminator: #f,
						       value: "4"));
					seq
				      end));

		add!(seq, make(<treeU>,
			       discriminator: #t,
			       value: begin
					let seq = make(corba/<sequence>);
					add!(seq, make(<treeU>,
						       discriminator: #f,
						       value: "6"));
					add!(seq, make(<treeU>,
						       discriminator: #f,
						       value: "7"));
					seq
				      end));

		seq
	      end);

define method treeU-equal? (tree1, tree2)
  tree1 := as(<treeU>, tree1);
  tree2 := as(<treeU>, tree2);
  if (CORBA/union/discriminator(tree1))
    (CORBA/union/discriminator(tree2) = #t
       & CORBA/union/value(tree1) = CORBA/union/value(tree2))
  else
    (CORBA/union/discriminator(tree2) = #f
       & treeU-equal?(CORBA/union/value(tree1), CORBA/union/value(tree2)))
  end if;
end method;

define test tree-operations-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let treetest = as(<treetest>, corba/orb/file-to-object(orb, *treetest-ior-file*));
  check-equal("", treetest/depth(treetest, $a-tree), 0);
  check-true("", tree-equal?(treetest/identity(treetest, $a-tree), $a-tree));
  check-true("", tree-equal?(treetest/identityB(treetest, $a-tree), $a-tree));
  check-true("", treeU-equal?(treetest/identityU(treetest, $u-tree), $u-tree));
  let wrapper = make(<tree-wrapper>, real-tree: $a-tree);
  check-true("", tree-equal?(treetest/extract-tree(treetest, wrapper), $a-tree));
end test;

define suite tree-test-suite ()
  test tree-operations-test;
end suite;

