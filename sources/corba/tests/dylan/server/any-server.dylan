Module:    any-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <tree-sequence> = limited(CORBA/<sequence>, of: <Tree>);

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

define class <AnyTest-implementation> (<AnyTest-servant>)
  slot AnyTest/any-attribute :: CORBA/<any>;
  slot AnyTest/any-tree-attribute :: CORBA/<any>;
end class;

define method AnyTest/check-any-attribute (object :: <AnyTest-implementation>)
 => ()
  let expected = make(CORBA/<any>, value: 0, type: corba/$short-typecode);
  unless (AnyTest/any-attribute(object) = expected)
    error(make(AnyTest/<failure>));
  end unless;
end method;

define method AnyTest/check-any-tree-attribute (object :: <AnyTest-implementation>)
 => ()
  let expected = as(CORBA/<any>, $tree);
  unless (AnyTest/any-tree-attribute(object) = expected)
    error(make(AnyTest/<failure>));
  end unless;
end method;

define method AnyTest/any-operation (object :: <AnyTest-implementation>, one :: CORBA/<any>, two :: CORBA/<any>)
 => (result :: CORBA/<any>, two :: CORBA/<any>, three :: CORBA/<any>)
  values(one, two, one);
end method;

define constant $AnyTest-ior-file = "c:\\temp\\AnyTest.ior";

define method start-AnyTest-server () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let AnyTest = make(<AnyTest-implementation>, poa: root-poa);
  let AnyTestRef = portableserver/poa/servant-to-reference(root-poa, AnyTest);
  corba/orb/object-to-file(orb, $AnyTest-ior-file, AnyTestRef);
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);
end method;

register-server(start-AnyTest-server);
