Module:    union-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <UnionTest-implementation> (<UnionTest-servant>)
  slot UnionTest/rle-entity-1-attribute :: <RLE-entity-1>;
  slot UnionTest/rle-entity-2-attribute :: <RLE-entity-2>;
  slot UnionTest/rle-entity-3-attribute :: <RLE-entity-3>;
  slot UnionTest/rle-entity-4-attribute :: <RLE-entity-4>;
  slot UnionTest/rle-entity-5-attribute :: <RLE-entity-5>;
end class;

define method UnionTest/check-rle-entity-1-attribute (object :: <UnionTest-implementation>)
 => ()
  let expected = make(<RLE-entity-1>, length: 10);
  unless (UnionTest/rle-entity-1-attribute(object) = expected)
    error(make(UnionTest/<failure>));
  end unless;
end method;

define method UnionTest/check-rle-entity-2-attribute (object :: <UnionTest-implementation>)
 => ()
  let expected = make(<RLE-entity-2>, character: 'h');
  unless (UnionTest/rle-entity-2-attribute(object) = expected)
    error(make(UnionTest/<failure>));
  end unless;
end method;

define method UnionTest/check-rle-entity-3-attribute (object :: <UnionTest-implementation>)
 => ()
  let expected = make(<RLE-entity-3>, length: 20);
  unless (UnionTest/rle-entity-3-attribute(object) = expected)
    error(make(UnionTest/<failure>));
  end unless;
end method;

define method UnionTest/check-rle-entity-4-attribute (object :: <UnionTest-implementation>)
 => ()
  let expected = make(<RLE-entity-4>, character: 'i');
  unless (UnionTest/rle-entity-4-attribute(object) = expected)
    error(make(UnionTest/<failure>));
  end unless;
end method;

define method UnionTest/check-rle-entity-5-attribute (object :: <UnionTest-implementation>)
 => ()
  let expected = make(<RLE-entity-5>, length: 30);
  unless (UnionTest/rle-entity-5-attribute(object) = expected)
    error(make(UnionTest/<failure>));
  end unless;
end method;

define method UnionTest/rle-entity-1-operation (object :: <UnionTest-implementation>, one :: <RLE-entity-1>, two :: <RLE-entity-1>)
 => (result :: <RLE-entity-1>, two :: <RLE-entity-1>, three :: <RLE-entity-1>)
  values(one, two, one);
end method;

define method UnionTest/rle-entity-2-operation (object :: <UnionTest-implementation>, one :: <RLE-entity-2>, two :: <RLE-entity-2>)
 => (result :: <RLE-entity-2>, two :: <RLE-entity-2>, three :: <RLE-entity-2>)
  values(one, two, one);
end method;

define method UnionTest/rle-entity-3-operation (object :: <UnionTest-implementation>, one :: <RLE-entity-3>, two :: <RLE-entity-3>)
 => (result :: <RLE-entity-3>, two :: <RLE-entity-3>, three :: <RLE-entity-3>)
  values(one, two, one);
end method;

define method UnionTest/rle-entity-4-operation (object :: <UnionTest-implementation>, one :: <RLE-entity-4>, two :: <RLE-entity-4>)
 => (result :: <RLE-entity-4>, two :: <RLE-entity-4>, three :: <RLE-entity-4>)
  values(one, two, one);
end method;

define method UnionTest/rle-entity-5-operation (object :: <UnionTest-implementation>, one :: <RLE-entity-5>, two :: <RLE-entity-5>)
 => (result :: <RLE-entity-5>, two :: <RLE-entity-5>, three :: <RLE-entity-5>)
  values(one, two, one);
end method;

define constant $UnionTest-ior-file = "c:\\temp\\UnionTest.ior";

define method start-UnionTest-server () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let UnionTest = make(<UnionTest-implementation>, poa: root-poa);
  let UnionTestRef = portableserver/poa/servant-to-reference(root-poa, UnionTest);
  corba/orb/object-to-file(orb, $UnionTest-ior-file, UnionTestRef);
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);
end method;

register-server(start-UnionTest-server);
