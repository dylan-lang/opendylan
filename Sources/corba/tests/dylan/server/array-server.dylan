Module:    array-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ArrayTest-implementation> (<ArrayTest-servant>)
  slot ArrayTest/string-array-attribute :: <StringArray>;
  slot ArrayTest/short-array-attribute :: <ShortArray>;
  slot ArrayTest/float-array-attribute :: <FloatArray>;
end class;

define constant $messages = #["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

define method ArrayTest/check-string-array-attribute (object :: <ArrayTest-implementation>)
 => ()
  for (i from 0 below size($messages))
    unless (object.ArrayTest/string-array-attribute[i] = $messages[i])
      error(make(ArrayTest/<failure>));
    end unless;
  end for;
end method;

define method ArrayTest/string-array-operation (object :: <ArrayTest-implementation>, one :: <StringArray>, two :: <StringArray>)
 => (result :: <StringArray>, two :: <StringArray>, three :: <StringArray>)
  values (one, two, one);
end method;


define method ArrayTest/check-short-array-attribute (object :: <ArrayTest-implementation>)
 => ()
  let counter = 0;
  for (i from 0 below 4)
    for (j from 0 below 4)
      unless (object.ArrayTest/short-array-attribute[i,j] = counter)
	error(make(ArrayTest/<failure>));
      end unless;
      counter := counter + 1;
    end for;
  end for;
end method;

define method ArrayTest/short-array-operation (object :: <ArrayTest-implementation>, one :: <ShortArray>, two :: <ShortArray>)
 => (result :: <ShortArray>, two :: <ShortArray>, three :: <ShortArray>)
  values (one, two, one);
end method;


define method ArrayTest/check-float-array-attribute (object :: <ArrayTest-implementation>)
 => ()
  let number = 0.0;
  for (i from 0 below 3)
    for (j from 0 below 3)
      for (k from 0 below 3)
	unless (object.ArrayTest/float-array-attribute[i,j,k] = number)
	  error(make(ArrayTest/<failure>));
	end unless;
	number := number + 0.7;
      end for;
    end for;
  end for;
end method;

define method ArrayTest/float-array-operation (object :: <ArrayTest-implementation>, one :: <FloatArray>, two :: <FloatArray>)
 => (result :: <FloatArray>, two :: <FloatArray>, three :: <FloatArray>)
  values (one, two, one);
end method;


define constant $ArrayTest-ior-file = "c:\\temp\\ArrayTest.ior";

define method start-ArrayTest-server () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let ArrayTest = make(<ArrayTest-implementation>, poa: root-poa);
  let ArrayTestRef = portableserver/poa/servant-to-reference(root-poa, ArrayTest);
  corba/orb/object-to-file(orb, $ArrayTest-ior-file, ArrayTestRef);
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);
end method;

register-server(start-ArrayTest-server);
