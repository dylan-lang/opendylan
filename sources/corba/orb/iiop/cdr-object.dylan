Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method marshall-object (typecode :: <object-reference-typecode>, object :: corba/<object>, stream :: <marshalling-stream>)
  marshall(class-typecode(iop/<ior>), corba/object/ior(object), stream);
end method;

define method unmarshall-object (typecode :: <object-reference-typecode>, stream :: <marshalling-stream>)
  let ior = as(iop/<ior>, unmarshall(class-typecode(iop/<ior>), stream));
  let repository-id = iop/ior/type-id(ior);
  if ((repository-id = "") & empty?(iop/ior/profiles(ior)))
    make-nil(corba/<object>);
  else
    let typecode = as(<typecode>, repository-id);
    let type = (typecode & typecode-native-type(typecode)) | corba/<object>;
    make(type, ior: ior);
  end if;
end method;

