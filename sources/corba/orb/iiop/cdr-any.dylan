Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TYPECODE

define method marshall-object (typecode :: <typecode-typecode>, object :: <typecode>, stream :: <marshalling-stream>)
  marshall-typecode(object, stream);
end method;

define method unmarshall-object (typecode :: <typecode-typecode>, stream :: <marshalling-stream>)
  with-typecode-input-alignment (stream)
    unmarshall-typecode(stream);
  end;
end method;

/// ANY

define method marshall-object (typecode :: <any-typecode>, object :: corba/<any>, stream :: <marshalling-stream>)
  marshall(corba/$typecode-typecode, corba/any/type(object), stream);
  marshall(corba/any/type(object), corba/any/value(object), stream);
end method;

define method unmarshall-object (typecode :: <any-typecode>, stream :: <marshalling-stream>)
  let any-typecode = unmarshall(corba/$typecode-typecode, stream);
  let value = unmarshall(any-typecode, stream);
  make(corba/<any>, type: as(<typecode>, any-typecode), value: value);
end method;

/// INDIRECTION

define method marshall-object (typecode :: <indirection-typecode>, object, stream :: <marshalling-stream>)
  marshall(typecode.typecode-indirected, object, stream);
end method;

define method unmarshall-object (typecode :: <indirection-typecode>, stream :: <marshalling-stream>)
  unmarshall(typecode.typecode-indirected, stream);
end method;



