Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TYPECODE

define method marshall-object (typecode :: <typecode-typecode>, object :: <typecode>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    with-typecode-output-alignment (stream, reset?: #t)
      marshall-typecode(object, stream);
    end;
  end;
end method;

define method unmarshall-object (typecode :: <typecode-typecode>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    with-typecode-input-alignment (stream, reset?: #t)
      unmarshall-typecode(stream);
    end;
  end;
end method;      

/// ANY

define method marshall-object (typecode :: <any-typecode>, object :: corba/<any>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
     marshall(corba/$typecode-typecode, corba/any/type(object), stream);
     marshall(corba/any/type(object), corba/any/value(object), stream);
  end;
end method;

define method unmarshall-object (typecode :: <any-typecode>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    let any-typecode = unmarshall(corba/$typecode-typecode, stream);
    let value = unmarshall(any-typecode, stream);
    make(corba/<any>, type: any-typecode, value: value);
  end;
end method;

/// INDIRECTION

define method marshall-object (typecode :: <indirection-typecode>, object, stream :: <marshalling-stream>)
  let indirected-typecode = typecode-from-nesting(typecode-nesting(typecode) - 1);
  marshall(indirected-typecode, object, stream);
end method;

define method unmarshall-object (typecode :: <indirection-typecode>, stream :: <marshalling-stream>)
  let indirected-typecode = typecode-from-nesting(typecode-nesting(typecode) - 1);
  unmarshall(indirected-typecode, stream);
end method;



