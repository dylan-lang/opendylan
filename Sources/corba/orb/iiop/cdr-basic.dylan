Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// VOID

define method marshall-object (typecode :: <void-typecode>, object :: corba/<void>, stream :: <marshalling-stream>)
end method;

define method unmarshall-object (typecode :: <void-typecode>, stream :: <marshalling-stream>)
end method;

/// SHORT
    
define method marshall-object (typecode :: <short-typecode>, object :: corba/<short>, stream :: <marshalling-stream>)
  write-bytes(stream, object, 2);  
end method;

define method unmarshall-object (typecode :: <short-typecode>, stream :: <marshalling-stream>)
  read-signed-bytes(stream, 2);
end method;

/// LONG

define method marshall-object (typecode :: <long-typecode>, object :: corba/<long>, stream :: <marshalling-stream>)
  write-bytes(stream, object, 4);
end method;

define method unmarshall-object (typecode :: <long-typecode>, stream :: <marshalling-stream>)
  read-signed-bytes(stream, 4);
end method;

/// UNSIGNED SHORT

define method marshall-object (typecode :: <unsigned-short-typecode>, object :: corba/<unsigned-short>, stream :: <marshalling-stream>)
  write-bytes(stream, object, 2);
end method;

define method unmarshall-object (typecode :: <unsigned-short-typecode>, stream :: <marshalling-stream>)
  read-unsigned-bytes(stream, 2);
end method;

/// UNSIGNED LONG

define method marshall-object (typecode :: <unsigned-long-typecode>, object :: corba/<unsigned-long>, stream :: <marshalling-stream>)
  write-bytes(stream, object, 4);
end method;

define method unmarshall-object (typecode :: <unsigned-long-typecode>, stream :: <marshalling-stream>)
  read-unsigned-bytes(stream, 4);
end method;

/// FLOAT

define method marshall-object (typecode :: <float-typecode>, object :: corba/<float>, stream :: <marshalling-stream>)
  write-bytes(stream,
	      make(<double-integer>,
		   low: decode-single-float(object),
		   high: as(<machine-word>, #x00000000)),
	      4);
end method;

define method unmarshall-object (typecode :: <float-typecode>, stream :: <marshalling-stream>)
  encode-single-float(as(<machine-word>, read-unsigned-bytes(stream, 4)));
end method;

/// DOUBLE

define method marshall-object (typecode :: <double-typecode>, object :: corba/<double>, stream :: <marshalling-stream>)
  let (low, high) = decode-double-float(object);
  let lowint = make(<double-integer>, low: low, high: as(<machine-word>, #x0000000));
  let highint = make(<double-integer>, low: high, high: as(<machine-word>, #x00000000));
  if (marshalling-stream-little-endian?(stream))
    write-bytes(stream, lowint, 4);
    write-bytes(stream, highint, 4);
  else
    write-bytes(stream, highint, 4);
    write-bytes(stream, lowint, 4);
  end if;
end method;

define method unmarshall-object (typecode :: <double-typecode>, stream :: <marshalling-stream>)
  if (marshalling-stream-little-endian?(stream))
    let low = read-unsigned-bytes(stream, 4);
    let high = read-unsigned-bytes(stream, 4);
    encode-double-float(as(<machine-word>, low),
			as(<machine-word>, high));
  else
    let high = read-unsigned-bytes(stream, 4);
    let low = read-unsigned-bytes(stream, 4);
    encode-double-float(as(<machine-word>, low),
			as(<machine-word>, high));
  end if;
end method;

/// BOOLEAN

define method marshall-object (typecode :: <boolean-typecode>, object :: corba/<boolean>, stream :: <marshalling-stream>)
  write-element(stream, if (object) 1 else 0 end if);
end method;

define method unmarshall-object (typecode :: <boolean-typecode>, stream :: <marshalling-stream>)
  read-element(stream) ~= 0;
end method;

/// CHAR

define method marshall-object (typecode :: <char-typecode>, object :: corba/<char>, stream :: <marshalling-stream>)
  write-element(stream, as(<integer>, object));
end method;

define method unmarshall-object (typecode :: <char-typecode>, stream :: <marshalling-stream>)
  as(<character>, read-element(stream));
end method;

/// OCTET

define method marshall-object (typecode :: <octet-typecode>, object :: corba/<octet>, stream :: <marshalling-stream>)
  write-element(stream, object);
end method;

define method unmarshall-object (typecode :: <octet-typecode>, stream :: <marshalling-stream>)
  read-element(stream);
end method;

/// STRING

define method marshall-object (typecode :: <string-typecode>, object :: corba/<string>, stream :: <marshalling-stream>)
  write-bytes(stream, (1 + size(object)), 4); // ---*** hmmm not preserving typecode-max-length
  for (char in object)
    write-element(stream, as(<integer>, char));
  end for;
  write-element(stream, 0);
end method;

define method unmarshall-object (typecode :: <string-typecode>, stream :: <marshalling-stream>)
  let length = read-unsigned-bytes(stream, 4) - 1;
  let string = make(<string>, size: length);
  for (i from 0 below length)
    string[i] := as(<character>, read-element(stream));
  end for;
  let terminator = read-element(stream);
  unless (terminator = 0)
    signal(make(<non-zero-string-terminator>, string: string, stream: stream))
  end unless;
  string;
end method;

