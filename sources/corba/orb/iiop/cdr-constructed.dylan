Module: orb-iiop
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// ENUM

define method marshall-object (typecode :: <enum-typecode>, object :: <symbol>, stream :: <marshalling-stream>)
  let value = typecode-symbol-index(typecode)[object];
  write-bytes(stream, value, 4);
end method;

define method unmarshall-object (typecode :: <enum-typecode>, stream :: <marshalling-stream>)
  let value = read-unsigned-bytes(stream, 4);
  typecode-members(typecode)[value];
end method;

/// SEQUENCE

define method marshall-object (typecode :: <sequence-typecode>, object :: corba/<sequence>, stream :: <marshalling-stream>)
  let element-typecode = typecode-element-typecode(typecode);
  let length = size(object);
  check-sequence-length(typecode, length, #"completed-no");
  write-bytes(stream, length, 4);
  with-typecoded-value-indirection (typecode)
    for (elt in object)
      marshall(element-typecode, elt, stream);
    end for;
  end;
end method;

define method unmarshall-object (typecode :: <sequence-typecode>, stream :: <marshalling-stream>)
  let length = read-unsigned-bytes(stream, 4);
  check-sequence-length(typecode, length, #"completed-maybe");
  let element-typecode = typecode-element-typecode(typecode);
  with-typecoded-value-indirection (typecode)
    let result = make(typecode-native-type(typecode));
    for (i from 0 below length)
      result := add!(result, unmarshall(element-typecode, stream));
    end for;
    result
  end;
end method;

define method check-sequence-length
    (typecode :: <sequence-typecode>, length :: <integer>, completed :: corba/<completion-status>)
    => ()
  let max = typecode-max-length(typecode);
  unless (zero?(max) | (length <= max))
    error(make(corba/<bad-param>, minor: 0, completed: completed));
  end unless;
end method;

/// ARRAY

define method marshall-object (typecode :: <array-typecode>, object :: corba/<array>, stream :: <marshalling-stream>)
  let element-typecode = typecode;
  for (i from 0 below rank(object))
    element-typecode := typecode-element-typecode(element-typecode);
  end for;
  with-typecoded-value-indirection (typecode)
    for (elt in object)
      marshall(element-typecode, elt, stream);
    end for;
  end;
end method;

define method unmarshall-object (typecode :: <array-typecode>, stream :: <marshalling-stream>)
  let dims = make(<stretchy-vector>);
  let element-typecode = typecode;
  while (instance?(element-typecode, <array-typecode>))
    dims := add!(dims, typecode-length(element-typecode));
    element-typecode := typecode-element-typecode(element-typecode);
  end while;
  with-typecoded-value-indirection (typecode)
    let x = unmarshall(element-typecode, stream);
    let result = make(typecode-native-type(typecode), fill: x, dimensions: dims);
    for (i from 1 below size(result))
      result[i] := unmarshall(element-typecode, stream);
    end for;
    result;
  end;
end method;

/// STRUCT

define method marshall-object (typecode :: <struct-typecode>, object :: corba/<struct>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    for (member :: <typecode-member> in typecode-members(typecode))
      marshall(typecode-member-typecode(member), typecode-member-getter(member)(object), stream);
    end for;
  end;
end method;

define method marshall-object (typecode :: <struct-typecode>, object :: <anonymous-object>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    for (member :: <typecode-member> in typecode-members(typecode),
	 property in anonymous-object-properties(object))
      marshall(typecode-member-typecode(member), property, stream);
    end for;
  end;
end method;

define method unmarshall-object (typecode :: <struct-typecode>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    let type = typecode-native-type(typecode);
    if (type)
      apply(make, type, unmarshall-members(typecode, stream, init-keywords?: #t));
    else
      make(typecode-anonymous-native-type(typecode), properties: unmarshall-members(typecode, stream));
    end if;
  end;
end method;

// NB separate loops to avoid retesting init-keywords
define method unmarshall-members
    (typecode :: <struct-typecode>, stream :: <marshalling-stream>, #key init-keywords? :: <boolean> = #f)
 => (result :: <sequence>)
  let result :: <stretchy-vector> = make(<stretchy-vector>);
  if (init-keywords?)
    for (member :: <typecode-member> in typecode-members(typecode))
      result := add!(result, typecode-member-init-keyword(member));
      result := add!(result, unmarshall(typecode-member-typecode(member), stream));
    end for;
  else
    for (member :: <typecode-member> in typecode-members(typecode))
      result := add!(result, unmarshall(typecode-member-typecode(member), stream));
    end for;
  end if;
  result
end method;


/// EXCEPTION
//
// NB this should just be inherited from struct
//
//define method marshall-object (typecode :: <except-typecode>, object, stream :: <marshalling-stream>)
//  ...
//end method;

/// UNION

define method marshall-object (typecode :: <union-typecode>, object :: corba/<union>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    let discriminator = corba/union/discriminator(object);
    marshall(typecode-discriminator-typecode(typecode), discriminator, stream);
    marshall(union-value-typecode(typecode, discriminator), corba/union/value(object), stream);
  end;
end method;

define method unmarshall-object (typecode :: <union-typecode>, stream :: <marshalling-stream>)
  with-typecoded-value-indirection (typecode)
    let type = typecode-native-type(typecode) | typecode-anonymous-native-type(typecode);
    let discriminator = unmarshall(typecode-discriminator-typecode(typecode), stream);
    let value = unmarshall(union-value-typecode(typecode, discriminator), stream);
    make(type, discriminator: discriminator, value: value);
  end;
end method;

define method union-value-typecode (typecode :: <union-typecode>, discriminator)
 => (typecode :: <typecode>)
  typecode-member-typecode(element(typecode-members(typecode),
				   (find-key(typecode-members(typecode),
					     method (branch :: <typecode-branch>)
					       discriminator = typecode-label-value(branch)
					     end method)
				      | typecode-default-used(typecode))));
end method;

