Module: orb-iiop
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MARSHALL-TYPECODE

define method marshall-typecode (object :: <empty-typecode>, stream :: <marshalling-stream>)
  with-typecode-output-alignment (stream)
    marshall(corba/$unsigned-long-typecode, typecode-code(object), stream);
    marshall-typecode-object(object, stream);
  end;
end method;

define method marshall-typecode (object :: <complex-typecode>, stream :: <marshalling-stream>)
  with-typecode-output-alignment (stream)
    marshall(corba/$unsigned-long-typecode, typecode-code(object), stream);
    let position = marshalling-stream-output-index(stream);
    marshall(corba/$unsigned-long-typecode, 0, stream); // NB reserve space for length to written in
    with-output-alignment-constraint (stream)
      marshall(corba/$octet-typecode,
	       if (marshalling-stream-little-endian?(stream)) 1 else 0 end if,
	       stream);
      marshall-typecode-object(object, stream);
      set-buffer-size(stream, position, (marshalling-stream-output-index(stream) - position - 4)); /// ---*** magic
    end;
  end;  
end method;

/// MARSHALL-TYPECODE-OBJECT

define method marshall-typecode-object (object :: <typecode>, stream :: <marshalling-stream>)
end method;

define method marshall-typecode-object (object :: <type-typecode>, stream :: <marshalling-stream>)
 => ()
  marshall(corba/$string-typecode, typecode-repository-id(object), stream);
  marshall(corba/$string-typecode, typecode-name(object), stream);
end method;

define method marshall-typecode-object (object :: <struct-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$unsigned-long-typecode, typecode-count(object), stream);
  for (member :: <typecode-member> in typecode-members(object))
    marshall-typecode-object(member, stream);
  end for;
end method;

define method marshall-typecode-object (object :: <union-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$typecode-typecode, typecode-discriminator-typecode(object), stream);
  marshall(corba/$long-typecode, typecode-default-used(object), stream);
  marshall(corba/$unsigned-long-typecode, typecode-count(object), stream);
  for (branch :: <typecode-branch> in typecode-members(object))
    marshall(typecode-discriminator-typecode(object), typecode-label-value(branch), stream);
    marshall-typecode-object(branch, stream); // NB do shared bit
  end for;
end method;

define method marshall-typecode-object (object :: <typecode-member>, stream :: <marshalling-stream>)
  marshall(corba/$string-typecode, typecode-member-name(object), stream);
  marshall(corba/$typecode-typecode, typecode-member-typecode(object), stream);
end method;

define method marshall-typecode-object (object :: <enum-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$unsigned-long-typecode, typecode-count(object), stream);
  for (member in typecode-members(object))
    marshall(corba/$string-typecode, as(<string>, member), stream);
  end for;
end method;

define method marshall-typecode-object (object :: <string-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$unsigned-long-typecode, typecode-max-length(object), stream);
end method;

define method marshall-typecode-object (object :: <sequence-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$typecode-typecode, typecode-element-typecode(object), stream);
  marshall(corba/$unsigned-long-typecode, typecode-max-length(object), stream);
end method;

define method marshall-typecode-object (object :: <array-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$typecode-typecode, typecode-element-typecode(object), stream);
  marshall(corba/$unsigned-long-typecode, typecode-length(object), stream);
end method;

define method marshall-typecode-object (object :: <alias-typecode>, stream :: <marshalling-stream>)
  next-method();
  marshall(corba/$typecode-typecode, typecode-aliased(object), stream);
end method;

// NB inherited from <struct-typecode>
//define method marshall-typecode-object (object :: <exception-typecode>, stream :: <marshalling-stream>)
//  ...
//end method;

define method marshall-typecode-object (object :: <indirection-typecode>, stream :: <marshalling-stream>)
  next-method();
  let offset = typecode-alignment-from-nesting(typecode-nesting(object)) - marshalling-stream-output-index(stream) - 7; // ---*** magic
  marshall(corba/$long-typecode, offset, stream)
end method;

/// UNMARSHALL-TYPECODE

define method unmarshall-typecode (stream :: <marshalling-stream>)
  let code = unmarshall(corba/$unsigned-long-typecode, stream);
  let typecode = as(<typecode>, code);
  unmarshall-typecode-into(typecode, stream);
  as(<typecode>, typecode); // NB interns/canonicalizes
end method;

/// UNMARSHALL-TYPECODE-INTO

define method unmarshall-typecode-into (object :: <typecode>, stream :: <marshalling-stream>)
  unmarshall-typecode-object-into(object, stream);
end method;

define method unmarshall-typecode-into (object :: <complex-typecode>, stream :: <marshalling-stream>)
  let position = marshalling-stream-input-index(stream);
  let length = unmarshall(corba/$unsigned-long-typecode, stream);
  with-input-alignment-constraint (stream)
    let encapsulation-little-endian? = (unmarshall(corba/$octet-typecode, stream) ~= 0);
    with-little-endianness (stream, encapsulation-little-endian?)
      unmarshall-typecode-object-into(object, stream);
    end;
  end;
  let actual-length = marshalling-stream-input-index(stream) - position - 4; // ---*** magic
  unless (actual-length = length)
    signal(make(<invalid-encapsulation-length>, length: length, actual-length: actual-length, object: object, stream: stream))
  end unless;
end method;

/// UNMARSHALL-TYPECODE-OBJECT-INTO

define method unmarshall-typecode-object-into (object :: <typecode>, stream :: <marshalling-stream>)
end method;

define method unmarshall-typecode-object-into (object :: <type-typecode>, stream :: <marshalling-stream>)
 => ()
  next-method();
  typecode-repository-id(object) := unmarshall(corba/$string-typecode, stream);
  typecode-name(object) := unmarshall(corba/$string-typecode, stream);
end method;

define method unmarshall-typecode-object-into (object :: <struct-typecode>, stream :: <marshalling-stream>)
  next-method();
  let count = unmarshall(corba/$unsigned-long-typecode, stream);
  let members = make(<vector>, size: count);
  for (i from 0 below count)
    let member = make(<typecode-member>);
    members[i] := member;
    unmarshall-typecode-object-into(member, stream);
  end for;
  typecode-members(object) := members;
end method;

define method unmarshall-typecode-object-into (object :: <union-typecode>, stream :: <marshalling-stream>)
  next-method();
  let discriminator-typecode = unmarshall(corba/$typecode-typecode, stream);
  let default-used = unmarshall(corba/$long-typecode, stream);
  let count = unmarshall(corba/$unsigned-long-typecode, stream);
  let branches = make(<vector>, size: count);
  for (i from 0 below count)
    let branch = make(<typecode-branch>);
    branches[i] := branch;
    typecode-label-value(branch) := unmarshall(discriminator-typecode, stream);
    unmarshall-typecode-object-into(branch, stream);
  end for;
  typecode-discriminator-typecode(object) := discriminator-typecode;
  typecode-default-used(object) := default-used;
  typecode-members(object) := branches;
end method;

define method unmarshall-typecode-object-into (object :: <typecode-member>, stream :: <marshalling-stream>)
  typecode-member-name(object) := unmarshall(corba/$string-typecode, stream);
  typecode-member-typecode(object) := unmarshall(corba/$typecode-typecode, stream);
end method;

define method unmarshall-typecode-object-into (object :: <enum-typecode>, stream :: <marshalling-stream>)
  next-method();
  let count = unmarshall(corba/$unsigned-long-typecode, stream);
  let members = make(<vector>, size: count);
  for (i from 0 below count)
    members[i] := as(<symbol>, unmarshall(corba/$string-typecode, stream));
  end for;
  typecode-members(object) := members;
end method;

define method unmarshall-typecode-object-into (object :: <string-typecode>, stream :: <marshalling-stream>)
  next-method();
  typecode-max-length(object) := unmarshall(corba/$unsigned-long-typecode, stream);
end method;

define method unmarshall-typecode-object-into (object :: <sequence-typecode>, stream :: <marshalling-stream>)
  next-method();
  typecode-element-typecode(object) := unmarshall(corba/$typecode-typecode, stream);
  typecode-max-length(object) := unmarshall(corba/$unsigned-long-typecode, stream);
end method;

define method unmarshall-typecode-object-into (object :: <array-typecode>, stream :: <marshalling-stream>)
  next-method();
  typecode-element-typecode(object) := unmarshall(corba/$typecode-typecode, stream);
  typecode-length(object) := unmarshall(corba/$unsigned-long-typecode, stream);
end method;

define method unmarshall-typecode-object-into (object :: <alias-typecode>, stream :: <marshalling-stream>)
  next-method();
  typecode-aliased(object) := unmarshall(corba/$typecode-typecode, stream);
end method;

// NB inherited from <struct-typecode>
//define method unmarshall-typecode-object-into (object :: <exception-typecode>, stream :: <marshalling-stream>)
//  ...
//end method;

define method unmarshall-typecode-object-into (object :: <indirection-typecode>, stream :: <marshalling-stream>)
  next-method();
  let offset = unmarshall(corba/$long-typecode, stream);
  let alignment = offset + marshalling-stream-input-index(stream) + 4; // ---*** magic
  typecode-offset(object) := offset;
  typecode-nesting(object) := typecode-nesting-from-alignment(alignment);
end method;


