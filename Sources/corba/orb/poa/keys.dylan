Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** This is manual marshalling that ought be done using
/// encapsulations or something I think.

define constant $object-key-version :: <integer> = 2;
/// GENERIC STUFF

define class <object-key> (<object>)
  constant each-subclass slot object-key-version :: <integer> = $object-key-version, init-keyword: version:;
  slot object-key-encoding :: corba/<encapsulation> = make(corba/<encapsulation>), init-keyword: encoding:;
  slot object-key-offset :: <integer> = 0;
end class;

define sealed domain make (subclass(<object-key>));
define sealed domain initialize (<object-key>);

define constant $object-key-classes :: <table> = make(<table>);

define class <unknown-object-key-version> (<simple-error>)
  keyword format-string: = "Unknown Object Key version: %d";
end class;

define sealed domain make (subclass(<unknown-object-key-version>));
define sealed domain initialize (<unknown-object-key-version>);

define method make (class == <unknown-object-key-version>, #key version)
 => (condition :: <unknown-object-key-version>)
  next-method(class, format-arguments: vector(version))
end method;

define method class-for-object-key (version :: <integer>)
  element($object-key-classes, version, default: #f)
    | error(make(<unknown-object-key-version>, version: version))
end method;

define method make (class == <object-key>, #rest initargs, #key version :: <integer> = $object-key-version)
 => (object-key :: <object-key>)
  apply(make, class-for-object-key(version), initargs)
end method;

define method encode-object-key (object-key :: <object-key>)
  encode-object(object-key, object-key-version(object-key));
  do-encode-object-key(object-key);
  object-key-encoding(object-key);
end method;

define method decode-object-key (encoded-key :: corba/<encapsulation>)
  let prototype-key :: <object-key> = make(<object-key>, encoding: encoded-key);
  let version :: <integer> = decode-object(prototype-key, <integer>);
  let object-key :: <object-key>
    = if (object-key-version(prototype-key) = version)
	prototype-key
      else
	make(<object-key>, version: version, encoding: encoded-key, offset: object-key-offset(prototype-key));
      end if;
  do-decode-object-key(object-key);
  object-key
end method;

/// VERSION 1

define class <object-key-1> (<object-key>)
  slot object-key-objectid :: <string> = "", init-keyword: objectid:;
  slot object-key-poa-path :: <sequence> = #[], init-keyword: poa-path:;
end class;

element($object-key-classes, 1) := <object-key-1>;

/// ENCODING

define method do-encode-object-key (object-key :: <object-key-1>)
  encode-object(object-key, object-key-objectid(object-key));
  encode-object(object-key, object-key-poa-path(object-key));
end method;

define method encode-object (object-key :: <object-key>, int :: <integer>)
  let key = object-key-encoding(object-key);
  key := add!(key, logand(#xff, ash(int, -24)));
  key := add!(key, logand(#xff, ash(int, -16)));
  key := add!(key, logand(#xff, ash(int, -8)));
  key := add!(key, logand(#xff, int));
  object-key-encoding(object-key) := key;
end method;

define method encode-object (object-key :: <object-key>, string :: <string>)
  encode-object(object-key, size(string));
  let key = object-key-encoding(object-key);
  for (char in string)
    key := add!(key, as(<integer>, char));
  end for;
  object-key-encoding(object-key) := key;
end method;
                             
define method encode-object (object-key :: <object-key>, seq :: <sequence>)
  encode-object(object-key, size(seq));
  let key = object-key-encoding(object-key);
  for (elt in seq)
    key := encode-object(object-key, elt);
  end for;
  object-key-encoding(object-key) := key;
end method;

/// DECODING

define method do-decode-object-key (object-key :: <object-key-1>)
  object-key-objectid(object-key) := decode-object(object-key, <string>);
  object-key-poa-path(object-key) := decode-object(object-key, <sequence>);
end method;

define method decode-object (object-key :: <object-key>, class == <sequence>);
  let length = decode-object(object-key, <integer>);
  let seq = make(<stretchy-vector>);
  for (i from 0 below length)
    seq := add!(seq, decode-object(object-key, <string>));
  end for;
  seq
end method;

define method decode-object (object-key :: <object-key>, class == <string>)
  let encoded-key = object-key-encoding(object-key);
  let length = decode-object(object-key, <integer>);
  let offset = object-key-offset(object-key);
  let string = make(<string>, size: length);
  for (i from 0 below length,
       j from offset)
    string[i] := as(<character>, encoded-key[j]);
  end for;
  object-key-offset(object-key) := offset + length;
  string
end method;

define method decode-object (object-key :: <object-key>, class == <integer>)
  let encoded-key = object-key-encoding(object-key);
  let offset = object-key-offset(object-key);
  let length = (ash(encoded-key[offset + 0], 24)
		  + ash(encoded-key[offset + 1], 16)
		  + ash(encoded-key[offset + 2], 8)
		  + encoded-key[offset + 3]);
  object-key-offset(object-key) := offset + 4;
  length
end method;

// NB OBJECT-KEY-POA-ID on <OBJECT-KEY-1> is for backwards compatibility
define method object-key-poa-id (object-key :: <object-key-1>)
 => (id :: <string>)
  ""
end method;


/// VERSION 2

define class <object-key-2> (<object-key-1>)
  slot object-key-poa-id :: <string> = "", init-keyword: poa-id:;
end class;

element($object-key-classes, 2) := <object-key-2>;

define method do-encode-object-key (object-key :: <object-key-2>) 
  next-method();
  encode-object(object-key, object-key-poa-id(object-key))
end method;

define method do-decode-object-key (object-key :: <object-key-2>)
  next-method();
  object-key-poa-id(object-key) := decode-object(object-key, <string>);
end method;

