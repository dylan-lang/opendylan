Module: dfmc-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Load-bound reference queries.

define generic load-bound-object? (object) => (boolean);

define method load-bound-object? (object) => (boolean)
  #f
end method;

define generic load-bound-reference?
    (referencing-object, referenced-object) => (boolean);

define method load-bound-reference?
    (referencing-object, referenced-object) => (boolean)
  load-bound-object?(referenced-object)
end method;

//// Load-bound references.

// A load-bound reference encapsulates a reference to an object
// whose identity and/or location can not be determined until load
// time. Examples are references to interned objects such as symbols,
// and cross-dll references to static data.

define abstract class <load-bound-reference> (<object>)
  constant slot load-bound-referenced-object,
    required-init-keyword: referenced-object:;
end class;

//// Load-bound references from bindings.

define class <load-bound-binding-reference> (<load-bound-reference>)
  constant slot load-bound-referencing-binding,
    required-init-keyword: referencing-binding:;
end class;

//// Load-bound references from objects.

define abstract class <load-bound-object-reference> (<load-bound-reference>)
  constant slot load-bound-referencing-object,
    required-init-keyword: referencing-object:;
end class;

/*
// A load-bound reference as the object-class of the referencing object.
define class <load-bound-object-class-reference>
    (<load-bound-object-reference>)
end class;
*/

// A load-bound reference as a slot value of the referencing object.

define abstract class <load-bound-slot-reference>
    (<load-bound-object-reference>)
  constant slot load-bound-referencing-slot,
    required-init-keyword: referencing-slot:
end class;

define class <load-bound-instance-slot-reference>
    (<load-bound-slot-reference>)
end class;

define class <load-bound-repeated-slot-reference>
    (<load-bound-slot-reference>)
  constant slot load-bound-referencing-slot-index,
    required-init-keyword: referencing-slot-index:;
end class;

//// Load-bound references from code.

// We don't currently require any information about the location of a
// code reference since we don't patch code, just generate an
// indirection.

define class <load-bound-code-reference> (<load-bound-reference>)
end class;
