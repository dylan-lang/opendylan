Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The <C-array> class

// We have an array value class. The size-of an array value may be 
// indeterminate if one of its dimensions is unspecified. We need
// to generate a different class for each size of array but that's
// pretty much true of Dylan's limited arrays anyhow.

// Is not being able to take slices through a multi-dimensional array
// going to be a problem? I doubt it - it's easy enough to simulate.
// Could support it if we really wanted to with methods on aref.

// Should be a subclass of array but the emulator's broken here.

define abstract class <C-array> (<C-value>)
end class;

define subclass-slot array-element-type = #f of <C-array> end;
define subclass-slot array-element-count = #f of <C-array> end;
define subclass-slot array-dimensions = #f of <C-array> end;

// Questionable!!!

define method make (class :: subclass(<C-array>), #rest args)
  apply(make, 
        array-element-type(class).pointer-type, 
        element-count: array-element-count(class),
        args)
end method;

// Modify value access on an array type to give you a pointer to
// the array type. Should we distinguish between array types 
// and pointers to elements? We'd like to do bounds checking
// on array pointers I guess. That implies that a pointer
// to an array should be a subclass of a pointer to the array's
// contents type? That's bound to cause confusion is there's
// much aliasing going on.

define method low-level-value-at (c :: subclass(<C-array>), addr)
/*
  make(array-element-type(c).pointer-type, 
       pointer: addr, 
       element-count: array-element-count(c))
*/
  make(c.pointer-type, 
       pointer: addr, 
       element-count: array-element-count(c))
end method;

define variable $array-types = make(<equal-table>);

define method array-type (element-type, #key dimensions = #(#f))
  let key = pair(element-type, dimensions);
  let array-type = element($array-types, key, default: #f);
  array-type | 
    begin
      let array-class = make(<class>, 
                             debug-name: #"<array-class>",
                             superclasses: list(<C-array>));
      array-element-type(array-class) := element-type;
      array-dimensions(array-class) := dimensions;
      let (elements, size) =
        compute-array-parameters(element-type, dimensions);
      array-element-count(array-class) := elements;
      size-of(array-class) := size;
      alignment-of(array-class) := alignment-of(element-type);
      element($array-types, key) := array-class;
      array-class
    end
end method;

define method compute-array-parameters (element-type, dimensions)
  if (~dimensions.first) 
    values(#f, #f)
  else 
    let elements = reduce1(\*, dimensions);
    let size = size-of(element-type) * elements;
    values(elements, size)
  end
end method;

//// The <C-array-pointer> class

define class <C-array-pointer> (<C-typed-pointer>)
end class;

define method superclass-for-pointer-type (type :: subclass(<C-array>))
  <C-array-pointer>
end method;

// Convenience methods:

define method array-element-type 
    (array-ptr-type :: subclass(<C-array-pointer>))
  array-element-type(referenced-type(array-ptr-type))
end method;

define method array-dimensions
    (array-ptr-type :: subclass(<C-array-pointer>))
  array-dimensions(referenced-type(array-ptr-type))
end method;

// Consider pointers to arrays? Consider arrays to be atomic - the
// element type of the pointer almost. All a bit tricky. Some way of
// taking the address of particular parts of a structure/array?

define method aref (a :: <C-array-pointer>, #rest indices)
  
end method;

define method aref-setter (value, a :: <C-array-pointer>, #rest indices)

end method;

// eof
