Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Assemble <stretchy-X-vector> that works for the <X-character> singletons.

define limited-stretchy-vector
    <byte-character>
    (fill: as(<byte-character>, ' '));


// Assemble <stretchy-byte-vector>, but rely on the
// concrete-limited-stretchy-vector-class defined below that picks a limited
// stretchy vector type based on an informed examination of the user-supplied
// limited integer, rather than the concrete-limited-stretchy-vector-class
// defined by limited-stretchy-vector-definer which works only for the <byte>
// singletons.

define limited-stretchy-vector-minus-selector
    <byte> (<limited-stretchy-vector>)
    (fill: as(<byte>, 0));

/// REALLY NEED SUBTYPE SPECIALIZERS TO GET THIS TO HAPPEN IN MACRO
define method concrete-limited-stretchy-vector-class
    (of :: <limited-integer>, default-fill)
 => (res :: <class>, fully-specified? :: <boolean>)
  let fully-specified? = (default-fill = 0);
  select (of by subtype?)
    <byte>        => values(<stretchy-byte-vector>, fully-specified?);
    // <double-byte> => <stretchy-double-byte-vector>;
    otherwise     => next-method();
  end select;
end method;


// Assemble the general <stretchy-element-type-vector>, using the functions
// below and the generic <limited-element-type-collection> functions that allow
// for arbitrary element types.

define limited-stretchy-vector-minus-constructor
    <element-type> (<limited-element-type-collection>, <limited-stretchy-vector>)
    (fill: #f);

define method initialize
    (vector :: <stretchy-element-type-vector>,
     #key size :: <integer> = 0, capacity :: <integer> = size,
          element-type :: <type> = <object>, fill =  #f,
          element-type-fill: default-fill = #f)
 => ()
  next-method();
  unless (size = 0)
    check-type(fill, element-type);
  end unless;
  vector.element-type-fill := default-fill;
  stretchy-initialize(vector, capacity, size, fill);
end method initialize;

define method concrete-limited-stretchy-vector-class
    (of :: <type>, default-fill)
 => (res :: <class>, fully-specified? :: <boolean>)
  values(<stretchy-element-type-vector>, #f)
end method;

define sealed inline method element-setter
    (new-value, collection :: <stretchy-element-type-vector>, index :: <integer>)
 => (object)
  check-type(new-value, element-type(collection));
  if (index < 0) element-range-error(collection, index) end if;
  let collection-size = collection.size;
  if (index >= collection-size)
    if (index = collection-size)
      trusted-size-setter(index + 1, collection, fill: new-value)
    else
      collection.size := index + 1
    end if
  end if;
  // We assume here that the underlying vector only grows.
  // If this ceases to be true the following code will need to be changed.
  stretchy-element-type-vector-element
    (collection.stretchy-representation, index) := new-value
end method element-setter;

/// TODO: COULD BE EXPENSIVE UNLESS TYPES ARE CACHED

define sealed inline method type-for-copy
    (vector :: <stretchy-element-type-vector>) => (type :: <type>)
  limited-stretchy-vector(element-type(vector), element-type-fill(vector))
end method type-for-copy;
