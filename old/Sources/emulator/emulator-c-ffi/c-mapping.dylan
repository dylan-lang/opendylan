Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic import-value (dylan-class, wrapper-class, low-level-value)
  => (high-level-value :: <object>);

define generic export-value (wrapper-class, high-level-value)
  => (low-level-value :: <object>);

define generic class-for-map (class :: subclass(<C-value>))
  => (class);

// The default method on class-for-map:

define method class-for-map (class :: subclass(<C-value>))
  #f
end method;

// Questionable!!! Default methods on import/export-value.
// These "as" coercions could potentially cause unrecoverable 
// allocation?

define method import-value 
    (dylan-class :: <class>, wrapper-class :: <class>, value)
  if (dylan-class == wrapper-class)
    value
  else
    as(dylan-class, value)
  end
end method;

define method export-value 
    (wrapper-class :: <class>, value)
  if (wrapper-class == object-class(value))
    value
  else
    as(wrapper-class, value)
  end
end method;

// Questionable!!! Make the default method on class-for-map the class
// itself??

define method class-for-map (class :: subclass(<C-value>))
  class
end method;

// Hack!!! Just plain incorrect - need more precise control. This will
// do until I put a sensible subclass above all the fundamental numeric
// types.

define method as (class :: subclass(<C-value>), n :: <number>)
  n
end method;

// eof
