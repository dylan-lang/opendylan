Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define limited-array <integer> (fill: 0);

define limited-array-minus-selector
    <byte> (<limited-fillable-collection>, <simple-array>)
    (fill: as(<byte>, 0));

define limited-array-minus-selector
    <double-byte> (<limited-fillable-collection>, <simple-array>)
    (fill: as(<double-byte>, 0));

define limited-array-minus-constructor
    <element-type> (<limited-element-type-collection>, <limited-fillable-collection>, <simple-array>)
    (fill: #f);

define sealed method make
    (class == <simple-element-type-array>,
     #key dimensions = unsupplied(), element-type = <object>, fill = #f,
          element-type-fill: default-fill = #f)
 => (array :: <simple-element-type-array>)
  let (dimensions, size) = compute-array-dimensions-and-size(dimensions);
  unless (size = 0)
    check-type(fill, element-type);
  end unless;
  next-method(class,
              element-type:      element-type,
              element-type-fill: default-fill,
              dimensions:        dimensions,
              size:              size,
              fill:              fill)
end method;

define method concrete-limited-array-class
    (of :: <type>, default-fill)
 => (res :: <class>, fully-specified?)
  values(<simple-element-type-array>, #f)
end method;

define sealed inline method element-setter
    (new-value, array :: <simple-element-type-array>, index :: <integer>)
 => (object)
  check-type(new-value, element-type(array));
  if (index >= 0 & index < array.size)
    row-major-element-type-array-element(array, index) := new-value
  else
    element-range-error(array, index)
  end if
end method element-setter;

define sealed inline method type-for-copy
    (array :: <simple-element-type-array>) => (type :: <limited-mutable-sequence-type>)
  limited-array(element-type(array), element-type-fill(array), #f)
end method type-for-copy;

/// REALLY NEED SUBTYPE SPECIALIZERS TO GET THIS TO HAPPEN IN MACRO
define method concrete-limited-array-class
    (of :: <limited-integer>, default-fill)
 => (res :: <class>, fully-specified?)
  let fully-specified? = (default-fill = 0);
  select (of by subtype?)
    <byte>        => values(<simple-byte-array>, fully-specified?);
    <double-byte> => values(<simple-double-byte-array>, fully-specified?);
    otherwise     => next-method();
  end select;
end method;

define limited-array <machine-word>  (fill: as(<machine-word>, 0));
define limited-array <single-float>  (fill: as(<single-float>, 0.0));
define limited-array <double-float>  (fill: as(<double-float>, 0.0));
