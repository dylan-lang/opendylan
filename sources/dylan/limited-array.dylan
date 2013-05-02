Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define limited-array <integer>       (fill: 0);

define limited-array-minus-selector <byte>          (<simple-array>) (fill: 0);
define limited-array-minus-selector <double-byte>   (<simple-array>) (fill: 0);

define limited-array-minus-constructor <element-type> (<simple-array>, <limited-collection>)
  (fill: #f);

define sealed domain element-type (<simple-element-type-array>);

define sealed method make
    (class == <simple-element-type-array>,
     #key dimensions = unsupplied(), element-type, fill = #f)
 => (array :: <simple-element-type-array>)
  let (dimensions, size) = compute-array-dimensions-and-size(dimensions);
  unless (size = 0)
    check-type(fill, element-type);
  end unless;
  next-method(class,
              element-type: element-type,
              dimensions:   dimensions,
              size:         size,
              fill:         fill)
end method;

define method concrete-limited-array-class
    (of :: <type>) => (res :: <class>)
  <simple-element-type-array>
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
  limited-array(element-type(array), #f)
end method type-for-copy;


/// REALLY NEED SUBTYPE SPECIALIZERS TO GET THIS TO HAPPEN IN MACRO
define method concrete-limited-array-class
    (of :: <limited-integer>) => (res :: <class>)
  select (of by subtype?)
    <byte>        => <simple-byte-array>;
    <double-byte> => <simple-double-byte-array>;
    otherwise     => <simple-element-type-array>;
  end select;
end method;

define limited-array <machine-word>  (fill: as(<machine-word>, 0));
define limited-array <single-float>  (fill: 0.0);
define limited-array <double-float>  (fill: as(<double-float>, 0.0));
