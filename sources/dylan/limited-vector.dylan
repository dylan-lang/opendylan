Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Assemble <simple-X-vector> that works for the <X> singletons.

define limited-vector <integer>       (fill: 0);
define limited-vector <machine-word>  (fill: as(<machine-word>, 0));
define limited-vector <single-float>  (fill: 0.0);
define limited-vector <double-float>  (fill: as(<double-float>, 0.0));


// Assemble <simple-byte-vector> and <simple-double-byte-vector>, but rely on
// the concrete-limited-vector-class defined below that picks a limited vector 
// type based on an informed examination of the user-supplied limited integer, 
// rather than the concrete-limited-vector-class defined by
// limited-vector-definer which works only for the <byte> or <double-byte>
// singletons.

define limited-vector-minus-selector <byte>
    (<limited-fillable-collection>, <simple-vector>) (fill: 0);
define limited-vector-minus-selector <double-byte>
    (<limited-fillable-collection>, <simple-vector>) (fill: 0);

/// REALLY NEED SUBTYPE SPECIALIZERS TO GET THIS TO HAPPEN IN MACRO
define inline method concrete-limited-vector-class
    (of :: <limited-integer>, default-fill)
 => (res :: <class>, fully-specified?)
  let fully-specified? = (default-fill = 0);
  select (of by subtype?)
    <byte>        => values(<simple-byte-vector>, fully-specified?);
    <double-byte> => values(<simple-double-byte-vector>, fully-specified?);
    otherwise     => next-method();
  end select;
end method;


// Assemble the general <simple-element-type-vector>, using the functions below
// and the generic <limited-element-type-collection> functions that allow for arbitrary
// element types.

define limited-vector-minus-constructor <element-type>
    (<limited-element-type-collection>, <limited-fillable-collection>, <simple-vector>) (fill: #f);

define method make
    (class == <simple-element-type-vector>,
     #key fill = #f, element-type :: <type> = <object>, size :: <integer> = 0,
          element-type-fill: default-fill = #f)
 => (vector :: <simple-element-type-vector>)
  unless (size = 0)
    check-type(fill, element-type);
  end unless;
  let instance = system-allocate-repeated-instance
    (<simple-element-type-vector>, <element-type>, element-type, size, fill);
  instance.element-type-fill := default-fill;
  instance
end method;

define inline sealed method element-setter
    (new-value, vector :: <simple-element-type-vector>, index :: <integer>) => (object)
  check-type(new-value, element-type(vector));
  if (element-range-check(index, size(vector)))
    element-no-bounds-check(vector, index) := new-value
  else
    element-range-error(vector, index)
  end if
end method element-setter;

/// TODO: COULD BE EXPENSIVE UNLESS TYPES ARE CACHED

define inline method type-for-copy (vector :: <simple-element-type-vector>)
 => (type :: <type>)
  limited-vector(element-type(vector), element-type-fill(vector), #f)
end method type-for-copy;


//
// SIMPLE-BYTE-VECTOR
//

/// Special <byte-vector> <-> <byte-string> coercions

define method as (bsc == <byte-string>, bv :: <simple-byte-vector>)
 => (bs :: <byte-string>)
  let bs :: <byte-string> = make(<byte-string>, size: bv.size);
  without-bounds-checks
    for (i :: <integer> from 0 below bv.size)
      bs[i] := as(<byte-character>, bv[i]);
    end;
  end without-bounds-checks;
  bs
end method;

define method as
    (bvc == <simple-byte-vector>, bs :: <byte-string>) => (bv :: <simple-byte-vector>)
  let bv :: <simple-byte-vector> = make(<simple-byte-vector>, size: bs.size);
  without-bounds-checks
    for (i :: <integer> from 0 below bs.size)
      bv[i] := as(<byte>, bs[i]);
    end;
  end without-bounds-checks;
  bv
end method;

define method as
    (class == <string>, x :: <simple-byte-vector>) => (string :: <byte-string>)
  as(<byte-string>, x)
end method;

// already is a vector.  Maybe want method for simple object vector?
define method as
    (class == <vector>, x :: <simple-byte-vector>) => (vector :: <vector>)
  x
end method;
