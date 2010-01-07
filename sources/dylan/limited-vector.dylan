Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define limited-vector <integer>       (fill: 0);

define limited-vector-minus-selector <byte>          (<simple-vector>) (fill: 0);
define limited-vector-minus-selector <double-byte>   (<simple-vector>) (fill: 0);

define limited-vector-minus-constructor <element-type>  (<limited-collection>, <simple-vector>)
  (fill: #f);

define method make
    (class == <simple-element-type-vector>,
     #key fill = #f, element-type :: <type>, size :: <integer> = 0)
 => (vector :: <simple-element-type-vector>)
  unless (size = 0)
    check-type(fill, element-type);
  end unless;
  system-allocate-repeated-instance
    (<simple-element-type-vector>, <element-type>, element-type, size, fill);
end method;

define sealed domain element-type (<simple-element-type-vector>);

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
  limited-vector(element-type(vector), #f)
end method type-for-copy;


/// REALLY NEED SUBTYPE SPECIALIZERS TO GET THIS TO HAPPEN IN MACRO
define inline method concrete-limited-vector-class
    (of :: <limited-integer>) => (res :: <class>)
  select (of by subtype?)
    <byte>        => <simple-byte-vector>;
    <double-byte> => <simple-double-byte-vector>;
    otherwise     => <simple-element-type-vector>;
  end select;
end method;

define limited-vector <machine-word>  (fill: as(<machine-word>, 0));
define limited-vector <single-float>  (fill: 0.0);
define limited-vector <double-float>  (fill: as(<double-float>, 0.0));


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
