Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// BOOTED: define ... class <array> ... end;


////////////
// INTERFACE
////////////


// Open generics on <array>

define open generic rank 
  (array :: <array>) => (rank :: <integer>);

define open generic row-major-index 
  (array :: <array>, #rest subscripts) => (index :: <integer>);

define open generic aref 
  (array :: <array>, #rest indices) => object;

define open generic aref-setter 
  (new-value, array :: <array>, #rest indices) => object;

define open generic dimensions 
  (array :: <array>) => (dims :: <sequence>);

define open generic dimension 
  (array :: <array>, axis :: <integer>) => (dim :: <integer>);

define open generic limited-array 
    (of :: <type>, dimensions :: false-or(<sequence>))
 => (type :: <type>);


/////////////////
// IMPLEMENTATION
/////////////////


//
// RANK
// 

define inline method rank (array :: <array>) => (rank :: <integer>)
  size(array.dimensions)
end method rank;


//
// ROW-MAJOR-INDEX
// 

define function aref-rank-error
    (array :: <array>, subscripts)
 => (will-never-return :: <bottom>)
  // We don't embed the collection in the condition as it will prevent the
  // collection having dynamic extent.  A debugger should be able to display
  // the collection.
  error(make(<subscript-out-of-bounds-error>,
	     format-string: "Number of subscripts not equal to "
	       "rank of array %=",
	     format-arguments: list(array)))
end function aref-rank-error;

define method general-row-major-index (array :: <array>, #rest subscripts :: <integer>)
    => (index :: <integer>)
  %dynamic-extent(subscripts);
  let sum :: <integer> = 0;
  for (dimension :: <integer> in array.dimensions,
       index :: <integer> in subscripts)
    unless (element-range-check(index, dimension))
      element-range-error(array, subscripts);
    end unless;
    sum := (sum * dimension) + index;
  end for;
  sum
end method general-row-major-index;

define inline method two-row-major-index (array :: <array>, #rest subscripts :: <integer>)
    => (index :: <integer>)
  %dynamic-extent(subscripts);
  let dimensions = dimensions(array);
  without-bounds-checks
    let dim-0 :: <integer> = dimensions[0];
    let idx-0 :: <integer> = subscripts[0];
    let dim-1 :: <integer> = dimensions[1];
    let idx-1 :: <integer> = subscripts[1];
    unless (element-range-check(idx-0, dim-0) & element-range-check(idx-1, dim-1))
      element-range-error(array, subscripts);
    end unless;
    idx-0 * dim-1 + idx-1
  end without-bounds-checks;
end method two-row-major-index;

define inline method row-major-index (array :: <array>, #rest subscripts :: <integer>)
    => (index :: <integer>)
  %dynamic-extent(subscripts);
  let n-subscripts = size(subscripts);
  unless (array.rank = n-subscripts)
    aref-rank-error(array, subscripts);
  end unless;
  if (n-subscripts = 2)
    apply(two-row-major-index, array, subscripts);
  else 
    apply(general-row-major-index, array, subscripts);
  end if;
end method row-major-index;

//
// AREF
// 

define inline method aref (array :: <array>, #rest indices :: <integer>) 
    => (object)
  without-bounds-checks
    array[apply(row-major-index, array, indices)]
  end without-bounds-checks;
end method aref;


//
// AREF-SETTER
// 

define inline method aref-setter 
    (new-value, array :: <array>, #rest indices :: <integer>)
        => (object)
  without-bounds-checks
    array[apply(row-major-index, array, indices)] := new-value
  end without-bounds-checks;
end method aref-setter;

 
//
// DIMENSION
// 

define inline method dimension (array :: <array>, axis :: <integer>)
    => (dimension :: <integer>)
  array.dimensions[axis]
end method dimension;



//
// Specialized inherited generic methods
//


//
// MAKE
//

define sealed method make 
    (class == <array>, #rest args, #key dimensions = unsupplied(),
                                        size: sz = unsupplied()) 
    => (result :: <array>)
  case
    supplied?(sz) =>
      if (supplied?(dimensions) & (size(dimensions) ~= 1 | dimensions[0] ~= sz))
	error("Dimensions %= incompatible to size %= in call to make(<array>)",
	      dimensions, sz);
      end if;
      apply(make, <simple-object-vector>, args);
    unsupplied?(dimensions) => // TODO: use proper error class
      error(make(<missing-keyword-error>, format-string: "No dimensions in call to make(<array>)"));
    dimensions.size = 1 =>
      apply(make, <simple-object-vector>, size: dimensions.first, args);
    otherwise =>
      apply(make, <multidimensional-array>, args);
  end case
end method make;

//
// SHALLOW-COPY
// 

define method shallow-copy (array :: <array>) => (array :: <array>)
  let size = size(array);
  if (size = 0)
    make(array.type-for-copy, dimensions: dimensions);
  else 
    let dimensions :: <sequence> = array.dimensions;
    let new-array :: <array> = 
      make(array.type-for-copy, dimensions: dimensions, fill: array[0]);

    for (key :: <integer> from 0 below size) 
      new-array[key] := array[key];
    end for;
    new-array
  end if;
end method shallow-copy;


//
// TYPE-FOR-COPY
// 

define method type-for-copy (array :: <array>) => (class :: <class>)
  <array>
end method type-for-copy;


//
// AS
// 

define method as (class == <array>, array :: <array>) => (array :: <array>)
  array
end method as;

define method as (class == <array>, collection :: <collection>)
    => (array :: <array>)
  as(<simple-object-vector>, collection)
end method as;


//
// FILL!
// 

define method fill!
    (target :: <array>, value, 
     #key start :: <integer> = 0, end: last = unsupplied())
        => (target :: <array>)
  let last :: <integer> = check-start-compute-end(target, start, last);
  for (index :: <integer> from start below last)
    target[index] := value
  end;
  target
end;


//
// SIZE
// 

define method size (x :: <array>) => (res :: <integer>)
  reduce(\*, 1, dimensions(x))
end method;


//
// MULTIDIMENSIONAL-ARRAY
//

define abstract class <multidimensional-array> (<array>)
end class;

define abstract class <simple-array> (<multidimensional-array>)
end class;

// eof
