Module:    WIN32-Automation
Synopsis:  Dylan accessors for OLE Automation arrays.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $unknown-element-type = <C-void*>;

// "Safe Array"

define C-subtype <ole-array> ( <array>, <LPSAFEARRAY> )
  slot element-pointer-type :: <class>, init-value: $unknown-element-type;
end;

define function import-safe-array ( ptr :: <LPSAFEARRAY> )
				  => ( value :: <ole-array> );
  let address = pointer-address(ptr);
  if ( zero?(address) | (ptr.cDims-value = 1) )
    make(<ole-vector>, address: address)
  else
    make(<ole-array>, address: address)
  end if
end import-safe-array;

define C-mapped-subtype <c-safe-array> ( <LPSAFEARRAY> )
  import-map <ole-array>, import-function: import-safe-array;
end;

define method make( class :: subclass(<ole-array>), #rest rest-args,
		   #key address = %no-value,
		   dimensions = %no-value, size: size-arg :: <integer> = 0,
		   fill = %no-value, vartype = $VT-VARIANT )
 => (array :: <ole-array>);

  if ( address ~== %no-value )
    next-method()
  else
    let element-ptr-type :: false-or(<class>) =
      if ( vartype = $VT-UI1 )
	// in this context, treat as a byte instead of a character.
	<C-unsigned-char*>
      else
	element($vt-c-pointer-classes, vartype, default: #f)
      end if;
    if ( element-ptr-type == #f | vartype > $VT-VARIANT-MAX )
      error("make %= invalid vartype = %d", class, vartype);
    end if;
    let ndims :: <integer> =
      if ( dimensions ~== %no-value ) size(dimensions) else 1 end if;
    let result :: <ole-array> =
      with-stack-structure ( bounds :: <LPSAFEARRAYBOUND>,
			     element-count: max(ndims,1) )
	bounds.cElements-value := size-arg;
	bounds.lLbound-value := 0;
        unless ( dimensions == %no-value ) 
	  // Note: the bounds array passed to SafeArrayCreate is in the
	  // reverse order from what ends up in the safe array structure.
	  for ( i :: <integer> from 0 below ndims )
	    let bound :: <LPSAFEARRAYBOUND>
	      = pointer-value-address(bounds, index: i);
	    let dim :: <integer> = dimensions[i];
	    bound.cElements-value := dim;
	    bound.lLbound-value := 0;
	  end for;
	end unless;
        let psa :: <LPSAFEARRAY> = SafeArrayCreate(vartype, ndims, bounds);
        if ( null-pointer?(psa) )
	  if ( ndims <= 0 )
	    error("0-dimension <ole-array> not supported");
	  else
	    apply(ole-error, $E-OUTOFMEMORY, "SafeArrayCreate",
		  $null-interface, rest-args);
	  end if;
	end if;
	import-safe-array(psa);
      end with-stack-structure;
    result.element-pointer-type := element-ptr-type;
    unless ( fill == %no-value )
      fill!(result, fill);
    end;
    result
  end if
end method make;

define method destroy ( a :: <ole-array>, #key ) => ()
  SafeArrayDestroy(a);
  values()
end method destroy;

define sealed method empty?( a :: <ole-array> ) => empty :: <boolean>;
  null-pointer?(a) | zero?(a.cDims-value)
    | zero?(a.rgsabound-value.cElements-value)
end;

define sealed method rank( a :: <ole-array> ) => (ndim :: <integer>);
  a.cDims-value
end;

define sealed method dimensions( a :: <ole-array> )
			=> dims :: <simple-object-vector>;
  let ndim :: <U16> = rank(a);
  let dims :: <simple-object-vector> =
    make(<simple-object-vector>, size: ndim);
  let ad = a.rgsabound-value;
  for ( i :: <U16> from 0 below ndim )
    let bound :: <LPSAFEARRAYBOUND> = pointer-value-address(ad, index: i);
    dims[ndim - 1 - i] := bound.cElements-value;
  end for;
  dims
end method dimensions;

// Note: we do not need to define `aref' and `aref-setter' methods on
// `<ole-array>' because the default methods will do the right thing
// by using our methods for `row-major-index' and `element'.

define sealed method row-major-index (array :: <ole-array>, #rest subscripts)
    => (index :: <integer>)
  // Compute the linear index for the array element, even though despite
  // the function name, safe arrays are actually stored in column major order.
  let ndim = array.rank;
  unless (ndim = subscripts.size)
   /* // can't do this yet because the class is not exported.
    error(make(<subscript-out-of-bounds-error>,
            format-string: "Number of subscripts %= not equal to "
                           "rank of array %=",
            format-arguments: list(subscripts, array)))
    */
    error("Wrong number of subscripts %= for %=", subscripts, array);
  end unless;
  let sum :: <integer> = 0;
  let ad = array.rgsabound-value; // dimensions in reverse order
  for ( i :: <integer> from ndim - 1 to 0 by -1,
        j :: <integer> from 0 by 1 )
    let bound :: <LPSAFEARRAYBOUND> = pointer-value-address(ad, index: j);
    let index :: <integer> = subscripts[i];
    let dimension :: <integer> = bound.cElements-value;
    if (index >= dimension | index < 0) 
      element-range-error(array, subscripts);
    end if;
    sum := (sum * dimension) + index;
  end for;
  sum
end method row-major-index;

define C-subtype <ole-vector> ( <vector>, <ole-array> )
end;

define sealed method size ( v :: <ole-vector> ) => size :: <integer>;
  v.rgsabound-value.cElements-value
end;

define constant $range-error-message = "ELEMENT outside of range: %=[%=]";

define function element-range-error( collection, index ) => ();
 /* // should do this for consistency with the built-in `element' methods,
    // but can't because the condition class is not exported.
  error(make(<invalid-index-error>,
	     format-string: $range-error-message,
	     format-arguments: vector(collection, index)));
 */
  error($range-error-message, collection, index);
end;

define sealed method element-pointer ( v :: <ole-array>, index :: <integer>,
			       default)
 => ptr :: <C-pointer>;

  // code shared by `element' and `element-setter'
  // returns a pointer to the designated element.

  if ( index < 0 | index >= size(v) )
    if ( default == %no-value )
      element-range-error(v, index);
    end if;
    $NULL-VOID
  else
    let pointer-class :: <class> = v.element-pointer-type;
    if ( pointer-class == $unknown-element-type )
      let kind = logand(v.fFeatures-value,
			logior($FADF-BSTR, $FADF-UNKNOWN,
			       $FADF-DISPATCH, $FADF-VARIANT));
      pointer-class :=
	select (kind)
	  $FADF-VARIANT => <LPVARIANT>;
	  $FADF-BSTR => <LPBSTR>;
	  $FADF-UNKNOWN => <LPLPUNKNOWN>;
	  $FADF-DISPATCH => <LPLPDISPATCH>;
	  otherwise => error("Unknown element type for %=", v);
	end select;
    end if;
    SafeArrayLock(v); // ensures validity of pvData-value
    make(pointer-class,
	 address: u%+(pointer-address(v.pvData-value),
		      index * v.cbElements-value))
  end if
end method element-pointer;

define method element ( v :: <ole-array>, index :: <integer>,
		       #key default = %no-value)
 => elem :: <object>;

  // Note: this does not use `SafeArrayGetElement' because we
  // don't want it copying strings or calling AddRef, since that is
  // not part of the expected protocol for `element'.

  let ptr :: <C-pointer> = element-pointer(v, index, default);
  if ( ptr == $NULL-VOID )
    default
  else 
    let result = pointer-value(ptr);
    SafeArrayUnlock(v);
    result
  end if
end method element;

define method element-setter ( new-value :: <object>, v :: <ole-array>,
			      index :: <integer> )
 => (element :: <object>);

  let ptr :: <C-pointer> = element-pointer(v, index, %no-value);
  pointer-value(ptr) := new-value;
  SafeArrayUnlock(v);
  new-value
end element-setter;


define method copy-safearray-element ( value :: <string> );
  // Even if it is already a <BSTR>, a new copy is needed because it will
  // be automatically deleted when the containing SAFEARRAY is deleted.
  copy-as-BSTR(value)
end;

define method copy-safearray-element ( value :: <LPUNKNOWN> );
  // Increment the ref count because it will be automatically decremented
  // when the containing SAFEARRAY is deleted.
  AddRef(value);
  value
end;

define method vt-of-elements ( elements :: <sequence> ) => (vt :: <integer>);
  let vt :: <integer> = $VT-VARIANT;
  unless ( empty?(elements) )
    let first-vt = vartype-of-value(first(elements));
    if ( first-vt ~= $VT-VARIANT )
      if ( every?(method(value) vartype-of-value(value) == first-vt end,
		  elements) )
	vt := first-vt;
      end if;
    end if;
  end unless;
  vt
end vt-of-elements;

define method vt-of-elements ( elements :: type-union(<simple-byte-vector>,
						      <simple-byte-array>) )
 => (vt :: <integer>);
  $VT-UI1
end;

define method vt-of-elements ( elements :: type-union(<simple-integer-vector>,
						      <simple-integer-array>))
 => (vt :: <integer>);
  $VT-I4
end;

define method vt-of-elements
    ( elements :: type-union(<simple-single-float-vector>,
			     <simple-single-float-array>))
 => (vt :: <integer>);
  $VT-R4
end;

define function ole-vector ( #rest elements ) => vector :: <ole-vector>;
  let vt :: <integer> = vt-of-elements(elements);
  let vector :: <ole-vector> = make(<ole-vector>, size: size(elements),
				    vartype: vt);
  if ( ~ zero?(logand(vector.fFeatures-value,
		      logior($FADF-BSTR, $FADF-UNKNOWN, $FADF-DISPATCH))) )
    map-into(vector, copy-safearray-element, elements);
  else
    // Either don't need to copy the elements, or else the copy will be done
    // by a `pointer-value-setter' method for <LPVARIANT>.
    for ( i from 0,
	  value in elements )
      vector[i] := value;
    end for;
  end if;
  vector
end ole-vector;

// type codes that could appear in a <VARIANT>:
define constant <variant-vt> = limited(<integer>, min: $VT-VARIANT-MIN,
				       max: $VT-VARIANT-MAX);

define generic vartype-of-value ( value ) => (vt :: <variant-vt>);
define method vartype-of-value ( value :: <integer> ) => (vt :: <variant-vt>)
  $VT-I4
end;
define method vartype-of-value ( value :: <machine-word> )
 => (vt :: <variant-vt>)
  $VT-I4
end;
define method vartype-of-value ( value :: <single-float> )
 => (vt :: <variant-vt>)
  $VT-R4
end;
define method vartype-of-value ( value :: <double-float> )
 => (vt :: <variant-vt>)
  $VT-R8
end;
define method vartype-of-value ( value :: <string> )
 => (vt :: <variant-vt>)
  $VT-BSTR
end;
define method vartype-of-value ( value :: <boolean> )
 => (vt :: <variant-vt>)
  $VT-BOOL
end;
define method vartype-of-value ( value :: <object> )
 => (vt :: <variant-vt>)
  $VT-VARIANT
end;
  
define method as( class == <ole-vector>, data :: <sequence> )
		=> v :: <ole-vector>;
  apply(ole-vector, data)
end;

define method as( class == <ole-vector>, data :: <ole-vector> )
		=> v :: <ole-vector>;
  data
end;

// used in "variant.dylan" but defined here so all the array stuff is together.
define function as-safe-array ( array :: <array> )
 => ( sa :: <ole-array>, vt :: <integer> );
  
  let vt :: <integer> = vt-of-elements(array);
  let dims :: <sequence> = dimensions(array);
  let sa :: <ole-array> = make(<ole-array>, dimensions: dims, vartype: vt);
  unless ( empty?(dims) )
    // Copy the elements.
    // Can't just do a sequential copy because storage order is different.
    // This currently works properly only for two-dimensional arrays.	???
    let pointer-class :: <class> = sa.element-pointer-type;
    SafeArrayLock(sa); // ensures validity of pvData-value
    let data-address = pointer-address(sa.pvData-value);
    let element-size :: <integer> = sa.cbElements-value;
    let total-size :: <integer> = size(array);
    let nrows = dims[0];
    let row :: <integer> = 0;
    let n :: <integer> = 0;
    let special? =
      ~ zero?(logand(sa.fFeatures-value,
		     logior($FADF-BSTR, $FADF-UNKNOWN, $FADF-DISPATCH)));
    for ( element in array )
      let ptr = make(pointer-class,
		     address: u%+(data-address, n * element-size));
      pointer-value(ptr) := 
	if ( special? )
	  copy-safearray-element(element)
	else
	  element
	end if;
      n := n + nrows;
      if ( n >= total-size )
	row := row + 1;
	n := row;
      end if;
    end for;
    SafeArrayUnlock(sa);
  end unless;
  values( sa, vt )
end as-safe-array;

define method copy-automation-value ( sa :: <ole-array> );
  if ( null-pointer?(sa) )
    // Shouldn't really happen, but just to be safe.
    sa
  else
    let dims = dimensions(sa);
    let result :: <array> = make(<array>, dimensions: dims);
    unless ( empty?(dims) )
      // Copy the elements.
      // Can't just do a sequential copy because storage order is different.
      // This currently works properly only for two-dimensional arrays.	???
      let pointer-class :: <class> = sa.element-pointer-type;
      SafeArrayLock(sa); // ensures validity of pvData-value
      let data-address = pointer-address(sa.pvData-value);
      let element-size :: <integer> = sa.cbElements-value;
      let total-size :: <integer> = size(result);
      let nrows = dims[0];
      let row :: <integer> = 0;
      let n :: <integer> = 0;
      for ( i from 0 below total-size )
	let ptr = make(pointer-class,
		       address: u%+(data-address, n * element-size));
	let element = pointer-value(ptr);
	result[i] := copy-automation-value(element);
	n := n + nrows;
	if ( n >= total-size )
	  row := row + 1;
	  n := row;
	end if;
      end for;
      SafeArrayUnlock(sa);
    end unless;
    result
  end if;
end;
