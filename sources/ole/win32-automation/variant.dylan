Module:    Win32-Automation
Synopsis:  Dylan accessors for data structures used with OLE Automation.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Dylan support for VARIANT and VARIANTARG objects
//
// An instance of class <LPVARIANT> is a pointer to a VARIANT object.
// Use  empty?(p)  to test whether it is empty (i.e. VT_EMPTY).
// If not empty, then  pointer-value(p)  will return a Dylan object
// corresponding to the value represented by the VARIANT structure.
//
// Similarly, use either
//	make(<LPVARIANT>, value: object)
// or
//	as(<LPVARIANT>, object)
// to construct a VARIANT that represents the value of the given 
// Dylan object.
//
// Direct access to the structure fields should not be necessary.


define constant <CY> = <LARGE-INTEGER>;
define constant <CY*> = <PLARGE-INTEGER>;
define constant <DATE> = <C-double>;
define constant <DATE*> = <C-double*>;
// alias for consistent naming for users
define constant <BSTR*> = <LPBSTR>;

define C-pointer-type <PLARGE-INTEGER*> => <PLARGE-INTEGER>;
define C-pointer-type <PULARGE-INTEGER*> => <PULARGE-INTEGER>;


define C-mapped-subtype <VARIANT-BOOL> ( <C-signed-short> )
  map <boolean>,
    import-function: method (value :: <integer>) => (value :: <boolean>);
		       ~ zero?(value)
		     end,
    export-function: method (value :: <boolean>) => (value :: <integer>);
		       if ( value ) $VARIANT-TRUE else $VARIANT-FALSE end if
		     end;
  pointer-type <VARIANT-BOOL*>;
end;



define constant $SQL-NULL = #"NULL"; // to represent $VT-NULL value

define C-union <variant-union>
  sealed inline-only slot lVal-value  :: <C-both-long>;
  sealed inline-only slot bVal-value  :: <C-unsigned-char>;
  sealed inline-only slot iVal-value  :: <C-short>;
  sealed inline-only slot fltVal-value :: <C-float>;
  sealed inline-only slot dblVal-value :: <C-double>;
  sealed inline-only slot bool-value  :: <VARIANT-BOOL>;
  sealed inline-only slot scode-value :: <C-HRESULT>;
  sealed inline-only slot cyVal-value :: <CY>;
  sealed inline-only slot date-value  :: <DATE>;

  sealed inline-only slot ptr-value :: <C-pointer>;

/*
  sealed inline-only slot bstrVal-value :: <BSTR>;
  sealed inline-only slot punkVal-value :: <Interface> /* IUnknown */;
  sealed inline-only slot pdispVal-value :: <Interface> /* IDispatch */;
*/
  sealed inline-only slot parray-value :: <c-safe-array>;
/*
  sealed inline-only slot pbVal-value :: <C-unsigned-char*>;
  sealed inline-only slot piVal-value :: <C-short*>;
  sealed inline-only slot plVal-value :: <C-long*>;
  sealed inline-only slot pfltVal-value :: <C-float*>;
  sealed inline-only slot pdblVal-value :: <C-double*>;
  sealed inline-only slot pbool-value :: <VARIANT-BOOL*>;
  sealed inline-only slot pscode-value :: <C-HRESULT*>;
  sealed inline-only slot pcyVal-value :: <CY*>;
  sealed inline-only slot pdate-value :: <DATE*>;
  sealed inline-only slot pbstrVal-value :: <BSTR*>;
  sealed inline-only slot ppunkVal-value :: <Interface*> /* IUnknown */;
  sealed inline-only slot ppdispVal-value :: <Interface*> /* IDispatch */;
  sealed inline-only slot pparray-value :: <c-safe-array*>;
  sealed inline-only slot pvarVal-value :: <VARIANT*>;
  sealed inline-only slot byref-value :: <C-void*>;
*/
end;

define C-struct <VARIANT>
  sealed inline-only slot vt-value    :: <VARTYPE>;
  sealed inline-only slot wReserved1  :: <WORD>, getter: #f, setter: #f;
  sealed inline-only slot wReserved2  :: <WORD>, getter: #f, setter: #f;
  sealed inline-only slot wReserved3  :: <WORD>, getter: #f, setter: #f;
  sealed inline-only slot u-value     :: <variant-union>;
  pointer-type-name: <LPVARIANT>;
end C-struct <VARIANT>;

define constant <VARIANTARG> = <VARIANT>;
define constant <LPVARIANTARG> = <LPVARIANT>;

define constant $NULL-VARIANT :: <LPVARIANT> = null-pointer(<LPVARIANT>);

//define constant <U8>  = limited(<integer>, min: 0, max: #xFF);
define constant <U16> = limited(<integer>, min: 0, max: #xFFFF);
define constant <S16> = limited(<integer>, min: -#x8000, max: #x7FFF);

// Maps an element of the VARENUM enumeration type to the corresponding
// C type descriptor.
define constant $vt-c-classes :: <simple-object-vector> = vector(

  // These are used in either a VARIANT, TYPEDESC, or SAFEARRAY:
    /* $VT-EMPTY =  0 */ #f,
    /* $VT-NULL  =  1 */ #f,
    /* $VT-I2    =  2 */ <C-short>,
    /* $VT-I4    =  3 */ <C-long>,
    /* $VT-R4    =  4 */ <C-float>,
    /* $VT-R8    =  5 */ <C-double>,
    /* $VT-CY    =  6 */ <CY>,
    /* $VT-DATE  =  7 */ <DATE>,
    /* $VT-BSTR  =  8 */ <BSTR>,
    /* $VT-DISPATCH =  9 */ <LPDISPATCH>,
    /* $VT-ERROR = 10 */ <C-HRESULT>,
    /* $VT-BOOL  = 11 */ <VARIANT-BOOL>,
    /* $VT-VARIANT = 12 */ <VARIANT>,
    /* $VT-UNKNOWN = 13 */ <LPUNKNOWN>,
    /* $VT-DECIMAL = 14 */ #f,
    /*           = 15 */ #f,
    /* $VT-I1    = 16 */ <C-signed-char>,
    /* $VT-UI1   = 17 */ <C-character>, // maps unsigned char to <character>

  // These are used only in a TYPEDESC or property set:
    /* $VT-UI2   = 18 */ <C-unsigned-short>,
    /* $VT-UI4   = 19 */ <C-unsigned-long>,
    /* $VT-I8    = 20 */ <PLARGE-INTEGER>,  // or <C-signed-long-long> ???
    /* $VT-UI8   = 21 */ <PULARGE-INTEGER>, // or <C-unsigned-long-long> ???
    /* $VT-INT   = 22 */ <C-signed-int>,
    /* $VT-UINT  = 23 */ <C-unsigned-int>,
    /* $VT-VOID  = 24 */ <C-void>,
    /* $VT-HRESULT = 25 */ <C-HRESULT>,
    /* $VT-PTR   = 26 */ #f, // pointer to type in lptdesc; not useful here.
    /* $VT-SAFEARRAY = 27 */ #f, // <LPSAFEARRAY>, // array of type in lptdesc
    /* $VT-CARRAY = 28 */ #f /* ??? */, // C array of type in lpadesc field
    /* $VT-USERDEFINED = 29 */ #f,	//user defined; see hreftype field
    /* $VT-LPSTR = 30 */ <LPSTR>,	// null terminated string
    /* $VT-LPWSTR = 31 */ <LPWSTR>	// wide null terminated string

  // These are used only in an OLE property set:
    /* $VT-RECORD	   = 36, */
    /* $VT-FILETIME	   = 64, */
    /* $VT-BLOB 	   = 65, */
    /* $VT-STREAM	   = 66, */
    /* $VT-STORAGE	   = 67, */
    /* $VT-STREAMED-OBJECT = 68, */
    /* $VT-STORED-OBJECT   = 69, */
    /* $VT-BLOB-OBJECT	   = 70, */
    /* $VT-CF		   = 71, */
    /* $VT-CLSID	   = 72, */
  );

debug-assert($vt-c-classes[$VT-LPWSTR] = <LPWSTR>);

define constant $VT-VARIANT-MIN = $VT-I2;  // index of first non-empty entry
define constant $VT-VARIANT-MAX = $VT-UI1; // largest index for a VARIANT
define constant $VT-TYPEDESC-MAX = $VT-LPWSTR; // largest index for a TYPEDESC

// referenced-type($vt-c-pointer-classes[i]) = $vt-c-classes[i]
define constant $vt-c-pointer-classes :: <simple-object-vector> = vector(

  // These are used in either a VARIANT, TYPEDESC, or SAFEARRAY:
    /* $VT-EMPTY =  0 */ #f,
    /* $VT-NULL  =  1 */ #f,
    /* $VT-I2    =  2 */ <C-short*>,
    /* $VT-I4    =  3 */ <C-long*>,
    /* $VT-R4    =  4 */ <C-float*>,
    /* $VT-R8    =  5 */ <C-double*>,
    /* $VT-CY    =  6 */ <CY*>,
    /* $VT-DATE  =  7 */ <DATE*>,
    /* $VT-BSTR  =  8 */ <LPBSTR>,
    /* $VT-DISPATCH =  9 */ <LPLPDISPATCH>,
    /* $VT-ERROR = 10 */ <C-HRESULT*>,
    /* $VT-BOOL  = 11 */ <VARIANT-BOOL*>,
    /* $VT-VARIANT = 12 */ <LPVARIANT>,
    /* $VT-UNKNOWN = 13 */ <LPLPUNKNOWN>,
    /*           = 14 */ #f,
    /*           = 15 */ #f,
    /* $VT-I1    = 16 */ <C-signed-char*>,
    /* $VT-UI1   = 17 */ <C-character*>, // maps unsigned char to <character>

  // These are used only in a TYPEDESC:
    /* $VT-UI2   = 18 */ <C-unsigned-short*>,
    /* $VT-UI4   = 19 */ <C-unsigned-long*>,
    /* $VT-I8    = 20 */ <PLARGE-INTEGER*>,  // or <C-signed-long-long> ???
    /* $VT-UI8   = 21 */ <PULARGE-INTEGER*>, // or <C-unsigned-long-long> ???
    /* $VT-INT   = 22 */ <C-signed-int*>,
    /* $VT-UINT  = 23 */ <C-unsigned-int*>,
    /* $VT-VOID  = 24 */ <C-void*>,
    /* $VT-HRESULT = 25 */ <C-HRESULT*>,
    /* $VT-PTR   = 26 */ <C-void**>, // pointer to type in lptdesc field.
    /* $VT-SAFEARRAY = 27 */ <SAFEARRAY**>, // array of type in lptdesc field.
    /* $VT-CARRAY = 28 */ #f /* ??? */, // C array of type in lpadesc field
    /* $VT-USERDEFINED = 29 */ #f,	//user defined; see hreftype field
    /* $VT-LPSTR = 30 */ <C-string*>,	// null terminated string
    /* $VT-LPWSTR = 31 */ <C-unicode-string*> // wide null terminated string
  );

define constant $vt-Dylan-classes :: <simple-object-vector> = vector(

  // These are used in either a VARIANT, TYPEDESC, or SAFEARRAY:
    /* $VT-EMPTY =  0 */ #f,
    /* $VT-NULL  =  1 */ #f,
    /* $VT-I2    =  2 */ <S16>,
    /* $VT-I4    =  3 */ <integer>,
    /* $VT-R4    =  4 */ <single-float>,
    /* $VT-R8    =  5 */ <double-float>,
    /* $VT-CY    =  6 */ <double-integer>,
    /* $VT-DATE  =  7 */ <double-float>,
    /* $VT-BSTR  =  8 */ <BSTR>,
    /* $VT-DISPATCH =  9 */ <LPDISPATCH>,
    /* $VT-ERROR = 10 */ <HRESULT>,
    /* $VT-BOOL  = 11 */ <boolean>,
    /* $VT-VARIANT = 12 */ <LPVARIANT>,
    /* $VT-UNKNOWN = 13 */ <LPUNKNOWN>,
    /*           = 14 */ #f,
    /*           = 15 */ #f,
    /* $VT-I1    = 16 */ <integer>,
    /* $VT-UI1   = 17 */ <character>,

  // These are used only in a TYPEDESC:
    /* $VT-UI2   = 18 */ <U16>,
    /* $VT-UI4   = 19 */ <machine-word>,
    /* $VT-I8    = 20 */ <double-integer>,  // ???
    /* $VT-UI8   = 21 */ <PULARGE-INTEGER>, // ???
    /* $VT-INT   = 22 */ <integer>,
    /* $VT-UINT  = 23 */ <integer>,
    /* $VT-VOID  = 24 */ #f,
    /* $VT-HRESULT = 25 */ <HRESULT>,
    /* $VT-PTR   = 26 */ <C-pointer>, // pointer to type in lptdesc field.
    /* $VT-SAFEARRAY = 27 */ <ole-array>, // array of type in lptdesc field.
    /* $VT-CARRAY = 28 */ #f /* ??? */, // C array of type in lpadesc field
    /* $VT-USERDEFINED = 29 */ #f,//user defined; see hreftype field
    /* $VT-LPSTR = 30 */ <LPSTR>,	// null terminated string
    /* $VT-LPWSTR = 31 */ <LPWSTR>	// wide null terminated string
  );

debug-assert($vt-Dylan-classes[$VT-LPWSTR] = <LPWSTR>);

define sealed method indexed-variant( var :: <LPVARIANT>, index :: <integer>)
				=> var :: <LPVARIANT>;
  // Given pointer to array, return pointer to indexed element.
  if ( zero?(index) )
    var 
  else
    pointer-value-address(var, index: index)
  end if
end method indexed-variant;

define sealed method pointer-value( varptr :: <LPVARIANT>, #key index = 0 )
		=> value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  let type :: <U16> = var.vt-value;
  if ( type = $VT-NULL )
    $SQL-NULL
  else
    let ptrclass = $vt-c-pointer-classes[ logand(type, #xFF) ];
    if ( ptrclass == #f )
      error("unsupported VARIANT type %d", type);
    end if;
    if ( logand(type, $VT-ARRAY) ~= 0 )
      let array :: <ole-array> = var.u-value.parray-value;
      array.element-pointer-type := ptrclass;
      array
    else
      if ( logand(type, $VT-BYREF) ~= 0 )
	pointer-cast(ptrclass, var.u-value.ptr-value);
      else
	pointer-value( pointer-cast(ptrclass, var.u-value) );
      end if
    end if
  end if
end method pointer-value;


define sealed method pointer-value-setter( value :: <integer>,
		varptr :: <LPVARIANT>, #key index = 0 ) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  // Always store as 32 bits so that the other program will get the right
  // value even if it assumes VT_I4 without checking.
  var.u-value.lVal-value := value;
  if ( value <= #x7FFF & value >= -#x7FFF )
    var.vt-value := $VT-I2;
    debug-assert(var.u-value.iVal-value = value);
  else
    var.vt-value := $VT-I4;
  end if;
  value
end;

define sealed method pointer-value-setter( value :: <machine-word>,
		varptr :: <LPVARIANT>, #key index = 0) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  var.vt-value := $VT-I4;
  var.u-value.lVal-value := value;
  value
end;

define sealed method pointer-value-setter( value :: <character>,
		varptr :: <LPVARIANT>, #key index = 0 ) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  var.vt-value := $VT-UI1;
  var.u-value.lVal-value := 0;
  let ivalue :: <integer> = as(<integer>, value);
  var.u-value.bVal-value := ivalue;
  value
end;

define sealed method pointer-value-setter( value :: <boolean>,
		varptr :: <LPVARIANT>, #key index = 0 ) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  var.vt-value := $VT-BOOL;
  var.u-value.bool-value := value;
  value
end;

define constant set-variant-pointer =
    method(varptr :: <LPVARIANT>,
	   index :: <integer>,
	   variant-type :: <U16>,
	   pointer :: <C-pointer>) => pointer :: <C-pointer>;

	let var :: <LPVARIANT> = indexed-variant(varptr,index);
	VariantClear(var); // release any previous value
	var.vt-value := variant-type;
	var.u-value.ptr-value := pointer
    end;

define sealed method pointer-value-setter( value :: <LPUNKNOWN>,
		varptr :: <LPVARIANT>, #key index = 0 ) => value :: <object>;
  AddRef(value);
  set-variant-pointer(varptr, index, $VT-UNKNOWN, value)
end;

// ??? later make this <LPDISPATCH> when that is distinct from <LPUNKNOWN>
define sealed method pointer-value-setter( value :: <IDispatch>,
		varptr :: <LPVARIANT>, #key index = 0 ) => value :: <object>;
  AddRef(value);
  set-variant-pointer(varptr, index, $VT-DISPATCH, value)
end;

define sealed method pointer-value-setter( string :: <string>,
		varptr :: <LPVARIANT>, #key index = 0 ) => string :: <string>;
  // Need to always copy the value because it will be 
  // automatically deleted by VariantClear.
  set-variant-pointer(varptr, index, $VT-BSTR, copy-as-BSTR(string));
  string
end;

// The following method should be redundant, but it is needed temporarily
// to work around Bug 1081.	-- DNG 8/22/97		???
define sealed method pointer-value-setter( string :: <BSTR>,
		varptr :: <LPVARIANT>, #key index = 0 ) => string :: <BSTR>;
  // Need to always copy the value because it will be 
  // automatically deleted by VariantClear.
  set-variant-pointer(varptr, index, $VT-BSTR, copy-as-BSTR(string));
  string
end;

define sealed method pointer-value-setter( sequence :: <sequence>,
				   varptr :: <LPVARIANT>, #key index = 0 )
 => sequence :: <sequence>;
  let sa :: <ole-vector> = apply(ole-vector, sequence);
  let ptrtype = sa.element-pointer-type;
  let vt = position($vt-c-pointer-classes, ptrtype);
  set-variant-pointer(varptr, index, logior($VT-ARRAY, vt), sa);
  sequence
end;

define sealed method pointer-value-setter( array :: <array>,
				   varptr :: <LPVARIANT>, #key index = 0 )
 => array :: <array>;
  if ( rank(array) = 1 )
    next-method() // use the <sequence> method
  else
    let (sa, vt) = as-safe-array(array);
    set-variant-pointer(varptr, index, logior($VT-ARRAY, vt), sa);
    array
  end if
end;

define sealed method pointer-value-setter( array :: <ole-array>,
				    varptr :: <LPVARIANT>, #key index = 0 )
 => array :: <ole-array>;
  let ( status :: <HRESULT>, sa :: <LPSAFEARRAY> ) = SafeArrayCopy(array);
  check-ole-status(status, "SafeArrayCopy", array);
  let ptrtype = array.element-pointer-type;
  let vt = position($vt-c-pointer-classes, ptrtype);
  set-variant-pointer(varptr, index, logior($VT-ARRAY, vt), sa);
  array
end;

define sealed method pointer-value-setter( value == $SQL-NULL,
		varptr :: <LPVARIANT>, #key index = 0 ) => value :: <object>;
  set-variant-pointer(varptr, index, $VT-NULL, $NULL-VOID);
  value
end;

define sealed method pointer-value-setter( value :: <single-float>,
		varptr :: <LPVARIANT>, #key index = 0) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  var.vt-value := $VT-R4;
  var.u-value.fltVal-value := value;
  value
end;

define sealed method pointer-value-setter( value :: <double-float>,
		varptr :: <LPVARIANT>, #key index = 0) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  var.vt-value := $VT-R8;
  var.u-value.dblVal-value := value;
  value
end;

define sealed method pointer-value-setter( value :: <double-integer>,
		varptr :: <LPVARIANT>, #key index = 0) => value :: <object>;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  VariantClear(var); // release any previous value
  var.vt-value := $VT-CY;
  pointer-value(var.u-value.cyVal-value) := value;
  value
end;

define sealed method pointer-value-setter (value :: <C-pointer>,
				    varptr :: <LPVARIANT>, #key index = 0 )
 => value :: <C-pointer>;
  block (return)
    for ( vt :: <integer> from $VT-VARIANT-MIN to $VT-VARIANT-MAX )
      let typei = $vt-c-pointer-classes[vt];
      if ( typei & instance?(value, typei) )
	set-variant-pointer(varptr, index, logior($VT-BYREF, vt), value);
	return(value);
      end if;
    end for;
    next-method();
  end block;
end;

define sealed method pointer-value-setter (value :: <object>,
				    varptr :: <LPVARIANT>, #key index = 0)
 => value :: <object>;
  error("Automation VARIANTARG can't represent the value %=", value);
  VariantClear(indexed-variant(varptr,index));
  value
end;


define constant %no-value = #(0 . 0);

define sealed method initialize ( var :: <LPVARIANT>,
			  #key value :: <object> = %no-value,
			  address = #f, #all-keys )
  next-method();
  unless ( address )
    // When memory space has been newly allocated, initialize it.
    // Note: properly, if `element-count:' is specified as greater than 1,
    //   then this should be applied to each element.  But that is currently
    //   only done in `make-dispparams' and it is easier to do the
    //   initialization there. 
    VariantInit(var);
  end unless;
  unless ( value == %no-value )
    pointer-value(var) := value;
  end unless;
  values()
end method initialize;

define sealed method destroy ( var :: <LPVARIANT>, #key ) => ();
  unless ( null-pointer?(var) )
    VariantClear(var);
    // Note that `destroy-dispparams' calls `VariantClear' on the additional
    // elements if more than one.
    next-method();
  end unless;
  values()
end method destroy;

define method as( class == <LPVARIANT>, data :: <object> ) => v :: <LPVARIANT>;
  let var :: <LPVARIANT> = make(class);
  pointer-value(var) := data;
  var
end method as;

define sealed method empty? ( var :: <LPVARIANT> ) => empty :: <boolean>;
  var.vt-value = $VT-EMPTY
end method empty?;

define sealed method null? ( var :: <LPVARIANT> ) => null :: <boolean>;
  null-pointer?(var) | ( var.vt-value = $VT-NULL )
end method null?;



define abstract class <ole-arg-spec> (<object>)
  sealed slot arg-spec-vt :: <U16>, init-keyword: vt:;
  keyword type:;
end class;

define sealed method make ( class == <ole-arg-spec>, #rest args,
		    #key direction = #"in", #all-keys )
 => ( arg-spec :: <ole-arg-spec> );
  apply(make,
	if ( direction == #"in" ) <ole-value-arg-spec>
	else <ole-by-ref-arg-spec>
	end if,
	args)
end method make;

define class <ole-value-arg-spec> (<ole-arg-spec>)
  slot arg-spec-value, init-keyword: value:;
end class;

/* // not needed
define method arg-spec-direction ( argspec :: <ole-value-arg-spec> )
  #"in"
end;
*/

define class <ole-by-ref-arg-spec> (<ole-arg-spec>)
  constant slot arg-spec-direction = #"in", init-keyword: direction:;
  slot arg-spec-ptr :: <C-pointer> = $null-void;
  keyword value:;
end class;

define method initialize ( arg-spec :: <ole-arg-spec>,
			  #rest ignore,
			  #key vt = %no-value, value = %no-value,
			  type = %no-value) => ();
  next-method();
  if ( vt == %no-value )
    let xtype =
      if ( type ~== %no-value )
	type
      elseif ( value ~== %no-value )
	object-class(value)
      else
	error("Must specify one of vt:, type:, or value: for <ole-arg-spec>");
      end if;
    arg-spec.arg-spec-vt := vt-from-type(xtype);
  end if;
end method initialize;

define sealed method initialize ( arg-spec :: <ole-by-ref-arg-spec>,
			  #rest ignore,
			  #key value = %no-value, #all-keys) => ();
  next-method();
  if ( value ~== %no-value )
    arg-spec-value(arg-spec) := value;
  end if;
end method initialize;

define method arg-spec-value ( arg-spec :: <ole-by-ref-arg-spec> )
 => ( value :: <object> );
  pointer-value(arg-spec.arg-spec-ptr)
end;

define method arg-spec-value-setter ( value :: <object>,
				      arg-spec :: <ole-by-ref-arg-spec> )
 => ( value :: <object> );
  let ptr = arg-spec.arg-spec-ptr;
  if ( null-pointer?(ptr) )
    drain-finalization-queue();
    ptr := make($vt-c-pointer-classes[arg-spec.arg-spec-vt]);
    arg-spec.arg-spec-ptr := ptr;
    finalize-when-unreachable(arg-spec);
  end if;
  pointer-value(ptr) := value
end;

define method finalize ( arg-spec :: <ole-by-ref-arg-spec> ) => ();
  destroy(arg-spec.arg-spec-ptr);
  arg-spec.arg-spec-ptr := $null-void;
  next-method();
end;

define generic vt-from-type ( type ) => (vt :: <U16>);

define method vt-from-type( type :: <object> ) => (vt :: <U16>);
  error("Invalid type for Automation argument: %=", type);
  $VT-EMPTY
end;

define method vt-from-type( type :: <type> ) => (vt :: <U16>);
  block(return)
    for ( i :: <U16> from $VT-VARIANT-MIN to $VT-TYPEDESC-MAX )
      let typei = $vt-Dylan-classes[i];
      if ( typei & subtype?(type, typei) )
	return(i);
      end if;
    end for;
    next-method()
  end block
end;

define method vt-from-type( class :: subclass(<C-value>) ) => (vt :: <U16>);
  block(return)
    for ( i :: <U16> from $VT-VARIANT-MIN to $VT-TYPEDESC-MAX )
      let typei = $vt-c-classes[i];
      if ( typei & subtype?(class, typei) )
	return(i);
      end if;
    end for;
    next-method()
  end block
end;

define method vt-from-type( class == <BOOL> ) => (vt :: <U16>);
  $VT-BOOL
end;

define method vt-from-type( class == <machine-word> ) => (vt :: <U16>);
  $VT-I4
end;

define method vt-from-type( class == <C-int> ) => (vt :: <U16>);
  // Allow using <C-int> instead of <C-long>, which is functionally
  // equivalent but not the same object.  (In a TYPEDESC, <C-int> would
  // correspond to $VT-INT, but that isn't allowed in a VARIANT.)
  $VT-I4
end;

define method vt-from-type
    ( class :: type-union(singleton(<C-both-unsigned-long>),
			  singleton(<C-raw-unsigned-long>)) )
 => (vt :: <U16>);
  $VT-UI4
end;

define method vt-from-type( class == <C-both-signed-long> ) => (vt :: <U16>);
  $VT-I4
end;

define method vt-from-type( class :: subclass(<IDispatch>) ) => (vt :: <U16>);
  $VT-DISPATCH
end;

define method vt-from-type( class :: subclass(<LPUNKNOWN>) ) => (vt :: <U16>);
  $VT-UNKNOWN
end;

define method vt-from-type( class :: subclass(<string>) ) => (vt :: <U16>);
  $VT-BSTR
end;

define method vt-from-type( class :: subclass(<sequence>) ) => (vt :: <U16>);
  logior($VT-ARRAY, $VT-VARIANT)
end;

define method vt-from-type( typedescr :: <array-type-description> )
 => (vt :: <U16>);
  logior($VT-ARRAY, vt-from-type(element-type(typedescr)))
end;

define method vt-from-type( td :: <LPTYPEDESC> ) => (vt :: <U16>);
  td.vt-value
end;

define method pointer-value-setter( arg-spec :: <ole-value-arg-spec>,
		varptr :: <LPVARIANT>, #key index = 0) => value :: <object>;
  let vt = arg-spec.arg-spec-vt;
  let var :: <LPVARIANT> = indexed-variant(varptr,index);
  let value = arg-spec.arg-spec-value;
  let varu = var.u-value;
  let var-vt = vt-for-variant(vt);
  select (var-vt)
    $VT-I2    =>  let value :: <integer> = value;
                  varu.iVal-value := value;
    $VT-I4, $VT-ERROR => let value :: <ffi-integer> = value;
                         varu.lVal-value := value;
    $VT-DISPATCH, $VT-UNKNOWN => let value :: <C-pointer> = value;
                                  varu.ptr-value := value;
    $VT-I1, $VT-UI1 => let value :: <integer> = logand(#xFF, as(<integer>, value));
                       varu.bVal-value := value;
    $VT-CY => pointer-value(varu.cyVal-value) := value;
    $VT-EMPTY, $VT-NULL => varu.ptr-value := $null-void;
    otherwise =>
      pointer-value(var) := as($vt-Dylan-classes[vt], value);
  end select;
  var.vt-value := var-vt;
  value
end method;

define constant $continue-message = "Continue anyway";

define function vt-for-variant ( vt :: <integer> ) => (vt :: <U16>);
  if ( vt >= 0 & vt <= $VT-VARIANT-MAX )
    vt
  else
    // map TYPEDESC codes to legal values for a VARIANT
    select(vt)
      $VT-UI2 => $VT-I2;
      $VT-UI4, $VT-INT, $VT-UINT => $VT-I4;
      $VT-HRESULT => $VT-ERROR;
      otherwise => 
	begin
	  cerror($continue-message, "Invalid VARTYPE for VARIANTARG: %=", vt);
	  vt
	end
    end select;
  end if
end vt-for-variant;

define method pointer-value-setter( arg-spec :: <ole-by-ref-arg-spec>,
		varptr :: <LPVARIANT>, #key index = 0) => value :: <object>;
  let vt = arg-spec.arg-spec-vt;
  let ptr = arg-spec.arg-spec-ptr;
  if ( null-pointer?(ptr) )
    if ( arg-spec.arg-spec-direction ~= #"out" )
      error("%s parameter with no value",
	    as(<string>, arg-spec.arg-spec-direction));
    end if;
    ptr := make($vt-c-pointer-classes[vt]);
    arg-spec.arg-spec-ptr := ptr;
  end if;
  set-variant-pointer(varptr, index,
		      logior($VT-BYREF, vt-for-variant(vt)), ptr);
  arg-spec
end;

define generic set-typedesc ( object, typedesc )
 => (direction-flags :: <U16>);

define method set-typedesc( argtype :: <ole-value-arg-spec>,
			    t :: <LPTYPEDESC> )
 => direction-flags :: <U16>;
  t.vt-value := argtype.arg-spec-vt;
  $IDLFLAG-FIN;
end method set-typedesc;

define method set-typedesc( argtype :: <ole-by-ref-arg-spec>,
			    t :: <LPTYPEDESC> )
 => direction-flags :: <U16>;
  t.vt-value := $VT-PTR;
  let type :: <type> = $vt-c-classes[argtype.arg-spec-vt];
  t.u-value.lptdesc-value := as-typedesc(type);
  select ( argtype.arg-spec-direction )
    #"in" => $IDLFLAG-FIN;
    #"out" => $IDLFLAG-FOUT;
    #"in-out" => logior($IDLFLAG-FIN, $IDLFLAG-FOUT);
  end select
end method set-typedesc;

define method out-ref (type :: <ole-type>) => (r :: <ole-by-ref-arg-spec>)
  make(<ole-by-ref-arg-spec>, type: type, direction: #"out")
end;
 
define method out-ref (vt :: <integer>) => (r :: <ole-by-ref-arg-spec>)
  make(<ole-by-ref-arg-spec>, vt: vt, direction: #"out")
end;
 
define function inout-ref (value, #rest args) => (r :: <ole-by-ref-arg-spec>)
 apply(make, <ole-by-ref-arg-spec>, value: value, direction: #"in-out", args)
end; 

define method pass-as (vt :: <integer>, value :: <object>) 
 => (arg-spec :: <ole-value-arg-spec>);
  make(<ole-value-arg-spec>, vt: vt, value: value)
end method;

define method pass-as (type :: <ole-type>, value :: <object>) 
 => (arg-spec :: <ole-value-arg-spec>);
  make(<ole-value-arg-spec>, type: type, value: value)
end method;



// <LPPROPVARIANT> is defined in the COM library for use with the
// IPropertyStorage interface, but it is equivalent to an <LPVARIANT>,
// at least as far as our implementation goes. 
define inline-only sealed sideways method u-value (var :: <LPPROPVARIANT>)
 => (union-pointer)
  let ptr :: <LPVARIANT> = pointer-cast(<LPVARIANT>, var);
  u-value(ptr);
end;

define inline sealed sideways method pointer-value (var :: <LPPROPVARIANT>,
						    #rest args, #key index)
 => (vt :: <integer>);
  let ptr :: <LPVARIANT> = pointer-cast(<LPVARIANT>, var);
  apply(pointer-value, ptr, args)
end;

define inline sealed sideways method pointer-value-setter
    (value :: <object>, var :: <LPPROPVARIANT>, #rest args, #key index)
  => (value :: <object>);
  let ptr :: <LPVARIANT> = pointer-cast(<LPVARIANT>, var);
  apply(pointer-value-setter, value, ptr, args)
end;

define inline sideways method as (class == <LPPROPVARIANT>, data :: <object>)
 => v :: <LPPROPVARIANT>;
  pointer-cast(<LPPROPVARIANT>, as(<LPVARIANT>, data))
end method as;

//--

// For compatibility with old structure
define sealed inline-only method idldesc-value ( ed :: <LPELEMDESC> )
 => ( id :: <LPIDLDESC> )
  // This should be just:
  //    ed.u-value.idldesc-value
  // but that is getting an infinite recursion in the compiler (Bug 2778),
  // so using the following work-around instead.  -- DNG 4/8/98
  pointer-cast(<LPIDLDESC>, ed.u-value)
end;


//  ---  TYPEDESC  ---

// Are the following three methods really needed???
// They are exported, but not used here and not documented.
define method typedesc-C-class ( t :: <LPTYPEDESC> )
			=> class :: <class>;
  let vt :: <U16> = t.vt-value;
  select ( vt )
    $VT-PTR  => typedesc-C-pointer-class(t.u-value.lptdesc-value);
//  $VT-SAFEARRAY => // element type in lptdesc-value
//  $VT-CARRAY => // element type and dimensions in lpadesc-value
//  $VT-USERDEFINED => // description handle in hreftype-value
    otherwise =>
      $vt-c-classes[vt];
  end select
end method typedesc-C-class;

define method typedesc-C-pointer-class ( t :: <LPTYPEDESC> )
			=> class :: <class>;
  let vt :: <U16> = t.vt-value;
  select ( vt )
//  $VT-PTR  => pointer-type(typedesc-C-pointer-class(t.u-value.lptdesc-value));
//  $VT-SAFEARRAY => // element type in lptdesc-value
//  $VT-CARRAY => // element type and dimensions in lpadesc-value
//  $VT-USERDEFINED => // description handle in hreftype-value
    otherwise =>
      $vt-c-pointer-classes[vt];
  end select
end method typedesc-C-pointer-class;

define method typedesc-Dylan-class ( t :: <LPTYPEDESC> ) => (class :: <ole-type>);
  let vt :: <U16> = t.vt-value;
  select ( vt )
    $VT-PTR  => typedesc-C-class(t);
    $VT-SAFEARRAY =>
      // limited(<array>, of: typedesc-Dylan-class(t.u-value.lptdesc-value));
      ole-array-type(typedesc-Dylan-class(t.u-value.lptdesc-value));
//  $VT-CARRAY => // element type and dimensions in lpadesc-value
//  $VT-USERDEFINED => // description handle in hreftype-value
    otherwise =>
      $vt-Dylan-classes[vt];
  end select
end method typedesc-Dylan-class;

define method pointer-value-setter( type :: <object>,
				   t :: <LPTYPEDESC>,
				   #key index :: <integer> = 0 )
		=> type :: <object>;
  if ( ~ zero?(index) )
    set-typedesc(type, pointer-value-address(t, index: index))
  else
    set-typedesc(type,t)
  end if;
  type
end method pointer-value-setter;

define method set-typedesc( type :: <object>, t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
  error("Can't convert %= to an OLE type description", type);
  t.vt-value := $VT-EMPTY;
  $IDLFLAG-NONE
end method set-typedesc;

define method set-typedesc( type :: <type>, t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
    block(return)
      for ( i :: <integer> from $VT-VARIANT-MIN below size($vt-Dylan-classes) )
	let typei = $vt-Dylan-classes[i];
	if ( typei & subtype?(type, typei) )
	  t.vt-value := i;
	  if ( i = $VT-PTR )
	    block()
	      t.u-value.lptdesc-value := as-typedesc(referenced-type(type))
	    exception ( condition :: <error> )
	      // Support for structure pointers not yet implemented   ???
	      // But suppress warning on inherited QueryInterface.
	      unless ( type = <LPGUID> )
		signal(make(<simple-warning>,
			    format-string:
			      "Type library using \"void*\" in place of %=\n"
			      "because %s\n",
			    format-arguments: vector(type, condition)));
	      end;
	      t.u-value.lptdesc-value := as-typedesc(<C-void>)
	    end block
	  else
	    t.u-value.lptdesc-value := null-pointer(<LPTYPEDESC>)
	  end if;
	  return()
	end if;
      end for;
      // Icky hack to support type-union(<integer>, <machine-word>):
      if (subtype?(type, <signed-int>))
	t.vt-value := $VT-I4;
	return()
      end if;

      next-method();
    end block;
    $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc( class :: subclass(<C-value>), t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
    block(return)
      for ( i :: <integer> from $VT-VARIANT-MIN to $VT-TYPEDESC-MAX )
	let vclass = $vt-c-classes[i];
	if ( vclass & subtype?(class, vclass) )
	  t.vt-value := i;
	  return()
	end if;
      end for;
      next-method();
    end block;
    $IDLFLAG-FIN
end method set-typedesc;

// <BOOL> is a special case because the table contains <VARIANT-BOOL> instead.
define method set-typedesc( class == <BOOL>, t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
  t.vt-value := $VT-BOOL;
  $IDLFLAG-FIN
end method set-typedesc;

// <machine-word> is a special case so that it doesn't map to $VT-ERROR
// (since <HRESULT> is actually the same as <machine-word>).
define method set-typedesc( class == <machine-word>, t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
  t.vt-value := $VT-UI4;
  $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc
    ( class :: type-union(subclass(<Interface>),
			  singleton(<C-both-unsigned-long>),
			  singleton(<C-raw-unsigned-long>),
			  singleton(<C-both-signed-long>)),
     t :: <LPTYPEDESC> )
 => direction-flags :: <U16>;
  t.vt-value := vt-from-type(class);
  $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc( class :: subclass(<string>), t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
  // Any <string> will be converted to a <BSTR>; it has to be copied anyway,
  // so there is no point in requiring the user to explicitly use <BSTR>.
  t.vt-value := $VT-BSTR;
  $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc( class :: subclass(<array>), t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
  error("%= is not a valid C type designator;\n"
	  "use ole-array-type(element-type)", class);
  t.vt-value := $VT-EMPTY;
  $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc( class == <object>, t :: <LPTYPEDESC> )
		=> direction-flags :: <U16>;
  // Map the Dylan representation of unrestricted type to the
  // OLE representation of unrestricted type.  This may be controversial???
  t.vt-value := $VT-VARIANT;
  $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc( typedescr :: <array-type-description>,
			    t :: <LPTYPEDESC> )
		=> (direction-flags :: <U16>);
  t.vt-value := $VT-SAFEARRAY;
  let eltyp = typedescr.element-type;
  t.u-value.lptdesc-value := as-typedesc(eltyp);
  $IDLFLAG-FIN
end method set-typedesc;

define method set-typedesc( typedescr :: <LPTYPEDESC>, t :: <LPTYPEDESC> )
		=> (direction-flags :: <U16>);
  let vt :: <integer> = typedescr.vt-value;
  t.vt-value := vt;
  t.u-value.lptdesc-value := typedescr.u-value.lptdesc-value;
/* // If we supported deallocation of the lptdesc-value field, then we
   // would want to do the following instead.
  t.u-value.lptdesc-value :=
    if ( vt < $VT-PTR | vt > $VT-SAFEARRAY )
      null-pointer(<LPTYPEDESC>)
    else
      // copy so we won't deallocate the original
      as-typedesc(typedescr.u-value.lptdesc-value)
    end if;
    // Oh, but what if we have a user-created structure that uses the
    // lpadesc-value or hreftype-value fields?
*/
  $IDLFLAG-FIN
end method set-typedesc;

// Since TYPEDESC structures aren't getting deallocated, cache them to
// avoid duplication:
define variable *typedesc-cache* :: false-or(<table>) = #f;

// Doing this instead of adding a method to `as' in order to make it easier
// to keep track of where this is being used.
define generic as-typedesc (type) => (v :: <LPTYPEDESC>);

define method as-typedesc(type :: <object>) => v :: <LPTYPEDESC>;
  element(*typedesc-cache* | (*typedesc-cache* := make(<table>)),
	  type, default: #f) |
    begin
      let td :: <LPTYPEDESC> = make(<LPTYPEDESC>);
      set-typedesc(type, td);
      element(*typedesc-cache*, type) := td;
      td
    end
end method;

define method as-typedesc(td :: <LPTYPEDESC>) => v :: <LPTYPEDESC>;
  td
/* // If we supported deallocation of the lptdesc-value field, we would
   // want to do it the following way instead.
  if ( null-pointer?(td) )
    td
  else
    next-method() // make a copy
  end if
*/
end method;

