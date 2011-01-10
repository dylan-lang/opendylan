Module:    COM
Synopsis:  Fundamental infrastructure and utility routines for COM interface.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//====================================================
//	temporary debugging hack
//====================================================

define constant $Continue-message :: <string> = "Continue anyway";

/*
define method local-debug-assert(ok? :: <boolean>) => ();
  unless (ok?)
    cerror($Continue-message, "assert failure in \"com.dylan\"");
  end unless;
  values();
end local-debug-assert;
*/

define inline-only function local-debug-assert(ok? :: <boolean>) => ();
end local-debug-assert;


//====================================================
//	basic types
//====================================================

// note: assuming that the following FFI types have been defined by
//   the Win32-common module:
//	define constant <DWORD> = <C-unsigned-long>;
//	define constant <ULONG> = <C-unsigned-long>;
//	define constant <LPVOID> = <C-void*>;
//	define C-struct <FILETIME> ...
//	define C-struct <LARGE-INTEGER> ...
//	define C-struct <ULARGE-INTEGER> ...

// integer subranges used internally:
define constant  <U8> = <fixnum>;
define constant <U16> = <fixnum>;
define constant <U24> = <fixnum>;
define constant <U32> = <object>;
/*
define constant  <U8> = limited(<integer>, min: 0, max: #xFF);
define constant <U16> = limited(<integer>, min: 0, max: #xFFFF);
define constant <U24> = limited(<integer>, min: 0, max: #xFFFFFF);
define constant <U32> = type-union(<abstract-integer>, <machine-word);
*/

//--

define constant <LPOLESTR>  = <C-unicode-string>;
define C-pointer-type <LPLPOLESTR> => <LPOLESTR>;
define constant <LPCOLESTR> = <LPOLESTR>;

define constant $NULL-OLESTR :: <LPOLESTR> = null-pointer(<LPOLESTR>);

// String Name Block 
define constant <SNB> = <LPLPOLESTR>;

define variable *ole-string-table* :: <table> = make(<table>);

define method OLESTR (string :: <string>) => value :: <LPOLESTR>;
  element(*ole-string-table*, string, default: #f)
  | (*ole-string-table*[string] := as(<LPOLESTR>, string))
end OLESTR;

define inline method OLESTR (string :: <LPOLESTR>) => value :: <LPOLESTR>;
  string
end OLESTR;

define method OLESTR (string :: <C-pointer>) => value :: <LPOLESTR>;
  // make sure we don't just coerce the pointer type of a C-string
  error("bug in OLESTR");	// ???
  next-method();
end OLESTR;

define open generic null? ( object ) => (value :: <boolean>);

define inline method null? ( object :: <C-pointer> ) => value :: <boolean>;
  null-pointer?(object);
end;

define inline sealed method null? ( object == #f ) => value :: <boolean>;
  #t
end;

define sealed domain null? (<Interface>);

define inline constant <VARTYPE> = <C-unsigned-short>;
define inline constant <LPPROPID> = <LPULONG>;

// should be <C-unsigned-long-long> if that were supported:
//define constant <ULONGLONG> = <C-double>;
define constant <ULONGLONG> = <ULARGE-INTEGER>;


//====================================================
//	result and status codes
//====================================================

// status code:
define constant <SCODE> = <machine-word>;

// result handle: (considered obsolete, but still pervasively documented.)
define constant <HRESULT> = <SCODE>;

// The following methods take the place of macros defined in "winerror.h":

define function SCODE-CODE(sc :: <SCODE>) => value :: <U16>;
  LOWORD(sc)
end;

define function SCODE-FACILITY(sc :: <SCODE>) => value :: <U16>;
  logand( HIWORD(sc), #x1fff )
end;

define function SCODE-SEVERITY(sc :: <SCODE>) => value :: <U16>;
  as(<integer>, u%shift-right(sc, 31))
end;

// Used internally, do not export:
define constant $severity-bit :: <SCODE> = as(<machine-word>,#x80000000);

/* See companion file "com-err.dylan" for error codes collected by:
	  gema -f com-err.pat winerror.h 
*/

define constant $NOERROR :: <HRESULT> = as(<machine-word>,0);

define inline sealed method SUCCEEDED?( sc :: <SCODE> ) => value :: <boolean>;
  ~ negative?(sc)
end SUCCEEDED?;

define inline sealed method FAILED?( sc :: <SCODE> ) => value :: <boolean>;
  negative?(sc)
end FAILED?;

define inline sealed method SUCCEEDED?(sc :: <integer>) => value :: <boolean>;
  sc >= 0
end SUCCEEDED?;

define inline sealed method FAILED?( sc :: <integer> ) => value :: <boolean>;
  sc < 0
end FAILED?;

define function MAKE-SCODE(severity :: <U16>, facility :: <U16>,
			   code :: <U16>) => value :: <SCODE>;
  let n :: <integer> = logior( ash(facility,16), code);
  let s :: <SCODE> = as(<machine-word>, n);
  unless ( zero?(severity) )
    s := %logior( $severity-bit, s );
  end;
  s
end MAKE-SCODE;

define C-subtype <C-HRESULT> ( <C-raw-signed-long> )
  pointer-type-name: <C-HRESULT*>;
end;

// Construct an HRESULT representation of a Win32 error code
define method HRESULT-FROM-WIN32(error-code :: <integer>) => scode :: <SCODE>;
  if ( error-code = $NO-ERROR )
    $S-OK
  else
    MAKE-SCODE(1, $FACILITY-WIN32, logand(error-code, #xFFFF))
  end if
end;

// Map an NT status value into a HRESULT
define function HRESULT-FROM-NT( nt-code ) => scode :: <SCODE>;
  %logior(nt-code, #x10000000)
end;


//====================================================
//	object handles for C (temporary until supported by C-FFI)
//====================================================

define constant $handle-offset = #x7000; // for validation

define constant $null-object-handle = #xDEAD;

define variable *handled-objects* :: <simple-object-vector> =
  make(<simple-object-vector>, size: 10, fill: #f);

define method intern-object ( object :: <object> ) => handle :: <U16>;
  let n :: <U16> = size(*handled-objects*);
  block(return)
    for ( i :: <U16> from 0 below n )
      if ( *handled-objects*[i] == object )
	return(i);
      end if;
    end for;
    for ( i :: <U16> from 1 below n ) // reserve 0 as handle for #f
      if ( *handled-objects*[i] == #f )
	*handled-objects*[i] := object;
	return(i);
      end if;
    end for;
    *handled-objects* := concatenate( *handled-objects*,
				      vector( object, #f, #f, #f ) );
    return(n);
  end block
  + $handle-offset
end method intern-object;

define method unintern-object ( handle :: <U16> ) => ();
  if ( handle ~= $null-object-handle )
    *handled-objects*[handle - $handle-offset] := #f;
  end if;
  values();
end method unintern-object;

define function object-from-handle ( handle :: <U16> ) => object :: <object>;
  *handled-objects*[handle - $handle-offset];
end object-from-handle;


//====================================================
//	interfaces
//====================================================

define C-subtype <C-COM-vtbl> ( <C-void*> ) end;

define C-struct <C-COM-struct> 
  slot vtbl :: <C-COM-vtbl>; // pointer to method table used by C code.
  slot handle :: <C-int>; // way to find the corresponding Dylan object.
  pointer-type-name: <Interface>;
end <C-COM-struct>;

define constant $null-vtable :: <C-COM-vtbl> = null-pointer(<C-COM-vtbl>);

// <Interface> is conceptually an abstract class -- to be used for declaring
// the type of variables that could be either a <C-interface> or
// <Dylan-interface>

define method make( class == <Interface>, #rest args,
		   #key address = #f, #all-keys )
 => ( interface :: <Interface> );
  if ( address & zero?(address) )
    $NULL-interface
  else
    apply(make, <C-interface>, args);
  end if
end method make;

define C-pointer-type <Interface*> => <Interface>;


// <C-interface> is an interface that will be accessed through the C++ virtual
// table.  This doesn't necessarily mean that the class is actually
// implemented in C, just that we don't currently know that it isn't.
define open primary C-subtype <C-interface> ( <Interface> )
  pointer-type-name: <C-interface*>;
end <C-interface>;

// <Dylan-interface> is conceptually an abstract base class from which all
// interfaces implemented in Dylan inherit.
define open primary C-subtype <Dylan-interface> ( <Interface> )
end <Dylan-interface>;

/* Note: $NULL-interface is implemented as a <Dylan-interface> instead of
    an <Interface> so that an attempt to de-reference it will be caught as
    a Dylan dispatch error instead of crashing the C code. 
    A separate sub-type is used simply for clarity in the debugger. */ 
define sealed C-subtype <null-interface> ( <Dylan-interface> )
end <null-interface>;

define constant $NULL-interface :: <null-interface> =
  null-pointer(<null-interface>);

define C-mapped-subtype <mapped-interface> ( <C-interface> )
  import-map <Dylan-interface>, import-function: import-interface;
end <mapped-interface>;

define function import-interface (ptr :: <C-interface>)
 => value :: <Dylan-interface>;

  // When calling from C to Dylan, we get the address of the C structure 
  // and need to find the Dylan interface object which is a wrapper for
  // that address. 
  let object-handle = ptr.handle;
  if ( object-handle == $null-object-handle )
    error("Importing C pointer to terminated Dylan OLE object");
  end if;
  let obj :: <Dylan-interface> = object-from-handle( object-handle );
  local-debug-assert( pointer-address(obj) = pointer-address(ptr) );
  obj
end;

define sealed domain make (singleton(<Interface>));
define sealed domain make (singleton(<C-interface>));
define sealed domain initialize (<C-interface>);

// It should be possible to define interfaces by just using `define class',
// but that doesn't work yet.  So for now use this macro to keep track of
// the affected code.  -- D.N.G. 1/22/96 [still needed with DFMC Jan. '97]
// (Can't just say `define interface' because that is used by Collage.)
define macro COM-interface-definer
  { define ?mods:* COM-interface ?name:name (?supers:*) ?specs:* end }
    => { define ?mods C-subtype ?name (?supers) ?specs end }
end macro;


//====================================================
//	mapping a C interface pointer to the Dylan object
//====================================================

// This uses the QueryInterface mechanism so that a C-implemented interface
// can be harmlessly queried, but this is not a real interface. 
// In particular, it does not follow the usual conventions for use count
// and for controlling unknown delegation.  But that doesn't matter because
// it is only used internally here.

define method dylan-interface( obj :: <C-interface> ) => obj :: <Interface>;
  if ( null-pointer?(obj) )
    $null-interface
  else
    let ( status :: <SCODE>, dobj :: <Interface> ) =
      IUnknown/QueryInterface(obj,$IID-IdylanObject);
    if ( status = $S-OK )
      // Note that even though QueryInterface has returned the Dylan object,
      // it has been returned through a C call which only passes through the
      // pointer address.  But now that we know that it is a Dylan object,
      // we can map the pointer to find it.
      import-interface(dobj)
    else
      obj
    end if
  end if
end method;

define method dylan-interface( obj :: <Dylan-Interface> )
		=> obj :: <Dylan-Interface>;
  // already have the Dylan object so just return it.
  obj
end method;

//====================================================
//	"IUnknown" interface
//====================================================

define open primary COM-interface <IUnknown> ( <Dylan-interface> )
  sealed slot ref-count :: <integer>, init-value: 0;
  // Use the following keyword to specify the ``controlling unknown'' of a
  // member of an aggregate object.
  sealed slot controlling-unknown :: <Interface>,
	init-keyword: controlling-unknown:, init-value: $NULL-interface;
  sealed slot interface-table :: <list>, init-value: #();
end <IUnknown>;

define C-struct <IUnknown>-vstruct
  sealed inline-only slot vtbl-QueryInterface :: <C-function-pointer>;
  sealed inline-only slot vtbl-AddRef :: <C-function-pointer>;
  sealed inline-only slot vtbl-Release :: <C-function-pointer>;
end C-struct;

define C-address IUnknown-DW-vtbl :: <C-COM-vtbl>
  c-name: "IUnknown_DW_vtbl";
end;

define C-address $IID-IUnknown :: <REFIID>
  c-name: "IID_IUnknown";
end;

define method initialize ( self :: <IUnknown>, #rest ignore, #key )
  if ( null-pointer?(self) )
    error( "Can't make null-pointer(%=)", object-class(self) );
  else
    next-method();
    if ( null?(self.controlling-unknown) )
      self.controlling-unknown := self;
    end if;
    self.vtbl := IUnknown-DW-vtbl;
    self.handle := intern-object(self);
  end if;
  values()
end initialize;

define method add-interface ( self :: <IUnknown>, iid :: <REFIID> )
  let c :: <Interface> = self.controlling-unknown;
  c.interface-table := pair( pair(iid,self), c.interface-table );
  if ( c ~= self )
    AddRef(self);
    SubRef(c);
  end if;
end add-interface;

define open generic IUnknown/AddRef (self :: <object>)
					=> value :: <integer>;

define C-callable-wrapper of IUnknown/AddRef
  input parameter self :: <mapped-interface>;
  result count :: <C-unsigned-long>;
  export: #t,
  c-name: "DW_IUnknown_AddRef",
  c-modifiers: "__stdcall";
end;

define method IUnknown/AddRef( self :: <IUnknown> ) => value :: <U24>;
  let new-count :: <fixnum> = self.ref-count + 1;
  self.ref-count := new-count;
  let cu :: <Interface> = self.controlling-unknown;
  unless ( cu == self )
    if ( cu == $null-interface & self.handle == $null-object-handle )
      cerror($Continue-message,
	     "AddRef on terminated object %=", self);
    end if;
    IUnknown/AddRef(cu);
  end unless;
  new-count
end;

define inline-only C-function IUnknown_AddRef
  parameter self :: <C-interface>;
  result count :: <C-unsigned-long>;
  c-name: "C_IUnknown_AddRef";
end;

define method IUnknown/AddRef( self :: <Interface> ) => value :: <U24>;
  // Do nothing when called on $NULL-interface
  if ( null-pointer?(self) )
    0
  else
    IUnknown_AddRef(self)
  end if;
end;

define open generic IUnknown/Release (self :: <object>)
		=> value :: <integer>;

define C-callable-wrapper of IUnknown/Release
  input parameter self :: <mapped-interface>;
  result count :: <C-unsigned-long>;
  export: #t,
  c-name: "DW_IUnknown_Release",
  c-modifiers: "__stdcall";
end;

define method IUnknown/Release( self :: <IUnknown> ) => value :: <U24>;
  let new-count :: <fixnum> = self.ref-count - 1;
  if ( new-count < 0 )
    cerror($Continue-message,
	   if ( self.handle == $null-object-handle )
	     "Release called more times than AddRef on %="
	   else
	     "Release called before AddRef on %="
	   end if,
	   self);
  end if;
  if ( zero?(new-count) )
    // Don't store the new count until after calling `terminate' in order
    // to avoid the possibility of an AddRef / Release pair causing a
    // recursive terminate.
    terminate(self);	// clean up references to other objects
    // Make sure no new references were created during termination.
    if ( self.ref-count > 1 )
      cerror($Continue-message, "AddRef within terminate(%=)", self);
    end if;
    self.ref-count := 0;
    unintern-object(self.handle);  // allow object to be GC'd
    self.handle := $null-object-handle; // trap attempt to use dead object
    self.controlling-unknown := $NULL-interface;
    destroy(self); // de-allocate the <C-COM-struct>
  else
    self.ref-count := new-count;
    let cu :: <Interface> = self.controlling-unknown;
    unless ( cu == self )
      IUnknown/Release(cu);
    end unless;
  end if;
  new-count
end;

define open generic SubRef (self :: <object>) => value :: <integer>;

define method SubRef( self :: <IUnknown> ) => value :: <integer>;
  // Like Release, except can decrement to 0 without terminating.
  // Use this to un-do an AddRef during initialization before the AddRef
  // for the real user of the object.
  let new-count :: <fixnum> = self.ref-count - 1;
  if ( new-count < 0 )
    cerror($Continue-message, "Negative ref count on %=", self);
  end if;
  self.ref-count := new-count;
  let cu :: <Interface> = self.controlling-unknown;
  unless ( cu == self )
    SubRef(cu);
  end unless;
  new-count
end;

// The original intent was that `terminate' would be called either when the
// reference count is decremented to zero or by GC finalization prior to
// garbage-collecting the object.  However, the use of `intern-object'
// prevents the object from being GC'd before the ref count becomes 0.
// If the object is being exported to C code or to another process, then we
// have to prevent GC, and there is no reliable way to know whether the
// pointer is going to be used that way.  So there does not appear to be
// any safe way to take advantage of finalization.   -- D.N.G. 9/23/97

define open generic terminate ( self :: <object> ) => ();
define method terminate ( self :: <IUnknown> ) => ();
  next-method();
  let interface-list :: <list> = self.interface-table;
  self.interface-table := #();
  for ( item :: <pair> in interface-list )
    let object :: <Interface> = tail(item);
    if ( object ~== self )
      if ( object.controlling-unknown == self )
	object.controlling-unknown := object;
      end if;
      IUnknown/Release(object); // undo increment in add-interface
    end if;
  end for;
  // Note: if this were to be called from finalization instead of `Release'
  //  (and if we could tell the difference), we would also want to do 
  //  `destroy(self)' here to release the C storage space.
  values()
end;

// last resort for next-method does nothing:
define inline method terminate ( self :: <object> ) => ();
  values()
end terminate;

define inline-only C-function IUnknown_Release
  parameter self :: <C-interface>;
  result count :: <C-unsigned-long>;
  c-name: "C_IUnknown_Release";
end;

define method IUnknown/Release( self :: <Interface> ) => value :: <U24>;
  // Do nothing when Release is called on $NULL-interface
  if ( null-pointer?(self) )
    0
  else
    IUnknown_Release(self)
  end if;
end;

define open generic IUnknown/QueryInterface ( self :: <object>,
					      riid :: <object> )
	=> ( status :: <HRESULT>, Object :: <Interface> );

define C-callable-wrapper of IUnknown/QueryInterface
  input parameter self :: <mapped-interface>;
  input parameter riid :: <REFIID>;
  output parameter Object :: <Interface*>;
  result status :: <C-HRESULT>;
  export: #t,
  c-name: "DW_IUnknown_QueryInterface",
  c-modifiers: "__stdcall";
end;

define method IUnknown/QueryInterface( self :: <IUnknown>, rrid :: <REFIID> )
		=> ( status :: <HRESULT> , Object :: <Interface> );
  if ( pointer-address(rrid) = pointer-address($IID-IdylanObject) )
    // Note: can compare addresses here instead of using `IsEqualIID?' because
    // this special IID is only used locally within this file.  Also, it
    // helps ensure that we don't try to return a Dylan object to a client
    // in a different process.
    values( $S-OK, self )
  else
    let c :: <Interface> = self.controlling-unknown;
    if ( c == self )
      if ( IsEqualIID?(rrid, $IID-IUnknown) )
	AddRef(self);
	values( $S-OK, self )
      else
	block (return)
	  for ( el :: <pair> in self.interface-table )
	    if ( IsEqualIID?( head(el), rrid ) )
	      let x :: <Dylan-Interface> = tail(el);
	      local-debug-assert( x.handle ~= $null-object-handle );
	      AddRef(x);
	      return( $S-OK, x );
	    end if;
	  end for;
	  if ( self.ref-count <= 0 & empty?(self.interface-table) )
	    cerror($Continue-message,
		   "QueryInterface on terminated object %=", self);
	  end if;
	  return( $E-NOINTERFACE, $NULL-interface );
	end block
      end if
    else // recurse in case the user defined an override method
      IUnknown/QueryInterface(c, rrid );
    end if
  end if
end method IUnknown/QueryInterface;

define inline-only C-function IUnknown_QueryInterface
  parameter self :: <C-interface>;
  input parameter riid :: <REFIID>;
  output parameter Object :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "C_IUnknown_QueryInterface";
end;

define method IUnknown/QueryInterface( self :: <C-interface>,
				      rrid :: <REFIID> )
	=> ( status :: <HRESULT> , Object :: <Interface> );
  IUnknown_QueryInterface(self, rrid)
end;

define method IUnknown/QueryInterface( self :: <interface>,
				       id-string :: <string> )
	=> ( status :: <HRESULT> , Object :: <Interface> );
  let id-struct :: <REFIID> = as(<REFIID>, id-string);
  block ()
    IUnknown/QueryInterface(self, id-struct)
  cleanup
    destroy(id-struct);
  end
end;

define constant AddRef = IUnknown/AddRef;
define constant Release = IUnknown/Release;
define constant QueryInterface = IUnknown/QueryInterface;

// Trap invalid pointer type conversions:
define method as (class :: subclass(<IUnknown>), value :: <C-pointer>)
 => (dummy :: <IUnknown>);
  error("Can't convert %= to %=", value, class);
end;

//====================================================


/* 
 Users can create their own interface by defining a subclass of one of the
 pre-defined interfaces:

    define COM-interface <IFoo> ( <IBar> )
      // ... slots ...
    end;

 And defining either:

    define method initialize ( self :: <IFoo>, #rest args )
      next-method();
      self.add-interface($IID-IFoo);
    end initialize;

 Or:

    define method QueryInterface( self :: <IFoo>, rrid :: <REFIID> )
	    => ( status :: <HRESULT> , Object :: <Interface> );
      if ( riid = $IID-IFoo)
	self.AddRef();
	values( $S-OK, self );
      else
	next-method();
      end if;
    end method QueryInterface;

 And then defining the methods.  However, these methods will not be
 callable from C code unless additional hacks are done.  (The object can
 however be passed to C functions that accept a pointer to an instance of 
 the base class interface.)

 Similarly, additional hacks would be needed for a Dylan program to be able
 to use a custom interface implemented in C.

*/

//====================================================
//	helper functions for more convenient use from Dylan
//====================================================

define variable int-ptr :: <C-both-long*> = make(<C-both-long*>);
define constant $int-size :: <integer> = size-of(<C-long>);


define method IStream/Write-integer( stream :: <Interface>,
				    value :: type-union(<integer>,
							<machine-word>) )
	=> ( status :: <HRESULT>, bytes :: <integer> );
  pointer-value(int-ptr) := value;
  /* return */ IStream/Write(stream, int-ptr, $int-size )
end;

define method IStream/Read-integer( stream :: <Interface> )
	=> ( status :: <HRESULT>,
	     value :: type-union(<integer>, <machine-word>),
	     bytes :: <integer> );
  let ( status, bytes ) = IStream/Read(stream, int-ptr, $int-size );
  values( status, pointer-value(int-ptr), bytes )
end;
