Module:    COM
Synopsis:  Additional COM features that need to be defined after all of
	   the types and classes have been defined.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <LPCLIPFORMAT> => <CLIPFORMAT>;

//====================================================
//	identifiers
//====================================================

// GUID = ``Globally Unique ID''
// structure type <GUID> is defined in "cmisc.dylan"

define method make-GUID( L :: <U32>, w1 :: <U16>, w2 :: <U16>,
			b1 :: <U8>, b2 :: <U8>, b3 :: <U8>, b4 :: <U8>,
			b5 :: <U8>, b6 :: <U8>, b7 :: <U8>, b8 :: <U8>)
		 => value :: <REFGUID>;
  let ID :: <REFGUID> = make(<REFGUID>);
  let lval :: <machine-word> = as(<machine-word>, L); // could be <double-integer>
  ID.Data1-value := lval;
  ID.Data2-value := w1;
  ID.Data3-value := w2;
  Data4-array(ID, 0) := b1;
  Data4-array(ID, 1) := b2;
  Data4-array(ID, 2) := b3;
  Data4-array(ID, 3) := b4;
  Data4-array(ID, 4) := b5;
  Data4-array(ID, 5) := b6;
  Data4-array(ID, 6) := b7;
  Data4-array(ID, 7) := b8;
  ID
end make-GUID;

define function IsEqualGUID? ( id1 :: <REFGUID>, id2 :: <REFGUID> )
			=> ( equal? :: <boolean> );
  equal-memory?(id1, id2, size-of(<GUID>))
end;

define inline sealed method \= ( id1 :: <REFGUID>, id2 :: <REFGUID> )
 => equal :: <boolean>;
  IsEqualGUID?(id1,id2)
end method;


// interface identifier:

// My first inclination was to make it a distinct type:
//    define C-subtype <REFIID> ( <REFGUID> ) end;
// but that won't work because there is no type distinction in DEFINE_GUID.

// This is now defined in "cmisc.dylan"
// define C-type-alias <REFIID> = <REFGUID>;

define inline constant IsEqualIID? = IsEqualGUID?;

define C-address $IID-NULL :: <REFIID> c-name: "GUID_NULL"; end;

define inline constant IsEqualCLSID? = IsEqualGUID?;

define C-address $CLSID-NULL :: <REFCLSID> c-name: "GUID_NULL"; end;

// used internally in "com.dylan":
define constant $IID-IdylanObject :: <REFGUID> =
  make-GUID(#x14FF2830, #x550F, #x11CF,
	    #x89, #xF1, #x02, #x07, #x01, #x19, #xF6, #x39);


//====================================================
//	"IClassFactory" interface
//====================================================

/* The following is a minimal implementation that can be inherited,
   but may need to be overridden depending on the requirements of the
   particular application.  */  
define method IClassFactory/LockServer ( self :: <IClassFactory>,
					 fLock :: <boolean> )
	=> status :: <HRESULT>;
  CoLockObjectExternal(self.controlling-unknown, fLock, #t);
end method;

//====================================================
//	alias names for backward compatibility
//====================================================

define constant IStream/Read  = ISequentialStream/Read;
define constant IStream/Write = ISequentialStream/Write;

define inline constant %memcpy = copy-into!; // from C-FFI library


//====================================================
//	convenience accessors
//====================================================

define sealed method indexed-medium( ptr :: <LPSTGMEDIUM>, index :: <integer>)
 => ptr :: <LPSTGMEDIUM>;
  // Given pointer to array, return pointer to indexed element.
  if ( zero?(index) )
    ptr 
  else
    pointer-value-address(ptr, index: index)
  end if
end method indexed-medium;


define sealed method pointer-value ( ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  let u = medium.u-value;
  select ( medium.tymed-value )
    $TYMED-GDI => u.hBitmap-value;
    $TYMED-MFPICT => u.hMetafilePict-value;
    $TYMED-ENHMF => u.hEnhMetaFile-value;
    $TYMED-HGLOBAL => u.hGlobal-value;
    $TYMED-FILE => u.lpszFileName-value;
    $TYMED-ISTREAM => u.pstm-value;
    $TYMED-ISTORAGE => u.pstg-value;
    $TYMED-NULL => $NULL-VOID;
  end select
end method pointer-value;

define sealed method initialize ( medium :: <LPSTGMEDIUM>, #rest ignore,
				 #key address = #f,
				 #all-keys ) => ();
  next-method();
  unless ( address ) // when new space allocated
    medium.tymed-value := $TYMED-NULL;
    medium.pUnkForRelease-value := $null-interface;
  end unless;
  values()
end method initialize;

define sealed method hGlobal-value ( medium :: <LPSTGMEDIUM> )
 => value :: <HGLOBAL>;
  medium.u-value.hGlobal-value
end;

define sealed method hGlobal-value-setter ( value :: <HANDLE>,
					    medium :: <LPSTGMEDIUM> )
			=> value :: <HANDLE>;
  medium.u-value.hGlobal-value := value
end;

define sealed method hMetaFilePict-value ( medium :: <LPSTGMEDIUM> )
 => value :: <HMETAFILEPICT>;
  medium.u-value.hMetaFilePict-value
end;

define sealed method hMetaFilePict-value-setter ( value :: <HANDLE>,
						  medium :: <LPSTGMEDIUM> )
			=> value :: <HMETAFILEPICT>;
  medium.u-value.hMetaFilePict-value := value
end;

define sealed method hEnhMetaFile-value ( medium :: <LPSTGMEDIUM> )
 => value :: <HENHMETAFILE>;
  medium.u-value.hEnhMetaFile-value
end;

define sealed method hEnhMetaFile-value-setter ( value :: <HANDLE>,
						 medium :: <LPSTGMEDIUM> )
			=> value :: <HENHMETAFILE>;
  medium.u-value.hEnhMetaFile-value := value
end;

define sealed method lpszFileName-value ( medium :: <LPSTGMEDIUM> )
 => value :: <LPOLESTR>;
  medium.u-value.lpszFileName-value
end;

define sealed method lpszFileName-value-setter ( value :: <LPOLESTR>,
						 medium :: <LPSTGMEDIUM> )
			=> value :: <LPOLESTR>;
  medium.u-value.lpszFileName-value := value
end;

define sealed method pstm-value ( medium :: <LPSTGMEDIUM> )
 => value :: <LPSTREAM>;
  medium.u-value.pstm-value
end;

define sealed method pstm-value-setter ( value :: <LPSTREAM>,
					 medium :: <LPSTGMEDIUM> )
			=> value :: <LPSTREAM>;
  medium.u-value.pstm-value := value
end;

define sealed method pstg-value ( medium :: <LPSTGMEDIUM> )
 => value :: <LPSTORAGE>;
  medium.u-value.pstg-value
end;

define sealed method pstg-value-setter ( value :: <LPSTORAGE>,
					 medium :: <LPSTGMEDIUM> )
			=> value :: <LPSTORAGE>;
  medium.u-value.pstg-value := value
end;

define sealed method pointer-value-setter
    ( value :: <HBITMAP>, ptr :: <LPSTGMEDIUM>, #key index = 0 )
 => value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-GDI;
  medium.u-value.hBitmap-value := value;
end;

define sealed method pointer-value-setter ( value :: <HMETAFILEPICT>,
				    ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-MFPICT;
  medium.u-value.hMetafilePict-value := value;
end;

define sealed method pointer-value-setter ( value :: <HENHMETAFILE>,
				    ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-ENHMF;
  medium.u-value.hEnhMetaFile-value := value;
end;


define sealed method pointer-value-setter ( value :: <HGLOBAL>,
				    ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-HGLOBAL;
  medium.u-value.hGlobal-value := value;
end;

define sealed method pointer-value-setter ( value :: <LPOLESTR>,
				    ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-FILE;
  medium.u-value.lpszFileName-value := value;
end;

define sealed method pointer-value-setter ( value :: <IStream>,
				    ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-ISTREAM;
  medium.u-value.pstm-value := value;
end;

define sealed method pointer-value-setter ( value :: <IStorage>,
				    ptr :: <LPSTGMEDIUM>, #key index = 0 )
				=> value :: <C-pointer>;
  let medium :: <LPSTGMEDIUM> = indexed-medium(ptr, index);
  medium.tymed-value := $TYMED-ISTORAGE;
  medium.u-value.pstg-value := value;
end;

define open-accessor vt-value;

// Just a dummy structure defined here; methods will be added in the
// OLE-Automation library.
define C-struct <PROPVARIANT>
  sealed inline-only slot vt-value :: <VARTYPE>;
  slot wReserved1      :: <WORD>, getter: #f, setter: #f;
  slot wReserved2      :: <WORD>, getter: #f, setter: #f;
  slot wReserved3      :: <WORD>, getter: #f, setter: #f;
  slot u-dummy-place-holder :: <C-double>, getter: #f, setter: #f;
  pointer-type-name: <LPPROPVARIANT>;
  c-name: "struct tagPROPVARIANT";
end C-struct <PROPVARIANT>;
