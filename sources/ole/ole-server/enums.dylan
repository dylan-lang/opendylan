Module:    OLE-Server
Synopsis:  in-code implementation of attributes formerly deferred to registry.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define COM-interface <CEnumXXXX> ( <IUnknown> )
  slot enum-data :: <sequence>, required-init-keyword: data:;
  slot enum-position :: <fixnum>, init-value: 0;
end <CEnumXXXX>;

define method terminate ( this :: <CEnumXXXX> ) => ()
  next-method();
  this.enum-data := #(); // let the data be GC'd
end method terminate;

define function IEnumXXXX/Next (this :: <CEnumXXXX>,
				requested-number :: <fixnum>,
				result-array :: <C-statically-typed-pointer>)
 => ( status :: <HRESULT>, count :: <fixnum> )

  let data-sequence = this.enum-data;
  let count :: <fixnum> = 0;
  let start-pos :: <fixnum> = this.enum-position;
  for ( i from start-pos below
	 min(start-pos + requested-number, data-sequence.size) )
    enum-set-next(pointer-value-address(result-array, index: i),
		  data-sequence[i]);
    count := count + 1;
  end for;
  this.enum-position := start-pos + count;
  values ( if ( count = requested-number ) $S-OK else $S-FALSE end if,
	   count )
end IEnumXXXX/Next;

define function IEnumXXXX/Skip (this :: <CEnumXXXX>, count :: <fixnum>)
 => ( status :: <HRESULT> )

  let new-position = this.enum-position + count;
  let length = this.enum-data.size;
  if ( new-position > length )
    this.enum-position := length;
    $S-FALSE
  else
    this.enum-position := new-position;
    $S-OK
  end if
end IEnumXXXX/Skip;

define function IEnumXXXX/Reset (this :: <CEnumXXXX>)
 => ( status :: <HRESULT> )

  this.enum-position := 0;
  $S-OK
end IEnumXXXX/Reset;


define function IEnumXXXX/Clone (this :: <CEnumXXXX>)
  => ( status :: <HRESULT>, enumerator :: <CEnumXXXX> );

  let enumerator = make(this.object-class, data: this.enum-data);
  AddRef(enumerator);
  values( $S-OK, enumerator )
end IEnumXXXX/Clone;


define method enum-set-next( dest-ptr :: <C-statically-typed-pointer>,
			     source-ptr :: <C-statically-typed-pointer> ) => ()
  check-type(source-ptr, dest-ptr.object-class);
  %memcpy(dest-ptr, source-ptr,
	  size-of(dest-ptr.object-class.referenced-type));
end method;

define method enum-set-next( dest-ptr :: <C-int*>,
			     source :: <integer> ) => ()
  pointer-value(dest-ptr) := source;
end method;



define COM-interface <CEnumFORMATETC> ( <IEnumFORMATETC>, <CEnumXXXX> )
end <CEnumFORMATETC>;

define class <FORMATETC-info> ( <object> )
  constant slot fi-format :: <fixnum>, required-init-keyword: format:;
  constant slot fi-aspect :: <fixnum>, required-init-keyword: aspect:;
  constant slot fi-tymed  :: <fixnum>, required-init-keyword: tymed:;
end class;

//define constant $format-info =
//  make(<FORMATETC-info>, format: $CF-METAFILEPICT,
//       aspect: $DVASPECT-CONTENT, tymed: logior($TYMED-MFPICT, $TYMED-ENHMF));

define method enum-set-next( formatetc :: <LPFORMATETC>,
			     source :: <FORMATETC-info> ) => ()
  formatetc.cfFormat-value	:= source.fi-format;
  formatetc.ptd-value  	:= null-pointer(<LPDVTARGETDEVICE>);
  formatetc.dwAspect-value	:= source.fi-aspect;
  formatetc.lindex-value	:= -1;
  formatetc.tymed-value 	:= source.fi-tymed;
end method;


// Enumerates the formats supported by this object.
define method IDataObject/EnumFormatEtc(this :: <CDataObject>,
					direction :: <fixnum> )
 => ( status :: <HRESULT>, enumerator :: <CEnumFORMATETC> );

  Output-Debug-String("IDataObject/EnumFormatEtc ");

  block(return)
    let data :: <sequence> =
      select ( direction )
	$DATADIR-GET =>
	  Output-Debug-String("$DATADIR-GET\r\n");
	  OLE-part-formats-for-get(this.get-obj);
	$DATADIR-SET =>
	  Output-Debug-String("$DATADIR-SET\r\n");
	  OLE-part-formats-for-set(this.get-obj);
	otherwise =>
	  Output-Debug-String("$E-INVALIDARG\r\n");
	  return($E-INVALIDARG, $null-interface)
      end select;
    let enumerator = make(<CEnumFORMATETC>, data: data);
    AddRef(enumerator);
    values( $S-OK, enumerator )
  end block
end method IDataObject/EnumFormatEtc;

define open generic OLE-part-formats-for-get (obj) => (formats :: <list>);
define open generic OLE-part-formats-for-set (obj) => (formats :: <list>);

define method OLE-part-formats-for-get (obj :: <basic-ole-server>)
 => (formats :: <list>)

  list(make(<FORMATETC-info>,
	    format: $CF-METAFILEPICT,
	    aspect: $DVASPECT-CONTENT,
	    tymed: logior($TYMED-MFPICT, $TYMED-ENHMF)))
end method OLE-part-formats-for-get;

define method OLE-part-formats-for-set (obj :: <basic-ole-server>)
 => (formats :: <list>)
  #()
end method OLE-part-formats-for-set;


define method IEnumFORMATETC/Next (this :: <CEnumFORMATETC>,
				   requested-number :: <fixnum>,
				   result-array :: <LPFORMATETC>)
 => ( status :: <HRESULT>, count :: <fixnum> )
  IEnumXXXX/Next(this, requested-number, result-array)
end IEnumFORMATETC/Next;

define method IEnumFORMATETC/Skip (this :: <CEnumFORMATETC>, count :: <fixnum>)
 => ( status :: <HRESULT> )
  IEnumXXXX/Skip(this, count)
end IEnumFORMATETC/Skip;

define method IEnumFORMATETC/Reset (this :: <CEnumFORMATETC>)
 => ( status :: <HRESULT> )
  IEnumXXXX/Reset(this)
end IEnumFORMATETC/Reset;

define method IEnumFORMATETC/Clone (this :: <CEnumFORMATETC>)
  => ( status :: <HRESULT>, enumerator :: <CEnumFORMATETC> );

  IEnumXXXX/Clone(this)
end IEnumFORMATETC/Clone;

