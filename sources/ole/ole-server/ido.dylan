Module:    OLE-Server
Synopsis:  Methods for class <CDataObject>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Called to determine if our object supports a particular FORMATETC.

define method IDataObject/QueryGetData(this :: <CDataObject>,
				       pformatetc :: <LPFORMATETC> )
			=> status :: <HRESULT>;
	
  Output-Debug-String("IDataObject/QueryGetData\r\n");
  // check the validity of the data format specified by formatetc.
  OLE-part-get-data(this.get-obj, pformatetc, null-pointer(<LPSTGMEDIUM>))
end method IDataObject/QueryGetData;


// Called by the container when it would like to be notified of 
// changes in the object data.

define method IDataObject/DAdvise(this :: <CDataObject>,
				  pFormatetc :: <LPFORMATETC>,
			  advf,// :: <unsigned-fixnum> or <machine-integer>
				  pAdvSink :: <LPADVISESINK>)
	=> ( status :: <HRESULT>, connection-ID :: <unsigned-fixnum> );

  Output-Debug-String("IDataObject/DAdvise\r\n");
  let obj = this.get-obj;

  // if no DataAdviseHolder has been created, then create one.
  if ( null?(obj.container-IDataAdviseHolder) )
    let ( status :: <HRESULT>, result :: <LPDATAADVISEHOLDER> ) =
      CreateDataAdviseHolder();
    check-ole-status(status, "CreateDataAdviseHolder", this);
    obj.container-IDataAdviseHolder := result;
  end if;
  
  // pass on to the DataAdviseHolder
  /* return */
  IDataAdviseHolder/Advise(obj.container-IDataAdviseHolder, this,
			   pFormatetc, advf, pAdvSink)
end method IDataObject/DAdvise;


// Returns the data in the format specified in pformatetcIn.

define method IDataObject/GetData(this :: <CDataObject>,
				  pformatetcIn :: <LPFORMATETC>,
				  pmedium :: <LPSTGMEDIUM> )
		=> status :: <HRESULT>;
  
  Output-Debug-String("IDataObject/GetData\r\n");
  if ( null-pointer?(pmedium) | null-pointer?(pformatetcIn) )
    $E-INVALIDARG
  else
    OLE-part-get-data(this.get-obj, pformatetcIn, pmedium)
  end if
end method IDataObject/GetData;


// Breaks down an Advise connection.

define method IDataObject/DUnadvise(this :: <CDataObject>,
				    advise-connection-ID :: <integer>) 
	=> status :: <HRESULT>;

  Output-Debug-String("IDataObject/DUnadvise\r\n");

  let holder = this.get-obj.container-IDataAdviseHolder;
  if ( null?(holder) ) // not sure why this would happen, but it did once.
    $OLE-E-ADVISENOTSUPPORTED
  else
    IDataAdviseHolder/Unadvise(holder, advise-connection-ID);
  end if
end method IDataObject/DUnadvise;


// Called to get a data format in a caller supplied location.
// Note that this operation is not applicable to metafiles; the only relevant
// storage media are $TYMED-ISTORAGE, $TYMED-ISTREAM, and $TYMED-FILE. 

define method IDataObject/GetDataHere(this :: <CDataObject>,
				      pformatetc :: <LPFORMATETC>,
				      pmedium :: <LPSTGMEDIUM>)
 => (status :: <HRESULT>);

  Output-Debug-String("IDataObject/GetDataHere\r\n");

  // This method implementation adapted from:
  //	\INOLE\CODE\CHAP19\POLYLINE\IDATAOBJ.CPP  (dated 05/03/95)

  /*
   * The only reasonable time this is called is for
   * CFSTR_EMBEDSOURCE and TYMED_ISTORAGE (and later for
   * CFSTR_LINKSOURCE).  This means the same as
   * IPersistStorage::Save.
   */ 

  // Aspect is unimportant to us here, as is lindex and ptd.
  if ( logtest($TYMED-ISTORAGE, pformatetc.tymed-value)
	& (pformatetc.cfFormat-value =
	     RegisterClipboardFormat("Embed Source")) )
    // We have an IStorage we can write into.
    pmedium.tymed-value := $TYMED-ISTORAGE;
    pmedium.pUnkForRelease-value := $null-interface;
    let istorage = this.get-obj.server-IPersistStorage;
    let status =
      IPersistStorage/Save(istorage, pmedium.u-value.pstg-value, #f);
    IPersistStorage/SaveCompleted(istorage, null-pointer(<LPSTORAGE>));
    status
  else
    $DATA-E-FORMATETC
  end if
end method IDataObject/GetDataHere;


// Returns a FORMATETC that is equivalent to the one passed in.

define method IDataObject/GetCanonicalFormatEtc(this :: <CDataObject>,
					pformatetc-in :: <LPFORMATETC>,
					pformatetc-out :: <LPFORMATETC>)
		=> status :: <HRESULT>;

  block(return)
	
    Output-Debug-String("IDataObject/GetCanonicalFormatEtc\r\n");
    
    if ( null?(pformatetc-out) )
      return( $E-INVALIDARG  ); // invalid argument
    end if;

    /* OLE2NOTE: we must make sure to set all out parameters to NULL. */ 
    pformatetc-out.ptd-value := null-pointer(<LPDVTARGETDEVICE>);

    if ( null?(pformatetc-in) )
      return( $E-INVALIDARG  );
    end if;

    // OLE2NOTE: we must validate that the format requested is supported
    let hresult :: <HRESULT> = IDataObject/QueryGetData(this, pformatetc-in);
    if ( FAILED?(hresult) )
      return( hresult );
    end if;

    /* OLE2NOTE: an app that is insensitive to target device (as
    **    SimpSvr is) should fill in the lpformatOut parameter
    **    but NULL out the "ptd" field; it should return NOERROR if the
    **    input formatetc->ptd what non-NULL. this tells the caller
    **    that it is NOT necessary to maintain a separate screen
    **    rendering and printer rendering. if should return
    **    DATA_S_SAMEFORMATETC if the input and output formatetc's are
    **    identical.
    */ 

    %memcpy(pformatetc-out, pformatetc-in, size-of(<FORMATETC>));

    if ( null?(pformatetc-in.ptd-value) )
      return( $DATA-S-SAMEFORMATETC  ); // Use same formatetc as was passed.
    else
      pformatetc-out.ptd-value := null-pointer(<LPDVTARGETDEVICE>);
      return( $S-OK );
    end if;
 end block
end method IDataObject/GetCanonicalFormatEtc;


// Called to set the data for the object.

define method IDataObject/SetData(this :: <CDataObject>,
				  pformatetc :: <LPFORMATETC>, // data format
				  pmedium :: <LPSTGMEDIUM>, // data location
				  release? :: <boolean>)
		=> status :: <HRESULT>;
  Output-Debug-String("IDataObject/SetData\r\n");

  let status = OLE-part-set-data(this.get-obj, pformatetc, pmedium);
  if ( release? & SUCCEEDED?(status) )
    ReleaseStgMedium(pmedium);
  end if;
  status
end method IDataObject/SetData;

// Default method just reports "not implemented"
define method OLE-part-set-data (obj :: <basic-ole-server>,
				 pformatetc, pmedium)
 => status :: <HRESULT>;
  $E-NOTIMPL
end method;


// Enumerates the formats supported by this object.
/* // This initial version just defers to the registry.
   // It has been replaced by a new version in "enums.dylan" in order to
   // simplify the registry and to be user-extensible.
define method IDataObject/EnumFormatEtc(this :: <CDataObject>,
					direction /* :: <unsigned-fixnum> */ )
			  
 => ( status :: <HRESULT>,
      penumFormatEtc :: <Interface> /* LPENUMFORMATETC */ );
  Output-Debug-String("IDataObject/EnumFormatEtc\r\n");
  values( $OLE-S-USEREG, $NULL-interface )
end method IDataObject/EnumFormatEtc;
*/

// Returns an enumerator for all of the advises set up on this data object.
// This just delegates to the DataAdviseHolder.

define method IDataObject/EnumDAdvise(this :: <CDataObject>)
	=> ( status :: <HRESULT>,
	     enumerator :: <Interface> /* LPENUMSTATDATA */ );

  Output-Debug-String("IDataObject/EnumDAdvise\r\n");

  IDataAdviseHolder/EnumAdvise(this.get-obj.container-IDataAdviseHolder)
end method IDataObject/EnumDAdvise;
