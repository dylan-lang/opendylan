Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
//
// CDataObject::QueryGetData
//
// Purpose:
//
//      Called to determine if our object supports a particular
//      FORMATETC.
//
// Parameters:
//
//      LPFORMATETC pformatetc  - Pointer to the FORMATETC being queried for.
//
// Return Value:
//
//      DATA_E_FORMATETC    - The FORMATETC is not supported
//      S_OK                - The FORMATETC is supported.
//
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//
//********************************************************************



define method IDataObject/QueryGetData(this :: <CDataObject>,
				       pformatetc :: <LPFORMATETC> )
			=> status :: <HRESULT>;
	
  OutputDebugString("In CDataObject::QueryGetData\r\n");

  // check the validity of the formatetc.
  if ( (pformatetc.cfFormat-value = $CF-METAFILEPICT)
	& (pformatetc.dwAspect-value = $DVASPECT-CONTENT)
	& (logand(pformatetc.tymed-value,
		  logior($TYMED-MFPICT, $TYMED-ENHMF)) ~= 0) )
    $S-OK
  else $DATA-E-FORMATETC
  end if
end method IDataObject/QueryGetData;

//**********************************************************************
//
// CDataObject::DAdvise
//
// Purpose:
//
//      Called by the container when it would like to be notified of
//      changes in the object data.
//
// Parameters:
//
//      FORMATETC FAR* pFormatetc   - The format the container is interested in.
//
//      DWORD advf                  - The type of advise to be set up.
//
//      LPADVISESINK pAdvSink       - Pointer to the containers IAdviseSink
//
//      DWORD FAR* pdwConnection    - Out parameter to return a unique connection id.
//
// Return Value:
//
//      passed on from IDataAdviseHolder
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CreateDataAdviseHolder      OLE API
//      IDataAdviseHolder::Advise   OLE API
//
// Comments:
//
//
//********************************************************************



define method IDataObject/DAdvise(this :: <CDataObject>,
				  pFormatetc :: <LPFORMATETC>,
			  advf,// :: <integer> or <machine-integer>
				  pAdvSink :: <LPADVISESINK>)
	=> ( status :: <HRESULT>, dwConnection :: <integer> );

  OutputDebugString("In CDataObject::DAdvise\r\n");

  // if no DataAdviseHolder has been created, then create one.
  if ( null?(this.m-lpObj.m-lpDataAdviseHolder) )
    let ( status :: <HRESULT>, result :: <LPDATAADVISEHOLDER> ) =
      CreateDataAdviseHolder();
    this.m-lpObj.m-lpDataAdviseHolder := result;
  end if;
  
  // pass on to the DataAdviseHolder
  /* return */
  IDataAdviseHolder/Advise(this.m-lpObj.m-lpDataAdviseHolder, this,
			   pFormatetc, advf, pAdvSink)
end method IDataObject/DAdvise;

//**********************************************************************
//
// CDataObject::GetData
//
// Purpose:
//
//      Returns the data in the format specified in pformatetcIn.
//
// Parameters:
//
//      LPFORMATETC pformatetcIn    -   The format requested by the caller
//
//      LPSTGMEDIUM pmedium         -   The medium requested by the caller
//
// Return Value:
//
//      DATA_E_FORMATETC    - Format not supported
//      S_OK                - Success
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      CSimpSvrObj::GetMetaFilePict()  OBJ.CPP
//      ResultFromScode                 OLE API
//
// Comments:
//
//
//********************************************************************



define method IDataObject/GetData(this :: <CDataObject>,
				  pformatetcIn :: <LPFORMATETC>,
				  pmedium :: <LPSTGMEDIUM> )
		=> status :: <HRESULT>;
	
  OutputDebugString("In CDataObject::GetData\r\n");

  // Check to the FORMATETC and fill pmedium if valid.
  if ( (pformatetcIn.cfFormat-value = $CF-METAFILEPICT)
	& (pformatetcIn.dwAspect-value = $DVASPECT-CONTENT)
	& (logand(pformatetcIn.tymed-value,
		  logior($TYMED-MFPICT, $TYMED-ENHMF)) ~= 0) )

    let kind =
      // Use new-style metafile if the container supports it.
      if ( logand(pformatetcIn.tymed-value, $TYMED-ENHMF) ~= 0 )
	$TYMED-ENHMF
      else
	$TYMED-MFPICT
      end if;
    let hmfPict :: <HANDLE> = GetMetaFilePict(this.m-lpObj, kind);
    pmedium.tymed-value := kind;
    pmedium.u-value.hGlobal-value := hmfPict;
    pmedium.pUnkForRelease-value := $null-interface;
    $S-OK
  else $DATA-E-FORMATETC
  end if
end method IDataObject/GetData;

//**********************************************************************
//
// CDataObject::DUnadvise
//
// Purpose:
//
//      Breaks down an Advise connection.
//
// Parameters:
//
//      DWORD dwConnection  - Advise connection ID.
//
// Return Value:
//
//      Returned from the DataAdviseHolder.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IDataAdviseHolder::Unadvise OLE
//
// Comments:
//
//
//********************************************************************



define method IDataObject/DUnadvise(this :: <CDataObject>,
				    dwConnection :: <integer>) 
	=> status :: <HRESULT>;

  OutputDebugString("In CDataObject::DUnadvise\r\n");

  let holder = this.m-lpObj.m-lpDataAdviseHolder;
  if ( null?(holder) )
    $OLE-E-ADVISENOTSUPPORTED
  else
    IDataAdviseHolder/Unadvise(holder, dwConnection)
  end if
end method IDataObject/DUnadvise;

//**********************************************************************
//
// CDataObject::GetDataHere
//
// Purpose:
//
//      Called to get a data format in a caller supplied location
//
// Parameters:
//
//      LPFORMATETC pformatetc  - FORMATETC requested
//
//      LPSTGMEDIUM pmedium     - Medium to return the data
//
// Return Value:
//
//      DATA_E_FORMATETC    - We don't support the requested format
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      In this simple implementation, we don't really support this
//      method, we just always return DATA_E_FORMATETC.
//
//********************************************************************



define method IDataObject/GetDataHere(this :: <CDataObject>,
				      pformatetc :: <LPFORMATETC>,
	pmedium :: <LPSTGMEDIUM> ) => status :: <HRESULT>;

  OutputDebugString("In CDataObject::GetDataHere\r\n");
  $DATA-E-FORMATETC
end method IDataObject/GetDataHere;

//**********************************************************************
//
// CDataObject::GetCanonicalFormatEtc
//
// Purpose:
//
//      Returns a FORMATETC that is equivalent to the one passed in.
//
// Parameters:
//
//      LPFORMATETC pformatetc      - FORMATETC to be tested.
//
//      LPFORMATETC pformatetcOut   - Out ptr for returned FORMATETC.
//
// Return Value:
//
//      DATA_S_SAMEFORMATETC    - Use the same formatetc as was passed.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CoGetMalloc                 OLE API
//      IMalloc::Alloc              OLE
//      IMalloc::Release            OLE
//      _fmemcpy                    C run-time
//
// Comments:
//
//
//********************************************************************



define method IDataObject/GetCanonicalFormatEtc(this :: <CDataObject>,
				    pformatetc :: <LPFORMATETC>,
				    pformatetcOut :: <LPFORMATETC>)
		=> status :: <HRESULT>;

  block(return)
	
    OutputDebugString("In CDataObject::GetCanonicalFormatEtc\r\n");
    
    if ( null?(pformatetcOut) )
      return( $E-INVALIDARG  );
    end if;

    /* OLE2NOTE: we must make sure to set all out parameters to NULL. */ 
    pformatetcOut.ptd-value := null-pointer(<LPDVTARGETDEVICE>);

    if ( null?(pformatetc) )
      return( $E-INVALIDARG  );
    end if;

    // OLE2NOTE: we must validate that the format requested is supported
    let hresult :: <HRESULT> = IDataObject/QueryGetData(this, pformatetc);
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

    copy-into!(pformatetcOut, pformatetc, size-of(<FORMATETC>));

    if ( null?(pformatetc.ptd-value) )
      return( $DATA-S-SAMEFORMATETC  ); 
    else
		
      pformatetcOut.ptd-value := null-pointer(<LPDVTARGETDEVICE>);
      return( $NOERROR );
    end if;
 end block
end method IDataObject/GetCanonicalFormatEtc;

//**********************************************************************
//
// CDataObject::SetData
//
// Purpose:
//
//      Called to set the data for the object.
//
// Parameters:
//
//      LPFORMATETC pformatetc      - the format of the data being passed
//
//      STGMEDIUM FAR * pmedium     - the location of the data.
//
//      BOOL fRelease               - Defines the ownership of the medium
//
// Return Value:
//
//      DATA_E_FORMATETC    - Not a valid FORMATETC for this object
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      This simple object does not support having its data set, so an
//      error value is always returned.
//
//********************************************************************



define method IDataObject/SetData(this :: <CDataObject>,
				  pformatetc :: <LPFORMATETC>,
				  pmedium :: <STGMEDIUM>,
				  fRelease :: <boolean>)
		=> status :: <HRESULT>;
  OutputDebugString("In CDataObject::SetData\r\n");
  $DATA-E-FORMATETC
end method IDataObject/SetData;

//**********************************************************************
//
// CDataObject::EnumFormatEtc
//
// Purpose:
//
//      Enumerates the formats supported by this object.
//
// Parameters:
//
//      DWORD dwDirection                       - Order of enumeration.
//
//      LPENUMFORMATETC FAR* ppenumFormatEtc    - Place to return a pointer
//                                                to the enumerator.
//
// Return Value:
//
//      OLE_S_USEREG    - Indicates that OLE should consult the REG DB
//                        to enumerate the formats.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//
//********************************************************************



define method IDataObject/EnumFormatEtc(this :: <CDataObject>,
					dwDirection :: <integer> )
			  
 => ( status :: <HRESULT>,
      penumFormatEtc :: <Interface> /* LPENUMFORMATETC */ );

  OutputDebugString("In CDataObject::EnumFormatEtc\r\n");
  values( $OLE-S-USEREG, $NULL-interface )
end method IDataObject/EnumFormatEtc;

//**********************************************************************
//
// CDataObject::EnumDAdvise
//
// Purpose:
//
//      Returns an enumerator that enumerates all of the advises
//      set up on this data object.
//
// Parameters:
//
//      LPENUMSTATDATA FAR* ppenumAdvise    - An out ptr in which to
//                                            return the enumerator.
//
// Return Value:
//
//      Passed back from IDataAdviseHolder::EnumAdvise
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      IDAtaAdviseHolder::EnumAdvise   OLE
//
// Comments:
//
//      This just delegates to the DataAdviseHolder.
//
//********************************************************************



define method IDataObject/EnumDAdvise(this :: <CDataObject>)
	=> ( status :: <HRESULT>,
	     ppenumAdvise :: <Interface> /* LPENUMSTATDATA */ );

  OutputDebugString("In CDataObject::EnumDAdvise\r\n");

  IDataAdviseHolder/EnumAdvise(this.m-lpObj.m-lpDataAdviseHolder)

end method IDataObject/EnumDAdvise;
