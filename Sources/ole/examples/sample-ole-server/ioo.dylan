Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $VERB-OPEN = 1;


//**********************************************************************
//
// COleObject::SetClientSite
//
// Purpose:
//
//      Called to notify the object of it's client site.
//
// Parameters:
//
//      LPOLECLIENTSITE pClientSite     - ptr to new client site
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IOleClientSite::Release     Container
//      IOleClientSite::AddRef      Container
//
// Comments:
//
//
//********************************************************************


define method IOleObject/SetClientSite(this :: <COleObject>,
				       pClientSite :: <LPOLECLIENTSITE>)
	=> status :: <HRESULT>;

  OutputDebugString("In COleObject::SetClientSite\r\n");

  // if we already have a client site, release it.
  Release(this.m-lpObj.m-lpOleClientSite);

  // store copy of the client site.
  this.m-lpObj.m-lpOleClientSite := pClientSite;

  // AddRef it so it doesn't go away.
  AddRef(this.m-lpObj.m-lpOleClientSite);

  /* return */ $S-OK;

end method IOleObject/SetClientSite;

//**********************************************************************
//
// COleObject::Advise
//
// Purpose:
//
//      Called to set up an advise on the OLE object.
//
// Parameters:
//
//      LPADVISESINK pAdvSink       - ptr to the Advise Sink for notification
//
//      DWORD FAR* pdwConnection    - place to return the connection ID.
//
// Return Value:
//
//      Passed back from IOleAdviseHolder::Advise.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CreateOleAdviseHolder       OLE API
//      IOleAdviseHolder::Advise    OLE
//
// Comments:
//
//
//********************************************************************


define method IOleObject/Advise(this :: <COleObject>,
				pAdvSink :: <LPADVISESINK>)
	=> (status :: <HRESULT>, dwConnection :: <integer>);

  OutputDebugString("In COleObject::Advise\r\n");

  let obj :: <CSimpSvrObj> = this.m-lpObj;

  // if we haven't made an OleAdviseHolder yet, make one.
  if ( null?(obj.m-lpOleAdviseHolder) )
    let ( status, holder ) = CreateOleAdviseHolder();
    check-ole-status(status, "CreateOleAdviseHolder", this);
    obj.m-lpOleAdviseHolder := holder;
  end if;

  // pass this call onto the OleAdviseHolder.
  /* return */
  IOleAdviseHolder/Advise(obj.m-lpOleAdviseHolder, pAdvSink);
end method IOleObject/Advise;

//**********************************************************************
//
// COleObject::SetHostNames
//
// Purpose:
//
//      Called to pass strings for Window titles.
//
// Parameters:
//
//      LPCSTR szContainerApp   -   ptr to string describing Container App
//
//      LPCSTR szContainerObj   -   ptr to string describing Object
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      This routine is called so that the server application can
//      set the window title appropriately.
//
//********************************************************************

define method IOleObject/SetHostNames(this :: <COleObject>,
			   szContainerApp :: <LPCOLESTR>,
			   szContainerObj :: <LPCOLESTR>)
		=> status :: <HRESULT>;

  OutputDebugString("In COleObject::SetHostNames\r\n");

  /* return */ $S-OK
end method IOleObject/SetHostNames;

//**********************************************************************
//
// COleObject::DoVerb
//
// Purpose:
//
//      Called by the container application to invoke a verb.
//
// Parameters:
//
//      LONG iVerb                  - The value of the verb to be
//                                    invoked.
//
//      LPMSG lpmsg                 - The message that caused the
//                                    verb to be invoked.
//
//      LPOLECLIENTSITE pActiveSite - Ptr to the active client site.
//
//      LONG lindex                 - Used in extended layout
//
//      HWND hwndParent             - This should be the window handle of
//                                    the window in which we are contained.
//                                    This value could be used to "fake"
//                                    inplace activation in a manner similar
//                                    to Video for Windows in OLE 1.0.
//
//      LPCRECT lprcPosRect         - The rectangle that contains the object
//                                    within hwndParent.  Also used to
//                                    "fake" inplace activation.
//
// Return Value:
//
//      OLE_E_NOTINPLACEACTIVE      - Returned if attempted to undo while not
//                                    inplace active.
//      S_OK
//
// Function Calls:
//      Function                            Location
//
//      OutputDebugString                   Windows API
//      ShowWindow                          Windows API
//      CSimpSvrObj::DoInPlaceActivate      OBJ.CPP
//      CSimpSvrObj::DoInPlaceHide          OBJ.CPP
//      COleObject::OpenEdit                IOO.CPP
//      CSimpSvrDoc::GethDocWnd             DOC.H
//      COleInPlaceObj::InPlaceDeactivate   IOIPO.CPP
//
// Comments:
//
//      Be sure to look at TECHNOTES.WRI included with the OLE
//      SDK for a description of handling the inplace verbs
//      properly.
//
//********************************************************************


define method IOleObject/DoVerb(this :: <COleObject>,
				iVerb :: <integer>,
   // Don't bother specializing on arguments that are not directly used here.
				lpmsg /* :: <LPMSG> */,
				pActiveSite /* :: <LPOLECLIENTSITE> */,
				lindex /* :: <signed-long> */,
				hwndParent /* :: <HWND> */,
				lprcPosRect /* :: <LPCRECT> */ )
	=> status :: <HRESULT>;

 block(return)
	
   OutputDebugString("In COleObject::DoVerb\r\n");

   select ( iVerb )
		
     $OLEIVERB-SHOW, $OLEIVERB-PRIMARY => 
       if ( this.m-fOpen )
	 SetFocus(GethAppWnd(this.m-lpObj.m-lpDoc)); 
       elseif ( DoInPlaceActivate(this.m-lpObj, iVerb) = #f )
	 OpenEdit(this, pActiveSite);
       end if;
       
     $OLEIVERB-UIACTIVATE => 
       if ( this.m-fOpen )
	 return( $E-FAIL );
       end if;

       // inplace activate
       unless ( DoInPlaceActivate(this.m-lpObj, iVerb) )
	 return( $E-FAIL );
       end unless;

     $OLEIVERB-DISCARDUNDOSTATE => 
       // don't have to worry about this situation as we don't
       // support an undo state.
       unless ( this.m-lpObj.m-fInPlaceActive )
	 return( $OLE-E-NOT-INPLACEACTIVE );
       end unless;

     $OLEIVERB-HIDE => 
       // if inplace active, do an "inplace" hide, otherwise
       // just hide the app window.
       if ( this.m-lpObj.m-fInPlaceActive )
				
	 DeactivateUI(this.m-lpObj);
	 DoInPlaceHide(this.m-lpObj);
				
       else
	 HideAppWnd(GetApp(this.m-lpObj.m-lpDoc));
       end if;

     $OLEIVERB-OPEN, $VERB-OPEN => 
       // if inplace active, deactivate
       if ( this.m-lpObj.m-fInPlaceActive )
	 IOleInPlaceObject/InPlaceDeactivate(this.m-lpObj.m-OleInPlaceObject);
       end if;

       // open into another window.
       OpenEdit(this, pActiveSite);

     otherwise => 
       if ( iVerb < 0 )
	 return( $E-FAIL );
       end if;
		
   end select;

   return( $S-OK );
 end block;
end method IOleObject/DoVerb;

//**********************************************************************
//
// COleObject::GetExtent
//
// Purpose:
//
//      Returns the extent of the object.
//
// Parameters:
//
//      DWORD dwDrawAspect  - The aspect in which to get the size.
//
//      LPSIZEL lpsizel     - Out ptr to return the size.
//
// Return Value:
//
//
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      XformWidthInPixelsToHimetric    OLESTD
//      XformHeightInPixelsToHimetric   OLESTD
//
// Comments:
//
//
//********************************************************************


define method IOleObject/GetExtent(this :: <COleObject>,
			dwDrawAspect :: <integer>,
			lpsizel :: <LPSIZEL>) => status :: <HRESULT>;

  OutputDebugString("In COleObject::GetExtent\r\n");

  let sc :: <SCODE> =  $E-FAIL;

  // Only DVASPECT_CONTENT is supported....
  if ( dwDrawAspect = $DVASPECT-CONTENT )
		
    sc := $S-OK;

    // return the correct size in HIMETRIC...
    let size :: <LPPOINT> = this.m-lpObj.m-pSize;
    let ( width, height ) = pixels-to-himetric (size.x-value, size.y-value);
    lpsizel.cx-value := width;
    lpsizel.cy-value := height;
  end if;

 /* return */ sc

end method IOleObject/GetExtent;

//**********************************************************************
//
// COleObject::Update
//
// Purpose:
//
//      Called to get the most up to date data
//
// Parameters:
//
//      None
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                            Location
//
//      OutputDebugString                   Windows API
//      CSimpSvrObj::SendOnDataChange       OBJ.CPP
//
// Comments:
//
//
//********************************************************************


define method IOleObject/Update(this :: <COleObject>) => status :: <HRESULT>;

  OutputDebugString("In COleObject::Update\r\n");

  // force an update
  SendOnDataChange(this.m-lpObj);

  /* return */ $S-OK
end method IOleObject/Update;

//**********************************************************************
//
// COleObject::Close
//
// Purpose:
//
//      Called when the OLE object needs to be closed
//
// Parameters:
//
//      DWORD dwSaveOption  - Flags to instruct the server how to prompt
//                            the user.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CSimpSvrDoc::CloseObject    DOC.CPP
//
// Comments:
//
//
//********************************************************************


define method IOleObject/Close(this :: <COleObject>,
			       dwSaveOption) => status :: <HRESULT>;

  OutputDebugString("In COleObject::Close\r\n");

  // delegate to the document object.
  CloseObject(this.m-lpObj.m-lpDoc);

  $S-OK
end method IOleObject/Close;

//**********************************************************************
//
// COleObject::Unadvise
//
// Purpose:
//
//      Breaks down an OLE advise that has been set up on this object.
//
// Parameters:
//
//      DWORD dwConnection  - Connection that needs to be broken down
//
// Return Value:
//
//      Passed back from IOleAdviseHolder::Unadvise
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IOleAdviseHolder::Unadvise  OLE
//
// Comments:
//
//
//********************************************************************


define method IOleObject/Unadvise(this :: <COleObject>,
				  dwConnection :: <integer>)
 => status :: <HRESULT>;
	
  OutputDebugString("In COleObject::Unadvise\r\n");

  // pass on to OleAdviseHolder.
  /* return */ 
  IOleAdviseHolder/Unadvise(this.m-lpObj.m-lpOleAdviseHolder, dwConnection);
end method IOleObject/Unadvise;

//**********************************************************************
//
// COleObject::EnumVerbs
//
// Purpose:
//
//      Enumerates the verbs associated with this object.
//
// Parameters:
//
//      LPENUMOLEVERB FAR* ppenumOleVerb    - Out ptr in which to return
//                                            the enumerator
//
// Return Value:
//
//      OLE_S_USEREG    - Instructs OLE to use the verbs found in the
//                        REG DB for this server.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      In a .DLL, an application cannot return OLE_S_USEREG.  This is
//      due to the fact that the default object handler is not being
//      used, and the container is really making direct function calls
//      into the server .DLL.
//
//********************************************************************


define method IOleObject/EnumVerbs(this :: <COleObject>)
	=> ( status :: <HRESULT>, ppenumOleVerb :: <LPENUMOLEVERB> );

  OutputDebugString("In COleObject::EnumVerbs\r\n");

  values( $OLE-S-USEREG, $null-interface )

end method IOleObject/EnumVerbs;

//**********************************************************************
//
// COleObject::GetClientSite
//
// Purpose:
//
//      Called to get the current client site of the object.
//
// Parameters:
//
//      LPOLECLIENTSITE FAR* ppClientSite   - Out ptr in which to return the
//                                            client site.
//
// Return Value:
//
//      S_OK
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


define method IOleObject/GetClientSite(this :: <COleObject>)
	=> ( status :: <HRESULT>, pClientSite :: <Interface>);

  OutputDebugString("In COleObject::GetClientSite\r\n");
  /* return */ values( $S-OK, this.m-lpObj.m-lpOleClientSite )
end method IOleObject/GetClientSite;

//**********************************************************************
//
// COleObject::SetMoniker
//
// Purpose:
//
//      Used to set the objects moniker
//
// Parameters:
//
//      DWORD dwWhichMoniker    - Type of moniker being set
//
//      LPMONIKER pmk           - Pointer to the moniker
//
// Return Value:
//
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


define method IOleObject/SetMoniker(this :: <COleObject>,
			 dwWhichMoniker /* :: <integer> */,
			 pmk :: <LPMONIKER>) => status :: <HRESULT>;

  block(return)
	
    OutputDebugString("In COleObject::SetMoniker\r\n");

    if ( null?(GetOleClientSite(this.m-lpObj)) )
      return($E-FAIL);
    end if;

    let ( status :: <HRESULT>, lpmk :: <Interface> ) =
      IOleClientSite/GetMoniker(GetOleClientSite(this.m-lpObj),
				$OLEGETMONIKER-ONLYIFTHERE,
				$OLEWHICHMK-OBJFULL);
    if ( FAILED?(status) )
      return(status);
    end if;

    if ( ~ null?(GetOleAdviseHolder(this.m-lpObj)) )
      IOleAdviseHolder/SendOnRename(GetOleAdviseHolder(this.m-lpObj), lpmk);
    end if;

    let ( status :: <HRESULT>, lpRot :: <LPRUNNINGOBJECTTABLE> ) =
      GetRunningObjectTable(0);
    if ( status = $NOERROR )
		
      if ( this.m-lpObj.m-dwRegister ~= 0 )
	IRunningObjectTable/Revoke(lpRot, this.m-lpObj.m-dwRegister);
      end if;

      let (  status2 :: <HRESULT>, x :: <integer> ) =
	IRunningObjectTable/Register(lpRot, 0, this.m-lpObj, lpmk);
      this.m-lpObj.m-dwRegister := x;
      Release(lpRot);
    end if;

    return( $S-OK );
  end block;
end method IOleObject/SetMoniker;

//**********************************************************************
//
// COleObject::GetMoniker
//
// Purpose:
//
////
// Parameters:
//
//      DWORD dwAssign          - Assignment for the moniker
//
//      DWORD dwWhichMoniker    - Which moniker to return
//
//      LPMONIKER FAR* ppmk     - An out ptr to return the moniker
//
// Return Value:
//
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

define method IOleObject/GetMoniker(this :: <COleObject>,
				    dwAssign /* :: <integer> */,
				    dwWhichMoniker /* :: <integer> */ )
		=> ( status :: <HRESULT> , ppmk :: <LPMONIKER> );

  OutputDebugString("In COleObject::GetMoniker\r\n");

  /* return */
  IOleClientSite/GetMoniker(GetOleClientSite(this.m-lpObj),
			    $OLEGETMONIKER-ONLYIFTHERE, $OLEWHICHMK-OBJFULL);

end method IOleObject/GetMoniker;

//**********************************************************************
//
// COleObject::InitFromData
//
// Purpose:
//
//      Initialize the object from the passed pDataObject.
//
// Parameters:
//
//      LPDATAOBJECT pDataObject    - Pointer to data transfer object
//                                    to be used in the initialization
//
//      BOOL fCreation              - TRUE if the object is currently being
//                                    created.
//
//      DWORD dwReserved            - Reserved
//
// Return Value:
//
//      S_FALSE
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      We don't support this functionality, so we will always return
//      error.
//
//********************************************************************


define method IOleObject/InitFromData(this :: <COleObject>,
				      pDataObject :: <LPDATAOBJECT>,
				      fCreation :: <boolean>,
				      dwReserved)
		=> status :: <HRESULT>;

  OutputDebugString("In COleObject::InitFromData\r\n");

  /* return */ $S-FALSE
end method IOleObject/InitFromData;


//**********************************************************************
//
// COleObject::GetClipboardData
//
// Purpose:
//
//      Returns an IDataObject that is the same as doing an OleSetClipboard
//
// Parameters:
//
//      DWORD dwReserved                - Reserved
//
//      LPDATAOBJECT FAR* ppDataObject  - Out ptr for the Data Object.
//
// Return Value:
//
//      OLE_E_NOTSUPPORTED
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Support of this method is optional.
//
//********************************************************************


define method IOleObject/GetClipboardData(this :: <COleObject>, dwReserved)
					  
	=> (status :: <HRESULT>, pDataObject :: <Interface>)

  OutputDebugString("In COleObject::GetClipboardData\r\n");
  values( $E-NOTIMPL , $NULL-interface )
end method IOleObject/GetClipboardData;

//**********************************************************************
//
// COleObject::IsUpToDate
//
// Purpose:
//
//      Determines if an object is up to date
//
// Parameters:
//
//      None
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Our embedded object is always up to date.  This function is
//      particularly useful in linking situations.
//
//********************************************************************

define method IOleObject/IsUpToDate(this :: <COleObject>)
	=> status :: <HRESULT>;

  OutputDebugString("In COleObject::IsUpToDate\r\n");
  $S-OK
end method IOleObject/IsUpToDate;

//**********************************************************************
//
// COleObject::GetUserClassID
//
// Purpose:
//
//      Returns the applications CLSID
//
// Parameters:
//
//      CLSID FAR* pClsid   - Out ptr to return the CLSID
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CPersistStorage::GetClassID IPS.CPP
//
// Comments:
//
//      This function is just delegated to IPS::GetClassID.
//
//********************************************************************


define method IOleObject/GetUserClassID(this :: <COleObject>,
					pClsid /* :: <LPCLSID> */ )
	=> status :: <HRESULT>;

  OutputDebugString("In COleObject::GetUserClassID\r\n");

  IPersist/GetClassID(this.m-lpObj.m-PersistStorage, pClsid);
  $S-OK
end method IOleObject/GetUserClassID;

//**********************************************************************
//
// COleObject::GetUserType
//
// Purpose:
//
//      Used to get a user presentable id for this object
//
// Parameters:
//
//      DWORD dwFormOfType      - The ID requested
//
//      LPSTR FAR* pszUserType  - Out ptr to return the string
//
// Return Value:
//
//      OLE_S_USEREG    - Use the reg db to get these entries.
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

define method IOleObject/GetUserType(this :: <COleObject>, dwFormOfType )
	=> (status :: <HRESULT>, pszUserType :: <LPOLESTR>);

  OutputDebugString("In COleObject::GetUserType\r\n");

  values( $OLE-S-USEREG, $NULL-OLESTR )
end method IOleObject/GetUserType;

//**********************************************************************
//
// COleObject::SetExtent
//
// Purpose:
//
//      Called to set the extent of the object.
//
// Parameters:
//
//      DWORD dwDrawAspect  - Aspect to have its size set
//
//      LPSIZEL lpsizel     - New size of the object.
//
// Return Value:
//
//      E_NOTIMPL   - This function is not curently implemented.
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


define method IOleObject/SetExtent(this :: <COleObject>,
				   dwDrawAspect /* :: <integer> */,
				   lpsizel /* :: <LPSIZEL> */)
	=> status :: <HRESULT>;

  OutputDebugString("In COleObject::SetExtent\r\n");
  $E-NOTIMPL
end method IOleObject/SetExtent;

//**********************************************************************
//
// COleObject::EnumAdvise
//
// Purpose:
//
//      Returns an enumerate which enumerates the outstanding advises
//      associated with this OLE object.
//
// Parameters:
//
//      LPENUMSTATDATA FAR* ppenumAdvise - Out ptr in which to return
//                                         the enumerator.
//
// Return Value:
//
//      Passed on from IOleAdviseHolder::EnumAdvise.
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      IOleAdviseHolder::EnumAdvise    OLE
//
// Comments:
//
//
//********************************************************************


define method IOleObject/EnumAdvise(this :: <COleObject>)
	=> ( status :: <HRESULT>,  penumAdvise :: <Interface>);
	
  OutputDebugString("In COleObject::EnumAdvise\r\n");

  // pass on to the OLE Advise holder.
  IOleAdviseHolder/EnumAdvise(this.m-lpObj.m-lpOleAdviseHolder)
end method IOleObject/EnumAdvise;

//**********************************************************************
//
// COleObject::GetMiscStatus
//
// Purpose:
//
//      Return status information about the object
//
// Parameters:
//
//      DWORD dwAspect          - Aspect interested in.
//
//      DWORD FAR* pdwStatus    - Out ptr in which to return the bits.
//
// Return Value:
//
//      CO_E_READREGDB  - Tell the library to use the reg DB.
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


define method IOleObject/GetMiscStatus(this :: <COleObject>,
				       dwAspect /* :: <integer> */)
	=> ( status :: <HRESULT>, dwStatus :: <integer> );

  OutputDebugString("In COleObject::GetMiscStatus\r\n");
  values( $OLE-S-USEREG, 0 )
end method IOleObject/GetMiscStatus;

//**********************************************************************
//
// COleObject::SetColorScheme
//
// Purpose:
//
//      Used to set the palette for the object to use.
//
// Parameters:
//
//      LPLOGPALETTE lpLogpal   - Pointer to the LOGPALETTE to be used.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      This server ignores this method.
//
//********************************************************************


define method IOleObject/SetColorScheme(this :: <COleObject>,
					lpLogpal :: <LPLOGPALETTE>)
	=> status :: <HRESULT>;

  OutputDebugString("In COleObject::SetColorScheme\r\n");
  $S-OK
end method IOleObject/SetColorScheme;

//**********************************************************************
//
// COleObject::OpenEdit
//
// Purpose:
//
//      Used to Open the object into a seperate window.
//
// Parameters:
//
//      LPOLECLIENTSITE pActiveSite - Pointer to the Active clientsite.
//
// Return Value:
//
//      None.
//
// Function Calls:
//      Function                        Location
//
//      IOleClientSite::OnShowWindow    Container
//      ShowWindow                      Windows API
//      UpdateWindow                    Windows API
//      OutputDebugString               Windows API
//      CSimpSvrDoc::GethAppWnd         DOC.H
//      CSimpSvrDoc::GethHatchWnd       DOC.H
//
// Comments:
//
//
//********************************************************************


define method OpenEdit(this :: <COleObject>, pActiveSite :: <LPOLECLIENTSITE>)
 => ();


  let site = GetOleClientSite(this.m-lpObj);
  unless( null?( site ) )
    IOleClientSite/ShowObject(site);
  end unless;

  this.m-fOpen := #t;

  // tell the site we are opening so the object can be hatched out.
  unless( null?( site ) )
    IOleClientSite/OnShowWindow(site, #t);
  end unless;

  let doc :: <CSimpSvrDoc> = this.m-lpObj.m-lpDoc;

  ShowDocWnd(doc);

  HideHatchWnd(doc);

  // Show app window.
  ShowAppWnd(GetApp(doc), $SW-SHOWNORMAL);

  SetFocus(GethAppWnd(doc));
  values();
end method OpenEdit;
