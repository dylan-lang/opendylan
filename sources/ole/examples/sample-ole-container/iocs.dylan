Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
//
// COleClientSite::SaveObject
//
// Purpose:
//
//      Called by the object when it wants to be saved to persistant
//      storage
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
//      IOleObject::QueryInterface          Object
//      IPersistStorage::SaveCompleted      Object
//      IPersistStorage::Release            Object
//      OleSave                             OLE API
//      ResultFromScode                     OLE API
//
// Comments:
//
//********************************************************************


define method IOleClientSite/SaveObject(this :: <COleClientSite>)
				=> status :: <HRESULT>;

  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pSite);

  let sc :: <SCODE> = $E-FAIL;

  OutputDebugString( "In IOCS::SaveObject\r\n");

  // get a pointer to IPersistStorage
  let ( hErr :: <HRESULT>, interface :: <Interface> ) =
    QueryInterface(this.m-pSite.m-lpOleObject, $IID-IPersistStorage);
  
  // save the object
  if ( hErr = $NOERROR )

    let lpPS :: <LPPERSISTSTORAGE> =
      pointer-cast(<LPPERSISTSTORAGE>, interface);
    
    sc := OleSave(lpPS, this.m-pSite.m-lpObjStorage, #t);
    IPersistStorage/SaveCompleted(lpPS, null-pointer(<LPSTORAGE>));
    Release(lpPS);
  end if;
  sc 
end method IOleClientSite/SaveObject;

//**********************************************************************
//
// COleClientSite::GetMoniker
//
// Purpose:
//
//      Not Implemented
//
// Parameters:
//
//      Not Implemented
//
// Return Value:
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      This function is not implemented because we dont support
//      linking.
//
//********************************************************************


define method IOleClientSite/GetMoniker (this :: <COleClientSite>,
					dwAssign :: <integer>,
					dwWhichMoniker :: <integer>)
	=> ( status :: <HRESULT>, pmk :: <LPMONIKER> );

  OutputDebugString( "In IOCS::GetMoniker\r\n");

  values( $E-NOTIMPL, $NULL-interface )
end method IOleClientSite/GetMoniker;

//**********************************************************************
//
// COleClientSite::GetContainer
//
// Purpose:
//
//      Not Implemented
//
// Parameters:
//
//      Not Implemented
//
// Return Value:
//
//      Not Implemented
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Not Implemented
//
//********************************************************************


define method IOleClientSite/GetContainer(this :: <COleClientSite> )
	=> ( status :: <HRESULT>, pContainer :: <LPOLECONTAINER> );
	
  OutputDebugString( "In IOCS::GetContainer\r\n");

  values( $E-NOTIMPL, $NULL-interface )

end method IOleClientSite/GetContainer;

//**********************************************************************
//
// COleClientSite::ShowObject
//
// Purpose:
//
//      Not Implemented
//
// Parameters:
//
//      Not Implemented
//
// Return Value:
//
//      Not Implemented
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      This function is not implemented because we do not support
//      linking.
//
//********************************************************************


define method IOleClientSite/ShowObject(this :: <COleClientSite>)
 => status :: <HRESULT>;

  OutputDebugString( "In IOCS::ShowObject\r\n");
  $NOERROR
end method IOleClientSite/ShowObject;

//**********************************************************************
//
// COleClientSite::OnShowWindow
//
// Purpose:
//
//      Object calls this method when it is opening/closing non-InPlace
//      Window
//
// Parameters:
//
//      BOOL fShow  - TRUE if Window is opening, FALSE if closing
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      InvalidateRect              Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleClientSite/OnShowWindow(this :: <COleClientSite>,
					  fShow :: <boolean>) 
 => status :: <HRESULT>;

  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pSite);
  OutputDebugString( "In IOCS::OnShowWindow\r\n");
  this.m-pSite.m-fObjectOpen := fShow;
  let window = this.m-pSite.m-lpDoc.m-hDocWnd;
  InvalidateRect(window, $NULL-RECT, #t);

  // if object window is closing, then bring container window to top
  if ( ~ fShow ) 
    BringWindowToTop(window);
    SetFocus(window);
  end if;
  $S-OK 
end method IOleClientSite/OnShowWindow;

//**********************************************************************
//
// COleClientSite::RequestNewObjectLayout
//
// Purpose:
//
//      Not Implemented
//
// Parameters:
//
//      Not Implemented
//
// Return Value:
//
//      Not Implemented
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Not Implemented
//
//********************************************************************


define method IOleClientSite/RequestNewObjectLayout(this :: <COleClientSite>) 
	=> status :: <HRESULT>;

  OutputDebugString( "In IOCS::RequestNewObjectLayout\r\n");
  $E-NOTIMPL 
end method IOleClientSite/RequestNewObjectLayout;
