Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//**********************************************************************
//
// COleInPlaceSite::GetWindow
//
// Purpose:
//
//      Returns the Window Handle of the client site
//
// Parameters:
//
//      HWND FAR* lphwnd    - place to return the handle
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleWindow/GetWindow(this :: <COleInPlaceSite>)
	=> ( status :: <HRESULT>, hwnd :: <HWND> );

  OutputDebugString("In IOIPS::GetWindow\r\n");
  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pSite);

  // return the handle to our editing window.
  values( $S-OK, this.m-pSite.m-lpDoc.m-hDocWnd )
end method IOleWindow/GetWindow;

//**********************************************************************
//
// COleInPlaceSite::ContextSensitiveHelp
//
// Purpose:
//
//
// Parameters:
//
//      BOOL fEnterMode - TRUE for entering Context Sensitive help mode
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//      Be sure to read the technotes included with the OLE toolkit.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(this :: <COleInPlaceSite>,
					      fEnterMode :: <boolean>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::ContextSensitiveHelp\r\n");
  let app = this.m-pSite.m-lpDoc.m-lpApp;
  if ( app.m-fCSHMode ~= fEnterMode )

    app.m-fCSHMode := fEnterMode;
  end if;
 $S-OK 
end method IOleWindow/ContextSensitiveHelp;

//**********************************************************************
//
// COleInPlaceSite::CanInPlaceActivate
//
// Purpose:
//
//      Object calls to find out if the container can InPlace activate
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
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/CanInPlaceActivate(this :: <COleInPlaceSite>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::CanInPlaceActivate\r\n");

  // return S_OK to indicate we can in-place activate
  $S-OK 

end method IOleInPlaceSite/CanInPlaceActivate;

//**********************************************************************
//
// COleInPlaceSite::OnInPlaceActivate
//
// Purpose:
//
//      Called by the object on InPlace Activation
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
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/OnInPlaceActivate(this :: <COleInPlaceSite>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::OnInPlaceActivate\r\n");
  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pSite);

  let ( hrErr :: <HRESULT>, interface :: <Interface> ) =
    QueryInterface(this.m-pSite.m-lpOleObject, $IID-IOleInPlaceObject);
  if ( hrErr ~= $NOERROR )
    $E-FAIL
  else
    this.m-pSite.m-lpInPlaceObject :=
      dylan-interface(pointer-cast(<LPOLEINPLACEOBJECT>, interface));

    // return S_OK to indicate we can in-place activate.
    $S-OK 
  end if
end method IOleInPlaceSite/OnInPlaceActivate;

//**********************************************************************
//
// COleInPlaceSite::OnUIActivate
//
// Purpose:
//
//      Object calls this method when it displays it's UI.
//
// Parameters:
//
//      None.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/OnUIActivate(this :: <COleInPlaceSite>)
 => status :: <HRESULT>;

  OutputDebugString("In IOIPS::OnUIActivate\r\n");
  let site :: <CSimpleSite> = this.m-pSite;
  // site.m-lpDoc.m-fAddMyUI := #f;
  site.m-lpDoc.m-fInPlaceActive := #t;
  site.m-fInPlaceActive := #t;
  
  let ( status, handle ) = IOleWindow/GetWindow(site.m-lpInPlaceObject);
  site.m-hwndIPObj := handle;

  // return S_OK to continue in-place activation
  $S-OK 
end method IOleInPlaceSite/OnUIActivate;

//**********************************************************************
//
// COleInPlaceSite::GetWindowContext
//
// Purpose:
//
//      Called by the object to get information for InPlace Negotiation.
//
// Parameters:
//
//      LPOLEINPLACEFRAME FAR* lplpFrame    - Location to return a pointer
//                                            to IOleInPlaceFrame.
//
//      LPOLEINPLACEUIWINDOW FAR* lplpDoc   - Location to return a pointer
//                                            to IOleInPlaceUIWindow.
//
//      LPRECT lprcPosRect                  - The rect that the object
//                                            occupies
//
//      LPRECT lprcClipRect                 - The clipping rect
//
//      LPOLEINPLACEFRAMEINFO lpFrameInfo   - Pointer to FRAMEINFO
//
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      COleInPlaceFrame::AddRef    IOIPF.CPP
//      CSimpleSite::GetObjRect     SITE.CPP
//      OutputDebugString           Windows API
//      SetMapMode                  Windows API
//      GetDC                       Windows API
//      ReleaseDC                   Windows API
//      CopyRect                    Windows API
//      GetClientRect               Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/GetWindowContext(this :: <COleInPlaceSite>,
	lprcPosRect :: <LPRECT>, lprcClipRect :: <LPRECT>,
	lpFrameInfo :: <LPOLEINPLACEFRAMEINFO>) 
 => ( status :: <HRESULT>,
      lpFrame :: <LPOLEINPLACEFRAME>, lpDoc :: <LPOLEINPLACEUIWINDOW> );

  let rect :: <LPRECT> = make(<LPRECT>);
  let site = this.m-pSite;
  let doc = site.m-lpDoc;
  // let stabilize :: <CStabilize> = make(<CStabilize>, site);

  OutputDebugString("In IOIPS::GetWindowContext\r\n");

  // the frame is associated with the application object.
  // need to AddRef() it...
  AddRef(doc.m-lpApp.m-OleInPlaceFrame);

  // get the size of the object in pixels
  GetObjRect(site, rect);

  // Copy this to the passed buffer
  CopyRect(lprcPosRect, rect);

  // fill the clipping region
  GetClientRect(doc.m-hDocWnd, rect);
  CopyRect(lprcClipRect, rect);

  // fill the FRAMEINFO
  lpFrameInfo.fMDIApp-value := #f;
  lpFrameInfo.hwndFrame-value := doc.m-lpApp.m-hAppWnd;
  lpFrameInfo.haccel-value := null-pointer(<HACCEL>);
  lpFrameInfo.cAccelEntries-value := 0;

  destroy(rect);
  values( $S-OK, doc.m-lpApp.m-OleInPlaceFrame,
	 null-pointer(<LPOLEINPLACEUIWINDOW>) //  NULL because we're SDI.
	   )
end method IOleInPlaceSite/GetWindowContext;

//**********************************************************************
//
// COleInPlaceSite::Scroll
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


define method IOleInPlaceSite/Scroll(this :: <COleInPlaceSite>,
				     scrollExtent :: <SIZE>) 
	=> status :: <HRESULT>;
	
  OutputDebugString("In IOIPS::Scroll\r\n");
  $E-FAIL 
end method IOleInPlaceSite/Scroll;

//**********************************************************************
//
// COleInPlaceSite::OnUIDeactivate
//
// Purpose:
//
//      Called by the object when its UI goes away
//
// Parameters:
//
//       BOOL fUndoable
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CSimpleAPP::AddFrameLevelUI APP.CPP
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/OnUIDeactivate(this :: <COleInPlaceSite>,
					     fUndoable :: <boolean>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::OnUIDeactivate\r\n");
  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pSite);

  let site = this.m-pSite;
  let doc = site.m-lpDoc;
  // need to clear this flag first
  doc.m-fInPlaceActive := #f;
  site.m-fInPlaceActive := #f;

  QueryNewPalette(doc.m-lpApp);
  AddFrameLevelUI(doc.m-lpApp);
  $S-OK
end method IOleInPlaceSite/OnUIDeactivate;

//**********************************************************************
//
// COleInPlaceSite::OnInPlaceDeactivate
//
// Purpose:
//
//      Called when the inplace session is over
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
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/OnInPlaceDeactivate(this :: <COleInPlaceSite>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::OnInPlaceDeactivate\r\n");

  let site = this.m-pSite;
  unless ( null?(site.m-lpInPlaceObject) ) 
    Release(site.m-lpInPlaceObject);
    site.m-lpInPlaceObject := null-pointer(<LPOLEINPLACEOBJECT>);
  end unless;
  $S-OK
end method IOleInPlaceSite/OnInPlaceDeactivate;

//**********************************************************************
//
// COleInPlaceSite::DiscardUndoState
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


define method IOleInPlaceSite/DiscardUndoState(this :: <COleInPlaceSite>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::DiscardUndoState\r\n");
  $E-FAIL
end method IOleInPlaceSite/DiscardUndoState;

//**********************************************************************
//
// COleInPlaceSite::DeactivateAndUndo
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


define method IOleInPlaceSite/DeactivateAndUndo(this :: <COleInPlaceSite>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::DeactivateAndUndo\r\n");
  $E-FAIL
end method IOleInPlaceSite/DeactivateAndUndo;

//**********************************************************************
//
// COleInPlaceSite::OnPosRectChange
//
// Purpose:
//
//      The object calls this method when it's size changes during an
//      InPlace Session
//
// Parameters:
//
//      LPCRECT lprcPosRect -   The new object rect
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                            Location
//
//      OutputDebugString                   Windows API
//      GetClientRect                       Windows API
//      IOleObject::GetExtent               Object
//      IOleObject::QueryInterface          Object
//      IOleInPlaceObject::SetObjectRects   Object
//      IOleInPlaceObject::Release          Object
//      ResultFromScode                     OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceSite/OnPosRectChange(this :: <COleInPlaceSite>,
					      lprcPosRect :: <LPCRECT>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPS::OnPosRectChange\r\n");
  let site = this.m-pSite;
  // let stabilize :: <CStabilize> = make(<CStabilize>, site);

  // update the size in the document object
  // NOTE: here we must call IOleObject::GetExtent to get actual extents
  //       of the running object. IViewObject2::GetExtent returns the
  //       last cached extents.
  IOleObject/GetExtent(site.m-lpOleObject, $DVASPECT-CONTENT, site.m-pSizel);
  let rect :: <LPRECT> = make(<LPRECT>);
  GetClientRect(site.m-lpDoc.m-hDocWnd, rect);
  
  // tell the object its new size
  IOleInPlaceObject/SetObjectRects(site.m-lpInPlaceObject, lprcPosRect, rect);
  destroy(rect);
  $S-OK
end method IOleInPlaceSite/OnPosRectChange;
