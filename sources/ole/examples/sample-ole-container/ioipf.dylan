Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//**********************************************************************
//
// COleInPlaceFrame::GetWindow
//
// Purpose:
//
//      Returns the frame window handle
//
// Parameters:
//
//      HWND FAR* lphwnd    - Location to return the window handle
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


define method IOleWindow/GetWindow(this :: <COleInPlaceFrame>) 
	=> ( status :: <HRESULT>, hwnd :: <HWND> );

  OutputDebugString("In IOIPF::GetWindow\r\n");
  values( $S-OK, this.m-pApp.m-hAppWnd )
end method IOleWindow/GetWindow;

//**********************************************************************
//
// COleInPlaceFrame::ContextSensitiveHelp
//
// Purpose:
//
//      Used in implementing Context sensitive help
//
// Parameters:
//
//      BOOL fEnterMode -   TRUE if starting Context Sensitive help mode
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
//      Be sure to read the technotes in the OLE toolkit.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(this :: <COleInPlaceFrame>,
					      fEnterMode :: <boolean>) 
 => status :: <HRESULT>;

  OutputDebugString("In IOIPF::ContextSensitiveHelp\r\n");

  this.m-pApp.m-fMenuMode := fEnterMode;

  $S-OK 
end method IOleWindow/ContextSensitiveHelp;

//**********************************************************************
//
// COleInPlaceFrame::GetBorder
//
// Purpose:
//
//      Returns the outermost border that frame adornments can be attached
//      during InPlace Activation.
//
// Parameters:
//
//      LPRECT lprectBorder - return parameter to contain the outermost
//                            rect for frame adornments
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      GetClientRect               Windows API
//      CopyRect                    Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceUIWindow/GetBorder(this :: <COleInPlaceFrame>,
					    lprectBorder :: <LPRECT>) 
 => status :: <HRESULT>;

  OutputDebugString("In IOIPF::GetBorder\r\n");

  // get the rect for the entire frame.
  GetClientRect(this.m-pApp.m-hAppWnd, lprectBorder);

 $S-OK 
end method IOleInPlaceUIWindow/GetBorder;

//**********************************************************************
//
// COleInPlaceFrame::RequestBorderSpace
//
// Purpose:
//
//      Approves/Denies requests for border space during InPlace
//      negotiation.
//
// Parameters:
//
//      LPCBORDERWIDTHS lpborderwidths  - The width in pixels needed on
//                                        each side of the frame.
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
//      This implementation doesn't care about how much border space
//      is used.  It always returns S_OK.
//
//********************************************************************

define method IOleInPlaceUIWindow/RequestBorderSpace
    (this :: <COleInPlaceFrame>, lpborderwidths /* :: <LPCBORDERWIDTHS> */ ) 
	=> status :: <HRESULT>;
	
  OutputDebugString("In IOIPF::RequestBorderSpace\r\n");
  // always approve the request
  $S-OK 
end method IOleInPlaceUIWindow/RequestBorderSpace;

//**********************************************************************
//
// COleInPlaceFrame::SetBorderSpace
//
// Purpose:
//
//      The object calls this method when it is actually going to
//      start using the border space.
//
// Parameters:
//
//      LPCBORDERWIDTHS lpborderwidths  - Border space actually being used
//                                        by the object
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                        Location
//
//      CSimpleApp::AddFrameLevelTools  APP.CPP
//      OutputDebugString               Windows API
//      GetClientRect                   Windows API
//      MoveWindow                      Windows API
//      ResultFromScode                 Windows API
//
// Comments:
//
//      This routine could be a little smarter and check to see if
//      the object is requesting the entire client area of the
//      window.
//
//********************************************************************


define method IOleInPlaceUIWindow/SetBorderSpace
    (this :: <COleInPlaceFrame>, lpborderwidths :: <LPCBORDERWIDTHS>) 
 => status :: <HRESULT>;

  OutputDebugString("In IOIPF::SetBorderSpace\r\n");
  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pApp);

  let app = this.m-pApp;
  if ( null-pointer?(lpborderwidths) )
    AddFrameLevelTools(app); 
  else
	        
    let rect :: <LPRECT> = make(<LPRECT>);

    GetClientRect(app.m-hAppWnd, rect);

    MoveWindow(app.m-lpDoc.m-hDocWnd,
	       rect.left-value + lpborderwidths.left-value,
	       rect.top-value + lpborderwidths.top-value,
	       rect.right-value
		 - lpborderwidths.right-value - lpborderwidths.left-value,
	       rect.bottom-value
		 - lpborderwidths.bottom-value - lpborderwidths.top-value,
	       #t);
    destroy(rect);
  end if;
  $S-OK 
end method IOleInPlaceUIWindow/SetBorderSpace;

//**********************************************************************
//
// COleInPlaceFrame::SetActiveObject
//
// Purpose:
//
//
// Parameters:
//
//      LPOLEINPLACEACTIVEOBJECT lpActiveObject     -   Pointer to the
//                                                      objects
//                                                      IOleInPlaceActiveObject
//                                                      interface
//
//@@WTK WIN32, UNICODE
//      //LPCSTR lpszObjName                          -   Name of the object
//      LPCOLESTR lpszObjName                          -   Name of the object
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                            Location
//
//      OutputDebugString                   Windows API
//      IOleInPlaceActiveObject::AddRef     Object
//      IOleInPlaceActiveObject::Release    Object
//      ResultFromScode                     OLE API
//
// Comments:
//
//********************************************************************

define method IOleInPlaceUIWindow/SetActiveObject
    (this :: <COleInPlaceFrame>,
     lpActiveObject :: <LPOLEINPLACEACTIVEOBJECT>,
     lpszObjName :: <LPCOLESTR>) 
 => status :: <HRESULT>;

  OutputDebugString("In IOIPF::SetActiveObject\r\n");
  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pApp);

  let app = this.m-pApp;
  // AddRef() it and save it...
  if ( ~ null-pointer?(lpActiveObject) )
	        
    AddRef(lpActiveObject);

    let ( status , window ) = IOleWindow/GetWindow(lpActiveObject);
    app.m-hwndUIActiveObj := window;

    unless ( null-handle?(window) )
      SendMessage(window, $WM-QUERYNEWPALETTE, 0, 0);
    end unless;
	        
  else
	        
    unless ( null?(app.m-lpDoc.m-lpActiveObject) )
      Release(app.m-lpDoc.m-lpActiveObject);
      app.m-hwndUIActiveObj := $NULL-HWND;
    end unless;

  end if;

  // in an MDI app, this method really shouldn't be called,
  // this method associated with the doc is called instead.

  app.m-lpDoc.m-lpActiveObject := lpActiveObject;
  // should set window title here

  $S-OK
end method IOleInPlaceUIWindow/SetActiveObject;

//**********************************************************************
//
// COleInPlaceFrame::InsertMenus
//
// Purpose:
//
//      Inserts the container menu into the combined menu
//
// Parameters:
//
//      HMENU hmenuShared                   -   Menu Handle to be set.
//      LPOLEMENUGROUPWIDTHS lpMenuWidths   -   Width of menus
//
// Return Value:
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      AppendMenu                  Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceFrame/InsertMenus(this :: <COleInPlaceFrame>,
	hmenuShared :: <HMENU>, lpMenuWidths :: <LPOLEMENUGROUPWIDTHS>) 
 => status :: <HRESULT>;

  OutputDebugString("In IOIPF::InsertMenus\r\n");
  // let stabilize :: <CStabilize> = make(<CStabilize>, this.m-pApp);

  AppendMenu(hmenuShared, logior($MF-BYPOSITION, $MF-POPUP),
	     pointer-address(this.m-pApp.m-hFileMenu), TEXT("&File"));
  AppendMenu(hmenuShared, logior($MF-BYPOSITION, $MF-POPUP),
	     pointer-address(this.m-pApp.m-hHelpMenu), TEXT("&Other"));

  let widths = lpMenuWidths.width-value;
  pointer-value(widths, index: 0) := 1;
  pointer-value(widths, index: 2) := 0;
  pointer-value(widths, index: 4) := 1;

  $S-OK
end method IOleInPlaceFrame/InsertMenus;


//**********************************************************************
//
// COleInPlaceFrame::SetMenu
//
// Purpose:
//
//      Sets the application menu to the combined menu
//
// Parameters:
//
//      HMENU hmenuShared       - The combined menu
//
//      HOLEMENU holemenu       - Used by OLE
//
//      HWND hwndActiveObject   - Used by OLE
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      SetMenu                     Windows API
//      OleSetMenuDescriptor        OLE API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceFrame/SetMenu(this :: <COleInPlaceFrame>,
				       hmenuShared :: <HMENU>,
				       holemenu :: <HOLEMENU>,
				       hwndActiveObject :: <HWND>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPF::SetMenu\r\n");
  let app = this.m-pApp;
  // let stabilize :: <CStabilize> = make(<CStabilize>, app);

  let hMenu :: <HMENU> = app.m-hMainMenu;

  unless ( null-handle?(holemenu) )
    hMenu := hmenuShared;
  end unless;

  // call the windows API, not this method
  SetMenu(app.m-hAppWnd, hMenu);

  OleSetMenuDescriptor(holemenu, app.m-hAppWnd, hwndActiveObject,
		       this, app.m-lpDoc.m-lpActiveObject);
end method IOleInPlaceFrame/SetMenu;


//**********************************************************************
//
// COleInPlaceFrame::RemoveMenus
//
// Purpose:
//
//      Removes the container menus from the combined menu
//
// Parameters:
//
//      HMENU hmenuShared   - Handle to the combined menu.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      GetMenuItemCount            Windows API
//      RemoveMenu                  Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//********************************************************************


define method IOleInPlaceFrame/RemoveMenus(this :: <COleInPlaceFrame>,
					   hmenuShared :: <HMENU>) 
	=> status :: <HRESULT>;
	
  OutputDebugString("In IOIPF::RemoveMenus\r\n");

  while( GetMenuItemCount(hmenuShared) > 0 )
    RemoveMenu(hmenuShared, 0, $MF-BYPOSITION);
  end while;
  $S-OK
end method IOleInPlaceFrame/RemoveMenus;

//**********************************************************************
//
// COleInPlaceFrame::SetStatusText
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
//      This function is not implemented due to the fact
//      that this application does not have a status bar.
//
//********************************************************************

define method IOleInPlaceFrame/SetStatusText(this :: <COleInPlaceFrame>,
					     lpszStatusText :: <LPCOLESTR>) 
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPF::SetStatusText\r\n");
  $E-FAIL
end method IOleInPlaceFrame/SetStatusText;

//**********************************************************************
//
// COleInPlaceFrame::EnableModeless
//
// Purpose:
//
//      Enables/Disables container modeless dialogs
//
// Parameters:
//
//      BOOL fEnable    - Enable/Disable
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
//      There are no modeless dialogs in this application, so the
//      implementation of this method is trivial.
//
//********************************************************************

define method IOleInPlaceFrame/EnableModeless(this :: <COleInPlaceFrame>,
					      fEnable /* :: <boolean> */ )
	=> status :: <HRESULT>;

  OutputDebugString("In IOIPF::EnableModeless\r\n");
  $S-OK
end method IOleInPlaceFrame/EnableModeless;

//**********************************************************************
//
// COleInPlaceFrame::TranslateAccelerator
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

define method IOleInPlaceFrame/TranslateAccelerator
    (this :: <COleInPlaceFrame>, lpmsg /* :: <LPMSG> */ ,
     wID /* :: <integer> */ )
 => status :: <HRESULT>;

  OutputDebugString("In IOIPF::TranslateAccelerator\r\n");
  $S-FALSE 
end method IOleInPlaceFrame/TranslateAccelerator;
