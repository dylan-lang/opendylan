Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//**********************************************************************
//
// COleInPlaceActiveObject::OnDocWindowActivate
//
// Purpose:
//
//      Called when the doc window (in an MDI App) is (de)activated.
//
// Parameters:
//
//      BOOL fActivate  - TRUE if activating, FALSE if deactivating
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                            Location
//
//      OutputDebugString                   Windows API
//      IOleInPlaceFrame::SetActiveObject   Container
//      CSimpSvrObject::AddFrameLevelUI     OBJ.CPP
//
//
// Comments:
//
//
//********************************************************************


define method IOleInPlaceActiveObject/OnDocWindowActivate
    (this :: <COleInPlaceActiveObject>, fActivate :: <boolean> )
		=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceActiveObject::OnDocWindowActivate\r\n");

  // Activating?
  if ( fActivate )
    AddFrameLevelUI(this.m-lpObj);
  end if;

  // No frame level tools to remove...
  
  $S-OK
end method IOleInPlaceActiveObject/OnDocWindowActivate;

//**********************************************************************
//
// COleInPlaceActiveObject::OnFrameWindowActivate
//
// Purpose:
//
//      Called when the Frame window is (de)activating
//
// Parameters:
//
//      BOOL fActivate  - TRUE if activating, FALSE if Deactivating
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      SetFocus                    Windows API
//
//
// Comments:
//
//
//********************************************************************


define method IOleInPlaceActiveObject/OnFrameWindowActivate(this :: <COleInPlaceActiveObject>,
				    fActivate :: <boolean>)
			=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceActiveObject::OnFrameWindowActivate\r\n");

  // set the focus to the object window if we are activating.
  /*    if (fActivate)
	  SetFocus(m_lpObj.m_lpDoc.GethDocWnd()); */ 

  $S-OK
end method IOleInPlaceActiveObject/OnFrameWindowActivate;

//**********************************************************************
//
// COleInPlaceActiveObject::GetWindow
//
// Purpose:
//
//      Gets the objects Window Handle.
//
// Parameters:
//
//      HWND FAR* lphwnd    - Location to return the window handle.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CSimpSvrDoc::GethDocWnd     DOC.H
//
//
// Comments:
//
//
//********************************************************************


define method IOleWindow/GetWindow(this :: <COleInPlaceActiveObject>)
		=> ( status :: <HRESULT>, lphwnd :: <HWND> );

  OutputDebugString("In COleInPlaceActiveObject::GetWindow\r\n");
  values( $S-OK, GethDocWnd(this.m-lpObj.m-lpDoc) )
end method IOleWindow/GetWindow;

//**********************************************************************
//
// COleInPlaceActiveObject::ContextSensitiveHelp
//
// Purpose:
//
//      Used to implement Context Sensitive help
//
// Parameters:
//
//      None
//
// Return Value:
//
//      E_NOTIMPL
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
//
// Comments:
//
//      See TECHNOTES.WRI include with the OLE SDK for proper
//      implementation of this function.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(this :: <COleInPlaceActiveObject>,
				   fEnterMode :: <boolean> )
		=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceActiveObject::ContextSensitiveHelp\r\n");
  $E-NOTIMPL
end method IOleWindow/ContextSensitiveHelp;

//**********************************************************************
//
// COleInPlaceActiveObject::TranslateAccelerator
//
// Purpose:
//
//      Used for translating accelerators in .DLL objects.
//
// Parameters:
//
//      LPMSG lpmsg - Pointer to a message
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
//
// Comments:
//
//      This method should never be called since we are implemented
//      in an executable.
//
//********************************************************************


define method IOleInPlaceActiveObject/TranslateAccelerator
    (this :: <COleInPlaceActiveObject>, lpmsg :: <LPMSG>)
	=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceActiveObject::TranslateAccelerator\r\n");
  // no accelerator table, return FALSE
  $S-FALSE
end method IOleInPlaceActiveObject/TranslateAccelerator;

//**********************************************************************
//
// COleInPlaceActiveObject::ResizeBorder
//
// Purpose:
//
//      Called when the border changes size.
//
// Parameters:
//
//      LPCRECT lprectBorder                - New Border
//
//      LPOLEINPLACEUIWINDOW lpUIWindow     - Pointer to UIWindow
//
//      BOOL fFrameWindow                   - True if lpUIWindow is the
//                                            frame window.
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
//
// Comments:
//
//      Need to call SetBorderSpace again...
//
//********************************************************************


define method IOleInPlaceActiveObject/ResizeBorder(this :: <COleInPlaceActiveObject>,
			   lprectBorder :: <LPCRECT>,
			   lpUIWindow :: <LPOLEINPLACEUIWINDOW>,
			   fFrameWindow :: <boolean>) => status :: <HRESULT>;

  OutputDebugString("In COleInPlaceActiveObject::ResizeBorder\r\n");

  let null-widths :: <LPCBORDERWIDTHS> = null-pointer(<LPCBORDERWIDTHS>);
  // should always have an inplace frame...
  IOleInPlaceUIWindow/SetBorderSpace(GetInPlaceFrame(this.m-lpObj), null-widths);

  // There will only be a UIWindow if in an MDI container
  unless ( null?( GetUIWindow(this.m-lpObj) ) )
    IOleInPlaceUIWindow/SetBorderSpace(GetUIWindow(this.m-lpObj), null-widths);
  end unless;

 $S-OK
end method IOleInPlaceActiveObject/ResizeBorder;

//**********************************************************************
//
// COleInPlaceActiveObject::EnableModeless
//
// Purpose:
//
//      Called to enable/disable modeless dialogs.
//
// Parameters:
//
//      BOOL fEnable    - TRUE to enable, FALSE to disable
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
//
// Comments:
//
//      Called by the container when a model dialog box is added/removed
//      from the screen.  The appropriate action for a server application
//      is to disable/enable any modeless dialogs currently being displayed.
//      Since this application doesn't display any modeless dialogs,
//      this method is essentially ignored.
//
//********************************************************************


define method IOleInPlaceActiveObject/EnableModeless(this :: <COleInPlaceActiveObject>,
			     fEnable :: <boolean>) => status :: <HRESULT>;

  OutputDebugString("In COleInPlaceActiveObject::EnableModeless\r\n");
  $S-OK
end method IOleInPlaceActiveObject/EnableModeless;
