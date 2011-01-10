Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//**********************************************************************
//
// COleInPlaceObject::InPlaceDeactivate
//
// Purpose:
//
//      Called to deactivat the object
//
// Parameters:
//
//      None
//
// Return Value:
//
//
//
// Function Calls:
//      Function                                Location
//
//      OutputDebugString                       Windows API
//      IOleClientSite::QueryInterface          Container
//      IOleInPlaceSite::OnInPlaceDeactivate    Container
//      IOleInPlaceSite::Release                Container
//
// Comments:
//
//
//********************************************************************



define method IOleInPlaceObject/InPlaceDeactivate(this :: <COleInPlaceObject>) 
	=> status :: <HRESULT>;

  block(return)
	
    OutputDebugString("In COleInPlaceObject::InPlaceDeactivate\r\n");

    // if not inplace active, return NOERROR

    let lpObj :: <CSimpSvrObj> = this.m-lpObj;
    if ( ~(lpObj.m-fInPlaceActive) )
      return( $NOERROR );
    end if;

    // clear inplace flag
    lpObj.m-fInPlaceActive := #f;

    // deactivate the UI
    DeactivateUI(lpObj);
    DoInPlaceHide(lpObj);

    // tell the container that we are deactivating.
    unless ( null?(lpObj.m-lpIPSite) )
		 
      IOleInPlaceSite/OnInPlaceDeactivate(lpObj.m-lpIPSite);
      Release(lpObj.m-lpIPSite);
      lpObj.m-lpIPSite := $NULL-interface;
    end unless;

    return( $S-OK  );
  end block;
end method IOleInPlaceObject/InPlaceDeactivate;

//**********************************************************************
//
// COleInPlaceObject::UIDeactivate
//
// Purpose:
//
//      Instructs us to remove our UI.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      NOERROR
//
// Function Calls:
//      Function                                Location
//
//      OutputDebugString                       Windows API
//      IOleInPlaceUIWindow::SetActiveObject    Container
//      IOleInPlaceFrame::SetActiveObject       Container
//      IOleClientSite::QueryInterface          Container
//      IOleInPlaceSite::OnUIDeactivate         Container
//      IOleInPlaceSite::Release                Container
//      CSimpSvrObj::DoInPlaceHide              OBJ.H
//      IDataAdviseHolder::SendOnDataChange     OLE
//
//
// Comments:
//
//
//********************************************************************


define method IOleInPlaceObject/UIDeactivate(this :: <COleInPlaceObject>)
			=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceObject::UIDeactivate\r\n");

  DeactivateUI(this.m-lpObj);

  $S-OK
end method IOleInPlaceObject/UIDeactivate;

//**********************************************************************
//
// COleInPlaceObject::SetObjectRects
//
// Purpose:
//
//      Called when the container clipping region or the object position
//      changes.
//
// Parameters:
//
//      LPCRECT lprcPosRect     - New Position Rect.
//
//      LPCRECT lprcClipRect    - New Clipping Rect.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IntersectRect               Windows API
//      OffsetRect                  Windows API
//      CopyRect                    Windows API
//      MoveWindow                  Windows API
//      CSimpSvrDoc::GethHatchWnd   DOC.H
//      CSimpSvrDoc::gethDocWnd     DOC.h
//      SetHatchWindowSize          OLESTD
//
// Comments:
//
//
//********************************************************************


define method IOleInPlaceObject/SetObjectRects(this :: <COleInPlaceObject>,
			     lprcPosRect :: <LPRECT>,
			     lprcClipRect :: <LPRECT>) => status :: <HRESULT>;

  OutputDebugString("In COleInPlaceObject::SetObjectRects\r\n");

  let resRect :: <LPRECT> = make(<LPRECT>);

  // Get the intersection of the clipping rect and the position rect.
  IntersectRect(resRect, lprcPosRect, lprcClipRect);

  let lpObj :: <CSimpSvrObj> = this.m-lpObj;

  lpObj.m-xOffset := abs(resRect.left-value - lprcPosRect.left-value);
  lpObj.m-yOffset := abs(resRect.top-value - lprcPosRect.top-value);

  lpObj.m-scale := as(<single-float>,
			     lprcPosRect.right-value - lprcPosRect.left-value)
    / this.m-lpObj.m-pSize.x-value;

  if ( lpObj.m-scale = 0 )
    lpObj.m-scale := 1.0;
  end if;

  OutputDebugString(format-to-string("New Scale %d\r\n",
				     lpObj.m-scale));

  // Adjust the size of the Hatch Window.
  let ( x-offset, y-offset ) =
    SetHatchWindowSize(GethHatchWnd(lpObj.m-lpDoc), lprcPosRect, lprcClipRect);

  CopyRect(lpObj.m-pPosRect, lprcPosRect);

  // Move the actual object window
  MoveWindow(GethDocWnd(lpObj.m-lpDoc),
	     x-offset, y-offset,
	     resRect.right-value - resRect.left-value,
	     resRect.bottom-value - resRect.top-value, #t);

  destroy(resRect);
  $S-OK 
end method IOleInPlaceObject/SetObjectRects;

//**********************************************************************
//
// COleInPlaceObject::GetWindow
//
// Purpose:
//
//      Returns the Window handle of the inplace object
//
// Parameters:
//
//      HWND FAR* lphwnd    - Out pointer in which to return the window
//                            Handle.
//
// Return Value:
//
//      S_OK
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CSimpleDoc::GethDocWnd      DOC.H
//
// Comments:
//
//
//********************************************************************


define method IOleWindow/GetWindow(this :: <COleInPlaceObject>) 
 => ( status :: <HRESULT>, hwnd :: <HWND> );

  OutputDebugString("In COleInPlaceObject::GetWindow\r\n");
  values( $S-OK, GethDocWnd(this.m-lpObj.m-lpDoc) )
end method IOleWindow/GetWindow;

//**********************************************************************
//
// COleInPlaceObject::ContextSensitiveHelp
//
// Purpose:
//
//      Used in performing Context Sensitive Help
//
// Parameters:
//
//      BOOL fEnterMode     - Flag to determine if enter or exiting
//                            Context Sensitive Help.
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
// Comments:
//
//      This function is not implemented due to the fact that it is
//      beyond the scope of a simple object.  All *real* applications
//      are going to want to implement this function, otherwise any
//      container that supports context sensitive help will not work
//      properly while the object is in place.
//
//      See TECHNOTES.WRI include with the OLE SDK for details on
//      Implementing this method.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(this :: <COleInPlaceObject>,
				   fEnterMode :: <boolean>) 
	=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceObject::ContextSensitiveHelp\r\n");
  $E-NOTIMPL
end method IOleWindow/ContextSensitiveHelp;

//**********************************************************************
//
// COleInPlaceObject::ReactivateAndUndo
//
// Purpose:
//
//      Called when the container wants to undo the last edit made in
//      the object.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      INPLACE_E_NOTUNDOABLE
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Since this server does not support undo, the value
//      INPLACE_E_NOTUNDOABLE is always returned.
//
//********************************************************************


define method IOleInPlaceObject/ReactivateAndUndo(this :: <COleInPlaceObject>) 
	=> status :: <HRESULT>;

  OutputDebugString("In COleInPlaceObject::ReactivateAndUndo\r\n");
  $INPLACE-E-NOTUNDOABLE
end method IOleInPlaceObject/ReactivateAndUndo;
