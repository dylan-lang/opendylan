Module:    OLE-Container
Synopsis:  Implementation of IOleInPlaceFrame interface,
	   which controls the top-level frame window of the container.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open primary COM-interface <container-ole-in-place-frame>
	( <IOleInPlaceFrame> ) 
  constant slot get-app :: <container-app>, required-init-keyword: App:;
end <container-ole-in-place-frame>;


// Returns the frame window handle
define method IOleWindow/GetWindow(this :: <container-ole-in-place-frame>) 
	=> ( status :: <HRESULT>, hwnd :: <HWND> );

  OutputDebugString("IOleWindow/GetWindow\r\n");
  values( $S-OK, this.get-app.container-frame-window )
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
//
// Comments:
//
//      Be sure to read the technotes in the OLE toolkit.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(this :: <container-ole-in-place-frame>,
					      enter-mode? :: <boolean>) 
 => status :: <HRESULT>;

  OutputDebugString("IOleWindow/ContextSensitiveHelp\r\n");

  this.get-app.help-menu-mode := enter-mode?;

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
//
// Comments:
//
//********************************************************************


define method IOleInPlaceUIWindow/GetBorder
    (this :: <container-ole-in-place-frame>, lprectBorder :: <LPRECT>) 
 => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceUIWindow/GetBorder\r\n");

  let frame-window = this.get-app.container-frame-window;
  if ( null-handle?(frame-window) )
    SetRectEmpty(lprectBorder);
    $INPLACE-E-NOTOOLSPACE
  else
    // get the rect for the entire frame.
    GetClientRect(frame-window, lprectBorder);
    $S-OK 
  end if;
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
//
// Comments:
//
//      This implementation doesn't care about how much border space
//      is used.  It always returns S_OK.
//
//********************************************************************

define method IOleInPlaceUIWindow/RequestBorderSpace
    (this :: <container-ole-in-place-frame>,
     lpborderwidths :: <LPCBORDERWIDTHS> ) 
	=> status :: <HRESULT>;
	
  OutputDebugString("IOleInPlaceUIWindow/RequestBorderSpace\r\n");
  if ( container-request-border-space(this.get-app,
                                      lpborderwidths.left-value,
				      lpborderwidths.top-value,
				      lpborderwidths.right-value,
				      lpborderwidths.bottom-value) )
    $S-OK
  else
    $INPLACE-E-NOTOOLSPACE
  end if
end method IOleInPlaceUIWindow/RequestBorderSpace;

define open generic container-request-border-space
    (app :: <object>, left-space, top-space, right-space, bottom-space>)
 => ( ok? :: <boolean> );

// default method; application can override.
define method container-request-border-space
    (app :: <container-app>, left-space :: <integer>, top-space :: <integer>,
     right-space :: <integer>, bottom-space :: <integer>)
 => ( ok? :: <boolean> );

  // This default method returns true if the request is not asking for
  // more than half of the space either vertically or horizontally.
  ( ~ null-handle?(app.container-frame-window) ) &
  with-stack-structure ( frame-rect :: <LPRECT> )
    GetClientRect(app.container-frame-window, frame-rect);
    (((left-space + right-space) * 2)
       < (frame-rect.right-value - frame-rect.left-value))
      &
      (((top-space + bottom-space) * 2)
	 < (frame-rect.bottom-value - frame-rect.top-value))
      & // make sure none of the sizes are negative.
      (logior(left-space, top-space, right-space, bottom-space) >= 0)
  end with-stack-structure
end;

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
//      CSimpleApp::container-add-frame-tools  APP.CPP
//      OutputDebugString               Windows API
//      GetClientRect                   Windows API
//      MoveWindow                      Windows API
//
// Comments:
//
//      This routine could be a little smarter and check to see if
//      the object is requesting the entire client area of the
//      window.
//
//********************************************************************


define method IOleInPlaceUIWindow/SetBorderSpace
    (this :: <container-ole-in-place-frame>,
     lpborderwidths :: <LPCBORDERWIDTHS>) 
 => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceUIWindow/SetBorderSpace\r\n");

  let app = this.get-app;
  if ( null-pointer?(lpborderwidths) )
    // Server does not need any space; container should keep or restore
    // its own tool bar.
    container-release-border-space(app); 
    $S-OK 
  elseif ( null-handle?(app.container-frame-window) )
    $E-NOTIMPL
  else
    // Need to remove the container's tool bar and make room for the
    // server's tool bar.
    if ( container-set-border-space(app, lpborderwidths.left-value,
				    lpborderwidths.top-value,
				    lpborderwidths.right-value,
				    lpborderwidths.bottom-value) )
      $S-OK
    else
      $OLE-E-INVALIDRECT
    end if
  end if
end method IOleInPlaceUIWindow/SetBorderSpace;

define open generic container-set-border-space
    (app :: <object>, left-space, top-space, right-space, bottom-space>)
 => ( ok? :: <boolean> );

// default method fails; application should override.
define method container-set-border-space
    (app :: <container-app>, left-space :: <integer>, top-space :: <integer>,
     right-space :: <integer>, bottom-space :: <integer>)
 => ( ok? :: <boolean> );
  #f
end;

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
//
// Comments:
//
//********************************************************************

define method IOleInPlaceUIWindow/SetActiveObject
    (this :: <container-ole-in-place-frame>,
     new-active-object :: <LPOLEINPLACEACTIVEOBJECT>,
     object-name :: <LPCOLESTR>)
 => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceUIWindow/SetActiveObject\r\n");

  let new-active-object :: <LPOLEINPLACEACTIVEOBJECT> =
    dylan-interface(new-active-object);
  let new-name :: <LPCOLESTR> = object-name;
  let app = this.get-app;
  // AddRef() it and save it...
  if ( null-pointer?(new-active-object) )
    app.active-object-window := $NULL-HWND;
    new-name := $NULL-OLESTR;
  else
    AddRef(new-active-object);

    let ( status , window ) = IOleWindow/GetWindow(new-active-object);
    app.active-object-window := window;

    unless ( null-handle?(window) )
      SendMessage(window, $WM-QUERYNEWPALETTE, 0, 0);
    end unless;
  end if;
  Release(app.in-place-active-object);

  // in an MDI app, this method really shouldn't be called,
  // this method associated with the doc is called instead. [???]

  app.in-place-active-object := new-active-object;

  unless ( new-name = app.active-object-name )
    app.active-object-name := 
      if ( empty?(new-name) ) "" else as(<byte-string>, new-name) end if;
    note-active-object-name(app, new-name);
  end unless;

  $S-OK
end method IOleInPlaceUIWindow/SetActiveObject;

define open generic note-active-object-name ( app, name );

// Default method does nothing; the application could override if it wants
// to use the name in its window title.
define method note-active-object-name ( app :: <object>, name :: <object> )
 => ()
  values()
end method;


define open generic container-insert-menus (app, shared-menu)
 => (file-count :: <integer>, edit-count :: <integer>,
     help-count :: <integer>);

// Inserts the container menu into the combined menu
define method IOleInPlaceFrame/InsertMenus
    (this :: <container-ole-in-place-frame>,
     shared-menu :: <HMENU>, lpMenuWidths :: <LPOLEMENUGROUPWIDTHS>)
 => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceFrame/InsertMenus\r\n");
  let ( file-count, edit-count, help-count ) =
    container-insert-menus(this.get-app, shared-menu);
  let widths = lpMenuWidths.width-value;
  pointer-value(widths, index: 0) := file-count;
  pointer-value(widths, index: 2) := edit-count;
  pointer-value(widths, index: 4) := help-count;
  $S-OK
end method IOleInPlaceFrame/InsertMenus;

define method container-insert-menus
    (app :: <container-app>, shared-menu :: <object>)
 => (file-count :: <integer>, edit-count :: <integer>,
     help-count :: <integer>);
  // default method adds no menus; application will usually override.
  values(0,0,0)
end method;


// Sets the application menu to the combined menu
define method IOleInPlaceFrame/SetMenu(this :: <container-ole-in-place-frame>,
				       shared-menu :: <HMENU>,
				       holemenu :: <HOLEMENU>,
				       active-object-handle :: <HWND>) 
	=> status :: <HRESULT>;

  OutputDebugString("IOleInPlaceFrame/SetMenu\r\n");
  let app = this.get-app;
  let frame-window :: <HWND> = app.container-frame-window;
  block(return)
    if ( null-handle?(frame-window) ) // if no frame window, can't set menu
      $E-NOTIMPL
    else
      let new-menu :: <HMENU> =
	if ( null-handle?(holemenu) )
	  // restore original menu
	  app.container-main-menu
	else
	  // remember the old menu
	  let old-menu = GetMenu(frame-window);
	  if ( null-handle?(old-menu) )
	    // frame does not have a menu bar, so can't update it.
	    return($E-NOTIMPL);
	  end if;
	  app.container-main-menu := old-menu;
	  shared-menu
	end if;

      // MSDN says:
      //   An SDI container's implementation of this method should call the
      //   Windows SetMenu function. An MDI container should send a
      //   WM_MDISETMENU message, using hmenuShared as the menu to install. 
      SetMenu(frame-window, new-menu);

      // install the OLE dispatching code:
      OleSetMenuDescriptor(holemenu, frame-window, active-object-handle,
			   this, app.in-place-active-object)
    end if
  end block
end method IOleInPlaceFrame/SetMenu;


// Removes the container menus from the combined menu
define method IOleInPlaceFrame/RemoveMenus
    (this :: <container-ole-in-place-frame>, shared-menu :: <HMENU>) 
 => status :: <HRESULT>;
	
  OutputDebugString("IOleInPlaceFrame/RemoveMenus\r\n");

  while( GetMenuItemCount(shared-menu) > 0 )
    RemoveMenu(shared-menu, 0, $MF-BYPOSITION);
  end while;
  $S-OK
end method IOleInPlaceFrame/RemoveMenus;



define open generic container-set-status-text
    (app :: <container-app>, text :: <string>) => (status :: <HRESULT>);

define method container-set-status-text
    (app :: <container-app>, text :: <string>) => (status :: <HRESULT>);
  // This default method reports that the container does not support a 
  // status bar.  Applications will likely provide an override method.
  $E-FAIL
end method;

define method IOleInPlaceFrame/SetStatusText
    (this :: <container-ole-in-place-frame>, message-text :: <LPCOLESTR>) 
	=> status :: <HRESULT>;

  OutputDebugString("IOleInPlaceFrame/SetStatusText\r\n");
  let app  :: <container-app> = this.get-app;
  if ( null-pointer?(app.in-place-active-object) )
    // Can't set status test if not running in-place active.
    $OLE-E-NOT-INPLACEACTIVE
  else
    container-set-status-text(app, message-text)
  end if
end method IOleInPlaceFrame/SetStatusText;

// Enables/Disables container modeless dialogs
define method IOleInPlaceFrame/EnableModeless
    (this :: <container-ole-in-place-frame>, enable? :: <boolean> )
	=> status :: <HRESULT>;

  OutputDebugString("IOleInPlaceFrame/EnableModeless\r\n");
  note-enable-modeless(this.get-app, enable?);
  $S-OK // no useful error conditions for this
end method IOleInPlaceFrame/EnableModeless;

define open generic note-enable-modeless(app, enable?);

// default method does nothing; application can override.
define method note-enable-modeless ( app :: <container-app>,
				    enable? :: <boolean> ) => ()
  values()
end;

// This function is called to dispatch an accelerator key belonging to the 
// container which was received by the message loop of an embedded server.
define method IOleInPlaceFrame/TranslateAccelerator
    (this :: <container-ole-in-place-frame>, msg :: <LPMSG>,
     command-ID /* :: <unsigned-fixnum> */ )
 => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceFrame/TranslateAccelerator\r\n");
  let app = this.get-app;
  let accel = app.container-accelerator-table;
  let window = app.container-frame-window;
  if ( null-handle?(accel) | null-handle?(window) )
    // if there isn't any accelerator table, we shouldn't have gotten here.
    $E-UNEXPECTED
  else
    // We have already been given the command ID, so we could use that
    // directly, but going back through TranslateAccelerator simplifies 
    // the protocol for users of this library.
    // Note that we need to use the container's frame window, not the 
    // window that originally received the message.
    if ( (~ zero?(TranslateAccelerator(window, accel, msg)))
	  | TranslateMDISysAccel(window, msg) )
      $S-OK // an accelerator was processed
    else
      $S-FALSE // the message has not been processed
    end if
  end if
end method IOleInPlaceFrame/TranslateAccelerator;
