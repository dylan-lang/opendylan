Module:    OLE-Server
Synopsis:  Methods for class <COleInPlaceActiveObject>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Called when the doc window (in an MDI App) is (de)activated.

define method IOleInPlaceActiveObject/OnDocWindowActivate
    (this :: <COleInPlaceActiveObject>, activate? :: <boolean> )
		=> status :: <HRESULT>;

  // Note: This would only be used by an MDI container.
  //	   It has not yet been tested.   -- DNG 9/5/96

  Output-Debug-String("IOleInPlaceActiveObject/OnDocWindowActivate");
  Output-Debug-Boolean(activate?);
  let obj = this.get-obj;

  // Activating?
  if ( activate? )
    do-in-place-activate(obj, $OLEIVERB-UIACTIVATE);
    $S-OK
  else
    IOleInPlaceObject/UIDeactivate(obj.server-IOleInPlaceObject)
  end if
end method IOleInPlaceActiveObject/OnDocWindowActivate;

define method Output-Debug-Boolean(flag :: <boolean>) => ();
  Output-Debug-String(if (flag) " #t\r\n" else " #f\r\n" end if);
end method;


// Called when the Frame window is (de)activating

define method IOleInPlaceActiveObject/OnFrameWindowActivate
    (this :: <COleInPlaceActiveObject>, activate? :: <boolean>)
			=> status :: <HRESULT>;

  Output-Debug-String("IOleInPlaceActiveObject/OnFrameWindowActivate");
  Output-Debug-Boolean(activate?);

  $S-OK
end method IOleInPlaceActiveObject/OnFrameWindowActivate;


// Gets the objects Window Handle.

define method IOleWindow/GetWindow(this :: <COleInPlaceActiveObject>)
		=> ( status :: <HRESULT>, window-handle :: <HWND> );

  Output-Debug-String("IOleWindow/GetWindow\r\n");
  values( $S-OK, OLE-part-doc-window(this.get-obj) )
end method IOleWindow/GetWindow;


// Used to implement Context Sensitive help

define method IOleWindow/ContextSensitiveHelp
	(this :: <COleInPlaceActiveObject>, enter? :: <boolean>)
 => status :: <HRESULT>;

  Output-Debug-String("IOleWindow/ContextSensitiveHelp");
  Output-Debug-Boolean(enter?);

  $E-NOTIMPL // not implemented yet
end method IOleWindow/ContextSensitiveHelp;


// Used for translating accelerators in .DLL objects.
define method IOleInPlaceActiveObject/TranslateAccelerator
    (this :: <COleInPlaceActiveObject>, msg :: <LPMSG>)
	=> status :: <HRESULT>;

  // Output-Debug-String("IOleInPlaceActiveObject/TranslateAccelerator\r\n");
  let accelerators = OLE-part-accelerators(this.get-obj);
  if ( null?(accelerators) |
	zero?(TranslateAccelerator(msg.hwnd-value, accelerators, msg)) )
    $S-FALSE // message not translated
  else
    $S-OK  // message was translated
  end if
end method IOleInPlaceActiveObject/TranslateAccelerator;

define open generic OLE-part-accelerators ( obj )
 => table :: false-or(<HACCEL>);

// default method:
define method OLE-part-accelerators ( obj :: <object> )
			=> table :: false-or(<HACCEL>);
  #f
end;


// Called when the border changes size.

define method IOleInPlaceActiveObject/ResizeBorder
    ( this :: <COleInPlaceActiveObject>,
      // don't bother specializing unused arguments.
      new-border /* :: <LPCRECT> */,
      in-place-UI-window :: <LPOLEINPLACEUIWINDOW>,
      is-frame-window? /* :: <boolean> */ ) => status :: <HRESULT>;

  Output-Debug-String("IOleInPlaceActiveObject/ResizeBorder\r\n");

  let obj = this.get-obj;
  if ( in-place-UI-window = obj.container-IOleInPlaceFrame )
    // may need to adjust the tool bar
    set-border-space(obj);
  else
    IOleInPlaceUIWindow/SetBorderSpace(in-place-UI-window, $NULL-RECT);
  end if;
  $S-OK
end method IOleInPlaceActiveObject/ResizeBorder;


// Called to enable/disable modeless dialogs.
// Comments:
//      Called by the container when a modal dialog box is added/removed
//      from the screen.  The appropriate action for a server application
//      is to disable/enable any modeless dialogs currently being displayed.

define method IOleInPlaceActiveObject/EnableModeless
    ( this :: <COleInPlaceActiveObject>, enable? :: <boolean> )
 => status :: <HRESULT>;

  Output-Debug-String("IOleInPlaceActiveObject/EnableModeless");
  Output-Debug-Boolean(enable?);
  OLE-part-enable-dialog(this.get-obj, enable?);
  $S-OK // no error codes are specified for this operation
end method IOleInPlaceActiveObject/EnableModeless;

// Default method does nothing
define method OLE-part-enable-dialog(obj :: <basic-ole-server>,
				       enable? :: <boolean>) => ();
  values()
end method;
