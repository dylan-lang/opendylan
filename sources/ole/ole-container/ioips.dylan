Module:    OLE-Container
Synopsis:  Implementation of IOleInPlaceSite interface for OLE container.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open primary COM-interface <container-ole-in-place-site>
	( <IOleInPlaceSite> ) 
  constant slot get-site :: <contained-object>, required-init-keyword: site:;
end <container-ole-in-place-site>;


// Returns the Window Handle of the client site
define method IOleWindow/GetWindow (this :: <container-ole-in-place-site>)
	=> ( status :: <HRESULT>, hwnd :: <HWND> );

  OutputDebugString("IOleWindow/GetWindow(<container-ole-in-place-site>)\r\n");

  // return the handle to our editing window.
  values( $S-OK, this.get-site.document-container-window )
end method IOleWindow/GetWindow;

// Parameters:
//
//      BOOL fEnterMode - TRUE for entering Context Sensitive help mode
define method IOleWindow/ContextSensitiveHelp
    (this :: <container-ole-in-place-site>,
     enter-mode? :: <boolean>) 
 => status :: <HRESULT>;

  OutputDebugString("IOleWindow/ContextSensitiveHelp\r\n");
  let app = this.get-site.document-application;
  if ( app.context-help-mode ~= enter-mode? )
    app.context-help-mode := enter-mode?;
  end if;
 $S-OK 
end method IOleWindow/ContextSensitiveHelp;

// Object calls to find out if the container can In-Place activate
define method IOleInPlaceSite/CanInPlaceActivate
    (this :: <container-ole-in-place-site>) => (status :: <HRESULT>)

  OutputDebugString("IOleInPlaceSite/CanInPlaceActivate\r\n");

  // return S_OK to indicate we can in-place activate
  $S-OK 

end method IOleInPlaceSite/CanInPlaceActivate;

// Called by the object on InPlace Activation
define method IOleInPlaceSite/OnInPlaceActivate
    (this :: <container-ole-in-place-site>) => (status :: <HRESULT>)

  OutputDebugString("IOleInPlaceSite/OnInPlaceActivate\r\n");

  let site = this.get-site;
  let ( status :: <HRESULT>, interface :: <Interface> ) =
    QueryInterface(site.document-ole-object, $IID-IOleInPlaceObject);
  if ( FAILED?(status) )
    $E-FAIL
  else
    site.document-in-place-ole-object :=
      dylan-interface(pointer-cast(<LPOLEINPLACEOBJECT>, interface));
    // return S_OK to indicate we can in-place activate.
    $S-OK 
  end if
end method IOleInPlaceSite/OnInPlaceActivate;

// Object calls this method when it displays its UI.
define method IOleInPlaceSite/OnUIActivate
    (this :: <container-ole-in-place-site>) => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceSite/OnUIActivate\r\n");

  // MSDN says:
  //   The container should remove any user interface associated with its
  //   own activation. If the container is itself an embedded object, it
  //   should remove its document-level user interface.
  //
  //   If there is already an object active in place in the same document,
  //   the container should call IOleInPlaceObject::UIDeactivate before
  //   calling OnUIDeactivate.

  let site :: <contained-object> = this.get-site;
  let doc = site;
  doc.document-in-place-active? := #t;
  
  let ( status, handle ) =
    IOleWindow/GetWindow(site.document-in-place-ole-object);
  site.document-ui-active-window := handle;

  // return S_OK to continue in-place activation
  $S-OK 
end method IOleInPlaceSite/OnUIActivate;

// Called by the object to get information for InPlace Negotiation.
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

define method IOleInPlaceSite/GetWindowContext
    (this :: <container-ole-in-place-site>,
	lprcPosRect :: <LPRECT>, lprcClipRect :: <LPRECT>,
	lpFrameInfo :: <LPOLEINPLACEFRAMEINFO>) 
 => ( status :: <HRESULT>,
      lpFrame :: <LPOLEINPLACEFRAME>, lpDoc :: <LPOLEINPLACEUIWINDOW> );

  let rect :: <LPRECT> = make(<LPRECT>);
  let site = this.get-site;
  let doc = site;

  OutputDebugString("IOleInPlaceSite/GetWindowContext\r\n");
  let app = doc.document-application;
  // the frame is associated with the application object.
  // need to AddRef() it...
  AddRef(app.app-ole-in-place-frame);

  // get the size of the object in pixels
  document-rectangle(site, rect);

  // Copy this to the passed buffer
  CopyRect(lprcPosRect, rect);

  // fill the clipping region
  GetClientRect(doc.document-container-window, rect);
  CopyRect(lprcClipRect, rect);

  // fill the FRAMEINFO
  lpFrameInfo.fMDIApp-value := #f;  // not an MDI application
  lpFrameInfo.hwndFrame-value := app.container-frame-window;
  let haccel :: <HACCEL> = app.container-accelerator-table;
  lpFrameInfo.haccel-value := haccel;
  lpFrameInfo.cAccelEntries-value :=
    if ( null-handle?(haccel) )
      0
    else
      // compute number of entries in accelerator table
      CopyAcceleratorTable(haccel, null-pointer(<LPACCEL>), #xFF)
    end if;

  destroy(rect);
  values( $S-OK, app.app-ole-in-place-frame,
	 null-pointer(<LPOLEINPLACEUIWINDOW>) //  NULL because we're SDI.
	   )
end method IOleInPlaceSite/GetWindowContext;

// not implemented
define method IOleInPlaceSite/Scroll(this :: <container-ole-in-place-site>,
				     scrollExtent :: <SIZE>) 
	=> status :: <HRESULT>;
	
  OutputDebugString("IOleInPlaceSite/Scroll\r\n");
  $E-FAIL  // or should this be $E-NOTIMPL ? (neither is documented for this)
end method IOleInPlaceSite/Scroll;

// Called by the object when its UI goes away
define method IOleInPlaceSite/OnUIDeactivate
    (this :: <container-ole-in-place-site>, undoable? :: <boolean>) 
 => (status :: <HRESULT>)

  // undoable? arg is not used???
  
  OutputDebugString("IOleInPlaceSite/OnUIDeactivate\r\n");

  let doc = this.get-site;
  // need to clear this flag first
  doc.document-in-place-active? := #f;
  doc.document-ui-active-window := $NULL-HWND;

  let app = doc.document-application;
  unless ( null-handle?(app.container-frame-window) )
    container-query-new-palette(app);
    container-add-frame-ui(app);
  end unless;
  $S-OK
end method IOleInPlaceSite/OnUIDeactivate;


// Called when the inplace session is over
define method IOleInPlaceSite/OnInPlaceDeactivate
    (this :: <container-ole-in-place-site>) => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceSite/OnInPlaceDeactivate\r\n");

  let site = this.get-site;
  unless ( null?(site.document-in-place-ole-object) ) 
    Release(site.document-in-place-ole-object);
    site.document-in-place-ole-object := null-pointer(<LPOLEINPLACEOBJECT>);
  end unless;
  $S-OK
end method IOleInPlaceSite/OnInPlaceDeactivate;

// not implemented
define method IOleInPlaceSite/DiscardUndoState
    (this :: <container-ole-in-place-site>) => status :: <HRESULT>;

  OutputDebugString("IOleInPlaceSite/DiscardUndoState\r\n");
  $E-FAIL  // or should this be $E-NOTIMPL ? (neither is documented for this)
end method IOleInPlaceSite/DiscardUndoState;


// not implemented
define method IOleInPlaceSite/DeactivateAndUndo
    (this :: <container-ole-in-place-site>) => status :: <HRESULT>;
  OutputDebugString("IOleInPlaceSite/DeactivateAndUndo\r\n");
  $E-FAIL  // or should this be $E-NOTIMPL ? (neither is documented for this)
end method IOleInPlaceSite/DeactivateAndUndo;

// The object calls this method when it's size changes during
// an in-place session.
//
// Parameters:
//
//      LPCRECT lprcPosRect -   The new object rect
define method IOleInPlaceSite/OnPosRectChange
    (this :: <container-ole-in-place-site>, lprcPosRect :: <LPCRECT>) 
	=> status :: <HRESULT>;

  OutputDebugString("IOleInPlaceSite/OnPosRectChange\r\n");
  let site = this.get-site;

  // update the size in the document object
  // NOTE: here we must call IOleObject::GetExtent to get actual extents
  //       of the running object. IViewObject2::GetExtent returns the
  //       last cached extents.
  IOleObject/GetExtent(site.document-ole-object, site.document-draw-aspect,
		       site.site-object-size);
  // need eror check???
  with-stack-structure( rect :: <LPRECT> )
    GetClientRect(site.document-container-window, rect);
  
    // tell the object its new size
    IOleInPlaceObject/SetObjectRects(site.document-in-place-ole-object,
				     lprcPosRect, // position rectangle
				     rect); // clipping rectangle
    // need eror check???
  end with-stack-structure;
  $S-OK
end method IOleInPlaceSite/OnPosRectChange;
