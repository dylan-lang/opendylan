Module:    OLE-Server
Synopsis:  Methods for class <COleInPlaceObject>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Called to deactivate the object

define method IOleInPlaceObject/InPlaceDeactivate(this :: <COleInPlaceObject>) 
 => status :: <HRESULT>;
  
  Output-Debug-String("IOleInPlaceObject/InPlaceDeactivate\r\n");

  let obj :: <basic-ole-server> = this.get-obj;
  if ( obj.in-place-active? )

    obj.in-place-active? := #f;

    OLE-part-in-place-deactivated(obj); // notify application before change

    // deactivate the user interface
    remove-frame-level-UI(obj);
    do-in-place-hide(obj);

    let site = obj.container-IOleInPlaceSite;
    // tell the container that we are deactivating.
    unless ( null?(site) )
      IOleInPlaceSite/OnInPlaceDeactivate(site);
      obj.container-IOleInPlaceSite := $NULL-interface;
      Release(site);
    end unless;
  end if;
  $S-OK
end method IOleInPlaceObject/InPlaceDeactivate;


// Instructs us to remove our UI.

define method IOleInPlaceObject/UIDeactivate(this :: <COleInPlaceObject>)
			=> status :: <HRESULT>;

  Output-Debug-String("IOleInPlaceObject/UIDeactivate\r\n");

  let obj = this.get-obj;
  OLE-util-flush-view-change(obj);
  let status = remove-frame-level-UI(obj);
  if ( obj.using-hatch-window? )
    // remove hatching, but leave doc window visible in container.
    let doc-window = OLE-part-doc-window(obj);
    SetParent(obj.get-hatch-window, obj.app-parent-window);
    check-win32-result("SetParent",
      SetParent(doc-window, obj.container-parent-window) );
    obj.using-hatch-window? := #f;
    with-stack-structure ( position-rect :: <LPRECT> )
      with-stack-structure ( clip-rect :: <LPRECT> )
	// get window position from the container
        let site = obj.container-IOleInPlaceSite;
	let ( status, frame, doc ) = 
	  IOleInPlaceSite/GetWindowContext(site, position-rect, clip-rect,
					   obj.in-place-frame-info);
        check-ole-status(status, "GetWindowContext", site);
        Release(frame); Release(doc); // don't need these here.
        set-window-sizes(obj, position-rect, clip-rect, #f, #f);
      end with-stack-structure;
    end with-stack-structure;
  end if;
  status
end method IOleInPlaceObject/UIDeactivate;


// Called when the container clipping region or the object position changes.

define method IOleInPlaceObject/SetObjectRects
    (this :: <COleInPlaceObject>,
     position-rect :: <LPCRECT>,
     clip-rect :: <LPCRECT>) => status :: <HRESULT>;

  Output-Debug-String("IOleInPlaceObject/SetObjectRects\r\n");
  let obj = this.get-obj;
  set-window-sizes(obj, position-rect, clip-rect, #t, obj.using-hatch-window?)
end method IOleInPlaceObject/SetObjectRects;


// Returns the window handle of the in-place object

define method IOleWindow/GetWindow(this :: <COleInPlaceObject>) 
 => ( status :: <HRESULT>, hwnd :: <HWND> );

  Output-Debug-String("IOleWindow/GetWindow\r\n");
  values( $S-OK, OLE-part-doc-window(this.get-obj) )
end method IOleWindow/GetWindow;


// Used in performing Context Sensitive Help

define method IOleWindow/ContextSensitiveHelp(this :: <COleInPlaceObject>,
					      entering? :: <boolean>) 
	=> status :: <HRESULT>;

  Output-Debug-String("IOleWindow/ContextSensitiveHelp");
  Output-Debug-Boolean(entering?);

  $E-NOTIMPL	// Not yet implemented		???
end method IOleWindow/ContextSensitiveHelp;


// Called when the container wants to undo the last edit made in the object.

define method IOleInPlaceObject/ReactivateAndUndo(this :: <COleInPlaceObject>) 
	=> status :: <HRESULT>;

  Output-Debug-String("IOleInPlaceObject/ReactivateAndUndo\r\n");
  $INPLACE-E-NOTUNDOABLE // undo not supported
end method IOleInPlaceObject/ReactivateAndUndo;
