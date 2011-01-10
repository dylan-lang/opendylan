Module:    OLE-Server
Synopsis:  OLE server object class <ole-server-framework> and its methods.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This is the object instantiated by the class factory and is the
// "controlling unknown" for all of the other interfaces.

define open COM-interface <basic-ole-server> ( <lib-init-mixin>,
					      <IUnknown>,
					      <factory-args-mixin> )

  // used during in-place negotiation
  slot in-place-active?  :: <boolean> = #f;
  slot in-place-visible? :: <boolean> = #f;
  slot UI-active?	 :: <boolean> = #f;
  slot shared-menu-handle :: <HMENU> = $NULL-HMENU;
  slot ole-menu :: <HOLEMENU> = null-handle(<HOLEMENU>);
  slot menu-group-widths :: <LPOLEMENUGROUPWIDTHS>
		= null-pointer(<LPOLEMENUGROUPWIDTHS>);
	
  constant slot in-place-frame-info :: <LPOLEINPLACEFRAMEINFO>
		= make(<LPOLEINPLACEFRAMEINFO>);
	
  slot container-parent-window	:: <HWND> = $NULL-HWND;
  slot get-hatch-window	        :: <HWND> = $NULL-HWND;	
  slot app-parent-window	:: <HWND> = $NULL-HWND;	

  // interfaces used
	
  slot container-IOleClientSite	     :: <Interface> = $NULL-interface; 
  slot container-IOleAdviseHolder    :: <Interface> = $NULL-interface;
  slot container-IDataAdviseHolder   :: <Interface> = $NULL-interface;
  slot container-IOleInPlaceFrame    :: <Interface> = $NULL-interface;
  slot container-IOleInPlaceUIWindow :: <Interface> = $NULL-interface;
  slot container-IOleInPlaceSite     :: <Interface> = $NULL-interface;

  // interfaces implemented
	
  slot server-IOleObject	:: <COleObject>;
  slot server-IDataObject	:: <CDataObject>;
  slot server-IOleInPlaceActiveObject :: <COleInPlaceActiveObject>;
  slot server-IOleInPlaceObject	:: <COleInPlaceObject>;
  slot server-IExternalConnection :: <CExternalConnection>;

  // context options

  slot obj-class-ID :: <REFCLSID> = $CLSID-NULL;

  // other state information

  slot ROT-registration	:: <unsigned-fixnum> = 0;

  slot deferred-data-change? :: <boolean> = #f;
	
  slot object-closing?	:: <boolean> = #f;

  slot embedded-width 	:: <fixnum> = 0;
  slot embedded-height	:: <fixnum> = 0;

  slot using-hatch-window? :: <boolean> = #f;

end <basic-ole-server>;

define open generic after-initialize (obj) => ();

define method initialize ( obj :: <basic-ole-server>,
			  #rest args, #key clsid, #all-keys) => ();
  if ( clsid )
    obj.obj-class-ID := as(<REFCLSID>, clsid);
  end if;

  next-method();

  ole-util-init();
  after-initialize(obj);
end initialize;


define method after-initialize ( obj :: <basic-ole-server> ) => ();

  unless ( slot-initialized?(obj, server-IOleObject) )
    // unless overridden in ole-control-framework
    obj.server-IOleObject := make( <COleObject>, server-framework: obj,
				  controlling-unknown: obj );
  end unless;
  obj.server-IDataObject := make( <CDataObject>, server-framework: obj,
				  controlling-unknown: obj );
  obj.server-IOleInPlaceActiveObject := make( <COleInPlaceActiveObject>,
					      server-framework: obj,
					      controlling-unknown: obj );
  obj.server-IOleInPlaceObject := make( <COleInPlaceObject>,
					server-framework: obj,
					controlling-unknown: obj );

  obj.server-IExternalConnection := make( <CExternalConnection>,
					 server-framework: obj,
					 controlling-unknown: obj );
end after-initialize;

define method terminate (obj :: <basic-ole-server>) => ();

  Output-Debug-String("terminate(<basic-ole-server>)\r\n");
  destroy(obj.in-place-frame-info);

  unless ( null-handle?(obj.get-hatch-window) )
    DestroyWindow(obj.get-hatch-window);
    obj.get-hatch-window := $NULL-HWND;
  end unless;

  obj.object-closing? := #t;  // disable `OLE-util-close-server' function.

  // Normally the following interface pointers should be NULL by now,
  // but just in case...
  Release(obj.container-IDataAdviseHolder);
  Release(obj.container-IOleAdviseHolder);
  Release(obj.container-IOleClientSite);
  Release(obj.container-IOleInPlaceFrame);
  Release(obj.container-IOleInPlaceUIWindow);
  Release(obj.container-IOleInPlaceSite);
  
  next-method();
end method terminate ;

// copy the class ID into the given pointer.
define method copy-class-ID (obj :: <basic-ole-server>, lpClassID :: <LPCLSID>)
 => (status :: <HRESULT>);
  let clsid :: <REFCLSID> = obj.obj-class-ID;
  %memcpy(lpClassID, clsid, size-of(<CLSID>));
  $S-OK
end copy-class-ID;

define method ole-local-server? (obj :: <basic-ole-server>)
 => (local-server? :: <boolean>);
  #t // .EXE file, not .DLL file
end;


// Note: the distinction between <ole-server-framework> and <basic-ole-server>
// was due to a misunderstanding, and is not serving any current purpose, so
// these might be recombined some day.

define open COM-interface <ole-server-framework> ( <basic-ole-server> )

  slot server-IPersistStorage :: false-or(<CPersistStorage>) = #f;

end <ole-server-framework>;


define method after-initialize ( obj :: <ole-server-framework> ) => ();
  next-method();
  if ( obj.server-IPersistStorage == #f )
    obj.server-IPersistStorage := make(<CPersistStorage>,
				       server-framework: obj,
				       controlling-unknown: obj);
  end if;
end method after-initialize;


define method OLE-util-in-place-active?(obj :: <basic-ole-server>)
	=> value :: <boolean>; 
 /* return */	obj.in-place-active? 
end method OLE-util-in-place-active?;

define method OLE-util-UI-active?(obj :: <basic-ole-server>)
	=> value :: <boolean>; 
 /* return */	obj.UI-active?
end method OLE-util-UI-active?;


// Returns a handle to a metafile representation of the object.

define method get-meta-file-picture (server-obj :: <basic-ole-server>,
				     kind :: <fixnum>)
	=> ( status :: <HRESULT>, value :: <HANDLE> );

  Output-Debug-String("get-meta-file-picture\r\n");

  // allocate the memory for the METAFILEPICT structure
  let hMFP :: <HANDLE> = GlobalAlloc(logior($GMEM-SHARE,$GHND),
				     size-of(<METAFILEPICT>));
  let lpMFP :: <LPMETAFILEPICT> =
    pointer-cast(<LPMETAFILEPICT>, GlobalLock(hMFP));

  // get the size of the object in HIMETRIC
  let ( pt-x, pt-y ) = OLE-util-HIMETRIC-size(server-obj);

  // fill out the METAFILEPICT structure
  lpMFP.mm-value := $MM-ANISOTROPIC;
  lpMFP.xExt-value := pt-x;
  lpMFP.yExt-value := pt-y;

  // Create the metafile
  let hDC :: <HDC> = 
    if ( kind = $TYMED-ENHMF )
      with-stack-structure ( rect :: <LPRECT> )
	rect.left-value := 0;
	rect.top-value := 0;
	rect.right-value := pt-x;
	rect.bottom-value := pt-y;
        CreateEnhMetaFile(null-handle(<HDC>), $NULL-string,
			  rect, $NULL-string)
      end with-stack-structure
    else
      CreateMetaFile($NULL-string)
    end if;

  SetWindowOrgEx(hDC, 0, 0, $NULL-POINT);
  let ( width, height ) = OLE-util-current-size(server-obj);
  SetWindowExtEx(hDC, width, height, null-pointer(<LPSIZE>));

  let status = OLE-part-draw-metafile(server-obj, hDC) | $S-OK;

  lpMFP.hMF-value := 
    if ( kind = $TYMED-ENHMF )
      CloseEnhMetaFile(hDC)
    else
      CloseMetaFile(hDC)
    end if;

  // unlock the metafilepict
  GlobalUnlock(hMFP);

  values( status, hMFP )
end method get-meta-file-picture;


// This is a default method which can be overridden by the user.
define method OLE-part-get-data (obj :: <basic-ole-server>,
				 pformatetcIn :: <LPFORMATETC>,
				 pmedium :: <LPSTGMEDIUM> )
 => status :: <HRESULT>;
	
  // Check the data format
  if ( pformatetcIn.cfFormat-value ~= $CF-METAFILEPICT )
    $DV-E-FORMATETC
  elseif ( pformatetcIn.dwAspect-value ~= $DVASPECT-CONTENT )
    $DV-E-DVASPECT
  elseif ( zero?(logand(pformatetcIn.tymed-value,
			logior($TYMED-MFPICT, $TYMED-ENHMF))) )
    $DV-E-TYMED
  elseif ( null?(pmedium) )
    // not actually returning any data, just validating the format.
    $S-OK
  else // format OK, now fill the medium
    let kind =
      // Use new-style metafile if the container supports it.
      if ( ~ zero?(logand(pformatetcIn.tymed-value, $TYMED-ENHMF)) )
	$TYMED-ENHMF
      else
	$TYMED-MFPICT
      end if;
    let ( status, picture-handle ) = get-meta-file-picture(obj, kind);
    pmedium.tymed-value := kind;
    pmedium.u-value.hGlobal-value := picture-handle;
    pmedium.pUnkForRelease-value := $null-interface;
    status
  end if
end method;


define method do-in-place-activate(obj :: <basic-ole-server>,
				   verb :: <fixnum>)
	=> done :: <boolean>;

  let done :: <boolean> = #f;

  block(error-return)
	
    Output-Debug-String("do-in-place-activate\r\n");
    obj.object-closing? := #f; // DLL server can be re-activated after close

    let notify-application? :: <boolean> = #f;
    let doc-window = OLE-part-doc-window(obj);
    // if not currently in place active
    if ( ~ obj.in-place-active? )

      if ( null?(doc-window) )
	// Application doesn't support in-place activation.
	error-return();
      end if;
		
      let site :: <LPOLEINPLACESITE> = obj.container-IOleInPlaceSite;
      if ( null?(site) )
	// get the in-place site
	let ( status, interface ) =
	  QueryInterface(obj.container-IOleClientSite, $IID-IOleInPlaceSite);
	site := pointer-cast(<LPOLEINPLACESITE>, interface);
	obj.container-IOleInPlaceSite := site;
	if ( FAILED?(status) )
	  error-return();
	end if;
      end if;

      // if the in-place site could not be obtained, or refuses to in-place
      // activate then return error. (e.g., might refuse in design mode)
      if ( null?(site) |
	   (IOleInPlaceSite/CanInPlaceActivate(site) ~= $S-OK) )
	Release(site);
	obj.container-IOleInPlaceSite := $NULL-interface;
	error-return();
      end if;

      // tell the site that we are activating.
      IOleInPlaceSite/OnInPlaceActivate(site);
      obj.in-place-active? := #t;

      notify-application? := #t;
    end if;

    let show-hatch? :: <boolean> =
      obj.hatch-when-UI-active? & (verb ~= $OLEIVERB-INPLACEACTIVATE);

    let make-visible? :: <boolean> =
      (~ obj.in-place-visible?) | ( show-hatch? & ~ obj.UI-active? );

    if ( make-visible? )
		
      // get the window handle of the site
      let site = obj.container-IOleInPlaceSite;
      let ( status , container-window ) = IOleWindow/GetWindow(site);
      check-ole-status(status, "GetWindow", site);
      obj.container-parent-window := container-window;

      with-stack-structure ( position-rect :: <LPRECT> )
       with-stack-structure ( clip-rect :: <LPRECT> )

	// get window context from the container
	let ( status, frame, doc ) = 
	  IOleInPlaceSite/GetWindowContext(site, position-rect, clip-rect,
					   obj.in-place-frame-info);
        check-ole-status(status, "GetWindowContext", site);
        Release(obj.container-IOleInPlaceFrame);
        Release(obj.container-IOleInPlaceUIWindow);
	obj.container-IOleInPlaceFrame := frame;
	obj.container-IOleInPlaceUIWindow := doc;

        // Adjust window configuration
        let hatch-window = obj.get-hatch-window;
        if ( show-hatch? )
	  if ( null-handle?(hatch-window) )
	    hatch-window := create-hatch-window(container-window,
						application-instance-handle());
	    obj.get-hatch-window := hatch-window;
	  else
	    check-win32-result("SetParent",
	      SetParent(hatch-window, container-window) );
	  end if;
	else
	  hatch-window := container-window;
	end if;
	let old-parent = SetParent(doc-window, hatch-window);
	check-win32-result("SetParent", old-parent);
	unless ( old-parent = hatch-window | old-parent = container-window )
	  obj.app-parent-window := old-parent;
	end unless;
        obj.using-hatch-window? := show-hatch?;
        set-window-sizes(obj, position-rect, clip-rect, #f, show-hatch?);

       end with-stack-structure;
      end with-stack-structure;
    end if; //  make-visible?

    // if not yet UI Active and not doing activate only
    if ( (~ obj.UI-active?) & (verb ~= $OLEIVERB-INPLACEACTIVATE) )
		
      // tell the in-place site that we are activating
      IOleInPlaceSite/OnUIActivate(obj.container-IOleInPlaceSite);

      // set the focus to our object window
      OLE-part-set-focus(obj);

      // add the frame level UI.
      add-frame-level-UI(obj);

      notify-application? := #t;
    end if; // end if not yet UI Active

    if ( make-visible? )
      obj.in-place-visible? := #t;
      if ( show-hatch? )
	show-hatch-window(obj);
      end if;

      // ensure that the document window is visible
      OLE-part-show-window(obj, doc-window);

      unless ( verb = $OLEIVERB-UIACTIVATE )
	// tell the client site to show the object
	IOleClientSite/ShowObject(obj.container-IOleClientSite);
      end unless;
    end if; // make-visible?

    if ( notify-application? )
      // notify user of change in state
      OLE-part-in-place-activated(obj);
    end if;

    done := #t;
  end block;
  unless ( done )
    Output-Debug-String("failed in-place activate\r\n");
  end unless;
  /* return */ done 
end method do-in-place-activate;



define method set-window-sizes ( obj :: <basic-ole-server>,
				position-rect :: <LPRECT>,
				clip-rect :: <LPRECT>,
				repaint? :: <boolean>,
				show-hatch? :: <boolean>,
				#key set-position? :: <boolean> = #t)
 => (status :: <HRESULT>);

  // (Use "%=" instead of "%d" because the value might be a <machine-word>.)
  debug-out("  position = %=, %=; %=, %=\n  clip = %=, %=; %=, %=\n",
	    position-rect.left-value, position-rect.top-value,
	    position-rect.right-value, position-rect.bottom-value,
	    clip-rect.left-value, clip-rect.top-value,
	    clip-rect.right-value, clip-rect.bottom-value);

  begin
    let new-width :: <fixnum> =
      position-rect.right-value - position-rect.left-value;
    let new-height :: <fixnum> =
      position-rect.bottom-value - position-rect.top-value;
    let ( old-width, old-height ) = OLE-util-current-size(obj);
    if ( new-width ~= old-width | new-height ~= old-height )
      debug-out("calling OLE-part-change-size; old = %=,%=; new = %=,%=\n",
		 old-width, old-height, new-width, new-height);
      OLE-part-change-size(obj, new-width, new-height);
      obj.embedded-width  := new-width;
      obj.embedded-height := new-height;
    end if;
  end;

  with-stack-structure( doc-rect :: <LPRECT> )

    if ( show-hatch? )
      // Set the size of the Hatch Window.
      set-hatch-window-size(obj.get-hatch-window, position-rect, clip-rect,
			    doc-rect, set-position?);
    else // no hatch window; just show the doc window directly.
      IntersectRect(doc-rect, position-rect, clip-rect);
    end if;
    // Move the actual object window
    debug-out("  doc-rect = %=, %=; %=, %=\n",
	      doc-rect.left-value, doc-rect.top-value,
	      doc-rect.right-value, doc-rect.bottom-value);
    if ( set-position? )
      OLE-part-position-window(obj, doc-rect, repaint?);
    else // when called from IOleObject/SetExtent
      let doc-window = OLE-part-doc-window(obj);
      SetWindowPos(doc-window, $NULL-HWND, 
		   0, 0, // x and y ignored by $SWP-NOMOVE
		   doc-rect.right-value - doc-rect.left-value,
		   doc-rect.bottom-value - doc-rect.top-value,
		   logior($SWP-NOZORDER, $SWP-NOACTIVATE, $SWP-NOMOVE));
      if ( repaint? )
	UpdateWindow(doc-window);
      end if;
    end if;
  end with-stack-structure;
  $S-OK
end method set-window-sizes;

define open generic hatch-when-UI-active? (obj) => ( show? :: <boolean> );

// An override method will be used for OLE Controls, for which
// hatching is an option controlled by the container.
define method hatch-when-UI-active? ( obj :: <basic-ole-server> )
 => ( show? :: <boolean> );
  #t
end method;

define method OLE-util-current-size ( obj :: <basic-ole-server> )
 => ( width :: <integer>, height :: <integer> );
  let width = obj.embedded-width;
  let height = obj.embedded-height;
  if ( zero?(logior(width, height)) )
    let ( x, y ) = OLE-part-requested-size(obj);
    obj.embedded-width := x;
    obj.embedded-height := y;
    width := x;
    height := y;
  end if;
  values(width, height)
end method;

// Default method
define method OLE-part-requested-size (obj :: <basic-ole-server>)
 => ( width :: <integer>, height :: <integer> );
  let window = OLE-part-doc-window(obj);
  if ( null?(window) )
    error("need OLE-part-requested-size method for %=", obj);
    values(99,99)
  else
    // note: need to work-around temporary lack of support for
    // multiple value return from with-stack-structure.
    let x :: <fixnum> = 0;
    let y :: <fixnum> = 0;
    with-stack-structure( rect :: <LPRECT> )
      GetClientRect(window, rect);
      x := rect.right-value;
      y := rect.bottom-value;
    end with-stack-structure;
    values(x,y)
  end if
end method;

// Default method
define method OLE-part-change-size (obj :: <basic-ole-server>,
				    width, height) => (ok :: <boolean>);
  #t
end method;

// Default method that will probably be adequate most of the time, but
// the application may want a wrapper method to take additional action.
// Note that this does have the side-effect of sending a WM_SIZE message,
// so that may be sufficient to notify the application.
define method OLE-part-position-window ( obj :: <basic-ole-server>,
				      rect :: <LPRECT>,
				      repaint? ) => ();
  // Move the actual object window
  MoveWindow(OLE-part-doc-window(obj),
	     rect.left-value, rect.top-value,
	     rect.right-value - rect.left-value,
	     rect.bottom-value - rect.top-value,
	     repaint?);
  values()
end method;

// Default method -- no title.
define method OLE-part-title (obj :: <basic-ole-server>)
				=> title :: <LPOLESTR>;
  $NULL-OLESTR
end method;

// Default method sets focus to the doc window.
define method OLE-part-set-focus ( obj :: <basic-ole-server> ) => ();
  SetFocus(OLE-part-doc-window(obj));
  values()
end method;

define open generic OLE-part-show-window (obj, window);

// Default method; needs to be overridden for DUIM.
define method OLE-part-show-window (obj :: <basic-ole-server>,
				    window :: <HWND>) => ();
  ShowWindow(window, $SW-SHOWNORMAL);
  InvalidateRect(window, $NULL-RECT, #t); // queues WM_PAINT message
  values()
end method;

define method show-hatch-window(obj :: <basic-ole-server>) => ();

  ShowWindow(obj.get-hatch-window, $SW-SHOW);
  values()
end method show-hatch-window;


define method hide-hatch-window(obj :: <basic-ole-server>) => ();

  ShowWindow(obj.get-hatch-window, $SW-HIDE);
  values()
end method hide-hatch-window;


// Create the combined menus used during inplace activation.

define method assemble-menus(obj :: <basic-ole-server>) => ();
  
  Output-Debug-String("assemble-menus\r\n");

  let menugroupwidths :: <LPOLEMENUGROUPWIDTHS> = make(<LPOLEMENUGROUPWIDTHS>);
  let widths :: <PLONG> = menugroupwidths.width-value;
  for ( i from 0 to 5 )
    pointer-value(widths, index: i) := 0;
  end for;

  //  Create the menu resource
  let hmenuShared = CreateMenu();

  // have the container insert its menus
  if ( SUCCEEDED?(IOleInPlaceFrame/InsertMenus(obj.container-IOleInPlaceFrame,
					       hmenuShared,
					       menugroupwidths)) )

    // Number of menus in container's file menu group:
    let edit-position :: <fixnum> = pointer-value(widths, index: 0);
    let object-position :: <fixnum> =
      edit-position + pointer-value(widths, index: 2);
    let help-position :: <fixnum> =
      object-position + pointer-value(widths, index: 4);
    // insert the server menus
    let ( nedit, nobject, nhelp ) =
      OLE-part-insert-menus(obj, hmenuShared,
			    edit-position, object-position, help-position);
    pointer-value(widths, index: 1) := nedit;   // Edit group menus
    pointer-value(widths, index: 3) := nobject; // Object group menus
    pointer-value(widths, index: 5) := nhelp;   // Help group menus
    obj.menu-group-widths := menugroupwidths;
    obj.shared-menu-handle := hmenuShared;

  else
    // Destroy the menu resource
    DestroyMenu(hmenuShared);
    obj.shared-menu-handle := $NULL-HMENU;
    destroy(menugroupwidths);
  end if;

  // tell OLE to create the menu descriptor
  obj.ole-menu := OleCreateMenuDescriptor(obj.shared-menu-handle,
					   obj.menu-group-widths);
  values()
end method assemble-menus;

// Default method -- no server menus.
define method OLE-part-insert-menus ( obj :: <basic-ole-server>,
				     hmenuShared, edit-position,
				     object-position, help-position )
  => ( nedit :: <integer>, nobject :: <integer>, nhelp :: <integer> );
  values(0, 0, 0)
end method;


// Add the Frame level user interface

define method add-frame-level-UI(obj :: <basic-ole-server>) => ();

  Output-Debug-String("add-frame-level-UI\r\n");

  let raw-title = OLE-part-title(obj);
  let title :: <LPOLESTR> =
    if ( raw-title == #f | empty?(raw-title) )
      $NULL-OLESTR
    else OLESTR(raw-title)
    end if;
  // set the active object on the frame
  // (must do this before updating the menu)
  IOleInPlaceUIWindow/SetActiveObject(obj.container-IOleInPlaceFrame,
				      obj.server-IOleInPlaceActiveObject,
				      title);

  // set the active object on the Doc, if available.
  unless ( null?(obj.container-IOleInPlaceUIWindow) )
    IOleInPlaceUIWindow/SetActiveObject(obj.container-IOleInPlaceUIWindow,
					obj.server-IOleInPlaceActiveObject,
					title);
  end unless;

  // create the combined menu bar
  assemble-menus(obj);

  // add the combined menu
  let status = IOleInPlaceFrame/SetMenu(obj.container-IOleInPlaceFrame,
					obj.shared-menu-handle,
					obj.ole-menu,
					OLE-part-command-window(obj));
  if ( FAILED?(status) )
    // Not signalling an error here because it is OK if the container
    // doesn't support a menu bar.  In particular, an MFC dialog box returns
    // a "not implemented" status even though that is not one of the
    // documented error codes for this operation.
    debug-out("SetMenu %=\n", status);
  end if;

  // Negotiate for toolbar space and set the border space.
  set-border-space(obj);
  
  obj.UI-active? := #t;
  OLE-part-UI-activated(obj);
  values()
end method add-frame-level-UI;

define method set-border-space (obj :: <basic-ole-server>) => ();

  let in-place-frame = obj.container-IOleInPlaceFrame;
  unless ( null?(in-place-frame) )

    // Negotiate for toolbar space and set the border space.   

    let tool-bar = OLE-part-toolbar-window(obj);
    if ( null?(tool-bar) )
      // let the container keep it's own tool bar
      IOleInPlaceUIWindow/SetBorderSpace(in-place-frame, $NULL-RECT);
    else
      with-stack-structure ( rect :: <LPRECT> )
	check-win32-result("GetWindowRect",
	  GetWindowRect(tool-bar, rect));
        let tool-bar-height :: <fixnum> = rect.bottom-value - rect.top-value;
	SetRect(rect, 0, tool-bar-height, 0, 0);
	if ( (tool-bar-height > 2)
	      & SUCCEEDED?(IOleInPlaceUIWindow/RequestBorderSpace
			     (in-place-frame, rect))
	      & SUCCEEDED?(IOleInPlaceUIWindow/SetBorderSpace(in-place-frame,
							      rect))
	      & SUCCEEDED?(IOleInPlaceUIWindow/GetBorder(in-place-frame,rect)))
	  Output-Debug-String("showing the tool bar\r\n");
	  let ( status, frame-window ) = IOleWindow/GetWindow(in-place-frame);
	  check-ole-status(status, "GetWindow", in-place-frame);
	  let old-parent = SetParent(tool-bar, frame-window);
	  check-win32-result("SetParent for tool-bar", old-parent);
	  check-win32-result("SetWindowPos for tool bar",
	    SetWindowPos(tool-bar, $NULL-HWND,
			 rect.left-value, rect.top-value,    // x, y
			 rect.right-value - rect.left-value, // width
			 tool-bar-height,		     // height
			 $SWP-NOZORDER));
	  if ( old-parent = frame-window )
	    ShowWindow(tool-bar, $SW-SHOW);
	  else
	    OLE-part-show-window(obj, tool-bar); // show and paint
	  end if;
	else
	  Output-Debug-String("failed to place tool bar\r\n");
	  // remove the container's tool bar even though can't display ours.
	  SetRectEmpty(rect);
	  IOleInPlaceUIWindow/SetBorderSpace(in-place-frame, rect);
	end if;
      end with-stack-structure;
    end if;
  end unless;

  // There will only be a UI Window if in an MDI container
  let ui-window = obj.container-IOleInPlaceUIWindow;
  unless ( null?(ui-window) )
    IOleInPlaceUIWindow/SetBorderSpace(ui-window, $NULL-RECT);
  end unless;
  values()
end set-border-space;

// default method:
define method OLE-part-toolbar-window ( obj :: <object> ) => window :: <HWND>;
  $NULL-HWND
end;

// Default method, usually needs to be overridden, but this allows it to
// be omitted when there aren't any server menus.
define method OLE-part-command-window(obj :: <basic-ole-server>)
 => window :: <HWND>;
  OLE-part-doc-window(obj)
end method;

// Hide the object while in-place active

define method do-in-place-hide(obj :: <basic-ole-server>) => ();

  Output-Debug-String("do-in-place-hide\r\n");

  OLE-util-flush-view-change(obj);

  if ( obj.in-place-visible? )

    obj.in-place-visible? := #f;

    disconnect-hatch-window(obj);

    // release the in-place frame
    Release(obj.container-IOleInPlaceFrame);
    // only holding one ref. to frame.
    obj.container-IOleInPlaceFrame := $NULL-Interface; 

    // release the UIWindow if it is there.
    Release(obj.container-IOleInPlaceUIWindow);
    obj.container-IOleInPlaceUIWindow := $NULL-Interface;

  end if;
  values()
end method do-in-place-hide;

define method remove-tool-bar (obj :: <basic-ole-server>) => ();

  let tool-bar = OLE-part-toolbar-window(obj);
  unless ( null?(tool-bar) )
    Output-Debug-String("removing tool bar\r\n");
    ShowWindow(tool-bar, $SW-HIDE);
    SetParent(tool-bar, obj.app-parent-window); // child, parent
  end unless;
  values()
end method;

define method disconnect-hatch-window ( obj :: <basic-ole-server>) => ();
  // change the parenting
  let doc-window = OLE-part-doc-window(obj);
  SetParent(doc-window, obj.app-parent-window); // child, parent
  if ( obj.using-hatch-window? )
    SetParent(obj.get-hatch-window, doc-window);
    obj.using-hatch-window? := #f;
  end if;
  values()
end method;

// Disassemble the combined menus used in in-place activation

define method disassemble-menus(obj :: <basic-ole-server>) => ();

  Output-Debug-String("disassemble-menus\r\n");
  let in-place-frame = obj.container-IOleInPlaceFrame;

  // stop container frame from using this menu.
  let status =
    IOleInPlaceFrame/SetMenu(in-place-frame,
			     $NULL-HMENU, null-handle(<HOLEMENU>), $NULL-HWND);
  if ( FAILED?(status) )
    // Not signalling an error here because it doesn't really matter
    // to us if the container doesn't support a menu bar.
    debug-out("SetMenu %=\n", status);
  end if;

  // destroy the menu descriptor
  OleDestroyMenuDescriptor(obj.ole-menu);

  let shared-menu = obj.shared-menu-handle;
  unless ( null?(shared-menu) )
		
    // remove the menus that we added
    let widths :: <PLONG> = obj.menu-group-widths.width-value;
    let offset :: <fixnum> = 0;
    for ( group :: <fixnum> from 1 to 5 by 2 )
      offset := offset + pointer-value(widths, index: group - 1);
      for ( i from 0 below pointer-value(widths, index: group) )
	let app-menu = GetSubMenu(shared-menu, offset);
	let ok = RemoveMenu( shared-menu, offset, $MF-BYPOSITION);
	check-win32-result("RemoveMenu", ok); // temporary???
	OLE-part-release-menu(obj, app-menu);
      end for;
    end for;
    destroy(obj.menu-group-widths);
    obj.menu-group-widths := null-pointer(<LPOLEMENUGROUPWIDTHS>);

    // have the container remove its menus
    status := IOleInPlaceFrame/RemoveMenus(in-place-frame, shared-menu);
    check-ole-status(status, "IOleInPlaceFrame/RemoveMenus",
		     in-place-frame, shared-menu); // temporary???

    // Destroy the menu resource
    let ok = DestroyMenu(shared-menu);
    check-win32-result("DestroyMenu", ok); // temporary???

    obj.shared-menu-handle := $NULL-HMENU;
  end unless;

  values();
end method disassemble-menus;

// Default method does nothing.
// The application should override this if DestroyMenu needs to be called.
define method OLE-part-release-menu (obj :: <basic-ole-server>,
				     hmenu :: <HMENU>) => ();
  values()
end method;

define open generic OLE-util-view-changed(obj);

define method OLE-util-view-changed(obj :: <basic-ole-server>) => ();
  OLE-util-send-view-change(obj)
end method OLE-util-view-changed;

/* // This is what we would like to do, but it isn't safe to do at this
   // level because we don't always know when to force the notification 
   // soon enough.  For example, if the container initiates a Save while
   // the component is still active, we don't have a chance to update the
   // view before the save begins.  However, a higher-level library may be
   // able to do that by calling OLE-util-flush-view-change when the
   // embedded pane loses focus.
define method OLE-util-view-changed(obj :: <basic-ole-server>) => ();
  if ( OLE-util-in-place-active?(obj) )
    // When running in-place active, don't update the container's image
    // immediately since it's window is covered anyway.  Wait until we
    // deactivate.
    OLE-util-defer-view-change(obj);
  else
    OLE-util-send-view-change(obj);
  end if;
end method OLE-util-view-changed;
*/

// Temporary for backward compatibility:
define constant OLE-util-data-changed = OLE-util-view-changed;

define inline function OLE-util-defer-view-change (obj :: <basic-ole-server>)
 => ();
  obj.deferred-data-change? := #t;
end;  

define inline function OLE-util-flush-view-change (obj :: <basic-ole-server>)
 => ();
  if ( obj.deferred-data-change? )
    OLE-util-send-view-change(obj)
  end if
end;

// Use the data advise holder to send a data change,
// then update the Running Object Table to note the time of change.

define open generic OLE-util-send-view-change(obj) => ();

define method OLE-util-send-view-change(obj :: <basic-ole-server>) => ();

  Output-Debug-String("OLE-util-send-view-change\r\n");

  obj.deferred-data-change? := #f;

  unless ( null?(obj.container-IDataAdviseHolder) )
    IDataAdviseHolder/SendOnDataChange(obj.container-IDataAdviseHolder,
				       obj.server-IDataObject, 0, 0);
  end unless;

  unless ( zero?(obj.ROT-registration) ) // used only when "linked"
    let ( status :: <HRESULT>,
	  ROT :: <LPRUNNINGOBJECTTABLE> ) = GetRunningObjectTable(0);
    unless ( null?(ROT) )
      with-stack-structure ( pFT :: <PFILETIME> )
	CoFileTimeNow(pFT);
	IRunningObjectTable/NoteChangeTime(ROT, obj.ROT-registration, pFT );
	Release(ROT);
      end with-stack-structure;
    end unless;
  end unless;

  values();
end method;

// Set the Container's status bar text
//      Note: this should be called on WM_MENUSELECT to clear the last
//      message in the status line.
// Return value:
//	$S-OK		-- whole text successfully displayed.
//	$S-TRUNCATED	-- displayed part of message too long to fit.
//	$E-FAIL		-- container does not support a status bar.
//	$OLE-E-NOT-INPLACEACTIVE -- not running in-place active.

define constant $empty-OLE-string :: <LPCOLESTR> = make(<LPCOLESTR>, size: 0);

define method OLE-util-set-status-text(obj :: <basic-ole-server>,
				       text :: false-or(<string>))
 => status :: <HRESULT>;

  // (Checking both flags so that this can be used within
  //  OLE-part-in-place-deactivated.)
  if ( obj.in-place-active? | obj.UI-active? )
    let message = text;
    let status =
      IOleInPlaceFrame/SetStatusText(obj.container-IOleInPlaceFrame,
				   if ( text & ~ empty?(text) )
				     (message := as(<LPCOLESTR>, text))
				   else
				     $empty-OLE-string
				   end if);
    unless ( message == text )
      destroy(message);
    end unless;
    status
  else
    $OLE-E-NOT-INPLACEACTIVE
  end if
end method;

// Break down the in-place UI

define method remove-frame-level-UI(obj :: <basic-ole-server>)
 => status :: <HRESULT>;

  OLE-util-flush-view-change(obj);
  if ( obj.UI-active? )

    Output-Debug-String("remove-frame-level-UI\r\n");
    OLE-part-UI-deactivated(obj); // notify application before change

    remove-tool-bar(obj);

    // rip down the combined menus
    disassemble-menus(obj);
		
    obj.UI-active? := #f;

    // if in an MDI container, call SetActiveObject on the doc.
    unless ( null?(obj.container-IOleInPlaceUIWindow) )
      IOleInPlaceUIWindow/SetActiveObject(obj.container-IOleInPlaceUIWindow,
					  $NULL-Interface,
					  $NULL-OLESTR );
    end unless;

    IOleInPlaceUIWindow/SetActiveObject(obj.container-IOleInPlaceFrame,
					$NULL-Interface,
					$NULL-OLESTR );

    // tell the container that our UI has gone away.
    unless ( null?(obj.container-IOleInPlaceSite) )
      IOleInPlaceSite/OnUIDeactivate(obj.container-IOleInPlaceSite,
				     #f /* no undo */ );
    end unless;

  end if;

  $S-OK
end method remove-frame-level-UI;


// default methods do nothing:
define method OLE-part-in-place-activated (obj :: <basic-ole-server>)
 => ();
  values()
end;

define method OLE-part-in-place-deactivated (obj :: <basic-ole-server>)
 => ();
  values()
end;

define method OLE-part-UI-activated (obj :: <basic-ole-server>)
 => ();
  values()
end;

define method OLE-part-UI-deactivated (obj :: <basic-ole-server>)
 => ();
  values()
end;


define method OLE-util-close-server( server-object :: <basic-ole-server>,
				     #key save = $OLECLOSE-SAVEIFDIRTY )
 => (status :: <HRESULT>);

  OLE-util-flush-view-change(server-object);

  if ( ~ server-object.object-closing? )
    Output-Debug-String("OLE-util-close-server\r\n");
    server-object.object-closing? := #t;

    AddRef(server-object); // hold object alive

    // if the object is currently in-place active, then deactivate
    if ( OLE-util-in-place-active?(server-object) )
      IOleInPlaceObject/InPlaceDeactivate(
		  server-IOleInPlaceObject(server-object));
    end if;

    // unregister from the Running Object Table
    if ( ROT-registration(server-object) ~= 0 )
      
      let ( status :: <HRESULT>, ROT :: <LPRUNNINGOBJECTTABLE> ) = 
	GetRunningObjectTable(0);
      if ( status = $NOERROR )
	Output-Debug-String("IRunningObjectTable/Revoke\r\n");
	IRunningObjectTable/Revoke(ROT, ROT-registration(server-object));
	Release(ROT);
      end if;
      ROT-registration(server-object) := 0;
    end if;

    // When clipboard support is added, should call `OleFlushClipboard' here
    // if this object owns the clipboard.		???

    // if we have a client site, instruct it to save the object
    let site = container-IOleClientSite(server-object);
    unless ( null?(site) )
      if ( server-object.server-IOleObject.open-as-separate-window )
	// opened out-of-place, update the size from the external window.
	let ( x, y ) = OLE-part-requested-size(server-object);
	if ( x > 0 & y > 0 )
	  server-object.embedded-width := x;
	  server-object.embedded-height := y;
	end if;
      end if;
      unless ( save = $OLECLOSE-NOSAVE )
	// (Ideally we would also have a special case for $OLECLOSE-PROMPTSAVE
	//  which would prompt the user and return $OLE-E-PROMPTSAVECANCELLED
	//  if the user chose not to save.  But that feature is a remnant of
	//  OLE1 that is not encouraged by the OLE2 guidelines.)
	IOleClientSite/SaveObject(site);
      end unless;
      IOleClientSite/OnShowWindow(site, #f);
    end unless;
    let local-server? :: <boolean> = ole-local-server?(server-object);

    // Do a final SendOnDataChange for those containers that have specified the
    // ADF_DATAONSTOP flag.
    begin
      let data-holder = container-IDataAdviseHolder(server-object);
      unless ( null?(data-holder) )
	IDataAdviseHolder/SendOnDataChange(data-holder,
					   server-IDataObject(server-object),
					   0, $ADVF-DATAONSTOP);
	if ( local-server? )
	  Release(data-holder);
	  server-object.container-IDataAdviseHolder := $NULL-Interface;
	end if;
      end unless;
    end;

    // Tell the container that we are shutting down.
    begin
      let advise-holder = container-IOleAdviseHolder(server-object);
      unless ( null?(advise-holder) )
	// Note: should not do this for a linked object whose application
	//	is still visible.  (See doc for IOleObject::Close)	???
	IOleAdviseHolder/SendOnClose(advise-holder);
	if ( local-server? )
	  Release(advise-holder);
	  server-object.container-IOleAdviseHolder := $NULL-Interface;
	end if;
      end unless;
    end;

    if ( local-server? ) // unless DLL
      Release(site);
      server-object.container-IOleClientSite := $NULL-Interface;
    end if;

    // release our streams and storage
    Release-Streams-And-Storage(server-object);

    // Disconnect the object.  NOTE: This call should not do anything
    // unless the container has caused a GP Fault or some other problem
    // has occurred...
    Output-Debug-String("before CoDisconnectObject\r\n");
    CoDisconnectObject(server-object, 0);
    Output-Debug-String("after CoDisconnectObject\r\n");

    Release(server-object); // undo AddRef above to let object close

  end if;
  $S-OK
end method OLE-util-close-server;

define open generic Release-Streams-And-Storage (obj);

// default method:
define inline method Release-Streams-And-Storage (obj :: <basic-ole-server>)
 => ();
  values()
end method;

// default method
define method OLE-part-dirty? ( obj :: <basic-ole-server> )
 => dirty? :: <boolean>;
  #t
end method;


define method OLE-util-translate-accelerator ( server-object, msg :: <LPMSG> )
 => handled? :: <boolean>;

  // Only key messages need to be sent to OleTranslateAccelerator.
  // Any other message would result in an extra FAR call to occur
  // for that message processing...
  let message :: <fixnum> = msg.message-value;
  if ( (message >= $WM-KEYFIRST) & (message <= $WM-KEYLAST)
	& server-object & OLE-util-in-place-active?(server-object) )
    // OleTranslateAccelerator MUST be called, even though this
    // application does not have an accelerator table.  This has to
    // be done in order for the mnemonics for the top level menu
    // items to work properly.
    OleTranslateAccelerator(container-IOleInPlaceFrame(server-object),
			    in-place-frame-info(server-object),
			    msg) = $S-OK 
  else #f
  end if
end method;

