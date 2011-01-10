Module:    OLE-Container
Synopsis:  Methods for class <contained-object>.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *storage-counter* :: <fixnum> = 0;

define method site-initialization(doc :: <contained-object>) => ()

  let site = doc;
  site.document-client-site := make(<container-ole-client-site>, site: site,
			       controlling-unknown: site);
  site.document-advise-sink := make(<container-advise-sink>, site: site,
			       controlling-unknown: site);
  site.document-in-place-site := make(<container-ole-in-place-site>,
				      site: site,
				      controlling-unknown: site);

  let app = doc.document-application;
  if ( null?(app.container-storage) )
    // Create storage for the document if not done already.
    let ( status :: <HRESULT>, storage ) =
      StgCreateDocfile($NULL-OLESTR,
		       logior($STGM-READWRITE, $STGM-TRANSACTED,
			      $STGM-SHARE-EXCLUSIVE),
		       0);
    app.container-storage := storage;
    check-ole-status(status, "StgCreateDocfile", app);
  end if;
  let app-storage = app.container-storage;

  let storage-name :: <LPOLESTR> =
    as(<LPOLESTR>,
       format-to-string("Object%d",
			(*storage-counter* := *storage-counter* + 1)));
  // create a sub-storage for the object
  let ( hErr :: <HRESULT>, storage ) = 
    IStorage/CreateStorage(app-storage, storage-name,
			   logior($STGM-READWRITE, $STGM-TRANSACTED,
				  $STGM-SHARE-EXCLUSIVE),
			   0, 0);
  check-ole-status(hErr, "IStorage/CreateStorage", app-storage);
  destroy(storage-name);
  site.document-sub-storage := storage;

end method site-initialization;


// Call IOleObject::Close on the object
define method close-OLE-object(site :: <contained-object>,
			       save-option :: <fixnum>) => ();

  if ( site.document-open? )
    OutputDebugString( "close-OLE-object\r\n");

    let ole-object = site.document-ole-object;

    unless ( null?(ole-object) )

      if ( site.document-in-place-active? )
	let ( status, object ) =
	  QueryInterface(ole-object, $IID-IOleInPlaceObject);
	unless ( null?(object) )
	  let lpObject :: <LPOLEINPLACEOBJECT> =
	    pointer-cast(<LPOLEINPLACEOBJECT>,object);
	  IOleInPlaceObject/UIDeactivate(lpObject);
	  // don't need to worry about inside-out because the object
	  // is going away.
	  IOleInPlaceObject/InPlaceDeactivate(lpObject);
	  Release(lpObject);
	end unless;
      end if;
      site.document-open? := #f;
      IOleObject/Close(ole-object, save-option);
    end unless;
  end if;
  values()
end method close-OLE-object;


// Close and release all pointers to the object
define method unload-OLE-object(site :: <contained-object>) => ();
  container-remove-verbs(site);
  let ole-object = site.document-ole-object;
  unless ( null-pointer?(ole-object) )
    OutputDebugString( "unload-OLE-object\r\n");

    // ensure object is closed; NOP if already closed
    close-OLE-object(site, $OLECLOSE-NOSAVE);

    let ( status, interface ) =
      QueryInterface(ole-object, $IID-IViewObject);
    if ( ~ null-pointer?(interface) )
      let view-object :: <LPVIEWOBJECT> =
	pointer-cast(<LPVIEWOBJECT>, interface);
      // Remove the view advise
      IViewObject/SetAdvise(view-object, site.document-draw-aspect, 0,
			    $null-interface);
      Release(view-object);
    end if;

    Release(ole-object);
    site.document-ole-object := null-pointer(<LPOLEOBJECT>);
  end unless;
  values()
end method unload-OLE-object;

//**********************************************************************
//
// CSimpleSite::InitObject
//
// Purpose:
//
//      Used to initialize a newly created object (can't be done in the
//      constructor).
//
// Parameters:
//
//      BOOL fCreateNew -   TRUE if insert NEW object
//                          FALSE if create object FROM FILE
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                        Location
//
//      IOleObject::SetHostNames        Object
//      IOleObject::QueryInterface      Object
//      IViewObject2::GetExtent         Object
//      IOleObject::DoVerb              Object
//      IViewObject::SetAdvise          Object
//      IViewObject::Release            Object
//      GetClientRect                   Windows API
//      OleSetContainedObject           OLE API
//
// Comments:
//
//********************************************************************

define open generic init-object-in-site (site, object-pointer, new?) => ();

define method init-object-in-site (site :: <contained-object>,
				   object-pointer :: <C-pointer>,
				   created-new? :: <boolean>) => ();

  let ole-object :: <LPOLEOBJECT> =
    dylan-interface(pointer-cast(<LPOLEOBJECT>, object-pointer));
  site.document-ole-object := ole-object;	
  site.document-open? := #t;
  let rect :: <LPRECT> = make(<LPRECT>);

  // Set a View Advise
  begin
    let ( q-status, q-interface ) =
      QueryInterface(ole-object, $IID-IViewObject2);
    check-ole-status(q-status, "QueryInterface",
		     ole-object, $IID-IViewObject2);
    let view-object :: <LPVIEWOBJECT2> =
      pointer-cast(<LPVIEWOBJECT2>, q-interface);

    IViewObject/SetAdvise(view-object, site.document-draw-aspect,
			  $ADVF-PRIMEFIRST, site.document-advise-sink);

    // get the initial size of the object
    IViewObject2/GetExtent(view-object,
			   site.document-draw-aspect,
			   -1, // lindex
			   null-pointer(<LPDVTARGETDEVICE>), // ptd
			   site.site-object-size);
    // check for error???
    // get the rectangle of the object in pixels
    document-rectangle(site, rect);
    Release(view-object);
  end;

  // give the object the name of the container app/document
  begin
    let ( container-app, container-obj ) =
      container-host-names(site.document-application);
    let application-name = container-app | $NULL-OLESTR;
    let document-name =
	site.containing-document-name | container-obj | $NULL-OLESTR;
    unless ( application-name == $NULL-OLESTR & document-name == $NULL-OLESTR )
      let app-name = as(<LPOLESTR>, application-name);
      let obj-name = as(<LPOLESTR>, document-name);
      IOleObject/SetHostNames(ole-object, app-name, obj-name);
      // Not checking status here because it doesn't matter to the container
      // if the server doesn't implement this.
      unless ( app-name == application-name )
	destroy(app-name);
      end;
      unless ( obj-name == document-name )
	destroy(obj-name);
      end;
    end unless;
  end;

  // inform object handler/DLL object that it is used in the
  //  embedding container's context
  OleSetContainedObject(ole-object, #t);

  // get the OLEMISC flag bits
  let ( m-status, misc-bits ) =
    IOleObject/GetMiscStatus(ole-object, site.document-draw-aspect);
  if ( SUCCEEDED?(m-status) )
    site.document-olemisc := misc-bits;
  end if;

  // ask the server for its class ID if we don't already know.
  if ( null-pointer?(site.document-class-id) )
    let id-ptr :: <LPCLSID> = make(<LPCLSID>);
    if ( SUCCEEDED?(IOleObject/GetUserClassID(ole-object, id-ptr)) )
      site.document-class-id := id-ptr;
    else
      destroy(id-ptr);
    end if;
  end if;

  if ( created-new? ) 
    // force new object to save to guarantee valid object in our storage.
    // OLE 1.0 objects may close w/o saving. this is NOT necessary if the
    // object is created FROM FILE; its data in storage is already valid.
    IOleClientSite/SaveObject(site.document-client-site);

    // we only want to DoVerb(SHOW) if this is an InsertNew object.
    // we should NOT DoVerb(SHOW) if the object is created FromFile.
    IOleObject/DoVerb(ole-object,
		      $OLEIVERB-SHOW,
		      null-pointer(<LPMSG>),
		      site.document-client-site,
		      0,
		      site.document-container-window,
		      rect);
  end if;
  destroy(rect);
  values()
end method init-object-in-site;


define open generic container-host-names( app :: <container-app> )
 => (application-name, document-name);

// default method, to be overwritten by application.
define method container-host-names( app :: <container-app> )
 => (application-name, document-name)
  values(#f, #f)
end;
    


define method paint-contained-document (site :: <contained-object>, hDC :: <HDC>)
 => ();

  unless ( null?(site.document-ole-object) )
	
    let pRect :: <LPRECT> = make(<LPRECT>);

    // convert it to pixels
    document-rectangle(site, pRect);

    // get a pointer to IViewObject
    let ( q-status, q-interface ) =
      QueryInterface(site.document-ole-object, $IID-IViewObject);
    let view-object :: <LPVIEWOBJECT> =
      pointer-cast(<LPVIEWOBJECT>, q-interface);
    let hPal    :: false-or(<HPALETTE>) = #f;
    let hOldPal :: false-or(<HPALETTE>) = #f;

    // if the QI succeeds, get the LOGPALETTE for the object
    unless ( null-pointer?(view-object) )
      let ( status, pColorSet :: <LPLOGPALETTE> ) =
	IViewObject/GetColorSet(view-object, site.document-draw-aspect,
				-1, $NULL-VOID,
				null-pointer(<LPDVTARGETDEVICE>),
				null-handle(<HDC>));
      // not testing SUCCEEDED?(status) because GetColorSet can return
      // $S-FALSE to mean that no information is available.
      if ( (status = $S-OK) & ~ null-pointer?(pColorSet) )
	// if a LOGPALETTE was returned (not guaranteed), create the palette
	// and realize it.  NOTE: A smarter application would want to get the
	// LOGPALETTE for each of its visible objects, and try to create
	// a palette that satisfies all of the visible objects.
	// ALSO: ole-free() is used to free the returned LOGPALETTE.
	hPal := CreatePalette(pColorSet);
	hOldPal := SelectPalette(hDC, hPal, #f);
	RealizePalette(hDC);
	ole-free(pColorSet);
      end if;
    end unless;

    block ()
      // draw the object
      let draw-status =
	OleDraw(site.document-ole-object, site.document-draw-aspect,
		hDC, pRect);
      if ( FAILED?(draw-status) )
	ole-warning(draw-status, "OleDraw", site.document-ole-object);
      end if;
    exception (<abort>)
      debug-message("OleDraw aborted\n");
      values();
    end block;

    // if the object is open, draw a hatch rect.
    if ( site.document-open-out-of-place? )
      let hBrush :: <HBRUSH> = CreateHatchBrush($HS-BDIAGONAL, RGB(0,0,0));
      let hOldBrush :: <HANDLE> = SelectObject(hDC, hBrush);
      SetROP2(hDC, $R2-MASKPEN);
      Rectangle(hDC, pRect.left-value, pRect.top-value,
		pRect.right-value, pRect.bottom-value);
      SelectObject(hDC, hOldBrush);
      DeleteObject(hBrush);
    end if;

    // if we created a palette, restore the old one, and destroy
    // the object.
    if ( hPal )
      SelectPalette(hDC, hOldPal, #f);
      DeleteObject(hPal);
    end if;

    // if a view pointer was successfully returned, it needs to be released.
    Release(view-object);

    destroy(pRect);
  end unless;
  values()
end method paint-contained-document;


//**********************************************************************
//
// ole-free
//
// Purpose:
//
//      free memory using the currently active IMalloc* allocator
//
// Parameters:
//
//      LPVOID pmem - pointer to memory allocated using IMalloc
//
// Return Value:
//
//      None
//
// Comments:
//
//********************************************************************

define method ole-free(pmem :: <LPVOID>) => ();

  unless ( null-pointer?(pmem) )
    let ( status :: <HRESULT>, pmalloc :: <LPMALLOC> ) =
      CoGetMalloc($MEMCTX-TASK);
    unless ( FAILED?(status) )
      IMalloc/Free(pmalloc, pmem);
      Release(pmalloc);
    end unless;
  end unless;
  values()
end method ole-free;

// Fills in object's rectangle, in pixels
define method document-rectangle(site :: <contained-object>,
				 doc-rect :: <LPRECT>)
 => ();
  let (left :: <fixnum>, top :: <fixnum>,
       right :: <fixnum>, bottom :: <fixnum>) = document-edges(site);
  doc-rect.top-value    := top;
  doc-rect.left-value   := left;
  doc-rect.right-value  := right;
  doc-rect.bottom-value := bottom;
  values()
end method document-rectangle;

define method document-edges(site :: <contained-object>)
 => (left :: <fixnum>, top :: <fixnum>,
     right :: <fixnum>, bottom :: <fixnum>)
  // returned values are in pixels
  let top :: <fixnum>  = site.y-origin;
  let left :: <fixnum> = site.x-origin;
  let size-ptr = site.site-object-size;
  let (width :: <fixnum>, height :: <fixnum> ) =
    himetric-to-pixels(size-ptr.cx-value, size-ptr.cy-value);
  values(left, top, left + width, top + height)
end method document-edges;

// Change the position of the embedded document.
// The new offset is given in pixels.
define method document-move(site :: <contained-object>,
			    new-x :: <fixnum>, new-y :: <fixnum>) => ()
  unless ( new-x = site.x-origin & new-y = site.y-origin )
    site.x-origin := new-x;
    site.y-origin := new-y;
    let in-place-object = site.document-in-place-ole-object;
    unless ( null?(in-place-object) )
      with-stack-structure( rect :: <LPRECT> )
	document-rectangle(site, rect);
	check-ole-status(
	  IOleInPlaceObject/SetObjectRects(in-place-object,
					   rect, // position rectangle
					   rect), // clipping rectangle
			 "IOleInPlaceObject/SetObjectRects",
			 in-place-object);
      end with-stack-structure;
    end unless;
  end unless;
end document-move;
