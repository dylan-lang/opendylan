Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $HIMETRIC-PER-INCH = 2540;

define method MAP-LOGHIM-TO-PIX(x,ppli);
  MulDiv(ppli, x, $HIMETRIC-PER-INCH)
end;

define method TransformWidthInHimetricToPixels(hDC :: <HDC>,
					   iWidthInHiMetric :: <integer>)
	=> value :: <integer>;

  let fSystemDC :: <boolean> = #f;

  if ( null-handle?(hDC) )
    hDC := GetDC($NULL-HWND);
    fSystemDC := #t;
  end if;

  let iXppli :: <integer>     // Pixels per logical inch along width
    = GetDeviceCaps(hDC, $LOGPIXELSX);

  // We got logical HIMETRIC along the display, convert them to pixel units
  let iWidthInPix :: <integer> = MAP-LOGHIM-TO-PIX(iWidthInHiMetric, iXppli);

  if ( fSystemDC )
    ReleaseDC($NULL-HWND, hDC);
  end if;

  iWidthInPix
end method TransformWidthInHimetricToPixels;


define method TransformHeightInHimetricToPixels(hDC :: <HDC>,
					    iHeightInHiMetric :: <integer>)
	=> value :: <integer>;

  let fSystemDC :: <boolean> = #f;

  if ( null-handle?(hDC) )
    hDC := GetDC($NULL-HWND);
    fSystemDC := #t;
  end if;

  let iYppli :: <integer>     // Pixels per logical inch along height
    = GetDeviceCaps(hDC, $LOGPIXELSY);

  // We got logical HIMETRIC along the display, convert them to pixel units
  let iHeightInPix :: <integer> = MAP-LOGHIM-TO-PIX(iHeightInHiMetric, iYppli);

  if ( fSystemDC )
    ReleaseDC($NULL-HWND, hDC);
  end if;

 iHeightInPix
end method TransformHeightInHimetricToPixels;


//**********************************************************************
//
// OleFree
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

/*
define method OleFree(pmem :: <LPVOID>) => ();

  unless ( null-pointer?(pmem) )

    let ( status :: <HRESULT>, pmalloc :: <LPMALLOC> ) =
      CoGetMalloc($MEMCTX-TASK);

    unless ( FAILED?(status) )
      IMalloc/Free(pmalloc, pmem);
      Release(pmalloc);
    end unless;

  end unless;
  values()
end method OleFree;
*/
//**********************************************************************
//
// CSimpleSite::Create
//
// Purpose:
//
//      Creation routine for CSimpleSite
//
// Parameters:
//
//      CSimpleDoc FAR *lpDoc   - Pointer to CSimpleDoc
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      IStorage::CreateStorage     OLE API
//      assert                      C Runtime
//
// Comments:
//
//********************************************************************


define method CSimpleSite-Create(lpDoc :: <CSimpleDoc>)
 => value :: <CSimpleSite>;

  let lpTemp :: <CSimpleSite> = make(<CSimpleSite>, lpDoc: lpDoc);

  // create a sub-storage for the object
  let ( hErr :: <HRESULT>, storage ) =
    IStorage/CreateStorage(lpDoc.m-lpStorage, OLESTR("Object"),
			   logior($STGM-READWRITE, $STGM-TRANSACTED,
				  $STGM-SHARE-EXCLUSIVE),
			   0, 0);

  if ( hErr ~= $NOERROR )
    error("Can't get storage.");
  end if;

  lpTemp.m-lpObjStorage := storage;

  // we will add one ref count on our Site. later when we want to destroy
  // the Site object we will release this  ref count. when the Site's ref
  // count goes to 0, it will be deleted.
  AddRef(lpTemp);

  lpTemp 
end method CSimpleSite-Create;

//**********************************************************************
//
// CSimpleSite::CSimpleSite
//
// Purpose:
//
//      Constructor for CSimpleSite
//
// Parameters:
//
//      CSimpleDoc FAR *lpDoc   - Pointer to CSimpleDoc
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
// Comments:
//
//********************************************************************


define method initialize( this :: <CSimpleSite>, #key lpDoc :: <CSimpleDoc>,
			 #all-keys)
 => ();
  next-method();
  this.m-pSizel := make(<LPSIZEL>);
  this.m-OleClientSite := make(<COleClientSite>, pSite: this,
			       controlling-unknown: this);
  this.m-AdviseSink := make(<CAdviseSink>, pSite: this,
			       controlling-unknown: this);
  /* this.m-OleInPlaceSite := */ make(<COleInPlaceSite>, pSite: this,
				controlling-unknown: this);

  // remember the pointer to the doc
  this.m-lpDoc := lpDoc;

  // clear the reference count

  this.m-dwDrawAspect := $DVASPECT-CONTENT;
  this.m-lpOleObject := null-pointer(<LPOLEOBJECT>);
  this.m-lpInPlaceObject := null-pointer(<LPOLEINPLACEOBJECT>);
  this.m-hwndIPObj := $NULL-HWND;
  this.m-fInPlaceActive := #f;
  this.m-fObjectOpen := #f;

  values()
end method initialize;


//**********************************************************************
//
// CSimpleSite::~CSimpleSite
//
// Purpose:
//
//      Destructor for CSimpleSite
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                                Location
//
//      OutputDebugString                       Windows API
//      IOleObject::Release                     Object
//      IStorage::Release                       OLE API
//
// Comments:
//
//********************************************************************


define method terminate (this :: <CSimpleSite>) => ();

  OutputDebugString( "In CSimpleSite's Destructor \r\n");

  next-method();
  Release(this.m-lpOleObject);
  Release(this.m-lpObjStorage);
  values()
end method terminate ;


//**********************************************************************
//
// CSimpleSite::CloseOleObject
//
// Purpose:
//
//      Call IOleObject::Close on the object of the CSimpleSite
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                                Location
//
//      OutputDebugString                       Windows API
//      IOleObject::QueryInterface              Object
//      IOleObject::Close                       Object
//      IOleInPlaceObject::UIDeactivate         Object
//      IOleInPlaceObject::InPlaceDeactivate    Object
//      IOleInPlaceObject::Release              Object
//
// Comments:
//
//********************************************************************


define method CloseOleObject(this :: <CSimpleSite>) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  OutputDebugString( "In CSimpleSite::CloseOleObject \r\n");

  let OleObject = this.m-lpOleObject;

  unless ( null?(OleObject) )
    
    if ( this.m-fInPlaceActive )
      let ( status, object ) =
	QueryInterface(OleObject, $IID-IOleInPlaceObject);
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

    IOleObject/Close(OleObject, $OLECLOSE-NOSAVE);
  end unless;
  values()
end method CloseOleObject;


//**********************************************************************
//
// CSimpleSite::UnloadOleObject
//
// Purpose:
//
//      Close and release all pointers to the object of the CSimpleSite
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                                Location
//
//      OutputDebugString                       Windows API
//      CSimpleSite::CloseOleObject             SITE.CPP
//      IOleObject::QueryInterface              Object
//      IViewObject::SetAdvise                  Object
//      IViewObject::Release                    Object
//      IStorage::Release                       OLE API
//
// Comments:
//
//********************************************************************

define method UnloadOleObject(this :: <CSimpleSite>) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);
  OutputDebugString( "In CSimpleSite::UnloadOleObject \r\n");

  if ( ~ null?(this.m-lpOleObject) )
	   
    CloseOleObject(this); // ensure object is closed; NOP if already closed

    let ( status, interface ) =
      QueryInterface(this.m-lpOleObject, $IID-IViewObject);

    if ( ~ null?(interface) )
      let lpViewObject :: <LPVIEWOBJECT> =
	pointer-cast(<LPVIEWOBJECT>, interface);
	           
      // Remove the view advise
      IViewObject/SetAdvise(lpViewObject, this.m-dwDrawAspect, 0,
			    $null-interface);
      Release(lpViewObject);
    end if;

    Release(this.m-lpOleObject);
    this.m-lpOleObject := null-pointer(<LPOLEOBJECT>);
  end if;
  values()
end method UnloadOleObject;

//**********************************************************************
//
// CSimpleSite::InitObject
//
// Purpose:
//
//      Used to initialize a newly create object (can't be done in the
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


define method InitObject(this :: <CSimpleSite>, fCreateNew :: <boolean>) => ();
	
  // let stabilize :: <CStabilize> = make(<CStabilize>, this);
  let rect :: <LPRECT> = make(<LPRECT>);

  // Set a View Advise
  begin
    let ( q-status, q-interface ) =
      QueryInterface(this.m-lpOleObject, $IID-IViewObject2);
    let lpViewObject2 :: <LPVIEWOBJECT2> =
      pointer-cast(<LPVIEWOBJECT2>, q-interface);

    IViewObject/SetAdvise(lpViewObject2, this.m-dwDrawAspect, $ADVF-PRIMEFIRST,
			  this.m-AdviseSink);

    // get the initial size of the object
    IViewObject2/GetExtent(lpViewObject2,
			   this.m-dwDrawAspect,
			   -1, // lindex
			   null-pointer(<LPDVTARGETDEVICE>), // ptd
			   this.m-pSizel);
    GetObjRect(this, rect);  // get the rectangle of the object in pixels
    Release(lpViewObject2);
  end;

  // give the object the name of the container app/document
  IOleObject/SetHostNames(this.m-lpOleObject, OLESTR("Simple Application"),
			  OLESTR("Simple OLE Container in Dylan"));

  // inform object handler/DLL object that it is used in the
  //  embedding container's context
  OleSetContainedObject(this.m-lpOleObject, #t);

  if ( fCreateNew ) 
    // force new object to save to guarantee valid object in our storage.
    // OLE 1.0 objects may close w/o saving. this is NOT necessary if the
    // object is created FROM FILE; its data in storage is already valid.
    IOleClientSite/SaveObject(this.m-OleClientSite);

    // we only want to DoVerb(SHOW) if this is an InsertNew object.
    // we should NOT DoVerb(SHOW) if the object is created FromFile.
    IOleObject/DoVerb(this.m-lpOleObject,
		      $OLEIVERB-SHOW,
		      null-pointer(<LPMSG>),
		      this.m-OleClientSite,
		      0,
		      this.m-lpDoc.m-hDocWnd,
		      rect);
  end if;
  destroy(rect);
  values()
end method InitObject;

//**********************************************************************
//
// CSimpleSite::PaintObj
//
// Purpose:
//
//      Paints the object
//
// Parameters:
//
//      HDC hDC     - Device context of the document window
//
// Return Value:
//
// Function Calls:
//      Function                        Location
//
//      IOleObject::QueryInterface      Object
//      IViewObject::GetColorSet        Object
//      IViewObject::Release            Object
//      SetMapMode                      Windows API
//      LPtoDP                          Windows API
//      CreateHatchBrush                Windows API
//      SelectObject                    Windows API
//      DeleteObject                    Windows API
//      CreatePalette                   Windows API
//      SelectPalette                   Windows API
//      RealizePalette                  Windows API
//      OleDraw                         OLE API
//
// Comments:
//
//********************************************************************


define method PaintObj(this :: <CSimpleSite>, hDC :: <HDC>) => ();

    // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  // need to check to make sure there is a valid object
  // available.  This is needed if there is a paint msg
  // between the time that CSimpleSite is instantiated
  // and OleUIInsertObject returns.
  unless ( null?(this.m-lpOleObject) )
	
    let pRect :: <LPRECT> = make(<LPRECT>);

    // convert it to pixels
    GetObjRect(this, pRect);

    let pColorSet :: false-or(<LPLOGPALETTE>) = #f;

    // get a pointer to IViewObject
    let ( q-status, q-interface ) =
      QueryInterface(this.m-lpOleObject, $IID-IViewObject);
    let lpView :: <LPVIEWOBJECT> = pointer-cast(<LPVIEWOBJECT>, q-interface);
    let hPal    :: false-or(<HPALETTE>) = #f;
    let hOldPal :: false-or(<HPALETTE>) = #f;

 /* //---- Don't mess with palletes yet, since the necessary functions are
    //		not yet supported by the Win32-GDI library.	???

    // if the QI succeeds, get the LOGPALETTE for the object
    unless ( null?(lpView) )
      let ( status, cs ) =
	IViewObject/GetColorSet(lpView, this.m-dwDrawAspect, -1, $NULL-VOID,
				null-pointer(<LPDVTARGETDEVICE>),
				null-handle(<HDC>));
      if ( SUCCEEDED?(status) )
	pColorSet := cs;
      end if;
    end unless;

    // if a LOGPALETTE was returned (not guaranteed), create the palette and
    // realize it.  NOTE: A smarter application would want to get the
    // LOGPALETTE for each of its visible objects, and try to create
    // a palette that satisfies all of the visible objects.
    // ALSO: OleFree() is used to free the returned LOGPALETTE.
    if ( pColorSet )
      hPal := CreatePalette(pColorSet);
      hOldPal := SelectPalette(hDC, hPal, #f);
      RealizePalette(hDC);
      OleFree(pColorSet);
    end if;
 */
    // draw the object
    OleDraw(this.m-lpOleObject, this.m-dwDrawAspect, hDC, pRect);

    // if the object is open, draw a hatch rect.
    if ( this.m-fObjectOpen )
      let hBrush :: <HBRUSH> = CreateHatchBrush($HS-BDIAGONAL, RGB(0,0,0));
      let hOldBrush :: <HANDLE> = SelectObject(hDC, hBrush);
      SetROP2(hDC, $R2-MASKPEN);
      Rectangle(hDC, pRect.left-value, pRect.top-value,
		pRect.right-value, pRect.bottom-value);
      SelectObject(hDC, hOldBrush);
      DeleteObject(hBrush);
    end if;

/* -- Later ???
    // if we created a palette, restore the old one, and destroy
    // the object.
    if ( hPal )
      SelectPalette(hDC, hOldPal, #f);
      DeleteObject(hPal);
    end if;
 */

    // if a view pointer was successfully returned, it needs to be released.
    Release(lpView);

    destroy(pRect);
  end unless;
  values()
end method PaintObj;

//**********************************************************************
//
// CSimpleSite::GetObjRect
//
// Purpose:
//
//      Retrieves the rect of the object in pixels
//
// Parameters:
//
//      LPRECT lpRect - Rect structure filled with object's rect in pixels
//
// Return Value:
//
// Function Calls:
//      Function                        Location
//
//      XformWidthInHimetricToPixels    OUTLUI Function
//      XformHeightInHimetricToPixels   OUTLUI Function
//
// Comments:
//
//********************************************************************


define method GetObjRect(this :: <CSimpleSite>, lpRect :: <LPRECT>) => ();

  // convert it to pixels
  lpRect.top-value    := 0;
  lpRect.left-value   := 0;
  lpRect.right-value  := TransformWidthInHimetricToPixels($NULL-HDC,
						      this.m-pSizel.cx-value);
  lpRect.bottom-value := TransformHeightInHimetricToPixels($NULL-HDC,
						       this.m-pSizel.cy-value);
  values()
end method GetObjRect;

