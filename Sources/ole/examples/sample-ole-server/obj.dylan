Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//**********************************************************************
//
// CSimpSvrObj::CSimpSvrObj
//
// Purpose:
//
//      Constructor for CSimpSvrObj
//
// Parameters:
//
//      CSimpSvrDoc FAR * lpSimpSvrDoc - ptr to the doc object
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//
// Comments:
//
//
//********************************************************************

define method initialize ( this :: <CSimpSvrObj>,
			  #key lpSimpSvrDoc :: <CSimpSvrDoc>, #all-keys) => ();
  next-method();

  this.m-pPosRect := make(<LPRECT>);
  this.m-pSize := make(<LPPOINT>);
  this.m-pFrameInfo := make(<LPOLEINPLACEFRAMEINFO>);

  this.m-OleObject := make( <COleObject>, SimpSvrObj: this,
			   controlling-unknown: this );
  this.m-PersistStorage := make( <CPersistStorage>, SimpSvrObj: this,
				controlling-unknown: this );
  this.m-DataObject := make( <CDataObject>, SimpSvrObj: this,
			    controlling-unknown: this );
  this.m-OleInPlaceActiveObject := make( <COleInPlaceActiveObject>,
					SimpSvrObj: this,
					controlling-unknown: this );
  this.m-OleInPlaceObject := make( <COleInPlaceObject>, SimpSvrObj: this,
				  controlling-unknown: this );

 // this.m-ExternalConnection := make( <CExternalConnection>, SimpSvrObj: this,
 //				    controlling-unknown: this );

  this.m-lpDoc := lpSimpSvrDoc;
  this.m-fInPlaceActive := #f;
  this.m-fInPlaceVisible := #f;
  this.m-fUIActive := #f;
  this.m-hmenuShared := null-pointer(<HMENU>);
  this.m-hOleMenu := null-pointer(<HOLEMENU>);

  this.m-dwRegister := 0;

  this.m-lpFrame := $NULL-interface;
  this.m-lpCntrDoc := $NULL-interface;

  this.m-lpStorage := null-pointer(<LPSTORAGE>);
  this.m-lpColorStm := null-pointer(<LPSTREAM>);
  this.m-lpSizeStm := null-pointer(<LPSTREAM>);
  this.m-lpOleClientSite := $NULL-interface;
  this.m-lpOleAdviseHolder := $NULL-interface;
  this.m-lpDataAdviseHolder := $NULL-interface;
  this.m-lpIPSite := $NULL-interface;

  this.m-red := 128;
  this.m-green := 0;
  this.m-blue := 0;

  this.m-pSize.x-value := 100;
  this.m-pSize.y-value := 100;

  this.m-xOffset := 0;
  this.m-yOffset := 0;

  this.m-scale := as(<single-float>, 1);

  this.m-fSaveWithSameAsLoad := #f;
  this.m-fNoScribbleMode := #f;

  values();
end initialize;

//**********************************************************************
//
// CSimpSvrObj::~CSimpSvrObj
//
// Purpose:
//
//      Destructor for CSimpSvrObj
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
//      Function                    Location
//
//      OutputDebugString           Windows API
//      PostMessage                 Windows API
//      CSimpSvrDoc::GetApp         DOC.H
//      CSimpSvrDoc::GethAppWnd     DOC.H
//      CSimpSvrDoc::ClearObj       DOC.H
//      CSimpSvrApp::IsStartedByOle APP.CPP
//
// Comments:
//
//
//********************************************************************


define method terminate (this :: <CSimpSvrObj>) => ();

	
  OutputDebugString("In CSimpSvrObj's Destructor \r\n");
  next-method();

  // if we were started by ole, post ourselves a close message
  if ( IsStartedByOle(GetApp(this.m-lpDoc)) )
    PostMessage(GethAppWnd(this.m-lpDoc), $WM-SYSCOMMAND, $SC-CLOSE, 0);
  end if;

  // clear the OBJ ptr in the doc class
  ClearObj(this.m-lpDoc);

  values();
end method terminate ;

//**********************************************************************
//
// CSimpSvrObj::Draw
//
// Purpose:
//
//      Draws the object into an arbitrary DC
//
// Parameters:
//
//      HDC hDC - DC to draw into
//
// Return Value:
//
//      NONE
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CreateBrushIndirect         Windows API
//      SelectObject                Windows API
//      Rectangle                   Windows API
//      DeleteObject                Windows API
//
// Comments:
//
//
//********************************************************************

define constant $message :: <LPTSTR> = as(<LPTSTR>, "Dylan");

define method Draw(this :: <CSimpSvrObj>, hDC :: <HDC>, fMeta :: <boolean>)
 => ();

  let scale :: <real> = this.m-scale;

  OutputDebugString("In CSimpSvrObj::Draw\r\n");
  OutputDebugString( format-to-string( "Drawing Scale %d\r\n",scale) );

  let x-size :: <integer> = this.m-pSize.x-value;
  let y-size :: <integer> = this.m-pSize.y-value;

  if ( ~ fMeta )
    
    SetMapMode(hDC, $MM-ANISOTROPIC);
    SetWindowOrgEx(hDC,  truncate/(this.m-xOffset, scale), 
		   truncate/(this.m-yOffset, scale), $NULL-POINT);
    SetWindowExtEx(hDC, x-size, y-size, $NULL-POINT);
    SetViewportExtEx(hDC,  truncate(x-size * scale), 
		     truncate(y-size * scale), $NULL-POINT);
  end if;

  begin
    // create the brush
    let color = RGB(this.m-red, this.m-green, this.m-blue);
    let hBrush = CreateSolidBrush(color);

    // select the brush
    let hOldBrush = SelectObject( hDC, hBrush );
    let hPen = CreatePen($PS-INSIDEFRAME, 6, RGB(0, 0, 0));

    let hOldPen = SelectObject( hDC, hPen );

    // draw the rectangle
    Rectangle(hDC, 0, 0, x-size, y-size);

    // restore the pen
    hPen := SelectObject(hDC, hOldPen);

    // free the pen
    DeleteObject(hPen);

    // restore the old brush
    hBrush := SelectObject(hDC, hOldBrush);

    // free the brush
    DeleteObject(hBrush);
  end;

  // write a label in the rectangle
  begin
    let old-color = SetTextColor(hDC, RGB(#xC0,#xC0,#xC0));
    let old-mode = SetBkMode(hDC, $TRANSPARENT);
    TextOut(hDC, 12, truncate/(y-size,2) - 8, $message, size($message));
    SetTextColor(hDC, old-color);
    SetBkMode(hDC, old-mode);
  end;

  values();
end method Draw;

//**********************************************************************
//
// CSimpSvrObj::GetMetaFilePict
//
// Purpose:
//
//      Returns a handle to a metafile representation of the object.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      Handle to the metafile.
//
// Function Calls:
//      Function                        Location
//
//      OutputDebugString               Windows API
//      GlobalAlloc                     Windows API
//      GlobalLock                      Windows API
//      SetWindowOrg                    Windows API
//      SetWindowExt                    Windows API
//      CreateMetaFile                  Windows API
//      CloseMetaFile                   Windows API
//      GlobalUnlock                    Windows API
//      XformWidthInPixelsToHimetric    OLESTD
//      XformHeightInPixelsToHimetric   OLESTD
//      CSimpSvrObj::Draw               OBJ.CPP
//
// Comments:
//
//
//********************************************************************


define method GetMetaFilePict(this :: <CSimpSvrObj>, kind :: <integer> )
	=> value :: <HANDLE>;

  OutputDebugString("In CSimpSvrObj::GetMetaFilePict\r\n");

  // allocate the memory for the METAFILEPICT structure
  let hMFP :: <HANDLE> = GlobalAlloc(logior($GMEM-SHARE,$GHND),
				     size-of(<METAFILEPICT>));
  let lpMFP :: <LPMETAFILEPICT> =
    pointer-cast(<LPMETAFILEPICT>, GlobalLock(hMFP));

  // get the size of the object in HIMETRIC
  let ( pt-x, pt-y ) =
    pixels-to-himetric (this.m-pSize.x-value, this.m-pSize.y-value);

  // fill out the METAFILEPICT structure
  lpMFP.mm-value := $MM-ANISOTROPIC;
  lpMFP.xExt-value := pt-x;
  lpMFP.yExt-value := pt-y;

  // Create the metafile
  let hDC :: <HDC> = 
    if ( kind = $TYMED-ENHMF )
      CreateEnhMetaFile($NULL-HDC, $NULL-string,
			$NULL-RECT, $NULL-string)
    else
      CreateMetaFile($NULL-string)
    end if;

  SetWindowOrgEx(hDC, 0, 0, $NULL-POINT);
  SetWindowExtEx(hDC, this.m-pSize.x-value, this.m-pSize.y-value,
		 null-pointer(<LPSIZE>));

  Draw(this, hDC, #t);

  lpMFP.hMF-value := 
    if ( kind = $TYMED-ENHMF )
      CloseEnhMetaFile(hDC)
    else
      CloseMetaFile(hDC)
    end if;

  // unlock the metafilepict
  GlobalUnlock(hMFP);

 /* return */  hMFP 
end method GetMetaFilePict;


//**********************************************************************
//
// CSimpSvrObj::SaveToStorage
//
// Purpose:
//
//      Saves the object to the passed storage
//
// Parameters:
//
//      LPSTORAGE lpStg - Storage in which to save the object
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::CreateStream      OLE
//      IStream::Write              OLE
//      IStream::Release            OLE
//
// Comments:
//
//      A real app will want to do better error checking / returning
//
//********************************************************************

define constant $NULL-ULARGE-INTEGER = null-pointer(<PULARGE-INTEGER>);

define method SaveToStorage(this :: <CSimpSvrObj>, lpStg :: <Interface>,
			    fSameAsLoad :: <boolean>) => ();

  OutputDebugString("In CSimpSvrObj::SaveToStorage\r\n");

  let lpTempColor :: <LPSTREAM> = $NULL-istream;
  let lpTempSize  :: <LPSTREAM> = $NULL-istream;

  if ( ~fSameAsLoad )
    let ( color, size ) = CreateTempStreams(this.m-PersistStorage, lpStg); 
    lpTempColor := color;
    lpTempSize := size;
  else
    lpTempColor := this.m-lpColorStm;
    AddRef(lpTempColor);
    lpTempSize := this.m-lpSizeStm;
    AddRef(lpTempSize);
  end if;

  let uli :: <PULARGE-INTEGER> = make( <PULARGE-INTEGER>, value: 0 );

  IStream/SetSize(lpTempColor, uli);
  IStream/SetSize(lpTempSize, uli);

  let li :: <PLARGE-INTEGER> = make( <PLARGE-INTEGER>, value: 0 );

  IStream/Seek(lpTempColor, li, $STREAM-SEEK-SET, $NULL-ULARGE-INTEGER);
  IStream/Seek(lpTempSize, li, $STREAM-SEEK-SET, $NULL-ULARGE-INTEGER);

  // write the colors to the stream
  IStream/Write-integer(lpTempColor, this.m-red );
  IStream/Write-integer(lpTempColor, this.m-green );
  IStream/Write-integer(lpTempColor, this.m-blue );

  // write the size to the stream
  IStream/Write(lpTempSize, this.m-pSize, size-of(<POINT>) );

  Release(lpTempColor);
  Release(lpTempSize);

  destroy(uli);
  destroy(li);

  values();
end method SaveToStorage;

//**********************************************************************
//
// CSimpSvrObj::LoadFromStorage
//
// Purpose:
//
//      Loads the object from the passed storage
//
// Parameters:
//
//      LPSTORAGE lpStg     - Storage in which to load the object from
//
// Return Value:
//
//      None.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IStorage::OpenStream        OLE
//      IStream::Read               OLE
//      IStream::Release            OLE
//
// Comments:
//
//
//********************************************************************


define method LoadFromStorage(this :: <CSimpSvrObj>) => ();

  OutputDebugString("In CSimpSvrObj::LoadFromStorage\r\n");

  local method read-integer( stream :: <Interface> )
	=> value :: <integer>;
	  let ( status, value, count ) = IStream/Read-integer( stream );
	  value
	end read-integer;

  // Read the colors
  this.m-red   := read-integer(this.m-lpColorStm);
  this.m-green := read-integer(this.m-lpColorStm);
  this.m-blue  := read-integer(this.m-lpColorStm);

	// read the size
  IStream/Read(this.m-lpSizeStm, this.m-pSize, size-of(<POINT>) );

  values();
end method LoadFromStorage;

//**********************************************************************
//
// CSimpSvrObj::DoInPlaceActivate
//
// Purpose:
//
//      Does the inplace activation for the object
//
// Parameters:
//
//      LONG lVerb  - Verb that caused this function to be called
//
// Return Value:
//
//      TRUE/FALSE depending on success or failure.
//
// Function Calls:
//      Function                                Location
//
//      IOleClientSite::QueryInterface          Container
//      IOleClientSite::ShowObject              Container
//      IOleInPlaceSite::CanInPlaceActivate     Container
//      IOleInPlaceSite::Release                Container
//      IOleInPlaceSite::OnInPlaceActivate      Container
//      IOleInPlaceSite::GetWindow              Container
//      IOleInPlaceSite::GetWindowContext       Container
//      IOleInPlaceSite::OnUIActivate           Container
//      IOleInPlaceSite::Release                Container
//      IOleInPlaceFrame::SetActiveObject       Container
//      IOleInPlaceUIWindow::SetActiveObject    Container
//      OutputDebugString                       Windows API
//      ShowWindow                              Windows API
//      SetParent                               Windows API
//      IntersectRect                           Windows API
//      OffsetRect                              Windows API
//      MoveWindow                              Windows API
//      CopyRect                                Windows API
//      SetFocus                                Windows API
//      SetHatchWindowSize                      OLESTD
//      CSimpSvrObj::AssembleMenus              OBJ.CPP
//      CSimpSvrObj::AddFrameLevelUI            OBJ.CPP
//
//
// Comments:
//
//      Be sure to read TECHNOTES.WRI included with the OLE SDK
//      for details on implementing inplace activation.
//
//********************************************************************


define method DoInPlaceActivate(this :: <CSimpSvrObj>, lVerb)
	=> done :: <boolean>;

  let retval :: <boolean> = #f;

  block(error-return)
	
    OutputDebugString("In CSimpSvrObj::DoInPlaceActivate\r\n");

    // if not currently in place active
    if ( ~this.m-fInPlaceActive )
		
      // get the inplace site
      let ( status, interface ) =
	QueryInterface(this.m-lpOleClientSite, $IID-IOleInPlaceSite);
      this.m-lpIPSite := interface;
      if ( FAILED?(status) )
	error-return();
      end if;

      // if the inplace site could not be obtained, or refuses to inplace
      // activate then goto error.
      if ( null?(this.m-lpIPSite) |
	   (IOleInPlaceSite/CanInPlaceActivate(this.m-lpIPSite) ~= $NOERROR) )
			
	unless ( null?(this.m-lpIPSite) )
	  Release(this.m-lpIPSite);
	end unless;
	this.m-lpIPSite := $NULL-interface;
	error-return();
      end if;

      // tell the site that we are activating.
      IOleInPlaceSite/OnInPlaceActivate(this.m-lpIPSite);
      this.m-fInPlaceActive := #t;
    end if;

    // if not currently inplace visible
    if ( ~this.m-fInPlaceVisible )
		
      this.m-fInPlaceVisible := #t;

      // get the window handle of the site
      let ( status , window ) =
	IOleWindow/GetWindow(this.m-lpIPSite);
      this.m-hWndParent := window;

      let pPosRect  :: <LPRECT> = make(<LPRECT>);
      let pClipRect :: <LPRECT> = make(<LPRECT>);

      // get window context from the container
      let ( status, frame, doc ) = 
	IOleInPlaceSite/GetWindowContext(this.m-lpIPSite, pPosRect,
							 pClipRect,
							 this.m-pFrameInfo);
      this.m-lpFrame := frame;
      this.m-lpCntrDoc := doc;

      // show the hatch window
      ShowHatchWnd(this.m-lpDoc);

      // Set the parenting
      let doc-window :: <HWND> = GethDocWnd(this.m-lpDoc);
      SetParent(GethHatchWnd(this.m-lpDoc), this.m-hWndParent);
      SetParent(doc-window, GethHatchWnd(this.m-lpDoc));

      // ensure that the document window is visible
      ShowWindow(doc-window, $SW-SHOWNORMAL);
      InvalidateRect(doc-window, $NULL-RECT, #t); // queues WM_PAINT message

      // tell the client site to show the object
      IOleClientSite/ShowObject(this.m-lpOleClientSite);

      let pResRect :: <LPRECT> = make(<LPRECT>);

      // figure out the "real" size of the object
      IntersectRect(pResRect, pPosRect, pClipRect);
      CopyRect(this.m-pPosRect, pPosRect);

      // adjust our hatch window size
      let ( x-offset, y-offset ) =
	SetHatchWindowSize(GethHatchWnd(this.m-lpDoc), pResRect, pPosRect);

      // move the object window
      MoveWindow(doc-window,
		 x-offset,
		 y-offset,
		 pResRect.right-value - pResRect.left-value,
		 pResRect.bottom-value - pResRect.top-value,
		 #f);

      // create the combined window
      AssembleMenus(this);

      destroy(pPosRect); destroy(pClipRect); destroy(pResRect);
    end if;

    // if not UIActive
    if ( ~this.m-fUIActive )
		
      this.m-fUIActive := #t;

      // tell the inplace site that we are activating
      IOleInPlaceSite/OnUIActivate(this.m-lpIPSite);

      // set the focus to our object window
      SetFocus(GethDocWnd(this.m-lpDoc));

      // set the active object on the frame
      IOleInPlaceUIWindow/SetActiveObject(this.m-lpFrame,
					  this.m-OleInPlaceActiveObject,
					  OLESTR("Simple Dylan OLE Server") );

      // set the active object on the Doc, if available.
      unless ( null?(this.m-lpCntrDoc) )
	IOleInPlaceUIWindow/SetActiveObject(this.m-lpCntrDoc,
					    this.m-OleInPlaceActiveObject,
					    OLESTR("Simple Dylan OLE Server"));
      end unless;

      // add the frame level UI.
      AddFrameLevelUI(this);
    end if;

    retval := #t;
  end block;
  /* return */ retval 
end method DoInPlaceActivate;

//**********************************************************************
//
// CSimpSvrObj::AssembleMenus
//
// Purpose:
//
//      Creates the combined menus used during inplace activation.
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
//      Function                        Location
//
//      OutputDebugString               Windows API
//      CreateMenu                      Windows API
//      IOleInPlaceFrame::InsertMenus   Container
//      InsertMenu                      Windows API
//      DestroyMenu                     Windows API
//      OleCreateMenuDescriptor         OLE API
//
// Comments:
//
//
//********************************************************************


define method AssembleMenus(this :: <CSimpSvrObj>) => ();
	
  OutputDebugString("In CSimpSvrObj::AssembleMenus\r\n");
  with-stack-structure ( menugroupwidths :: <LPOLEMENUGROUPWIDTHS> )

  //  Create the menu resource
  this.m-hmenuShared := CreateMenu();

  // have the container insert its menus
  if ( IOleInPlaceFrame/InsertMenus(this.m-lpFrame, this.m-hmenuShared,
				    menugroupwidths ) = $NOERROR )
		
    let widths = menugroupwidths.width-value;
    let nFirstGroup :: <integer> = pointer-value(widths, index: 0);

    // insert the server menus
    InsertMenu( this.m-hmenuShared, nFirstGroup,
	       logior($MF-BYPOSITION,$MF-POPUP), 
	       pointer-address( GetColorMenu(GetApp(this.m-lpDoc))),
	       "&Color");
    pointer-value(widths, index: 1) := 1;
    pointer-value(widths, index: 3) := 0;
    pointer-value(widths, index: 5) := 0;
		
  else
		
    // Destroy the menu resource
    DestroyMenu(this.m-hmenuShared);
    this.m-hmenuShared := null-pointer(<HOLEMENU>);
  end if;

  // tell OLE to create the menu descriptor
  this.m-hOleMenu := OleCreateMenuDescriptor(this.m-hmenuShared,
					     menugroupwidths);
  end with-stack-structure;
  values();
end method AssembleMenus;

//**********************************************************************
//
// CSimpSvrObj::AddFrameLevelUI
//
// Purpose:
//
//      Adds the Frame level user interface
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
//      Function                    Location
//
//      OutputDebugString                   Windows API
//      IOleInPlaceFrame::SetMenu           Container
//      IOleInPlaceFrame::SetBorderSpace    Container
//      IOleInPlaceUIWindow::SetBorderSpace Container
//      CSimpSvrDoc::GethDocWnd             DOC.H
//
// Comments:
//
//
//********************************************************************


define method AddFrameLevelUI(this :: <CSimpSvrObj>) => ();

  OutputDebugString("In CSimpSvrObj::AddFrameLevelUI\r\n");

  // add the combined menu
  IOleInPlaceFrame/SetMenu(this.m-lpFrame, this.m-hmenuShared,
					  this.m-hOleMenu,
					  GethDocWnd(this.m-lpDoc));

  // do hatched border
  SetParent(GethHatchWnd(this.m-lpDoc), this.m-hWndParent);
  SetParent(GethDocWnd(this.m-lpDoc), GethHatchWnd(this.m-lpDoc));

  // set the border space.  Normally we would negotiate for toolbar
  // space at this point.  Since this server doesn't have a toolbar,
  // this isn't needed...
  unless ( null?(this.m-lpFrame) )
    IOleInPlaceUIWindow/SetBorderSpace(this.m-lpFrame, null-pointer(<LPCBORDERWIDTHS>));
  end unless;

  unless ( null?(this.m-lpCntrDoc) )
    IOleInPlaceUIWindow/SetBorderSpace(this.m-lpCntrDoc, null-pointer(<LPCBORDERWIDTHS>));
  end unless;
  values();
end method AddFrameLevelUI;

//**********************************************************************
//
// CSimpSvrObj::DoInPlaceHide
//
// Purpose:
//
//      Hides the object while inplace actvie
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
//      Function                        Location
//
//      OutputDebugString               Windows API
//      SetParent                       Windows API
//      CSimpSvrDoc::GethDocWnd         DOC.H
//      CSimpSvrDoc::GethAppWnd         DOC.H
//      CSimpSvrDoc::GethHatchWnd       DOC.H
//      CSimpSvrObj::DisassembleMenus   OBJ.CPP
//      IOleInPlaceFrame::Release       Container
//      IOleInPlaceUIWindow::Release    Container
//
//
// Comments:
//
//      Be sure to read TECHNOTES.WRI included with the OLE SDK
//      for details on implementing inplace activation.
//
//********************************************************************


define method DoInPlaceHide(this :: <CSimpSvrObj>) => ();

 block(return)
	
   OutputDebugString("In CSimpSvrObj::DoInPlaceHide\r\n");

   // if we aren't inplace visible, then this routine is a NOP,
   if ( ~this.m-fInPlaceVisible )
     return();
   end if;

   this.m-fInPlaceVisible := #f;

   // change the parenting
   SetParent(GethDocWnd(this.m-lpDoc), GethAppWnd(this.m-lpDoc));
   SetParent(GethHatchWnd(this.m-lpDoc),GethDocWnd(this.m-lpDoc));

   // rip down the combined menus
   DisassembleMenus(this);

   // release the inplace frame
   Release(this.m-lpFrame);

   this.m-lpFrame := $NULL-Interface;  // only holding one ref. to frame.

   // release the UIWindow if it is there.
   unless ( null?(this.m-lpCntrDoc) )
     Release(this.m-lpCntrDoc);
   end unless;

   this.m-lpCntrDoc := $NULL-Interface;

 end block;
 values();
end method DoInPlaceHide;

//**********************************************************************
//
// CSimpSvrObj::DisassembleMenus
//
// Purpose:
//
//      Disassembles the combined menus used in inplace activation
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
//      Function                    Location
//
//      OutputDebugString               Windows API
//      OleDestroyMenuDescriptor        OLE API
//      RemoveMenu                      Windows API
//      IOleInPlaceFrame::RemoveMenus   Container
//      DestroyMenu                     Windows API
//
// Comments:
//
//      Be sure to read TECHNOTES.WRI included with the OLE SDK
//      for details on implementing inplace activation.
//
//********************************************************************


define method DisassembleMenus(this :: <CSimpSvrObj>) => ();
	
  // destroy the menu descriptor
  OleDestroyMenuDescriptor(this.m-hOleMenu);

  if ( ~ null?(this.m-hmenuShared) )
		
    // remove the menus that we added
    RemoveMenu( this.m-hmenuShared, 1, $MF-BYPOSITION);

    // have the container remove its menus
    IOleInPlaceFrame/RemoveMenus(this.m-lpFrame, this.m-hmenuShared);

    // Destroy the menu resource
    DestroyMenu(this.m-hmenuShared);

    this.m-hmenuShared := null-pointer(<HMENU>);
  end if;

  values();
end method DisassembleMenus;

//**********************************************************************
//
// CSimpSvrObj::SendOnDataChange
//
// Purpose:
//
//      Uses the data advise holder to send a data change, then updates
//      the ROT to note the time of change.
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
//      IDataAdviseHolder::SendOnDataChange     OLE API
//      GetRunningObjectTable                   OLE API
//      CoFileTimeNow                           OLE API
//      IRunningObjectTable::NoteChangeTime     OLE API
//
// Comments:
//
//
//********************************************************************


define method SendOnDataChange(this :: <CSimpSvrObj>) => ();

  unless ( null?(this.m-lpDataAdviseHolder) )
    IDataAdviseHolder/SendOnDataChange(this.m-lpDataAdviseHolder,
				       this.m-DataObject, 0, 0);
  end unless;

  let ( status :: <HRESULT>,
        lpRot :: <LPRUNNINGOBJECTTABLE> ) = GetRunningObjectTable(0);

  if ( (~ null?(lpRot)) & (this.m-dwRegister ~= 0) )
		
    with-stack-structure( pFT :: <PFILETIME> )

      CoFileTimeNow(pFT);

      IRunningObjectTable/NoteChangeTime(lpRot, this.m-dwRegister, pFT );
      Release(lpRot);
    end with-stack-structure;
  end if;

  values();
end method SendOnDataChange;

//**********************************************************************
//
// CSimpSvrObj::DeactivateUI
//
// Purpose:
//
//      Breaks down the inplace ui
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
//      SetParent                               Windows API
//      IOleInPlaceUIWindow::SetActiveObject    Container
//      IOleInPlaceFrame::SetActiveObject       Container
//      IOleInPlaceSite::UIDeactivate           Container
//
// Comments:
//
//
//********************************************************************


define method DeactivateUI(this :: <CSimpSvrObj>) => ();

  // if not UI active, or no pointer to IOleInPlaceFrame, then
  // return NOERROR
  if ( this.m-fUIActive | ~ null?(this.m-lpFrame) )
		
    this.m-fUIActive := #f;

    // remove hatching
    SetParent(GethDocWnd(this.m-lpDoc), GethAppWnd(this.m-lpDoc));
    SetParent(GethHatchWnd(this.m-lpDoc),GethDocWnd(this.m-lpDoc));

    // if in an MDI container, call SetActiveObject on the DOC.
    unless ( null?(this.m-lpCntrDoc) )
      IOleInPlaceUIWindow/SetActiveObject(this.m-lpCntrDoc, $NULL-Interface,
							   $NULL-OLESTR );
    end unless;

    IOleInPlaceUIWindow/SetActiveObject(this.m-lpFrame, $NULL-Interface,
						       $NULL-OLESTR );

    // tell the container that our UI is going away.
    unless ( null?(this.m-lpIPSite) )
      IOleInPlaceSite/OnUIDeactivate(this.m-lpIPSite, #f);
    end unless;
  end if;

  values();
end method DeactivateUI;
