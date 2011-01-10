Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $main-window-class :: <LPTSTR> = TEXT("SimpCntrAppWClass");

define constant $doc-window-class :: <LPTSTR> = TEXT("SimpCntrDocWClass");

/* -- Later ???
define constant cpeAppPal = 256;  // number of colors in our apps palette
define C-struct <LOGPAL>
  slot wVersion :: <WORD>;
  slot cpe :: <WORD>;
  slot rgpe :: array-type(<PALETTEENTRY>, dimensions: list(cpeAppPal));
end class <LOGPAL>;
*/


//**********************************************************************
//
// CreateStandardPalette
//
// Purpose:
//
//      Creates a standard Apps palette.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
//********************************************************************

/* // not yet supported ???
define method CreateStandardPalette() => value :: <HPALETTE>;
   
  let hpal :: <HPALETTE> = null-pointer(<HPALETTE>);
  let hdc :: <HDC> = GetDC($NULL-HWND);
  if ( ( ~ null-handle?(hdc) )
	& ~ zero?(logand( GetDeviceCaps(hdc, $RASTERCAPS), $RC-PALETTE)) )
      
    let cpeSysPal :: <integer> = GetDeviceCaps( hdc, $SIZEPALETTE);
    let cpeReserved :: <integer> = GetDeviceCaps( hdc, $NUMRESERVED);
    if ( cpeSysPal > cpeReserved )
	 
      let logpal :: <LOGPAL> = make(<LOGPAL>);
      let cpeReserved2 :: <integer> = truncate/(cpeReserved,2);

      // Get the system palette entries at the beginning and end.
      GetSystemPaletteEntries( hdc, 0, cpeReserved2, logpal.rgpe);
      GetSystemPaletteEntries( hdc, cpeSysPal - cpeReserved2, cpeReserved2,
			      logpal.rgpe + ( cpeAppPal - cpeReserved2));

      logpal.cpe := cpeAppPal;
      logpal.wVersion := #x300;

      for ( i :: <integer> from cpeReserved2
	     below cpeAppPal - cpeReserved2,
	    j :: <integer> from 10 )
	let color :: <vector> = palSVGA[j];
	logpal.rgpe[i].peFlags-value := $PC-NOCOLLAPSE;
	logpal.rgpe[i].peRed-value   := color[0];
	logpal.rgpe[i].peGreen-value := color[1];
	logpal.rgpe[i].peBlue-value  := color[2];
      end for;

      hpal := CreatePalette( pointer-cast(<LPLOGPALETTE>, logpal));
      destroy (logpal);
    end if;
  end if;
  ReleaseDC($NULL-HWND, hdc);
  hpal
end method CreateStandardPalette;
*/

//**********************************************************************
//
// CSimpleApp::CSimpleApp()
//
// Purpose:
//
//      Constructor for CSimpleApp
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
//      OutputDebugString           Windows API
//      SetRectEmpty                Windows API
//
// Comments:
//
//      CSimpleApp has a contained COleInPlaceFrame.  On construction
//      of CSimpleApp, we explicitly call the constructor of this
//      contained class and pass a copy of the this pointer, so that
//      COleInPlaceFrame can refer back to this class
//
//********************************************************************

define method initialize ( this :: <CSimpleApp>, #key, #all-keys) => ();

  next-method();
  OutputDebugString("In CSimpleApp's Constructor \r\n");

  this.m-OleInPlaceFrame := make(<COleInPlaceFrame>, App: this,
				 controlling-unknown: this);

  // clear members
  this.m-hAppWnd := $NULL-HWND;
  this.m-hInst := $NULL-HINSTANCE;
  this.m-lpDoc := #f;
  this.m-hwndUIActiveObj := $NULL-HWND;

  // clear flags
  this.m-fInitialized := #f;
  this.m-fCSHMode := #f;
  this.m-fMenuMode := #f;
  this.m-fAppActive := #f;
  
  // used for inplace
  SetRectEmpty(this.nullRect := make(<LPRECT>));
	
  this.m-hStdPal := null-handle(<HPALETTE>);
  values()
end method initialize;

//**********************************************************************
//
// CSimpleApp::~CSimpleApp()
//
// Purpose:
//
//      Destructor for CSimpleApp Class.
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
//      OutputDebugString           Windows API
//      OleUninitialize             OLE API
//
// Comments:
//
//********************************************************************


define method terminate (this :: <CSimpleApp>) => ();

  OutputDebugString( "In CSimpleApp's Destructor\r\n");
  next-method();
  unless ( null-handle?(this.m-hStdPal) )
    DeleteObject(this.m-hStdPal);
  end unless;

  // need to uninit the library...
  if ( this.m-fInitialized )
    OleUninitialize();
  end if;
  destroy(this.nullRect);
  values()
end method terminate  ;

//**********************************************************************
//
// CSimpleApp::DestroyDocs()
//
// Purpose:
//
//      Destroys all of the open documents in the application (Only one
//      since this is an SDI app, but could easily be modified to
//      support MDI).
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
// Comments:
//
//********************************************************************


define method DestroyDocs(this :: <CSimpleApp>) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);
  CloseDoc(this.m-lpDoc);   // we have only 1 document
  values()
end method DestroyDocs;

//**********************************************************************
//
// CSimpleApp::QueryInterface
//
// Purpose:
//
//      Used for interface negotiation at the Frame level.
//
// Parameters:
//
//      REFIID riid         -   A reference to the interface that is
//                              being queried.
//
//      LPVOID FAR* ppvObj  -   An out parameter to return a pointer to
//                              the interface.
//
// Return Value:
//
//      S_OK    -   The interface is supported.
//      S_FALSE -   The interface is not supported
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      IsEqualIID                  OLE API
//      ResultFromScode             OLE API
//      COleInPlaceFrame::AddRef    IOIPF.CPP
//
// Comments:
//
//      Note that this QueryInterface is associated with the frame.
//      Since the application could potentially have multiple documents
//      and multiple objects, a lot of the interfaces are ambiguous.
//      (ie. which IOleObject is returned?).  For this reason, only
//      pointers to interfaces associated with the frame are returned.
//      In this implementation, Only IOleInPlaceFrame (or one of the
//      interfaces it is derived from) can be returned.
//
//********************************************************************


define method IUnknown/QueryInterface(this :: <CSimpleApp>, riid :: <REFIID>)
	=> ( status :: <HRESULT>, interface :: <Interface> );

  OutputDebugString( "In CSimpleApp::QueryInterface\r\n");

  if ( riid = $IID-IUnknown )
    AddRef(this);
    values($S-OK, this)
  elseif ( riid = $IID-IOleWindow | riid = $IID-IOleInPlaceUIWindow
	    | riid = $IID-IOleInPlaceFrame )
    AddRef(this.m-OleInPlaceFrame);
    values($S-OK, this.m-OleInPlaceFrame)
  else
    // Not a supported interface
    values($E-NOINTERFACE, $null-interface)
  end if
end method IUnknown/QueryInterface;

//**********************************************************************
//
// CSimpleApp::AddRef
//
// Purpose:
//
//      Adds to the reference count at the Application level.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      ULONG   -   The new reference count of the application.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//      Due to the reference counting model that is used in this
//      implementation, this reference count is the sum of the
//      reference counts on all interfaces of all objects open
//      in the application.
//
//********************************************************************

/*
define method AddRef(this :: <CSimpleApp>) => value :: <integer>;

  OutputDebugString( "In CSimpleApp::AddRef\r\n");
  SafeAddRef(this)
end method AddRef;
*/

//**********************************************************************
//
// CSimpleApp::Release
//
// Purpose:
//
//      Decrements the reference count at this level
//
// Parameters:
//
//      None
//
// Return Value:
//
//      ULONG   -   The new reference count of the application.
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//
// Comments:
//
//********************************************************************

/*
define method Release(this :: <CSimpleApp>) => value :: <integer>;
	
  OutputDebugString( "In CSimpleApp::Release\r\n");

  SafeRelease(this)
end method Release;
*/

//**********************************************************************
//
// CSimpleApp::fInitApplication
//
// Purpose:
//
//      Initializes the application
//
// Parameters:
//
//      HANDLE hInstance    -   Instance handle of the application.
//
// Return Value:
//
//      TRUE    -   Application was successfully initialized.
//      FALSE   -   Application could not be initialized
//
// Function Calls:
//      Function                    Location
//
//      LoadIcon                    Windows API
//      LoadCursor                  Windows API
//      GetStockObject              Windows API
//      RegisterClass               Windows API
//
// Comments:
//
//********************************************************************


define method fInitApplication(this :: <CSimpleApp>, hInstance :: <HINSTANCE>)
	=> ok :: <boolean>;

  block(return)
	
   with-stack-structure ( wc :: <PWNDCLASS> )

    // Fill in window class structure with parameters that describe the
    // main window.

    wc.style-value := 0;                  // Class style(s).
    wc.lpfnWndProc-value := MainWndProc;  // Function to retrieve messages for
    					  //   windows of this class.
    wc.cbClsExtra-value := 0;             // No per-class extra data.
    wc.cbWndExtra-value := 0;             // No per-window extra data.
    wc.hInstance-value := hInstance;      // Application that owns the class.
    wc.hIcon-value := LoadIcon(hInstance, TEXT("SimpCntr"));
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := GetStockObject($WHITE-BRUSH);
    wc.lpszMenuName-value := TEXT("SIMPLEMENU"); // Name of menu resource in .RC file.
    wc.lpszClassName-value := $main-window-class;  // Name used in CreateWindow call

    if ( zero?(RegisterClass(wc)) )
      return( #f );
    end if;

    wc.style-value := $CS-DBLCLKS;        // Class style(s). allow DBLCLK's
    wc.lpfnWndProc-value := DocWndProc;   // Function to retrieve messages for
					  //   windows of this class.
    wc.cbClsExtra-value := 0;             // No per-class extra data.
    wc.cbWndExtra-value := 0;             // No per-window extra data.
    wc.hInstance-value := hInstance;      // Application that owns the class.
    wc.hIcon-value := null-pointer(<HICON>);
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := GetStockObject($WHITE-BRUSH);
    wc.lpszMenuName-value := null-pointer(<LPCSTR>);
    wc.lpszClassName-value := $doc-window-class; // Name used in CreateWindow call.

    // Register the window class and return success/failure code.

    let atom = RegisterClass(wc);
    return(~ zero?(atom));
   end with-stack-structure;
  end block
end method fInitApplication;

//**********************************************************************
//
// CSimpleApp::fInitInstance
//
// Purpose:
//
//      Instance initialization.
//
// Parameters:
//
//      HANDLE hInstance    -   App. Instance Handle.
//
//      int nCmdShow        -   Show parameter from WinMain
//
// Return Value:
//
//      TRUE    -   Initialization Successful
//      FALSE   -   Initialization Failed.
//
//
// Function Calls:
//      Function                    Location
//
//      CreateWindow                Windows API
//      ShowWindow                  Windows API
//      UpdateWindow                Windows API
//      OleBuildVersion             OLE API
//      OleInitialize               OLE API
//
// Comments:
//
//      Note that successful Initalization of the OLE libraries
//      is remembered so the UnInit is only called if needed.
//
//********************************************************************


define method fInitInstance(this :: <CSimpleApp>, hInstance :: <HINSTANCE>,
			    nCmdShow :: <integer>) => ok :: <boolean>;
  block(return)
	
/*	-- obsolete
    let dwVer :: <integer> = OleBuildVersion();
    // check to see if we are compatible with this version of the libraries
    if ( HIWORD( dwVer) ~= rmm | ( LOWORD( dwVer) <  rup) ) 
      #ifdef -DEBUG
	OutputDebugString( "WARNING: Incompatible OLE library version\r\n");
      #else
      return( #f );
      #endif
    end if;
*/

    if ( SUCCEEDED?(OleInitialize(null-pointer(<LPMALLOC>))) )
      this.m-fInitialized := #t;
    else
      // Replacing the standard allocator may not be legal.
      // Try again using the default allocator.
      if ( SUCCEEDED?(OleInitialize($NULL-interface)) )
	this.m-fInitialized := #t;
      end if;
    end if;

    this.m-hInst := hInstance;

    // Create the "application" windows
    this.m-hAppWnd :=
      CreateWindow($main-window-class,
		   TEXT("Simple OLE In-Place Container in Dylan"), // title
		   logior($WS-OVERLAPPEDWINDOW, $WS-CLIPCHILDREN), // style
		   $CW-USEDEFAULT,	// x position
		   $CW-USEDEFAULT,	// y position
		   400,	// $CW-USEDEFAULT,	// width
		   300,	// $CW-USEDEFAULT,	// height
		   $NULL-HWND,	// no parent
		   null-pointer(<HMENU>),
		   hInstance,
		   $NULL-VOID);

    if ( null-handle?(this.m-hAppWnd) )
      return(#f );
    end if;

/*	// later ???
    this.m-hStdPal := CreateStandardPalette();
 */

    ShowWindow(this.m-hAppWnd, nCmdShow);
    UpdateWindow(this.m-hAppWnd);

    return(this.m-fInitialized );
  end block
end method fInitInstance;

//**********************************************************************
//
// CSimpleApp::lCommandHandler
//
// Purpose:
//
//      Handles the processing of WM_COMMAND.
//
// Parameters:
//
//      HWND hWnd       -   Handle to the application Window
//
//      UINT message    -   message (always WM_COMMAND)
//
//      WPARAM wParam   -   Same as passed to the WndProc
//
//      LPARAM lParam   -   Same as passed to the WndProc
//
// Return Value:
//
//      NULL
//
// Function Calls:
//      Function                                    Location
//
//      IOleInPlaceActiveObject::QueryInterface     Object
//      IOleInPlaceObject::ContextSensitiveHelp     Object
//      IOleInPlaceObject::Release                  Object
//      IOleObject::DoVerb                          Object
//      GetClientRect                               Windows API
//      MessageBox                                  Windows API
//      DialogBox                                   Windows API
//      MakeProcInstance                            Windows API
//      FreeProcInstance                            Windows API
//      SendMessage                                 Windows API
//      DefWindowProc                               Windows API
//      CSimpleDoc::InsertObject                    DOC.CPP
//
// Comments:
//
//********************************************************************


define method lCommandHandler(this :: <CSimpleApp>, hWnd :: <HWND>,
			      message /* :: <integer> */,
			      wParam :: <integer>,
			      lParam /* :: <signed-long> */ )
	=> value :: <integer>;

  block(return)
	
    // let stabilize :: <CStabilize> = make(<CStabilize>, this);

    // context sensitive help...
    if ( this.m-fMenuMode | this.m-fCSHMode )
	        
      if ( this.m-fCSHMode )
	                
	// clear context sensitive help flag
	this.m-fCSHMode := #f;

	// if there is an InPlace active object, call its context sensitive help
	// method with the FALSE parameter to bring the object out of the
	// csh state.  See the technotes for details.
	let activeobj = this.m-lpDoc.m-lpActiveObject;
	unless ( null?(activeobj) )
	  let ( status, interface ) = 
	    QueryInterface(activeobj, $IID-IOleInPlaceObject);
	  let lpInPlaceObject :: <LPOLEINPLACEOBJECT> =
	    pointer-cast(<LPOLEINPLACEOBJECT>, interface);
	  IOleWindow/ContextSensitiveHelp(lpInPlaceObject, #f);
	  Release(lpInPlaceObject);
	end unless;
      end if;

      // see the technotes for details on implementing context sensitive
      // help
      if ( this.m-fMenuMode )
	this.m-fMenuMode := #f;
	let activeobj = this.m-lpDoc.m-lpActiveObject;
	unless ( null?(activeobj) )
	  IOleWindow/ContextSensitiveHelp(activeobj, #f);
	end unless;
      end if;
      // if we provided help, we would do it here...
      MessageBox(hWnd, "Help", "Help", $MB-OK);

      return(0);
    end if;

    // see if the command is a verb selections
    if ( LOWORD(wParam) >= $IDM-VERB0 )
      with-stack-structure ( pRect :: <LPRECT> )
	// get the rectangle of the object
	let site = this.m-lpDoc.m-lpSite;
	GetObjRect(site, pRect);
	IOleObject/DoVerb(site.m-lpOleObject,
			  LOWORD(wParam) - $IDM-VERB0, null-pointer(<LPMSG>),
			  site.m-OleClientSite,
			  0, this.m-lpDoc.m-hDocWnd, pRect);
      end with-stack-structure;
	        
    else
	        
      select (LOWORD(wParam) ) 
	// bring up the About box
	$IDM-ABOUT => 
	  DialogBox(this.m-hInst,          // current instance
		    TEXT("AboutBox"),      // resource to use
		    this.m-hAppWnd,        // parent handle
		    About);                // About() instance address
	    

	// bring up the InsertObject Dialog
	$IDM-INSERTOBJECT => 
	  InsertObject(this.m-lpDoc);

	// exit the application
	$IDM-EXIT => 
	  SendMessage(hWnd, $WM-SYSCOMMAND, $SC-CLOSE, 0);

	$IDM-NEW => 
	  CloseDoc(this.m-lpDoc);
	  this.m-lpDoc := #f;
	  lCreateDoc(this, hWnd, 0, 0, 0);

	otherwise => 
	  return(DefWindowProc(hWnd, message, wParam, lParam));
	                
      end select;   // end of switch
    end if;  // end of else
    return(0);
 end block
end method lCommandHandler;

//**********************************************************************
//
// CSimpleApp::lSizeHandler
//
// Purpose:
//
//      Handles the WM_SIZE message
//
// Parameters:
//
//      HWND hWnd       -   Handle to the application Window
//
//      UINT message    -   message (always WM_SIZE)
//
//      WPARAM wParam   -   Same as passed to the WndProc
//
//      LPARAM lParam   -   Same as passed to the WndProc
//
// Return Value:
//
//      LONG    -   returned from the "document" resizing
//
// Function Calls:
//      Function                    Location
//
//      GetClientRect               Windows API
//      CSimpleDoc::lResizeDoc      DOC.CPP
//
// Comments:
//
//********************************************************************

define method lSizeHandler(this :: <CSimpleApp>, hWnd /* :: <HWND> */,
			   message /* :: <integer> */,
			   wParam /* :: <integer> */,
			   lParam /* :: <signed-long> */ )
	=> value :: <integer>;

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  with-stack-structure ( pRect :: <PRECT> )
    GetClientRect(this.m-hAppWnd, pRect);
    lResizeDoc(this.m-lpDoc, pRect)
  end with-stack-structure
end method lSizeHandler;

//**********************************************************************
//
// CSimpleApp::lCreateDoc
//
// Purpose:
//
//      Handles the creation of a document.
//
// Parameters:
//
//      HWND hWnd       -   Handle to the application Window
//
//      UINT message    -   message (always WM_CREATE)
//
//      WPARAM wParam   -   Same as passed to the WndProc
//
//      LPARAM lParam   -   Same as passed to the WndProc
//
// Return Value:
//
//      NULL
//
// Function Calls:
//      Function                    Location
//
//      GetClientRect               Windows API
//      CSimpleDoc::CSimpleDoc      DOC.CPP
//
// Comments:
//
//********************************************************************

define method lCreateDoc(this :: <CSimpleApp>, hWnd :: <HWND>,
			 message /* :: <integer> */,
			 wParam /* :: <integer> */,
			 lParam /* :: <signed-long> */ )
	=> value :: <integer>;

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);
	
  with-stack-structure ( pRect :: <PRECT>)
    GetClientRect(hWnd, pRect);
    this.m-lpDoc := CSimpleDoc-Create(this, pRect, hWnd);
  end with-stack-structure;
  0
end method lCreateDoc;

//**********************************************************************
//
// CSimpleApp::AddFrameLevelUI
//
// Purpose:
//
//      Used during InPlace negotiation.
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
//      Function                            Location
//
//      COleInPlaceFrame::SetMenu           IOIPF.CPP
//      CSimpleApp::AddFrameLevelTools      APP.CPP
//
// Comments:
//
//      Be sure to read the Technotes included in the OLE 2.0 toolkit
//
//********************************************************************

define method AddFrameLevelUI(this :: <CSimpleApp>) => ();
	
  // let stabilize :: <CStabilize> = make(<CStabilize>, this);
  IOleInPlaceFrame/SetMenu(this.m-OleInPlaceFrame,
			   $NULL-HMENU, null-pointer(<HOLEMENU>), $NULL-HWND);
  AddFrameLevelTools(this);
  values()
end method AddFrameLevelUI;

//**********************************************************************
//
// CSimpleApp::AddFrameLevelTools
//
// Purpose:
//
//      Used during InPlace negotiation.
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
//      Function                              Location
//
//      COleInPlaceFrame::SetBorderSpace      IOIPF.CPP
//      InvalidateRect                        Windows API
//
// Comments:
//
//      Be sure to read the Technotes included in the OLE 2.0 toolkit
//
//********************************************************************

define method AddFrameLevelTools(this :: <CSimpleApp>) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);
  IOleInPlaceUIWindow/SetBorderSpace(this.m-OleInPlaceFrame, this.nullRect);
  InvalidateRect(this.m-hAppWnd, $NULL-RECT, #t);
  values()
end method AddFrameLevelTools;

//**********************************************************************
//
// CSimpleApp::HandleAccelerators
//
// Purpose:
//
//      To properly handle accelerators in the Message Loop
//
// Parameters:
//
//      LPMSG lpMsg -   A pointer to the message structure.
//
// Return Value:
//
//      TRUE    -   The accelerator was handled
//      FALSE   -   The accelerator was not handled
//
// Function Calls:
//      Function                                        Location
//
//      IOleInPlaceActiveObject::TranslateAccelerator   Object
//
// Comments:
//
//      If an object is InPlace active, it gets the first shot at
//      handling the accelerators.
//
//********************************************************************


define method HandleAccelerators(this :: <CSimpleApp>, lpMsg :: <LPMSG>)
	=> value :: <boolean>;
	
  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  let activeobj = this.m-lpDoc.m-lpActiveObject;
  if ( null?(activeobj) )
    #f
  else
    // if we have an InPlace Active Object
    // Pass the accelerator on...
    let hResult :: <HRESULT> =
      IOleInPlaceActiveObject/TranslateAccelerator(activeobj, lpMsg);
    // Not `SUCCEEDED?' because returns $S-FALSE when not done.
    hResult = $NOERROR
  end if
end method HandleAccelerators;

//**********************************************************************
//
// CSimpleApp::PaintApp
//
// Purpose:
//
//      Handles the painting of the doc window.
//
//
// Parameters:
//
//      HDC hDC -   hDC to the Doc Window.
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      CSimpleDoc::PaintDoc        DOC.CPP
//
// Comments:
//
//      This is an app level function in case we want to do palette
//      management.
//
//********************************************************************

define method PaintApp(this :: <CSimpleApp>, hDC /* :: <HDC> */) => ();

	// let stabilize :: <CStabilize> = make(<CStabilize>, this);

  // at this level, we could enumerate through all of the
  // visible objects in the application, so that a palette
  // that best fits all of the objects can be built.

  // This app is designed to take on the same palette
  // functionality that was provided in OLE 1.0, the palette
  // of the last object drawn is realized.  Since we only
  // support one object at a time, it shouldn't be a big
  // deal.

  // if we supported multiple documents, we would enumerate
  // through each of the open documents and call paint.
  
  unless ( null?(this.m-lpDoc) )
    PaintDoc(this.m-lpDoc, hDC);
  end unless;

  values()
end method PaintApp;

//**********************************************************************
//
// CSimpleApp::ContextSensitiveHelp
//
// Purpose:
//      Used in supporting context sensitive haelp at the app level.
//
//
// Parameters:
//
//      BOOL fEnterMode    -   Entering/Exiting Context Sensitive
//                             help mode.
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                                    Location
//
//      IOleInPlaceActiveObject::QueryInterface     Object
//      IOleInPlaceObject::ContextSensitiveHelp     Object
//      IOleInPlaceObject::Release                  Object
//
// Comments:
//
//      This function isn't used because we don't support Shift+F1
//      context sensitive help.  Be sure to look at the technotes
//      in the OLE 2.0 toolkit.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(this :: <CSimpleApp>,
					      fEnterMode :: <boolean>)
 => (status :: <HRESULT>)

	// let stabilize :: <CStabilize> = make(<CStabilize>, this);
  let result :: <HRESULT> = $S-OK;
  if ( this.m-fCSHMode ~= fEnterMode )
	        
    this.m-fCSHMode := fEnterMode;

    // this code "trickles" the context sensitive help via shift+f1
    // to the inplace active object.  See the technotes for implementation
    // details.
    let activeobj = this.m-lpDoc.m-lpActiveObject;
    unless ( null?(activeobj) )
      let ( status, interface ) =
	QueryInterface(activeobj, $IID-IOleInPlaceObject);
      let lpInPlaceObject :: <LPOLEINPLACEOBJECT> =
	pointer-cast(<LPOLEINPLACEOBJECT>, interface);
      result := IOleWindow/ContextSensitiveHelp(lpInPlaceObject, fEnterMode);
      Release(lpInPlaceObject);
    end unless;
  end if;
  result
end method IOleWindow/ContextSensitiveHelp;


/* OLE2NOTE: forward the WM_QUERYNEWPALETTE message (via
**    SendMessage) to UIActive in-place object if there is one.
**    this gives the UIActive object the opportunity to select
**    and realize its color palette as the FOREGROUND palette.
**    this is optional for in-place containers. if a container
**    prefers to force its color palette as the foreground
**    palette then it should NOT forward the this message. or
**    the container can give the UIActive object priority; if
**    the UIActive object returns 0 from the WM_QUERYNEWPALETTE
**    message (ie. it did not realize its own palette), then
**    the container can realize its palette.
**    (see ContainerDoc_ForwardPaletteChangedMsg for more info)
**
**    (It is a good idea for containers to use the standard
**    palette even if they do not use colors themselves. this
**    will allow embedded object to get a good distribution of
**    colors when they are being drawn by the container)
**
*/


define method QueryNewPalette(this :: <CSimpleApp>) => value :: <integer>;

  if ( (~ null?(this.m-hwndUIActiveObj) )
	& ( ~ zero?(SendMessage(this.m-hwndUIActiveObj,
				$WM-QUERYNEWPALETTE, 0, 0)) ) )
    /* Object selected its palette as foreground palette */ 
    1
  else
    wSelectPalette(this.m-hAppWnd, this.m-hStdPal, #f /* fBackground */ )
  end if
end method QueryNewPalette;


/* This is just a helper routine */


define method wSelectPalette(hWnd :: <HWND>, hPal :: <HPALETTE>,
			     fBackground :: <boolean>)
	=> value :: <integer>;
  0
/* // later ???
  if ( null-handle?(hPal) )
    0
  else
    let hdc :: <HDC> = GetDC(hWnd);
    let hOldPal :: <HPALETTE> = SelectPalette(hdc, hPal, fBackground);
    let iPalChg :: <integer> = RealizePalette(hdc);
    SelectPalette(hdc, hOldPal, #t /* fBackground */ );
    ReleaseDC(hWnd, hdc);

    if ( iPalChg >  0 )
      InvalidateRect(hWnd, $NULL-RECT, #t);
    end if;
    1
  end if
*/
end method wSelectPalette;


