Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $GUID-SIMPLE :: <REFGUID> =
  make-GUID(#xf0c2c790, #xc6e3, #x11ce,
	    #x89, #xe6, #x02, #x07, #x01, #x19, #xf6, #x39);

//**********************************************************************
//
// CSimpSvrApp::CSimpSvrApp()
//
// Purpose:
//
//      Constructor for CSimpSvrApp
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
//
//********************************************************************


define method initialize (this :: <CSimpSvrApp>, #rest ignore, #key ) => ();
	
  OutputDebugString("In CSimpSvrApp's Constructor \r\n");
  next-method();

  // clear members
  this.m-hAppWnd := $NULL-HWND;
  this.m-hInst := $NULL-HINSTANCE;
  this.m-lpDoc := $NULL-interface;

  // clear flags
  this.m-fInitialized := #f;

  // used for inplace
 // SetRectEmpty( this.nullRect := make(<LPRECT>) );

  values();
end method initialize ;

//**********************************************************************
//
// CSimpSvrApp::~CSimpSvrApp()
//
// Purpose:
//
//      Destructor for CSimpSvrApp Class.
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
//      DestroyWindow               Windows API
//      CSimpSvrApp::IsInitialized  APP.H
//      OleUninitialize             OLE API
//
// Comments:
//
//********************************************************************


define method terminate (this :: <CSimpSvrApp>) => ();
  
  OutputDebugString("In CSimpSvrApp's Destructor\r\n");

  next-method();

  // need to uninit the library...
  if ( IsInitialized(this) )
    OleUninitialize();
  end if;

  unless ( null-handle?(this.m-hAppWnd) )
    DestroyWindow(this.m-hAppWnd);
    this.m-hAppWnd := $NULL-HWND;
  end unless;
  values();
end terminate;


//**********************************************************************
//
// CSimpSvrApp::fInitApplication
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
//      RegisterHatchWindowClass    OUTLUI.DLL
//
// Comments:
//
//********************************************************************

define constant $main-window-class :: <LPTSTR> =
						as(<LPTSTR>,"DWSimpSvrWClass");
define constant $doc-window-class :: <LPTSTR> = as(<LPTSTR>,"DWDocWClass");

define function fInitApplication (this :: <CSimpSvrApp>,
				  hInstance :: <HINSTANCE>)
	=> ok? :: <boolean>;
  block(return)
	
      /* CStabilize stabilize(this); */
   with-stack-structure ( wc :: <PWNDCLASS> )

    // Fill in window class structure with parameters that describe the
    // main window.

    wc.style-value := 0;                 // Class style(s).
    wc.lpfnWndProc-value := MainWndProc; // Function to retrieve messages for
					 //   windows of this class.
    wc.cbClsExtra-value := 0;            // No per-class extra data.
    wc.cbWndExtra-value := 0;            // No per-window extra data.
    wc.hInstance-value := hInstance;     // Application that owns the class.
    wc.hIcon-value := LoadIcon(hInstance, TEXT("SimpSvr"));
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := GetStockObject($WHITE-BRUSH);
    wc.lpszMenuName-value := TEXT("SimpSvrMENU");   // Name of menu resource in .RC file.
    wc.lpszClassName-value := $main-window-class;  // Name used in call to CreateWindow.

    if ( ~ RegisterClass(wc) )
      return( #f );
    end if;

    wc.style-value := logior($CS-VREDRAW,$CS-HREDRAW); // Class style(s).
    wc.lpfnWndProc-value := DocWndProc;  // Function to retrieve messages for
					 //   windows of this class.
    wc.cbClsExtra-value := 0;            // No per-class extra data.
    wc.cbWndExtra-value := 0;            // No per-window extra data.
    wc.hInstance-value := hInstance;     // Application that owns the class.
    wc.hIcon-value := null-pointer(<HICON>);
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := GetStockObject($WHITE-BRUSH);
    wc.lpszMenuName-value := $NULL-string;
    wc.lpszClassName-value := $doc-window-class; // Name used in call to CreateWindow.

    // Register the window class and return success/failure code.

    if ( ~ RegisterClass(wc) )
      return( #f );
    end if;
   end with-stack-structure;
   return( RegisterHatchWindowClass(hInstance) );
 end block;
end fInitApplication;

//**********************************************************************
//
// CSimpSvrApp::fInitInstance
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
//      InvalidateRect              Windows API
//      ShowWindow                  Windows API
//      UpdateWindow                Windows API
//      CoRegisterClassObject       OLE API
//      OleBuildVersion             OLE API
//      OleInitialize               OLE API
//      CSimpSvrDoc::CreateObject   DOC.CPP
//
// Comments:
//
//      Note that successful Initalization of the OLE libraries
//      is remembered so the UnInit is only called if needed.
//
//********************************************************************


define function fInitInstance(this :: <CSimpSvrApp>, hInstance :: <HINSTANCE>,
			    nCmdShow :: <integer> )
 => ( initialized :: <boolean>, lpClassFactory :: <Interface> )

  /* CStabilize stabilize(this); */
  this.m-hInst := hInstance;

  // initialize the libraries
  if ( SUCCEEDED?(OleInitialize($NULL-interface)) )
    this.m-fInitialized := #t;
  else cerror("Continue", "OleInitialize failed");
  end if;

  // Create the "application" windows
  this.m-hAppWnd := CreateWindow($main-window-class, // class name
				 TEXT("Simple OLE Server in Dylan"), // title
				 $WS-OVERLAPPEDWINDOW, // style
				 $CW-USEDEFAULT, // x position
				 $CW-USEDEFAULT, // y position
				 300, // $CW-USEDEFAULT, // width
				 250, // $CW-USEDEFAULT, // height
				 $NULL-HWND, // no parent
				 $NULL-HMENU,
				 hInstance,
				 $NULL-VOID);

  if ( null?( this.m-hAppWnd ) )
    cerror("Continue", "CreateWindow error #x%x", GetLastError() );
    values(#f, $null-interface)
  else 
    ole-util-init();

    // if not started by OLE, then show the Window, and create a "fake"
    // object, else Register a pointer to IClassFactory so that OLE can 
    // instruct us to make an object at the appropriate time.
    let lpClassFactory :: <Interface> = $NULL-interface;
    if ( ~ this.m-fStartByOle )
      
      ShowAppWnd(this, nCmdShow);
      let ( status, object ) = CreateObject(this.m-lpDoc, $IID-IOleObject);
      this.m-OleObject := object;
      InvalidateRect( GethDocWnd(this.m-lpDoc), $NULL-RECT, #t);
      
    else
      
      lpClassFactory := make(<CClassFactory>, app: this,
			     controlling-unknown: this);

      // shouldn't pass an API an object with a zero ref count
      AddRef(lpClassFactory);

      let ( status, regnum ) = 
	CoRegisterClassObject($GUID-SIMPLE, lpClassFactory,
			      $CLSCTX-LOCAL-SERVER, $REGCLS-SINGLEUSE);
      if ( SUCCEEDED?(status) )
	OutputDebugString("CoRegisterClassObject OK\r\n");
      else
	error("CoRegisterClassObject status = %=", status);
      end if;
      this.m-dwRegisterClass := regnum;

      // remove artificial Ref. count
      Release(lpClassFactory);
    end if;

    this.m-hMainMenu := GetMenu(this.m-hAppWnd);
    this.m-hColorMenu := GetSubMenu(this.m-hMainMenu, 1);
    // this.m-hHelpMenu := GetSubMenu(this.m-hMainMenu, 2);

    values( this.m-fInitialized, lpClassFactory )
  end if;
end fInitInstance;

//**********************************************************************
//
// CSimpSvrApp::lCommandHandler
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
//      GetClientRect                               Windows API
//      MessageBox                                  Windows API
//      DialogBox                                   Windows API
//      MakeProcInstance                            Windows API
//      FreeProcInstance                            Windows API
//      SendMessage                                 Windows API
//      DefWindowProc                               Windows API
//      InvalidateRect                              Windows API
//      CSimpSvrDoc::InsertObject                   DOC.CPP
//      CSimpSvrObj::SetColor                       OBJ.CPP
//      CSimpSvrObj::RotateColor                    OBJ.CPP
//
// Comments:
//
//********************************************************************


define method lCommandHandler(this :: <CSimpSvrApp>, hWnd :: <HWND>,
			      message, wParam, lParam ) => value :: <integer>;

 block(return)
	
     /* CStabilize stabilize(this); */
   //@@WTK WIN32, UNICODE
   //switch (wParam) {
   select ( LOWORD(wParam) ) 
     // bring up the About box
     $IDM-ABOUT => 
       DialogBox(this.m-hInst,		// current instance
		 TEXT("AboutBox"),		// resource to use
		 this.m-hAppWnd,	// parent handle
		 About);		// About() instance address

     // exit the application
     $IDM-EXIT => 
       SendMessage(hWnd, $WM-SYSCOMMAND, $SC-CLOSE, 0);

     $IDM-RED => 
       SetColor(GetObj(this.m-lpDoc), 128, 0, 0);
       InvalidateRect(GethDocWnd(this.m-lpDoc), $NULL-RECT, #t);

     $IDM-GREEN => 
       SetColor(GetObj(this.m-lpDoc), 0,128, 0);
       InvalidateRect(GethDocWnd(this.m-lpDoc), $NULL-RECT, #t);

     $IDM-BLUE => 
       SetColor(GetObj(this.m-lpDoc), 0, 0, 128);
       InvalidateRect(GethDocWnd(this.m-lpDoc), $NULL-RECT, #t);

     $IDM-ROTATE => 
       RotateColor(GetObj(this.m-lpDoc));
       InvalidateRect(GethDocWnd(this.m-lpDoc), $NULL-RECT, #t);

     otherwise => 
       return( DefWindowProc(hWnd, message, wParam, lParam) );
                   
   end select;   // end of switch
   return( 0 );
 end block;
end method lCommandHandler;

//**********************************************************************
//
// CSimpSvrApp::lSizeHandler
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
//      CSimpSvrDoc::lResizeDoc      DOC.CPP
//
// Comments:
//
//********************************************************************


define method lSizeHandler(this :: <CSimpSvrApp>, hWnd :: <HWND>,
			   message, wParam, lParam ) => value :: <integer>;
    /* CStabilize stabilize(this); */

  let lpRect :: <LPRECT> = make(<LPRECT>);
  GetClientRect(this.m-hAppWnd,lpRect);
  lResizeDoc(this.m-lpDoc, lpRect);
end method lSizeHandler;

//**********************************************************************
//
// CSimpSvrApp::lCreateDoc
//                                                         d
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
//      CSimpSvrDoc::Create         DOC.CPP
//
// Comments:
//
//********************************************************************


define function lCreateDoc(this :: <CSimpSvrApp>, hWnd :: <HWND>,
			   message, wParam, lParam ) => value :: <integer>;
    /* CStabilize stabilize(this); */

  let lpRect :: <LPRECT> = make(<LPRECT>);

  GetClientRect(hWnd,lpRect);

  this.m-lpDoc := /* CSimpSvrDoc:: */ Create(this, lpRect, hWnd);

  0;
end lCreateDoc;



//**********************************************************************
//
// CSimpSvrApp::PaintApp
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
//      CSimpSvrDoc::PaintDoc        DOC.CPP
//
// Comments:
//
//
//********************************************************************


define function PaintApp(this :: <CSimpSvrApp>, hDC :: <HDC>) => ();

    /* CStabilize stabilize(this); */

  // if we supported multiple documents, we would enumerate
  // through each of the open documents and call paint.

  unless ( null?(this.m-lpDoc) )
    PaintDoc(this.m-lpDoc, hDC);
  end unless;

  values();
end PaintApp;

//**********************************************************************
//
// CSimpSvrApp::ParseCmdLine
//
// Purpose:
//
//      Determines if the app was started by OLE
//
//
// Parameters:
//
//      LPSTR lpCmdLine -   Pointer to the command line
//
// Return Value:
//
//      None
//
// Comments:
//      Parses the command line looking for the -Embedding or /Embedding
//      flag.
//
//********************************************************************


define function ParseCommandLine(this :: <CSimpSvrApp>, lpCmdLine :: <LPSTR>)
 => ();
	
  // use utility function in the COM library
  this.m-fStartByOle := OLE-util-started-by-OLE?();
  values();
end ParseCommandLine;

//**********************************************************************
//
// CSimpSvrApp::SetStatusText
//
// Purpose:
//
//      Blanks out the text in the status bar
//
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
//      CSimpSvrDoc::SetStatusText  DOC.CPP
//
//
// Comments:
//
//
//********************************************************************


define method SetStatusText(this :: <CSimpSvrApp>) => ();

    /* CStabilize stabilize(this); */
  SetStatusText(this.m-lpDoc);
end SetStatusText;


//**********************************************************************
//
// CSimpSvrApp::IsInPlaceActive
//
// Purpose:
//
//      Safely determines from the app level if currently inplace active.
//
//
// Parameters:
//
//      None
//
// Return Value:
//
//      TRUE    - Inplace active
//      FALSE   - Not Inplace active
//
// Function Calls:
//      Function                    Location
//
//      CSimpSvrDoc::GetObject      OBJ.H
//      CSimpSvrObj:IsInPlaceActive OBJ.H
//
//
// Comments:
//
//
//********************************************************************


define method IsInPlaceActive(this :: <CSimpSvrApp>)
	=> value :: <boolean>;

    /* CStabilize stabilize(this); */
  let retval :: <boolean> =  #f;

  unless ( null?(this.m-lpDoc) )
    let obj = GetObj(this.m-lpDoc);
    unless ( null?(obj) )
      retval := IsInPlaceActive(obj);
    end unless;
  end unless;
  
  retval
end method IsInPlaceActive;

//**********************************************************************
//
// CSimpSvrApp::ShowAppWnd
//
// Purpose:
//
//      Shows the Application Window
//
// Parameters:
//
//      int nCmdShow    - Window State
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                        Location
//
//      ShowWindow                      Windows API
//      UpdateWindow                    Windows API
//      CoLockObjectExternal            OLE API
//
// Comments:
//
//********************************************************************


define method ShowAppWnd(this :: <CSimpSvrApp>, nCmdShow :: <integer>) => ();

  CoLockObjectExternal(this, #t, #f);
  ShowWindow(this.m-hAppWnd, nCmdShow);
  UpdateWindow(this.m-hAppWnd);
  values();
end method ShowAppWnd;

//**********************************************************************
//
// CSimpSvrApp::HideAppWnd
//
// Purpose:
//
//      Hides the Application Window
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
//      ShowWindow                      Windows API
//      CoLockObjectExternal            OLE API
//
// Comments:
//
//********************************************************************


define method HideAppWnd(this :: <CSimpSvrApp>) => ();

  CoLockObjectExternal(this, #f, #t);
  ShowWindow(this.m-hAppWnd, $SW-HIDE);
  values()
end method HideAppWnd;
