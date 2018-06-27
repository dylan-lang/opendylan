Module:    windows-ffi-example
Synopsis:  This is a simple example of a Windows program implemented in Dylan
	   using the low-level interface libraries to the Win32 API.
	   It was adapted from sample C programs in the Win32 SDK.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *hInst* // current instance
			 :: <HINSTANCE> = null-handle(<HINSTANCE>);
define constant $szAppName = TEXT("DylanWinExamp");  // The name of this application
define constant $szTitle = TEXT("Dylan example");    // The title bar text

define constant $null-hPen = null-handle(<HPEN>);
define constant $null-hBrush = null-handle(<HBRUSH>);

define variable *hDashPen* :: <HPEN> = $null-hPen; /* "---" pen handle */
define variable *hGreenBrush* :: <HBRUSH> = $null-hBrush; /* green brush */


define method Main-program() => (exit-status :: <integer>);

  let hInstance :: <HINSTANCE> = application-instance-handle();
  let nCmdShow :: <integer> = application-show-window();

  if ( Init-Application(hInstance)  // Initialize shared things
	// Perform initializations that apply to a specific instance 
	& Init-Instance(hInstance, nCmdShow) )
    // initialized OK

    let hAccelTable :: <HACCEL> =
      LoadAccelerators(hInstance, MAKEINTRESOURCE($IDR-GENERIC));

    // Acquire and dispatch messages until a WM_QUIT message is received.

    with-stack-structure ( pMsg :: <PMSG> )
      while( GetMessage(pMsg, // message structure
			$NULL-HWND,  // handle of window receiving the message
			0,      // lowest message to examine
			0))     // highest message to examine

	if ( TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 )
	  TranslateMessage(pMsg); // Translates virtual key codes
	  DispatchMessage(pMsg);  // Dispatches message to window
	end if;

      end while;
      pMsg.wParam-value
    end with-stack-structure
  else
    GetLastError()     // return error code if initialization fails
  end if
end method Main-program;


define method Init-Application(hInstance :: <HINSTANCE>) => (ok :: <boolean>);

  with-stack-structure ( wc :: <PWNDCLASS> )

    // Fill in window class structure with parameters that describe the
    // main window.

    wc.style-value := logior($CS-HREDRAW, $CS-VREDRAW); // Class style(s).
    wc.lpfnWndProc-value := WndProc;       // Window Procedure
    wc.cbClsExtra-value := 0;              // No per-class extra data.
    wc.cbWndExtra-value := 0;              // No per-window extra data.
    wc.hInstance-value := hInstance;       // Owner of this class
    wc.hIcon-value := LoadIcon( null-handle(<HINSTANCE>), $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor(null-handle(<HINSTANCE>), $IDC-ARROW);
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // Default color
    wc.lpszMenuName-value := MAKEINTRESOURCE($IDR-GENERIC); // Menu from .RC
    wc.lpszClassName-value := $szAppName;              // Name to register as

    // Register the window class and return success/failure code.
    /* return */
    RegisterClass(wc) ~= 0;
  end with-stack-structure
end method Init-Application;


define method Init-Instance(
        hInstance :: <HINSTANCE>, nCmdShow :: <integer>) => (ok :: <boolean>);

  // Save the instance handle in static variable, which will be used in
  // many subsequence calls from this application to Windows.
  
  *hInst* := hInstance; // Store instance handle in our global variable

  // Create a main window for this application instance.

  let hWnd :: <HWND> =   // Main window handle.
    CreateWindow(
		 $szAppName,           // See RegisterClass() call.
		 $szTitle,             // Text for window title bar.
                 $WS-OVERLAPPEDWINDOW, // Window style.
		 100, 200, 300, 200,   // size and position
                 $NULL-HWND,	       // Overlapped windows have no parent.
                 null-handle(<HMENU>), // Use the window class menu.
                 hInstance,            // This instance owns this window.
                 $NULL-VOID // We don't use any data in our WM_CREATE
		   );

  // If window could not be created, return "failure"
  if ( null-handle?(hWnd) ) 
    #f
  else
    // Make the window visible; update its client area; and return "success"
    ShowWindow(hWnd, nCmdShow); // Show the window
    UpdateWindow(hWnd);         // Sends WM_PAINT message and returns status
  end if
end method Init-Instance;

define constant $message :: <LPTSTR> = TEXT("Dylan lives!");

define callback WndProc :: <WNDPROC> = main-window-function;

define method main-window-function(
                hWnd :: <HWND>,		// window handle
                message :: <integer>,  	// type of message
                uParam,			// additional information
                lParam)			// additional information
	=> (value :: <integer>);
  block(return)
	
    select ( message ) 

      $WM-COMMAND =>   // message: command from application menu

	let wmId :: <integer> = LOWORD(uParam);
	/* let wmEvent :: <integer> = HIWORD(uParam); */

	select ( wmId ) 
	  $IDM-ABOUT => 
	    DialogBox(*hInst*,           // current instance
		      MAKEINTRESOURCE($IDD-ABOUTBOX), // dlg resource to use
		      hWnd,                  // parent handle
		      About-Proc); // About() instance address

	  $IDM-EXIT => 
	    DestroyWindow(hWnd);

	  $IDM-HELPCONTENTS => 
	    if ( ~ WinHelp(hWnd, TEXT("EXAMPLE.HLP"), $HELP-KEY,
			   pointer-address(TEXT("CONTENTS"))) )
	      MessageBox(GetFocus(),
			 TEXT("Unable to activate help"),
			 $szAppName,
			 logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
	    end if;

	  $IDM-HELPSEARCH => 
	    if ( ~ WinHelp(hWnd, TEXT("EXAMPLE.HLP"), $HELP-PARTIALKEY,
			   pointer-address(TEXT(""))) )
	      MessageBox(GetFocus(),
			 TEXT("Unable to activate help"),
			 $szAppName,
			 logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
	    end if;
	    
	  $IDM-HELPHELP => 
	    if ( ~ WinHelp(hWnd, $NULL-string, $HELP-HELPONHELP, 0) )
	      MessageBox(GetFocus(),
			 TEXT("Unable to activate help"),
			 $szAppName,
			 logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
	    end if;

	  // Here are all the other possible menu options,
	  // all of these are currently disabled:
	  /*
	    $IDM-NEW, $IDM-OPEN, $IDM-SAVE, $IDM-SAVEAS, $IDM-UNDO, $IDM-CUT,
	    $IDM-COPY, $IDM-PASTE, $IDM-LINK, $IDM-LINKS => 
	  */
  
	  otherwise => 
	    return(DefWindowProc(hWnd, message, uParam, lParam));
                        
	end select;

      $WM-CREATE => 

	/* Create the brush objects */ 

	*hGreenBrush* := CreateSolidBrush(RGB(0, 255, 0));

	/* Create the "---" pen */ 

	*hDashPen* := CreatePen($PS-DASH,	       /* style */ 
				1,		      /* width */ 
				RGB(0, 0, 0));	      /* color */ 

      $WM-PAINT => 
	begin 
	  let szText :: <string> = "";

	  with-stack-structure ( ps :: <PPAINTSTRUCT> )
	  
	    // Set up a display context to begin painting 
	    let hDC :: <HDC> = BeginPaint(hWnd, ps);

	    // Display some text
	    TextOut(hDC, 100, 50, $message, size($message));

	    // Draw a green rectangle
	    let hOldBrush = SelectObject(hDC, *hGreenBrush*);
	    Rectangle(hDC, 20, 20, 70, 70);

	    // Restore the old brush  
	    SelectObject(hDC, hOldBrush);

	    // Select a "---" pen, save the old value
	    let hOldPen = SelectObject(hDC, *hDashPen*);

	    // Move to a specified point
	    MoveToEx(hDC, 100, 100, $NULL-POINT);

	    // Draw a line
	    LineTo(hDC, 250, 100);

	    // Restore the old pen  
	    SelectObject(hDC, hOldPen);

	    // Tell Windows you are done painting  
	    EndPaint(hWnd, ps);
	  end with-stack-structure;
	end;

      $WM-DESTROY =>   // message: window being destroyed
	DeleteObject(*hGreenBrush*);
	DeleteObject(*hDashPen*);

	*hGreenBrush* := $null-hBrush;
	*hDashPen* := $null-hPen;

	PostQuitMessage(0);

      otherwise =>           // Passes it on if unproccessed
	return(DefWindowProc(hWnd, message, uParam, lParam));
      
    end select;
    return(0);
  end block;
end method main-window-function;


define method About(hDlg :: <HWND>, message :: <integer>, wParam, lParam)
	=> (value :: <boolean>);

   select ( message ) 
     $WM-INITDIALOG => #t; 	// initialize dialog box

     $WM-COMMAND => 		// input received
       let button :: <integer> = LOWORD(wParam);
       if ( button = $IDOK | button = $IDCANCEL )
	 EndDialog(hDlg, 1);
	 #t
       else #f
       end if;
 
     otherwise => #f;
   end select
end method About;

define callback About-Proc :: <DLGPROC> = About;


Main-program(); // run the main program
