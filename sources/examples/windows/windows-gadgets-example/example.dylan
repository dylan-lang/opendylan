Module:    windows-gadgets-example
Synopsis:  An example demonstrating the use of Win32 gadgets
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The main program

define constant $szAppName = TEXT("DylanGadgetsExample");
define constant $szTitle   = TEXT("Dylan gadgets example");

define method main-program () => (exit-status :: <signed-int>)

  let hInstance :: <HINSTANCE> = application-instance-handle();
  let nCmdShow :: <signed-int> = application-show-window();

  if ( init-application(hInstance)  // Initialize shared things
	// Perform initializations that apply to a specific instance 
	& init-instance(hInstance, nCmdShow) )
    // initialized OK

    let hAccelTable :: <HACCEL> =
      LoadAccelerators(hInstance, MAKEINTRESOURCE($IDR-GENERIC));

    // Acquire and dispatch messages until a WM_QUIT message is received.

    let pMsg :: <PMSG> = make(<PMSG>);
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
  else
    GetLastError()     // return error code if initialization fails
  end if
end method main-program;


define method init-application (hInstance :: <HINSTANCE>) => (ok :: <boolean>)

  let wc :: <PWNDCLASS> = make(<PWNDCLASS>);

  // Fill in window class structure with parameters that describe the
  // main window.

  wc.style-value := logior($CS-HREDRAW, $CS-VREDRAW); // Class style(s).
  wc.lpfnWndProc-value := WndProc;       // Window Procedure
  wc.cbClsExtra-value := 0;                      // No per-class extra data.
  wc.cbWndExtra-value := 0;                      // No per-window extra data.
  wc.hInstance-value := hInstance;              // Owner of this class
  wc.hIcon-value := LoadIcon( null-handle(<HINSTANCE>), $IDI-APPLICATION);
  wc.hCursor-value := LoadCursor(null-handle(<HINSTANCE>), $IDC-ARROW);
  wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // Default color
  wc.lpszMenuName-value := MAKEINTRESOURCE($IDR-GENERIC); // Menu from .RC
  wc.lpszClassName-value := $szAppName;              // Name to register as

  // Register the window class and return success/failure code.
  /* return */
  RegisterClass(wc) ~= 0;
end method init-application;

define method init-instance
    (hInstance :: <HINSTANCE>, nCmdShow :: <signed-int>)
 => (ok :: <boolean>)
  let window :: <HWND>
    = CreateWindow($szAppName,           // See RegisterClass() call.
		   $szTitle,             // Text for window title bar.
		   $WS-OVERLAPPEDWINDOW, // Window style.
		   100, 200, 300, 500,   // size and position
		   $NULL-HWND,	       // Overlapped windows have no parent.
		   null-handle(<HMENU>), // Use the window class menu.
		   hInstance,            // This instance owns this window.
		   $NULL-VOID // We don't use any data in our WM_CREATE
		   );
  if (~null-handle?(window))
    make-menu-bar(window);
    make-every-gadget(window);
    ShowWindow(window, nCmdShow);
    UpdateWindow(window);
  end
end method init-instance;

define callback WndProc :: <WNDPROC> = main-window-function;

define method main-window-function
    (window :: <HWND>,         // window handle
     message :: <unsigned-int>,      // type of message
     uParam :: <signed-int>,     // additional information
     lParam :: <signed-int>)   // additional information
 => (value :: <integer>)
  block(return)
    select (message) 
      $WM-COMMAND =>   // message: command from application menu
	let wmId :: <signed-int> = LOWORD(uParam);
	/* let wmEvent :: <signed-int> = HIWORD(uParam); */

	select ( wmId ) 
	  $OPEN-MENU-ITEM =>
	    display-open-dialog(window);
	  $SAVE-AS-MENU-ITEM =>
	    display-save-as-dialog(window);
	  $PAGE-SETUP-MENU-ITEM =>
	    display-page-setup-dialog(window);
	  $PRINT-MENU-ITEM =>
	    display-print-dialog(window);
	  $EXIT-MENU-ITEM => 
	    DestroyWindow(window);

	  $CHOOSE-COLOR-MENU-ITEM =>
	    display-color-dialog(window);
	  $CHOOSE-FONT-MENU-ITEM =>
	    display-font-dialog(window);

	  $FIND-MENU-ITEM =>
	    display-find-dialog(window);
	  $REPLACE-MENU-ITEM =>
	    display-replace-dialog(window);

	  $HELP-CONTENTS-MENU-ITEM => 
	    if (~WinHelp
		  (window, TEXT("EXAMPLE.HLP"), $HELP-KEY,
		   pointer-address(TEXT("CONTENTS"))))
	      MessageBox(GetFocus(),
			 TEXT("Unable to activate help"),
			 $szAppName,
			 logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
	    end if;

	  $IDM-HELPSEARCH => 
	    if (~WinHelp(window, TEXT("EXAMPLE.HLP"), $HELP-PARTIALKEY,
			 pointer-address(TEXT(""))))
	      MessageBox(GetFocus(),
			 TEXT("Unable to activate help"),
			 $szAppName,
			 logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
	    end if;
	    
	  $IDM-HELPHELP => 
	    if ( ~ WinHelp(window, $NULL-string, $HELP-HELPONHELP, 0) )
	      MessageBox(GetFocus(),
			 TEXT("Unable to activate help"),
			 $szAppName,
			 logior($MB-SYSTEMMODAL, logior($MB-OK, $MB-ICONHAND)));
	    end if;

	  otherwise => 
	    return(DefWindowProc(window, message, uParam, lParam));
                        
	end select;

      $WM-DESTROY =>   // message: window being destroyed
	PostQuitMessage(0);

      otherwise =>           // Passes it on if unproccessed
	return(DefWindowProc(window, message, uParam, lParam));
      
    end select;
    return(0);
  end block;
end method main-window-function;
