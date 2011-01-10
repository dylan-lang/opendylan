Module:    sample-OLE-container
Synopsis:  window handling for OLE container example.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// command codes for menu items
define constant $IDM-ABOUT = 100;
define constant $IDM-EXIT = 106;
define constant $IDM-INSERTOBJECT = 111;
define constant $IDM-NEW = 112;
define constant $IDM-VERB0 = 1000;

// window class names
define constant $main-window-class :: <LPTSTR> = TEXT("SimpCntrAppWClass");
define constant $doc-window-class :: <LPTSTR> = TEXT("SimpCntrDocWClass");


// ====  program initialization  ====


define method init-application(app :: <sample-container-app>,
			       hInstance :: <HINSTANCE>)
	=> ok :: <boolean>;

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
    wc.lpszMenuName-value := TEXT("SIMPLEMENU"); // menu resource in .RC file.
    wc.lpszClassName-value := $main-window-class; // used in CreateWindow call

    // Register the window class

    if ( zero?(RegisterClass(wc)) )
      report-win32-error("RegisterClass");
    end if;

    // Not set the parameters for the document window.

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
    wc.lpszClassName-value := $doc-window-class; // used in CreateWindow call.

    // Register the window class

    if ( zero?(RegisterClass(wc)) )
      report-win32-error("RegisterClass");
    end if;
  end with-stack-structure;
end method init-application;


define constant app-title = "Simple OLE In-Place Container in Dylan";

define method init-instance(app :: <sample-container-app>,
			    hInstance :: <HINSTANCE>,
			    nCmdShow :: <integer>) => ()
  app.app-instance-handle := hInstance;

  // Create the "application" windows
  app.container-frame-window :=
    CreateWindow($main-window-class,
		 app-title, // title
		 logior($WS-OVERLAPPEDWINDOW, $WS-CLIPCHILDREN), // style
		 $CW-USEDEFAULT,	// x position
		 $CW-USEDEFAULT,	// y position
		 400,			// width
		 300,			// height
		 $NULL-HWND,		// no parent
		 null-pointer(<HMENU>),
		 hInstance,
		 $NULL-VOID);

  if ( null-handle?(app.container-frame-window) )
    report-win32-error("CreateWindow");
  end if;

  ShowWindow(app.container-frame-window, nCmdShow);
  UpdateWindow(app.container-frame-window);

end method init-instance;


// ====  window message handler functions  ====

define variable *sample-app* :: false-or(<sample-container-app>) = #f;

// Processes messages for the frame window
define method main-wnd-proc(hWnd :: <HWND>, message :: <integer>,
			    wParam, lParam)
	=> value :: <integer>;

  block(return)

    select ( message )
	        
      $WM-COMMAND =>            // message: command from application menu
	return(window-command-handler(*sample-app*, hWnd, message,
				      wParam, lParam));

      $WM-CREATE => 
	return(create-document(*sample-app*, hWnd));

      $WM-DESTROY =>                   // message: window being destroyed
	// we currently have only 1 document
	close-document(*sample-app*.app-document, #t);
	// Otherwise should use:
	// container-destroy-documents(*sample-app*);
	PostQuitMessage(0);

      $WM-INITMENUPOPUP => 
	// is this the edit menu?
	if ( LOWORD(lParam) = 1 )
	  // Adds the object's verbs to the edit menu, if not done already.
	  container-add-verbs(*sample-app*.contained-object,
			      *sample-app*.app-edit-menu, $IDM-VERB0);
	  0
	end if;

      // this code is needed for 256 color objects to work properly.
      $WM-QUERYNEWPALETTE => 
	unless ( *sample-app*.app-activated? )
	  return(0);
	end unless;
	return(container-query-new-palette(*sample-app*));

      $WM-PALETTECHANGED => 
	if ( container-palette-changed(*sample-app*, hWnd, wParam, lParam) )
	  return(0);
	end if;
	  
      $WM-ACTIVATEAPP =>
	let active? :: <boolean> = ~ zero?(wParam);
	*sample-app*.app-activated? := active?;
	container-activate-application(*sample-app*, active?);

      $WM-SIZE => 
	return(window-size-handler(*sample-app*, hWnd, message,
				   wParam, lParam));

      otherwise =>                           // Passes it on if unproccessed
	return(DefWindowProc(hWnd, message, wParam, lParam));
	        
    end select;
    return(0);
  end block;
end method main-wnd-proc;

define callback MainWndProc :: <WNDPROC> = main-wnd-proc;

// Processes dialog box messages
define method about-proc(hDlg :: <HWND>, message :: <integer>,
			 wParam, lParam)
 => value :: <boolean>;

  select ( message ) 
    $WM-INITDIALOG =>			/* message: initialize dialog box */ 
      #t;

    $WM-COMMAND =>			   /* message: received a command */ 
      let button :: <integer> = LOWORD(wParam);
      if ( button = $IDOK	   /* "OK" box selected?	  */
	    |	 button = $IDCANCEL) /* System menu close command? */ 
	EndDialog(hDlg, 1);	      /* Exits the dialog box	     */ 
	#t
      end if;
    otherwise =>
      #f		     /* Didn't process a message    */ 
  end select
end method about-proc;

define callback About :: <DLGPROC> = about-proc;

// Processes document window messages
define method Doc-Wnd-Proc(hWnd :: <HWND>, message :: <integer>,
			   wParam, lParam)
 => value :: <integer>;

  block(return)
    select ( message ) 
      $WM-PAINT => 
	begin
	  let ps :: <LPPAINTSTRUCT> = make(<LPPAINTSTRUCT>);
	  let hDC :: <HDC> = BeginPaint(hWnd, ps);

	  if ( *sample-app* )
	    paint-application(*sample-app*, hDC);
	  end if;

	  EndPaint(hWnd, ps);
	  destroy(ps);
	end;

      $WM-LBUTTONDBLCLK => 
	begin 
	  let pt :: <LPPOINT> = make(<LPPOINT>);
	  let ( x, y ) = LPARAM-TO-XY(lParam);
	  pt.x-value := x;
	  pt.y-value := y;
	  let contained = *sample-app*.app-document.contained-object;
	  if ( contained )
	    with-stack-structure ( rect :: <LPRECT> )
	      document-rectangle(contained, rect);
	      if ( PtInRect(rect, pt) )
		// Execute object's default verb
  		with-stack-structure ( pmsg :: <LPMSG> )
                  pmsg.hwnd-value := hWnd;
                  pmsg.message-value := message;
                  pmsg.wParam-value := wParam;
                  pmsg.lParam-value := lParam;
                  container-do-verb(contained, $OLEIVERB-PRIMARY, pmsg);
                end with-stack-structure;
	      end if;
	    end with-stack-structure;
	  end if;
	end;

      // no code is added to WM_LBUTTONDOWN for context sensitive help,
      // because this app does not do context sensitive help.
      $WM-LBUTTONDOWN => 

	let doc = *sample-app*.app-document;
	let contained = doc & doc.contained-object;
	if ( contained & contained.document-in-place-active? )
	  with-stack-structure ( pmsg :: <LPMSG> )
	    pmsg.hwnd-value := hWnd;
	    pmsg.message-value := message;
	    pmsg.wParam-value := wParam;
	    pmsg.lParam-value := lParam;
	    container-UI-deactivate(contained);
	    container-do-verb(contained, $OLEIVERB-HIDE, pmsg);
	  end with-stack-structure;
	end if;

      otherwise => // Passes it on if unproccessed
	return(DefWindowProc(hWnd, message, wParam, lParam));
    end select;
    return(0);
  end block
end method Doc-Wnd-Proc;

define callback DocWndProc :: <WNDPROC> = Doc-Wnd-Proc;



// ====  functions called to handle specific window messages  ====

// Purpose:
//
//      Handles the processing of WM_COMMAND.
//
// Parameters:
//
//      HWND hWnd       -   Handle to the application Window
//      UINT message    -   message (always WM_COMMAND)
//      WPARAM wParam   -   Same as passed to the WndProc
//      LPARAM lParam   -   Same as passed to the WndProc

define method window-command-handler(app :: <sample-container-app>,
				     hWnd :: <HWND>,
				     message /* :: <integer> */,
				     wParam /* :: <integer> */,
				     lParam /* :: <signed-long> */ )
	=> value :: <integer>;

  block(return)
	
    // context sensitive help...
    if ( container-context-help(app) )
      // if we provided help, we would do it here...
      MessageBox(hWnd, "Help", "Help", $MB-OK);
      return(0);
    end if;

    // see if the command is a verb selection
    let low-param :: <integer> = LOWORD(wParam);
    if ( low-param >= $IDM-VERB0 )
      container-do-verb(app.contained-object,
			low-param - $IDM-VERB0, null-pointer(<LPMSG>));
    else
      select (low-param) 
	// bring up the About box
	$IDM-ABOUT => 
	  DialogBox(app.app-instance-handle,          // current instance
		    TEXT("AboutBox"),      // resource to use
		    app.container-frame-window,        // parent handle
		    About);                // About() instance address
	    

	// bring up the InsertObject Dialog
	$IDM-INSERTOBJECT => 
	  insert-object(app, app.app-document);

	// exit the application
	$IDM-EXIT => 
	  SendMessage(hWnd, $WM-SYSCOMMAND, $SC-CLOSE, 0);

	$IDM-NEW => 
	  close-document(app.app-document, #f);
	  app.app-document := #f;
	  create-document(app, hWnd);

	otherwise => 
	  return(DefWindowProc(hWnd, message, wParam, lParam));
	                
      end select;   // end of switch
    end if;  // end of else
    return(0);
 end block
end method window-command-handler;

// Purpose:
//
//      Handles the WM_SIZE message
//
// Parameters:
//
//      HWND hWnd       -   Handle to the application Window
//      UINT message    -   message (always WM_SIZE)
//      WPARAM wParam   -   Same as passed to the WndProc
//      LPARAM lParam   -   Same as passed to the WndProc

define method window-size-handler(app :: <sample-container-app>,
				  hWnd /* :: <HWND> */,
				  message /* :: <integer> */,
				  wParam /* :: <integer> */,
				  lParam /* :: <signed-long> */ )
	=> (value :: <integer>);

  let doc = app.app-document;
  if ( doc ) 
    with-stack-structure ( rectangle :: <PRECT> )
      GetClientRect(app.container-frame-window, rectangle);
      let contained = doc.contained-object;
      // If there is an InPlace object, then call ResizeBorder on the object,
      // otherwise just move the document window.
      if (contained)
	container-size-changed(contained, rectangle);
      else
	MoveWindow(doc.doc-container-window,
		   rectangle.left-value, rectangle.top-value,
		   rectangle.right-value, rectangle.bottom-value, #t);
      end if;
    end with-stack-structure;
  end if;
  0
end method window-size-handler;


define method create-document(app :: <sample-container-app>,
			      main-window :: <HWND>)
	=> value :: <integer>;

  with-stack-structure ( rectangle :: <PRECT>)
    GetClientRect(main-window, rectangle);
    app.app-document := open-document(app, rectangle,
					  main-window, main-window);
  end with-stack-structure;
  0
end method create-document;


define method paint-application(app :: <sample-container-app>,
				hDC :: <HDC>) => ();

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
  
  let contained = app.contained-object;
  if ( contained )
    paint-contained-document(contained, hDC);
  end if;

  values()
end method paint-application;


