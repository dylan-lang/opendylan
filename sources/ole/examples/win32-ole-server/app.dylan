Module:    sample-OLE-server
Synopsis:  application class and accessors.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// ===  application object  ===

define class <simple-server-app> (<object>)
  constant slot app-module-handle :: <HMODULE>,
    required-init-keyword: module-handle:;
  slot app-ole-object :: false-or(<simple-server>),
    required-init-keyword: ole-object:;
  slot app-frame :: <HWND> = $null-hwnd;   // main window handle
  slot app-doc-window :: <HWND> = $null-hwnd; // document window handle
end class <simple-server-app>;

define method app-color-menu (app :: <simple-server-app>) => menu :: <HMENU>;
  GetSubMenu(GetMenu(app.app-frame), 1)
end;

// Returns true if loaded in-process, i.e. we're in a DLL, not the EXE
define method app-in-process? (app :: <simple-server-app>) => in? :: <boolean>;
  app.app-module-handle ~= application-instance-handle()
end;

define variable *app-instances* = #();

define method window-app (w :: <HWND>) => app :: false-or(<simple-server-app>);
  any?(method (app :: <simple-server-app>)
	 (app.app-frame = w | app.app-doc-window = w) & app
       end,
       *app-instances*)
end;

define method initialize (app :: <simple-server-app>, #key)
  next-method();
  let first? = empty?(*app-instances*);
  *app-instances* := add!(*app-instances*, app);
  if (first?)
    register-window-classes(app);
  end;
  app.app-frame := create-window(app.app-module-handle,
				 $main-window-class,
				 title: "Simple OLE Server in Dylan",
				 width: 300, height: 250);
  app.app-doc-window := create-doc-window(app);
end;

define method terminate-frame (app :: <simple-server-app>)
  let hwnd = app.app-frame;
  unless (null-handle?(hwnd))
    app.app-frame := $NULL-HWND;
    DestroyWindow(hwnd);
    *app-instances* := remove!(*app-instances*, app);
    when (empty?(*app-instances*))
      unregister-window-classes(app);
    end;
  end;
end terminate-frame;


// Application has disconnected from server object.
define method server-disconnected (app :: <simple-server-app>)
  // Tell the frame to close
  PostMessage(app.app-frame, $WM-SYSCOMMAND, $SC-CLOSE, 0);
end server-disconnected;


define constant $main-window-class :: <LPTSTR> = as(<LPTSTR>,"DWoSvrFrame");
define constant $doc-window-class :: <LPTSTR> = as(<LPTSTR>,"DWoSvrDoc");

define method register-window-classes (app :: <simple-server-app>)
  let hinstance :: <HMODULE> = app.app-module-handle;
  register-window-class(hinstance,
			MainWndProc, $main-window-class,
			icon: LoadIcon(hinstance, "SimpSvr"),
			menu: "SimpSvrMENU");
  register-window-class(hinstance, DocWndProc, $doc-window-class);
end method register-window-classes;

define method unregister-window-classes (app :: <simple-server-app>)
  check-win32-result("UnregisterClass",
     UnregisterClass($main-window-class, app.app-module-handle));
  check-win32-result("UnregisterClass",
     UnregisterClass($doc-window-class, app.app-module-handle));
end unregister-window-classes;

// Handle the processing of $WM-COMMAND.
define constant $IDM-ABOUT = 100;

define constant $IDM-EXIT = 106;
define constant $IDM-RED = 113;
define constant $IDM-GREEN = 114;
define constant $IDM-BLUE = 115;
define constant $IDM-ROTATE = 116;

define method command-handler (app :: <simple-server-app>,
			       hWnd :: <HWND>,
			       wParam :: <ffi-integer>,
			       lParam :: <ffi-integer>)
 => value :: <integer>;

   select (LOWORD(wParam))

     $IDM-ABOUT =>      // bring up the About box
       DialogBox(app.app-module-handle, // current instance
		 "AboutBox",	        // resource to use
		 app.app-frame,	        // parent handle
		 About);		// About() instance address
       0;

     $IDM-EXIT =>      // exit the application
       SendMessage(hWnd, $WM-SYSCOMMAND, $SC-CLOSE, 0);

     $IDM-RED => 
       part-set-color(app.app-ole-object, red: 128);
       0;

     $IDM-GREEN => 
       part-set-color(app.app-ole-object, green: 128);
       0;

     $IDM-BLUE => 
       part-set-color(app.app-ole-object, blue: 128);
       0;

     $IDM-ROTATE => 
       part-rotate-color(app.app-ole-object);
       0;

     otherwise => 
       DefWindowProc(hWnd, $WM-COMMAND, wParam, lParam);
                   
   end select;
end command-handler;

// Set the Container's status bar text
//      Even though there is no status line in this sample, this
//      method must be called on $WM-MENUSELECT to clear the last
//      message in the status line.

define method set-status-text (app :: <simple-server-app>) => ();
  OLE-util-set-status-text(app.app-ole-object, #f)
end method set-status-text;

// Show the Application Window

define method show-app-frame (app :: <simple-server-app>,
			      nCmdShow :: <integer>)
  let hwnd = app.app-frame;
  ShowWindow(hwnd, nCmdShow);
  UpdateWindow(hwnd);
  show-doc-window(app);
end show-app-frame;


// Create the document window

define method create-doc-window (app :: <simple-server-app>)
 => (w :: <HWND>);
  Output-Debug-String("create the document window");
  with-stack-structure (lpRect :: <LPRECT>)
    let hWnd = app.app-frame;
    GetClientRect(hWnd, lpRect);
    create-window(app.app-module-handle,
                  $doc-window-class,
                  style: %logior($WS-CHILD,$WS-CLIPSIBLINGS),
                  x: lpRect.left-value,
                  y: lpRect.top-value,
                  width: lpRect.right-value,
                  height: lpRect.bottom-value,
                  parent: hWnd);
  end with-stack-structure;
end create-doc-window;

// Resize the document

define method resize-doc-window (app :: <simple-server-app>)
 => value :: <boolean>;
  with-stack-structure (lpRect :: <LPRECT>)
    GetClientRect(app.app-frame, lpRect);
    MoveWindow(app.app-doc-window,
	       lpRect.left-value,
	       lpRect.top-value,
	       lpRect.right-value,
	       lpRect.bottom-value,
	       #t);
  end with-stack-structure
end resize-doc-window;


// Paint the Document

define method paint-doc (app :: <simple-server-app>, hDC :: <HDC>)
  let obj = app.app-ole-object;
  // if the object hasn't been created yet, then don't draw
  when (obj)
    OLE-part-draw-metafile(obj, hDC);
  end when;
end method paint-doc;


// Show the Document Window

define method show-doc-window (app :: <simple-server-app>)
  let window = app.app-doc-window;
  ShowWindow(window, $SW-SHOWNORMAL);  // Show the window
  UpdateWindow(window);                // Sends $WM-PAINT message
end method show-doc-window;

define method doc-changed (app :: <simple-server-app>)
  InvalidateRect(app.app-doc-window, $NULL-RECT, #t); // force window repaint
end;


define function register-window-class (hinstance :: <HMODULE>,
                                       proc :: <c-function-pointer>,
                                       class :: <LPTSTR>,
                                       #key icon = null-pointer(<HICON>),
                                            menu = null-pointer(<LPCSTR>))

  with-stack-structure (wc :: <PWNDCLASS>)
    wc.style-value := 0;                 // Class style(s).
    wc.lpfnWndProc-value := proc; // Function to retrieve messages for
					 //   windows of this class.
    wc.cbClsExtra-value := 0;            // No per-class extra data.
    wc.cbWndExtra-value := 0;            // No per-window extra data.
    wc.hInstance-value := hinstance;  // Application that owns the class.
    wc.hIcon-value := icon;
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := GetStockObject($WHITE-BRUSH);
    wc.lpszMenuName-value := menu;   // Name of menu resource in .RC file.
    wc.lpszClassName-value := class;  // Name used in call to CreateWindow.
    let atom = RegisterClass(wc);
    if (zero?(atom))
      report-win32-error("RegisterClass");
    end if;
    atom
  end with-stack-structure;
end register-window-class;

define function create-window (hinstance :: <HMODULE>,
			       class :: <string>,
			       #key title :: <string> = $null-string,
			            x :: <ffi-integer> = $CW-USEDEFAULT,
			            y :: <ffi-integer> = $CW-USEDEFAULT,
			            width :: <ffi-integer> = $CW-USEDEFAULT,
			            height :: <ffi-integer> = $CW-USEDEFAULT,
			            style :: <ffi-integer> = $WS-OVERLAPPEDWINDOW,
                                    parent :: <HWND> = $NULL-HWND)
 => (hWnd :: <HWND>)
  let w = CreateWindow(class, title, style, x, y, width, height,
		       parent,
		       $NULL-HMENU,
		       hinstance,
		       $NULL-VOID);
  check-win32-result("CreateWindow", w);
  w
end create-window;

