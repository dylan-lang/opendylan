Module:    win32-invisible-control
Synopsis:  Window management.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $main-window-class :: <LPTSTR> = TEXT("HDInvCtlFrame");
define constant $doc-window-class  :: <LPTSTR> = TEXT("HDInvCtlDoc");

 // size of design-time image, in pixels
define constant $image-width  = 88;
define constant $image-height = 32;

define function register-window-classes (hInstance :: <HINSTANCE>) => ();

  with-stack-structure( wc :: <PWNDCLASS> )
	
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
    wc.lpszMenuName-value := $NULL-string;
    wc.lpszClassName-value := $main-window-class;  // used for CreateWindow.

    register-class(wc);    // Register the main window class

    wc.style-value := 0;		 // Class style(s).
    wc.lpfnWndProc-value := DocWndProc;  // Function to retrieve messages for
					 //   windows of this class.
    wc.cbClsExtra-value := 0;            // No per-class extra data.
    wc.cbWndExtra-value := 0;            // No per-window extra data.
    wc.hInstance-value := hInstance;     // Application that owns the class.
    wc.hIcon-value := null-pointer(<HICON>);
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := GetStockObject($WHITE-BRUSH);
    wc.lpszMenuName-value := $NULL-string;
    wc.lpszClassName-value := $doc-window-class; // Name used in CreateWindow.

    register-class(wc);     // Register the document window class
 end with-stack-structure;
 values()
end register-window-classes;

define function register-class ( wc :: <PWNDCLASS> );
  let atom = RegisterClass(wc);
  if ( zero?(atom) )
    report-win32-error("RegisterClass");
  end if;
  atom
end;

define function unregister-window-classes (hInstance :: <HINSTANCE>) => ();
  check-win32-result("UnregisterClass",
     UnregisterClass($main-window-class, hInstance));
  check-win32-result("UnregisterClass",
     UnregisterClass($doc-window-class, hInstance));
end unregister-window-classes;

define method create-windows(hInstance :: <HINSTANCE>)
	=> ( main-window :: <HWND>, document-window :: <HWND> );

  // Create the application frame window
  let main-window =
    CreateWindow($main-window-class, // class name
		 $NULL-string,	     // title (not needed since won't be seen)
		 $WS-OVERLAPPEDWINDOW, // style
		 $CW-USEDEFAULT, // x position
		 $CW-USEDEFAULT, // y position
		 100,		 // width (arbitrary)
		 100,		 // height (arbitrary)
		 $NULL-HWND,  // no parent
		 $NULL-HMENU, // no menu bar
		 hInstance, // module handle
		 $NULL-VOID);
  check-win32-result("CreateWindow", main-window);

  // Create the document window
  let document-window =
    CreateWindow($doc-window-class, // class name
		 $NULL-string,	// title not applicable
		 %logior($WS-CHILD,$WS-CLIPSIBLINGS), // style
		 0,		// x position
		 0,		// y position
		 $image-width,	// width
		 $image-height,	// height
		 main-window,	// parent
		 $NULL-HMENU,
		 hInstance,
		 $NULL-VOID);
  check-win32-result("CreateWindow", document-window);
  values( main-window, document-window )
end method create-windows;

// Process messages for the frame window

define method main-wnd-proc (hWnd :: <HWND>, message :: <integer>,
			     wParam, lParam)
			=> value :: <integer>;
  select ( message )

    $WM-DESTROY =>
      if ( *the-application* )
        *the-application*.app-window := $NULL-HWND;
      end if;
      0;

    otherwise =>		// passes it on if unprocessed
      DefWindowProc(hWnd, message, wParam, lParam);
  end select
end method main-wnd-proc;

define callback MainWndProc :: <WNDPROC> = main-wnd-proc;


// Process document window messages

define method Doc-Wnd-Proc(hWnd :: <HWND>, message :: <integer>,
			   wParam, lParam)
		=> value :: <integer>;

  select ( message ) 

    $WM-PAINT => 
      // unless during initial creation of window
      unless ( null-handle?(*the-application*.doc-window) )
	with-stack-structure( ps :: <PPAINTSTRUCT> )
	  let hDC :: <HDC> = BeginPaint(hWnd, ps);
	  draw-control(*the-application*, hDC, $NULL-RECT); 
	  EndPaint(hWnd, ps);
	end with-stack-structure;
      end unless;
      0;

    $WM-DESTROY =>
      if ( *the-application* )
        *the-application*.doc-window := $NULL-HWND;
      end if;
      0;

    otherwise =>	// Passes it on if unprocessed
      DefWindowProc(hWnd, message, wParam, lParam);
  end select
end method Doc-Wnd-Proc;

define callback DocWndProc :: <WNDPROC> = Doc-Wnd-Proc;

define constant $caption-message :: <string> = "Dylan OCX";

// Draw an image to represent the control at design time.
define function draw-image(hDC :: <HDC>,
			   x-org :: <integer>, y-org :: <integer>,
			   x-size :: <integer>, y-size :: <integer>)
 => ();

  // draw a rectangle
  begin
    // background color
    let hBrush = GetStockObject($LTGRAY-BRUSH);
    let hOldBrush = SelectObject( hDC, hBrush );

    // border -- black, 2 pixels wide
    let hPen = CreatePen($PS-INSIDEFRAME, 2, RGB(0, 0, 0));
    let hOldPen = SelectObject( hDC, hPen );

    // draw the rectangle
    Rectangle(hDC, x-org, y-org, x-size, y-size);

    // restore the pen
    SelectObject(hDC, hOldPen);
    DeleteObject(hPen);

    // restore the old brush
    SelectObject(hDC, hOldBrush);
  end;

  // write a label in the rectangle
  begin
    let old-color = SetTextColor(hDC, RGB(0,0,0)); // black
    let old-mode = SetBkMode(hDC, $TRANSPARENT);
    let message = $caption-message;
    TextOut(hDC, x-org + 10,
	    y-org + truncate/(y-size,2) - 8,
	    message, size(message));
    SetTextColor(hDC, old-color);
    SetBkMode(hDC, old-mode);
  end;

  values();
end draw-image;

