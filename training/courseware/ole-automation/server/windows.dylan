Module:    select-viewer-server
Synopsis:  window management for sample automation server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $window-class = "HDSelectViewerWClass";

define constant $continue-message = "Try to continue anyway";

define method report-last-error(name :: <string>) => ();
  cerror($continue-message, "%s error %d", name, GetLastError());
  values()
end;


define method InitApplication(hinst :: <HINSTANCE>) => value :: <boolean>;

  with-stack-structure( wc :: <PWNDCLASS> )

    wc.style-value := 0;
    wc.lpfnWndProc-value := WndProc;
    wc.cbClsExtra-value := 0;
    wc.cbWndExtra-value := 0;
    wc.hInstance-value := hinst;
    wc.hIcon-value := LoadIcon( $NULL-HINSTANCE, $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // Default color
    wc.lpszMenuName-value := $NULL-string;
    wc.lpszClassName-value := $window-class;

    if ( zero?(RegisterClass(wc)) )
      report-last-error("RegisterClass");
      #f
    else
      #t
    end if
  end with-stack-structure
end method InitApplication;

define method InitInstance(hinst :: <HINSTANCE>, nCmdShow :: <integer>)
	=>  hWnd :: <HWND>;

  let hWnd :: <HWND> =   // Main window handle.
    CreateWindow($window-class,
		 $window-title,
		 $WS-OVERLAPPEDWINDOW,
		 $CW-USEDEFAULT, $CW-USEDEFAULT, // position
		 400, 300,	// size
		 $NULL-HWND,
		 $NULL-HMENU,
		 hinst,
		 $NULL-VOID);
  if ( null-handle?(hWnd) )
    report-last-error("CreateWindow");
  else
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd) | report-last-error("UpdateWindow")
  end if;
  hWnd
end method InitInstance;

define method window-function(hwnd :: <HWND>,
			      message :: <integer>,
			      wParam, lParam)
	=> value :: <integer>;

  select ( message )

    $WM-CLOSE => 
      DestroyWindow(hwnd);
      0;
      
    $WM-DESTROY => 
      quit-server();
      0;

    otherwise =>
      DefWindowProc(hWnd, message, wParam, lParam)
  end select
end method window-function;

define callback WndProc :: <WNDPROC> = window-function;

