Module:    sample-automation-server
Synopsis:  win32 window management.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $window-class = "DWautoSvWClass";

define method application-init-window () => (hWnd :: false-or(<HWND>))
  let hinst :: <HINSTANCE> = application-instance-handle();
  init-application(hinst) & init-instance(hinst, application-show-window())
end;

define method init-application (hinst :: <HINSTANCE>) => value :: <boolean>;

  with-stack-structure (wc :: <PWNDCLASS>)

    wc.style-value := 0;
    wc.lpfnWndProc-value := WndProc;
    wc.cbClsExtra-value := 0;
    wc.cbWndExtra-value := 0;
    wc.hInstance-value := hinst;
    wc.hIcon-value := LoadIcon($NULL-HINSTANCE, $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // Default color
    wc.lpszMenuName-value := $NULL-string;
    wc.lpszClassName-value := $window-class;

    if (zero?(RegisterClass(wc)))
      report-win32-error("RegisterClass");
      #f
    else
      #t
    end if
  end with-stack-structure
end method init-application;

define method init-instance (hinst :: <HINSTANCE>, nCmdShow :: <integer>)
	=>  hWnd :: false-or(<HWND>);

  let hWnd :: <HWND> =   // Main window handle.
    CreateWindow($window-class,
		 "Dylan OLE Automation Server",
		 $WS-OVERLAPPEDWINDOW,
		 $CW-USEDEFAULT, $CW-USEDEFAULT, // position
		 400, 300,	// size
		 $NULL-HWND,
		 $NULL-HMENU,
		 hinst,
		 $NULL-VOID);
  if (null-handle?(hWnd))
    report-win32-error("CreateWindow");
    #f
  else
    ShowWindow(hWnd, nCmdShow);
    if (UpdateWindow(hWnd))
      hWnd
    else
      report-win32-error("UpdateWindow");
      #f
    end;
  end if;
end method init-instance;

define method window-function (hwnd :: <HWND>, message, wParam, lParam)
	=> value :: <integer>;

  select (message)

    $WM-CLOSE => 
      DestroyWindow(hwnd);
      0;
      
    $WM-DESTROY => 
      PostQuitMessage(0);
      0;

    otherwise =>
      DefWindowProc(hWnd, message, wParam, lParam)
  end select
end method window-function;

define callback WndProc :: <WNDPROC> = window-function;
