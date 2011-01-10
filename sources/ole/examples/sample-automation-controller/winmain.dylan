Module:    sample-automation-controller
Synopsis:  This file is the main program.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $server-class-id = "{ef462960-bb53-11cf-89f8-02070119f639}";

define variable *server* :: <LPDISPATCH> = $NULL-interface;

define constant $window-class = "DWautoCtlWClass";

define constant $window-title = "Dylan OLE Automation controller";

define method init-application (hinst :: <HINSTANCE>) => value :: <boolean>;

  with-stack-structure (wc :: <PWNDCLASS>)
    wc.style-value := 0;
    wc.lpfnWndProc-value := WndProc;
    wc.cbClsExtra-value := 0;
    wc.cbWndExtra-value := 0;
    wc.hInstance-value := hinst;
    wc.hIcon-value := LoadIcon(null-handle(<HINSTANCE>), $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor(null-handle(<HINSTANCE>), $IDC-ARROW);
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
	=> value :: <boolean>;

  let hWnd :: <HWND> =   // Main window handle.
    CreateWindow($window-class,
		 $window-title,
		 $WS-OVERLAPPEDWINDOW,
		 $CW-USEDEFAULT, $CW-USEDEFAULT, // position
		 300, 200,	// size
		 $NULL-HWND,
		 null-handle(<HMENU>),
		 hinst,
		 $NULL-VOID);
  if (null-handle?(hWnd))
    report-win32-error("CreateWindow");
    #f
  else
    ShowWindow(hWnd, nCmdShow);
    make-buttons(hWnd);
    if (~UpdateWindow(hWnd))
      report-win32-error("UpdateWindow");
      #f
    else
      #t
    end;
  end if
end method init-instance;


define method window-function (hwnd :: <HWND>,
			       message,
			       wParam,
			       lParam)
	=> value :: <integer>;

  select (message)

    $WM-COMMAND => 
      if (HIWORD(wParam) = $BN-CLICKED)
	process-button(hWnd, *server*, LOWORD(wParam));
	0
      else
	DefWindowProc(hWnd, message, wParam, lParam)
      end if;

    $WM-CLOSE => 
      DestroyWindow(hWnd);
      0;

    $WM-DESTROY => 
      PostQuitMessage(0);
      0;

    otherwise =>
      DefWindowProc(hWnd, message, wParam, lParam)
  end select
end method window-function;

define callback WndProc :: <WNDPROC> = window-function;


// Replace "Not registered" errors with a more specific error message...
define function handle-unregistered-class (c :: <ole-error>, next-handler)
  if (c.ole-error-status == $REGDB-E-CLASSNOTREG)
    error("CoCreateInstance: class not registered.\n\n"
	    "The \"sample-automation-server\" program needs to be\n"
	    "started or registered before running this program.")
  else
    next-handler()
  end
end;

define method main-program () => ();

  let hinst :: <HINSTANCE> = application-instance-handle();

  if (init-application(hinst))
    let handler (<ole-error>) = handle-unregistered-class;
    with-dispatch-interface (server = $server-class-id)
      // Store the server in a global variable to make it available
      // in the window callback function.
      *server* := server;
      if (init-instance(hinst, application-show-window()))
	let pmsg :: <PMSG> = make(<PMSG>);
	while (GetMessage(pmsg, $NULL-HWND, 0, 0))
	  TranslateMessage(pmsg);
	  DispatchMessage(pmsg);
	end while;
      end if;
    end with-dispatch-interface;
  end if;
end method main-program;

// Run the main program.
main-program();
