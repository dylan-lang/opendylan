Module:    with-splash-screen
Synopsis:  Launch another app, providing a splash screen until it's ready.
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// NOTE:
/// This program silently ignores errors and attempts to continue
/// safely.  There's not much point issuing a warning while trying to
/// show a splash-screen!

/// NOTE:
/// Much of the following code is adapted from the MLWorks runtime.
/// The functions 'paint-window' and 'show-window' here correspond to the
/// functions 'paint_splash_bitmap' and 'get_splash_bitmap' in
/// MLWrts!src:OS:Win32:window.c(trunk.78), but simplified because (a) we
/// have the bitmap linked in as a resource, rather than loaded manually


/// Win32 Initialization

define variable *hInst* // current instance
    :: <HINSTANCE> = null-handle(<HINSTANCE>);
define constant $szAppName :: <string> = TEXT("HDSplashScreen");
define constant $szTitle :: <string> = TEXT("Open Dylan");
define callback WndProc :: <WNDPROC> = main-window-function;

define method init-application (hInstance :: <HINSTANCE>) => (ok :: <boolean>)
  // Store instance handle in our global variable
  *hInst* := hInstance;

  // Register our window class
  let null-HINSTANCE :: <HINSTANCE> = null-handle(<HINSTANCE>);
  with-stack-structure (wc :: <PWNDCLASS>)
    wc.style-value := 0;
    wc.lpfnWndProc-value := WndProc;
    wc.cbClsExtra-value := 0;
    wc.cbWndExtra-value := 0;
    wc.hInstance-value := hInstance;
    wc.hIcon-value := LoadIcon(null-HINSTANCE, $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor(null-HINSTANCE, $IDC-ARROW);
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // Default color
    wc.lpszMenuName-value := $NULL-string;
    wc.lpszClassName-value := $szAppName;

    // Register the window class and return success/failure code.
    RegisterClass(wc) ~= 0
  end with-stack-structure
end method init-application;

// Create the main window, centred on the screen.
define method create-window
    (width :: <integer>, height :: <integer>)
 => (hWnd :: <HWND>)
  let screen-width  = GetSystemMetrics($SM-CXSCREEN);
  let screen-height = GetSystemMetrics($SM-CYSCREEN);
  CreateWindowEx
    ($WS-EX-TOOLWINDOW,
     $szAppName, $szTitle,
     %logior($WS-BORDER, $WS-POPUP),
     floor/(max(screen-width  - width,  0), 2), // horizontal position of window
     floor/(max(screen-height - height, 0), 2), // vertical position of window
     width,  height,
     $NULL-HWND, null-handle(<HMENU>), *hInst*, $NULL-VOID)
end method create-window;

/// Bitmap Handling

define variable *hBitmap* // splash-screen bitmap
    :: <HBITMAP> = null-handle(<HBITMAP>);

define variable *version* :: false-or(<byte-string>) = #f;

// Set up the global splash-screen bitmap and return its width and height
// (or two zeros, if the bitmap couldn't be found).
define function load-bitmap () => (width :: <integer>, height :: <integer>)
  let (width :: <integer>, height :: <integer>) = values(0, 0);
  *hBitmap*
    := pointer-cast
         (<HBITMAP>,
          LoadImage(*hInst*, "SPLASHSCREEN",
                    $IMAGE-BITMAP, 0, 0, $LR-DEFAULTCOLOR));
  unless (null-pointer?(*hBitmap*))
    with-stack-structure (bitmap :: <LPBITMAP>)
      unless (GetObject(*hBitmap*, size-of(<BITMAP>), bitmap) = 0)
        width  := bitmap.bmWidth-value;
        height := bitmap.bmHeight-value;
      end;
    end;
  end;
  values(width, height)
end function load-bitmap;

define function paint-window (hWnd :: <HWND>) => ()
  let free-closures = make(<deque>);
  block (exit)
    let hdc :: <HDC> = GetDC(hWnd);
    if (null-pointer?(hdc)) exit()
    else push(free-closures, method () ReleaseDC(hWnd, hdc) end);
    end;

    // Paint the bitmap (by selecting it into a different DC then copying
    // from that to the window's DC -- don't ask, that's just what you do!)
    let hdcMem :: <HDC> = CreateCompatibleDC(hdc); 
    if (null-pointer?(hdcMem)) exit()
    else push(free-closures, method () DeleteDC(hdcMem) end);
    end;
    when (null-pointer?(SelectObject(hdcMem, *hBitmap*))) exit() end;
    with-stack-structure (pBitmap :: <LPBITMAP>)
      when (GetObject(*hBitmap*, size-of(<BITMAP>), pBitmap) == 0) exit(); end;
      BitBlt(hdc, 0, 0, pBitmap.bmWidth-value, pBitmap.bmHeight-value,
             hdcMem, 1, 1, $SRCCOPY) | exit();
      when (*version*)
	with-stack-structure (lpsize :: <LPSIZE>)
	  let hFnt :: <HFONT> = pointer-cast(<HFONT>, GetStockObject($ANSI-VAR-FONT));
	  when (null-pointer?(hFnt)) exit() end;
	  let hOldFnt :: <HFONT> = pointer-cast(<HFONT>, SelectObject(hdc, hFnt));
	  if (null-pointer?(hOldFnt)) exit()
	  else push(free-closures, method() SelectObject(hdc, hOldFnt) end);
	  end;
	  let oldBkMode = SetBkMode(hdc, $TRANSPARENT);
	  if (oldBkMode = 0) exit()
	  else push(free-closures, method() SetBkMode(hdc, oldBkMode) end);
	  end;
	  with-stack-structure (lptm :: <LPTEXTMETRIC>)
	    with-c-string (face = make(<byte-string>, size: $LF-FACESIZE + 1, fill: '\0'))
	      when (GetTextFace(hdc, $LF-FACESIZE + 1, face) = 0) exit() end;
	      when (GetTextMetrics(hdc, lptm) = 0) exit() end;
	      let height
		= truncate/(5 * (lptm.tmHeight-value - lptm.tmInternalLeading-value), 2);
	      let width = truncate/(5 * lptm.tmAveCharWidth-value, 2);
	      let hNewFnt :: <HFONT>
		= CreateFont(height, width, 0, 0, 700 /* $FM-BOLD */, 0, 0, 0,
			     $ANSI-CHARSET, $OUT-DEFAULT-PRECIS, $CLIP-DEFAULT-PRECIS,
			     $PROOF-QUALITY, logior($VARIABLE-PITCH, $FF-SWISS), face);
	      if (null-pointer?(hNewFnt)) exit()
	      else 
		push(free-closures, method() DeleteObject(hNewFnt) end);
		SelectObject(hdc, hNewFnt);
	      end;
	      when (*version*)
		with-c-string (c-version = *version*)
		  let version-size = size(*version*);
		  if (GetTextExtentPoint32(hdc, c-version, version-size, lpsize) = 0) exit()
		  else
		    let x = pBitmap.bmWidth-value - lpsize.cx-value - 5;
		    let y = 5;
		    TextOut(hdc, x, y, c-version, version-size);
		  end;
		end;
	      end;
	    end;
	  end;
	end;
      end;
    end;
  end;
  // Tidy up
  for (free in free-closures)
    free();
  end;
end function paint-window;

define function show-window (hWnd :: <HWND>) => ()
  block (exit)
    // Clear the background
    with-stack-structure (pRect :: <LPRECT>)
      GetClientRect(hWnd, pRect) | exit();
      InvalidateRect(hWnd, pRect, #t) | exit();
    end;
    ShowWindow(hWnd, $SW-SHOW);
    UpdateWindow(hWnd) | exit();

    // Then paint
    paint-window(hWnd);
  end;
end function show-window;

define method main-window-function
    (hWnd :: <HWND>,
     message :: <integer>,
     uParam,
     lParam)
 => (value :: <integer>)
  select (message) 
    $WM-PAINT =>
      // Without Begin-/EndPaint there's too much redraw, causing cursor flicker.
      with-stack-structure (pPaintStruct :: <LPPAINTSTRUCT>)
        let hDC = BeginPaint(hWnd, pPaintStruct);
        ignore(hDC);
        paint-window(hWnd);
        EndPaint(hWnd, pPaintStruct);
      end;
      0;
    $WM-DESTROY =>
      PostQuitMessage(0);
      0;
    otherwise =>
      DefWindowProc(hWnd, message, uParam, lParam);
  end select;
end method main-window-function;

/// Splash Screen Handling

define function show-splash-screen () => (hWnd :: false-or(<HWND>))
  let hWnd :: false-or(<HWND>) = #f;
  let (width :: <integer>, height :: <integer>) = load-bitmap();

  // If the bitmap can't be found, don't show the splash screen.
  unless (null-pointer?(*hBitmap*))
    hWnd := create-window(width, height);
    unless (null-pointer?(hWnd))
      show-window(hWnd);
    end unless;
  end unless;
  hWnd
end function show-splash-screen;

define function splash-screen-event-loop (hWnd :: <HWND>) => ()
  block (exit)
    with-stack-structure (pMsg :: <PMSG>)
      //---*** This should test for GetMessage returning zero or minus-one,
      // but it's defined incorrectly in win32-user.
      while (GetMessage(pMsg, $NULL-HWND, 0, 0))
        TranslateMessage(pMsg);
        DispatchMessage(pMsg);
      end while;
      pMsg.wParam-value
    end with-stack-structure;
  end;
  DeleteObject(*hBitmap*);
end function splash-screen-event-loop;

define function hide-splash-screen (window :: <HWND>) => ()
  PostMessage(window, $WM-CLOSE, 0, 0);
end function hide-splash-screen;

/// Application Handling

define function do-launch-app
    (command-line :: <string>)
 => (handle :: false-or(<HANDLE>))
  let handle :: false-or(<HANDLE>) = #f;
  with-stack-structure (lpStartupInfo :: <LPSTARTUPINFO>)
    with-stack-structure (lpProcessInformation :: <LPPROCESS-INFORMATION>)
      // Initialise the StartupInfo struct:  We're not using any of the fields (i.e.,
      // we're setting the dwFlags slot to 0.  So, we'll simply clear the structure
      // and set the size field (cb) appropriately.
      clear-memory!(lpStartupInfo, size-of(<STARTUPINFO>));
      lpStartupInfo.cb-value := size-of(<STARTUPINFO>);
      // Create the process.
      let created? :: <boolean>
        = CreateProcess(null-pointer(<LPCSTR>),
                        command-line,
                        null-pointer(<LPSECURITY-ATTRIBUTES>),
                        null-pointer(<LPSECURITY-ATTRIBUTES>),
                        #f,
                        $DETACHED-PROCESS,
                        null-pointer(<LPVOID>),
                        null-pointer(<LPCTSTR>),
                        lpStartupInfo,
                        lpProcessInformation);
      // Grab the process handle (and close the thread handle), if it worked.
      when (created?)
        handle := lpProcessInformation.hProcess-value;
        CloseHandle(lpProcessInformation.hThread-value);
      end;
    end;
  end;
  handle
end function do-launch-app;

// Number of milliseconds to wait; $INFINITE means "don't time out".
define constant $timeout :: <machine-word> = as(<machine-word>, 60000);

define function wait-for-app (handle :: <HANDLE>)
  let result = WaitForInputIdle(handle, $timeout);
  CloseHandle(handle);
end function wait-for-app;

define function launch-app
    (command-line :: <string>, window :: false-or(<HWND>))
 => ()
  let handle = do-launch-app(command-line);
  when (handle)
    wait-for-app(handle);
  end;
  when (window)
    hide-splash-screen(window);
  end;
end function launch-app;

/// Main Entry Point

define inline function requote-if-needed (arg :: <byte-string>) => (arg :: <byte-string>)
  if (position(arg, ' ') | position(arg, '\t'))
    concatenate("\"", arg, "\"")
  else
    arg
  end
end function requote-if-needed;

define method main () => ()
  when (init-application(application-instance-handle()))
    initialize-dde-server();
    let args = application-arguments();
    let arg-count = size(args);
    let command-line = #f;
    let building-cl? :: <boolean> = #f;
    let i = 0;
    while (i < arg-count)
      if (building-cl?)
	command-line := concatenate(command-line, " ", requote-if-needed(args[i]))
      else
	let arg = args[i];
	if (arg = "/version" | arg = "/v")
	  i := i + 1;
	  when (i < arg-count)
	    *version* := args[i]
	  end
	else
	  command-line := requote-if-needed(arg);
	  building-cl? := #t
	end;
      end;
      i := i + 1;
    end;
    when (command-line)
      let window = show-splash-screen();
      make(<thread>, function: method () launch-app(command-line, window) end);
      when (window)
        splash-screen-event-loop(window);
      end;
    end;
    destroy-dde-server();
  end;
end method main;

begin
  main();
end;
