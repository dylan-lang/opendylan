Module:    win32-duim
Synopsis:  Win32 mirror implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 Mirrors

//---*** <HMenu> is a subclass of <HANDLE> not <HWnd>.  Is that right???
define constant <HWND/HMENU> = type-union(<HWND>, <HMENU>);

define open abstract class <win32-mirror> (<mirror>)
  slot mirror-sheet :: <sheet>,
    required-init-keyword: sheet:;
  slot %window-handle :: <HWND/HMENU>,
     required-init-keyword: handle:;
end class <win32-mirror>;

define sealed class <window-mirror> (<win32-mirror>)
  slot %region :: <bounding-box>,
    required-init-keyword: region:;
  slot %DC :: <HDC> = $null-hDC;
end class <window-mirror>;

//---*** Use an <equal-table> because C pointers don't compare with ==
define variable *mirror-handle-table* :: <table> = make(<equal-table>);

// Returns the handle of a container, or #f
define method sheet-parent-window
    (sheet :: <top-level-sheet>)
 => (parent :: false-or(<HWND/HMENU>))
  let container = sheet-container(sheet);
  if (container)
    select (container by instance?)
      <frame> => 
	let top-sheet = top-level-sheet(container);
	top-sheet & top-sheet.%window-handle;
      <sheet> => 
	container.%window-handle;
      <mirror> =>
	container.%window-handle;
      otherwise =>	// presumably an <HWND>...
	container;
    end
  end
end method sheet-parent-window;

// Returns the handle of the parent window, or #f
define method sheet-parent-window
    (sheet :: <sheet>)
 => (parent :: false-or(<HWND/HMENU>))
  let parent-mirror = sheet-parent-mirror(sheet);
  parent-mirror & parent-mirror.%window-handle
end method sheet-parent-window;

define function window-mirror
    (handle :: <HWND/HMENU>) => (mirror :: false-or(<win32-mirror>))
  element(*mirror-handle-table*, pointer-address(handle), default: #f)
end function window-mirror;

define method window-mirror-setter
    (mirror :: <win32-mirror>, handle :: <HWND/HMENU>) => (mirror :: <win32-mirror>)
  element(*mirror-handle-table*, pointer-address(handle)) := mirror
end method window-mirror-setter;

define method window-mirror-setter
    (mirror == #f, handle :: <HWND/HMENU>) => (mirror == #f)
  remove-key!(*mirror-handle-table*, pointer-address(handle));
  #f
end method window-mirror-setter;

define method initialize (mirror :: <win32-mirror>, #key) => ()
  next-method();
  sheet-direct-mirror(mirror-sheet(mirror)) := mirror;
  let handle = mirror.%window-handle;
  if (handle)
    window-mirror(handle) := mirror
  end
end method initialize;

define method %window-handle 
    (sheet :: <mirrored-sheet-mixin>)
 => (handle :: <HWND/HMENU>)
  let mirror = sheet-direct-mirror(sheet);
  mirror & mirror.%window-handle
end method %window-handle;


// Names of "classes" (in Microsoft Windows terminology) for windows:
define constant $window-class-name = "DUIM-default";

define function register-window-classes () => ()
  //---*** Don't we want to register a class with $CS-OWNDC, too?
  with-stack-structure (wc :: <PWNDCLASS>)
    // Fill in window class structure with parameters that describe the
    // main window.
    wc.lpszClassName-value := $window-class-name;  // name of window class
    wc.style-value := logior($CS-HREDRAW, logior($CS-VREDRAW, $CS-DBLCLKS));
    wc.lpfnWndProc-value := WndProc;       // Window Procedure
    wc.cbClsExtra-value := 0;              // No per-class extra data.
    wc.cbWndExtra-value := 0;              // No per-window extra data.
    wc.hInstance-value := application-instance-handle(); // owner
    wc.hIcon-value := LoadIcon($null-hInstance, $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor($null-hInstance, $IDC-ARROW);
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // default
    wc.lpszMenuName-value := $null-string; // no menu yet
    if (zero?(RegisterClass(wc)))	   // register the window class
      report-error("RegisterClass")
    end
  end
end function register-window-classes;


/// Mirror creation and destruction

define method do-make-mirror
    (_port :: <win32-port>, sheet :: <sheet>) => (mirror :: <window-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  //---*** Use the OWNDC class if the sheet has a permanent medium?
  let hWnd :: <HWND> =
    CreateWindow($window-class-name,	// See RegisterClass call
		 $null-string,		// no title
		 $WS-CHILD,		// Window style
		 left,			// x position
		 top,			// y position
		 right - left,		// width
		 bottom - top,		// height
		 sheet-parent-window(sheet), // parent window
		 $null-hMenu,		// Use the window class menu
		 application-instance-handle(),
		 $NULL-VOID);		// We don't use any data in our WM_CREATE
  check-result("CreateWindow", hWnd);
  make(<window-mirror>,
       sheet: sheet, handle: hWnd,
       region: make-bounding-box(left, top, right, bottom))
end method do-make-mirror;

define method destroy-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let hwnd :: <HWND/HMENU> = mirror.%window-handle;
  check-result("DestroyWindow", DestroyWindow(hwnd));
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <win32-mirror>) => ()
  let hwnd :: <HWND/HMENU> = mirror.%window-handle;
  window-mirror(hwnd) := #f;
  mirror.%window-handle := $NULL-HWND
end method note-mirror-destroyed;


/// Mirror DC handling

define method get-DC 
    (mirror :: <window-mirror>) => (hDC :: <HDC>)
  let hDC :: <HDC> = mirror.%DC;
  if (null-handle?(hDC))
    hDC := GetDC(mirror.%window-handle);
    check-result("GetDC", hDC);
    mirror.%DC := hDC
  end;
  hDC
end method get-DC;

define method release-DC
    (mirror :: <window-mirror>) => ();
  let hDC :: <HDC> = mirror.%DC;
  unless (null-handle?(hDC))
    unless (zero?(ReleaseDC(mirror.%window-handle, hDC)))
      // For CS_OWNDC windows, 'ReleaseDC' is a no-op.  Invalidate
      // the drawing state only if we actually released the DC.
      mirror.%DC := $null-hDC;
      let medium = sheet-medium(mirror-sheet(mirror));
      if (medium)
	drawing-state-cached?(medium) := #f
      end
    end
  end
end method release-DC;


/// Mirror manipulation

define method map-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let hWnd :: <HWND> = mirror.%window-handle;
  ShowWindow(hWnd, $SW-SHOWNORMAL);	// no status code for this
  // Sends WM_PAINT message and returns status
  check-result("UpdateWindow", UpdateWindow(hWnd))
end method map-mirror;

define method unmap-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let hWnd :: <HWND> = mirror.%window-handle;
  ShowWindow(hWnd, $SW-HIDE)		// no status code for this
end method unmap-mirror;

define method raise-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let hWnd :: <HWND> = mirror.%window-handle;
  check-result("BringWindowToTop", BringWindowToTop(hWnd))
end method raise-mirror;

define method lower-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let hWnd :: <HWND> = mirror.%window-handle;
  SetWindowPos(hWnd, $HWND-BOTTOM)
end method lower-mirror;

define method mirror-visible? 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (visible? :: <boolean>)
  let hWnd :: <HWND> = mirror.%window-handle;
  IsWindowVisible(hWnd)
end method mirror-visible?;


define method mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <win32-mirror>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  values(0, 0, 100, 100)	//--- kludge city
end method mirror-edges;

define method mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

define method set-mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>,
     left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => ()
  let hWnd :: <HWND> = mirror.%window-handle;
  mirror.%region := make-bounding-box(left, top, right, bottom);
  // Just change the size and position without doing anything else
  let flags :: <integer> = logior($SWP-NOACTIVATE, $SWP-NOZORDER);
  // For the top-level window, just set the size, not the position
  if (instance?(sheet, <top-level-sheet>))
    flags := logior(flags, $SWP-NOMOVE);
  end;
  // Note that using 'MoveWindow(hWnd, left, top, right, bottom, #f)'
  // would send a WM_SIZE message, which would cause redundant overhead
  // at best, or infinite recursion at worst.
  check-result("SetWindowPos",
	       SetWindowPos(hWnd, $NULL-HWND,
			    left, top, right - left, bottom - top, flags))
end method set-mirror-edges;

define method sheet-screen-position
    (_port :: <win32-port>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let ancestor = sheet-mirrored-ancestor(sheet);
  let mirror = sheet-direct-mirror(ancestor);
  let transform = sheet-delta-transform(sheet, ancestor);
  // Get the position of the sheet in its mirrored parent's coordinates
  let (x, y) = transform-position(transform, 0, 0);
  //---*** Multiple values don't make it out of with-stack-structure in Webster-8000
  //---*** so we've hacked around it for now.
  let rx = 0;
  let ry = 0;
  with-stack-structure (rect :: <PRECT>)
    GetWindowRect(mirror.%window-handle, rect);
    // This is now the position of the sheet on the screen
    rx := x + rect.left-value;
    ry := y + rect.top-value
  end;
  values(rx, ry)
end method sheet-screen-position;
