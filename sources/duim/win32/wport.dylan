Module:    win32-duim
Synopsis:  Win32 port implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Win32 constants

define constant $caret-width :: <integer> = 1;


/// Win32 ports

define sealed class <win32-port> (<basic-port>)
  sealed slot port-metrics :: <win32-metrics>;
  sealed slot %memory-hDC  :: <hDC> = $null-hDC;
  sealed slot %common-controls-initialized? = #f;
  sealed slot %os-name = #"Windows-NT";	// #"Windows-NT", #"Windows-95", or #"Windows-98"
  sealed slot %use-3d? = #t;		// #t when we're using the Win-95 look and feel
  sealed slot %display = #f;		// the Windows display device
  sealed slot %altgr-key? = #t;		// #t iff there's an AltGr key on this keyboard
  sealed slot %alt-key-state = #f;	// for keyboard event handling
  sealed slot %wm-char-state = 0;	// for keyboard event handling
  sealed slot %extended-key-state = #f;	// for keyboard event handling
  // The currently recorded focus
  sealed slot %focus :: false-or(<HWND>) = #f;
  // Cache to ensure WM_SETCURSOR handling is fast
  sealed slot %last-cursor  = #f;
  sealed slot %last-hCursor = #f;
  // Cache for image cursors
  sealed slot %cursor-cache :: <object-table> = make(<table>);
  // Keep track of the sheet that owns the Windows caret
  sealed slot %caret-sheet  :: false-or(<sheet>) = #f;
  // Maps each frame to the hot key identifier
  sealed constant slot %hot-keys :: <object-table> = make(<table>);
  keyword focus-policy: = #"click-to-select";
end class <win32-port>;

define sealed domain make (singleton(<win32-port>));
define sealed domain initialize (<win32-port>);

define sealed method initialize
    (_port :: <win32-port>, #key) => ()
  next-method();
  let hDC :: <hDC>
    = check-result("CreateCompatibleDC", CreateCompatibleDC($null-hDC));
  _port.%memory-hDC := hDC;
  with-stack-structure (info :: <LPOSVERSIONINFO>)
    info.dwOSVersionInfoSize-value := safe-size-of(<OSVERSIONINFO>);
    check-result("GetVersionEx", GetVersionEx(info));
    let (os-name, os-version)
      = select (info.dwPlatformId-value)
	  $VER-PLATFORM-WIN32S =>
	    values(#"Windows-95", 3);
	  $VER-PLATFORM-WIN32-WINDOWS =>
	    values(if (info.dwMinorVersion-value = 0) #"Windows-95" else #"Windows-98" end,
		   info.dwMajorVersion-value);
	  $VER-PLATFORM-WIN32-NT =>
	    values(#"Windows-NT", info.dwMajorVersion-value);
	end;
    _port.%os-name := os-name;
    _port.%use-3d? := (   os-name == #"Windows-NT"
		       |  os-name == #"Windows-98"
		       | (os-name == #"Windows-95" & os-version > 3))
  end;
  register-window-classes(_port);
  load-default-resources();
  port-metrics(_port) := make-win32-metrics();
  initialize-stock-objects(_port);
  install-default-palette(_port);
  initialize-graphics(_port);
  //--- We should correctly initialize the state of 'port.%altgr-key?',
  //--- but until then we'll leave it set to #t...
  initialize-keysym-translations(_port);
  read-key-translations(_port)
end method initialize;

register-port-class(#"win32", <win32-port>, default?: #t);

define sideways method class-for-make-port
    (type == #"win32", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<win32-port>, concatenate(initargs, #(event-processor-type:, #"n")))
end method class-for-make-port;

define sealed method port-type
    (_port :: <win32-port>) => (type :: <symbol>)
  #"win32"
end method port-type;

define sealed method port-name
    (_port :: <win32-port>) => (name :: false-or(<string>))
  #f
end method port-name;

define sealed method destroy-port
    (_port :: <win32-port>) => ()
  next-method();
  DeleteDC(_port.%memory-hDC);
  for (hCursor keyed-by cursor in _port.%cursor-cache)
    when (stencil?(cursor))
      // If we created it with CreateCursor, destroy it now
      //---*** DestroyCursor not imported yet!
      // DestroyCursor(hCursor)
    end
  end
end method destroy-port;


define function shutdown-win32-duim ()
  let ports :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-ports(method (_port)
	     when (instance?(_port, <win32-port>))
	       add!(ports, _port)
	     end
	   end method);
  do(destroy-port, ports);
  unregister-window-classes()
end function shutdown-win32-duim;


/// Port metrics

define method port-metrics
    (client) => (metrics :: <win32-metrics>)
  let _port = port(client);
  if (_port)
    port-metrics(_port)
  else
    error("Port metrics called for unattached %=", client)
  end
end method port-metrics;


/// Useful port metrics trampolines

define sealed inline method win32-dialog-x-units
    (client) => (x-units :: <integer>)
  win32-dialog-x-units(port-metrics(client))
end method win32-dialog-x-units;

define sealed inline method win32-dialog-y-units
    (client) => (y-units :: <integer>)
  win32-dialog-y-units(port-metrics(client))
end method win32-dialog-y-units;

define sealed inline method win32-dialog-x-pixels
    (client, x :: <integer>)
 => (units :: <integer>)
  floor/(x * win32-dialog-x-units(port-metrics(client)), 4)
end method win32-dialog-x-pixels;

define sealed inline method win32-dialog-y-pixels
    (client, y :: <integer>)
 => (units :: <integer>)
  floor/(y * win32-dialog-y-units(port-metrics(client)), 8)
end method win32-dialog-y-pixels;

define sealed inline method win32-dialog-units->pixels
    (client, x :: <integer>, y :: <integer>) => (x :: <integer>, y :: <integer>)
  let metrics = port-metrics(client);
  values(floor/(x * win32-dialog-x-units(metrics), 4),
	 floor/(y * win32-dialog-y-units(metrics), 8))
end method win32-dialog-units->pixels;

define sealed inline method win32-mouse-buttons
    (client) => (units :: <integer>)
  win32-mouse-buttons(port-metrics(client))
end method win32-mouse-buttons;

define sealed inline method win32-pixels-per-inch
    (client) => (units :: <integer>)
  win32-pixels-per-inch(port-metrics(client))
end method win32-pixels-per-inch;


/// Beeping, etc

define sealed method beep
    (_port :: <win32-port>) => ()
  MessageBeep($MB-OK)
end method beep;


/// Pointer position hacking

define sealed method do-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let (dx, dy) = sheet-screen-position(_port, sheet);
  with-stack-structure (point :: <LPPOINT>)
    GetCursorPos(point);
    values(point.x-value - dx, point.y-value - dy)
  end
end method do-pointer-position;

define sealed method do-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  with-stack-structure (point :: <LPPOINT>)
    GetCursorPos(point);
    values(point.x-value, point.y-value)
  end
end method do-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>) => ()
  let (dx, dy) = sheet-screen-position(_port, sheet);
  SetCursorPos(x + dx, y + dy)
end method do-set-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  SetCursorPos(x, y)
end method do-set-pointer-position;


// Returns the position of the mouse within the current window
define sealed method pointer-position-within-window
    (window :: <HWND>) => (x :: <integer>, y :: <integer>)
  with-stack-structure (point :: <LPPOINT>)
    GetCursorPos(point);
    ScreenToClient(window, point);
    values(point.x-value, point.y-value)
  end
end method pointer-position-within-window;


/// Pointer cursor hacking

define table $cursor-table :: <object-table>
  = { #"default"           => $IDC-ARROW,
      #"busy"              => $IDC-WAIT,
      #"vertical-scroll"   => $IDC-SIZENS,
      #"horizontal-scroll" => $IDC-SIZEWE,
      #"scroll-up"         => $IDC-ARROW,
      #"scroll-down"       => $IDC-ARROW,
      #"scroll-left"       => $IDC-ARROW,
      #"scroll-right"      => $IDC-ARROW,
      #"upper-left"        => $IDC-SIZENWSE,
      #"upper-right"       => $IDC-SIZENESW,
      #"lower-left"        => $IDC-SIZENESW,
      #"lower-right"       => $IDC-SIZENWSE,
      #"vertical-thumb"    => $IDC-SIZENS,
      #"horizontal-thumb"  => $IDC-SIZEWE,
      #"button"            => $IDC-ARROW,
      #"prompt"            => $IDC-ARROW,
      #"move"              => $IDC-CROSS,
      #"position"          => $IDC-CROSS,
      #"i-beam"            => $IDC-IBEAM,
      #"cross"             => $IDC-CROSS,
      #"starting"          => $IDC-APPSTARTING,
      #"hand"              => $IDC-SIZENS	/*---*** $IDC-HAND */ };

define sealed method do-set-pointer-cursor
    (_port :: <win32-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  let hCursor :: <HCURSOR> = realize-cursor(_port, cursor);
  SetCursor(hCursor);
  refresh-cursor(_port, cursor)
end method do-set-pointer-cursor;

define sealed method do-set-sheet-cursor
    (_port :: <win32-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  //--- What does the "sheet cursor" actually mean in Windows?
  let hCursor :: <HCURSOR> = realize-cursor(_port, cursor);
  SetCursor(hCursor);
  refresh-cursor(_port, cursor)
end method do-set-sheet-cursor;

define method refresh-cursor
    (_port :: <win32-port>, cursor :: <cursor>) => ()
  //--- How should we really do this? The Windows doc is very unclear,
  //--- it just says that the cursor won't be redrawn until the mouse
  //--- is moved or a 'system command' is called.
  ShowCursor(#f);
  ShowCursor(#t)
end method refresh-cursor;


define method grab-pointer
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (success? :: <boolean>)
  let mirror = sheet-mirror(sheet);
  let handle = mirror & window-handle(mirror);
  when (handle)
    let top-sheet  = top-level-sheet(sheet);
    let top-mirror = top-sheet  & sheet-direct-mirror(top-sheet);
    let top-handle = top-mirror & window-handle(top-mirror);
    when (top-handle = GetForegroundWindow())
      SetCapture(handle);
      #t
    end
  end
end method grab-pointer;

define method ungrab-pointer
    (_port :: <win32-port>, pointer :: <pointer>)
 => (success? :: <boolean>)
  ReleaseCapture();
  #t
end method ungrab-pointer;


define sealed method realize-cursor
    (_port :: <win32-port>, cursor :: <symbol>) => (hCursor :: <HCURSOR>)
  realize-cursor(_port, gethash($cursor-table, cursor) | $IDC-ARROW)
end method realize-cursor;

define sealed method realize-cursor
    (_port :: <win32-port>, cursor :: <LPCSTR>) => (hCursor :: <HCURSOR>)
  gethash(_port.%cursor-cache, cursor)
  | begin
      let hCursor = LoadCursor($null-hInstance, cursor);
      when (null-handle?(hCursor))
	hCursor := LoadCursor($null-hInstance, $IDC-ARROW)
      end;
      gethash(_port.%cursor-cache, cursor) := hCursor;
      hCursor
    end
end method realize-cursor;

define sealed method realize-cursor
    (_port :: <win32-port>, cursor :: <stencil>) => (hCursor :: <HCURSOR>)
  gethash(_port.%cursor-cache, cursor)
  | begin
      //---*** Decode the stencil
      //---*** Call CreateCursor
      let hCursor = LoadCursor($null-hInstance, $IDC-ARROW);
      //--- gethash(_port.%cursor-cache, cursor) := hCursor;
      hCursor
    end
end method realize-cursor;

define sealed method realize-cursor
    (_port :: <win32-port>, cursor :: <image>) => (hCursor :: <HCURSOR>)
  error("Windows doesn't support full color cursors")
end method realize-cursor;


/// Focus and carets

define constant $null-bitmap = null-pointer(<HBITMAP>);

define sealed class <win32-caret> (<basic-caret>)
end class <win32-caret>;

define sealed domain make (singleton(<win32-caret>));
define sealed domain initialize (<win32-caret>);

define sealed method make-caret
    (_port :: <win32-port>, sheet :: <sheet>, #key x, y, width, height)
 => (caret :: <win32-caret>)
  make(<win32-caret>,
       port: _port, sheet: sheet,
       x: x | 0, y: y | 0,
       //--- This should really compute the width/height as described in CreateCaret
       width:  width  | $caret-width,
       height: height | (sheet-line-height(sheet) + sheet-line-spacing(sheet)))
end method make-caret;

define sealed method do-set-caret-position
    (caret :: <win32-caret>, x :: <integer>, y :: <integer>) => ()
  let sheet = caret-sheet(caret);
  let _port = sheet & port(sheet);
  when (_port & sheet == _port.%caret-sheet)
    let transform = sheet-device-transform(caret-sheet(caret));
    with-device-coordinates (transform, x, y)
      SetCaretPos(x, y)
    end
  end
end method do-set-caret-position;

// This is a no-op until the next time CreateCaret gets called
define sealed method do-set-caret-size
    (caret :: <win32-caret>, width :: <integer>, height :: <integer>) => ()
  #f
end method do-set-caret-size;

define sealed method do-show-caret
    (caret :: <win32-caret>, #key tooltip?) => ()
  let sheet  = caret-sheet(caret);
  let handle = sheet & sheet-parent-window(sheet);
  when (handle)
    ShowCaret(handle)
  end;
  when (tooltip?)
    // Make sure the ToolTip is visible
    let top-mirror = sheet & top-level-mirror(sheet);
    let tooltip    = top-mirror & top-mirror.%tool-tip;
    when (tooltip)
      SendMessage(tooltip, $TTM-ACTIVATE, $true, 0)
    end
  end
end method do-show-caret;

define sealed method do-hide-caret
    (caret :: <win32-caret>, #key tooltip?) => ()
  let sheet  = caret-sheet(caret);
  let handle = sheet & sheet-parent-window(sheet);
  when (handle)
    HideCaret(handle)
  end;
  when (tooltip?)
    // Hide the ToolTip, too.  We do this because people tend to wrap
    // 'with-caret-hidden' around drawing code, and drawing with the tool tip
    // visible may screw things up
    let top-mirror = sheet & top-level-mirror(sheet);
    let tooltip    = top-mirror & top-mirror.%tool-tip;
    when (tooltip)
      SendMessage(tooltip, $TTM-ACTIVATE, $false, 0)
    end
  end
end method do-hide-caret;


/// Focus handling

define sealed method note-focus-in
    (_port :: <win32-port>, sheet :: <sheet>) => ()
  next-method();
  let mirror = sheet-mirror(sheet);
  _port.%focus := mirror & window-handle(mirror);
  let frame = sheet-frame(sheet);
  frame & call-in-frame(frame, method () set-focus(sheet) end)
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <win32-port>, sheet :: <sheet>) => ()
  next-method();
  _port.%focus := #f;
  let frame = sheet-frame(sheet);
  if (frame & ~frame-input-focus(frame))
    call-in-frame(frame, method () remove-focus() end)
  end
end method note-focus-out;

define method set-focus
    (sheet :: <sheet>) => (set? :: <boolean>)
  let parent = sheet-device-parent(sheet, error?: #f);
  parent & set-focus(parent)
end method set-focus;

define method set-focus
    (sheet :: <mirrored-sheet-mixin>) => (set? :: <boolean>)
  let sheet :: false-or(<mirrored-sheet-mixin>)
    = if (sheet-accepts-focus?(sheet))
	sheet
      else
	find-child-for-focus(sheet)
      end;
  let handle = sheet & window-handle(sheet);
  case
    ~handle =>
      warn("Ignored attempt to set focus to unattached sheet %=", sheet);
      #f;
    GetFocus() = handle =>		// avoid recursion
      duim-debug-message("'set-focus' avoiding recursion");
      #f;
    otherwise =>
      duim-debug-message("'set-focus' setting focus to %=", sheet);
      // Don't check the result, because SetFocus doesn't properly clear
      // the error code.
      // check-result("SetFocus", SetFocus(handle));
      SetFocus(handle);
      #t;
  end
end method set-focus;

define method remove-focus
    () => ()
  duim-debug-message("'remove-focus' removing the focus");
  // We don't check the return result, because if we remove the focus
  // when there is no current input focus, it will look like an error
  SetFocus($NULL-HWND)
end method remove-focus;

// Finds the first mirrored child that can accept the input focus
//---*** The whole way we do this is really horrid
define method find-child-for-focus
    (sheet :: <sheet>)
 => (child :: false-or(<mirrored-sheet-mixin>))
  let child
    = block (return)
	local method find-child (sheet :: <sheet>)
		for (child :: <sheet> in sheet-children(sheet))
		  unless (instance?(child, <menu>))
		    find-child(child);
		    let mirror = sheet-direct-mirror(child);
		    when (instance?(mirror, <window-mirror>)
			  & sheet-mapped?(child)
			  & sheet-accepts-focus?(child))
		      return(child)
		    end
		  end
		end
	      end method;
	find-child(sheet);
	#f
      end;
  // If the child is a radio button within a radio box, put the focus
  // on the selected radio button
  when (instance?(child, <radio-button>))
    let box = button-gadget-box(child);
    when (instance?(box, <radio-box>))
      let selection = gadget-selection(box);
      let index     = ~empty?(selection) & selection[0];
      let button    = index & gadget-box-buttons(box)[index];
      button & (child := button)
    end
  end;
  child
end method find-child-for-focus;

// If the focus is set to the top level sheet, try to revert the focus
// to the frame's input focus. If there isn't one, then just use the
// ordinary guessing algorithm.
define method find-child-for-focus
    (sheet :: <top-level-sheet>)
 => (child :: false-or(<mirrored-sheet-mixin>))
  let frame = sheet-frame(sheet);
  let focus = frame & frame-input-focus(frame);
  if (instance?(focus, <mirrored-sheet-mixin>))
    focus
  else
    next-method()
  end
end method find-child-for-focus;

define sealed method maybe-update-focus
    (_port :: <win32-port>) => ()
  let focus = GetFocus();
  let old-focus = _port.%focus;
  let sheet = handle-sheet(focus);
  if (sheet)
    if (sheet-ignore-focus-change?(sheet))
      duim-debug-message("Ignoring focus change for %=", sheet)
    end;
    unless (sheet-ignore-focus-change?(sheet)
	      | focus = old-focus)
      let new
	= if (sheet-accepts-focus?(sheet))
	    sheet
	  else
	    duim-debug-message("Focus set to non-accepting sheet: %=", sheet);
	    find-child-for-focus(sheet)
	  end;
      case
	new =>
	  duim-debug-message("Focus now set to %=", new);
	  _port.%focus := focus;
	  unless (new == sheet)
	    let handle = window-handle(new);
	    handle & SetFocus(handle)
	  end;
	  port-input-focus(_port) := new;
	old-focus =>
	  duim-debug-message("Reverting focus from %= back to %=",
			     sheet,
			     handle-sheet(old-focus) | old-focus);
	  SetFocus(old-focus);
	otherwise =>
	  #f;
      end
    end
  else
    _port.%focus := focus;
    port-input-focus(_port) := #f
  end
end method maybe-update-focus;

define method sheet-ignore-focus-change?
    (sheet :: <sheet>) => (ignore? :: <boolean>)
  #f
end method sheet-ignore-focus-change?;
