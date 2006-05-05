Module:    win32-duim
Synopsis:  Win32 event processing implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 events

// The Win32 backend event protocol
define protocol <<win32-event-protocol>> ()
  function handle-message 
    (sheet :: <abstract-sheet>, message :: <message-type>, 
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>);
  function handle-scrolling
    (sheet :: <abstract-sheet>,
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>);
  function handle-command
    (sheet :: <abstract-sheet>, mirror :: <win32-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>);
  function handle-notify
    (sheet :: <abstract-sheet>, mirror :: <win32-mirror>,
     wParam :: <wparam-type>, lParam :: <lparam-type>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>);
  function handle-command-for-id
    (sheet :: <abstract-sheet>, id :: <integer>)
 => (handled? :: <boolean>);
  function handle-button
    (sheet :: <abstract-sheet>,
     button :: <integer>, event-class :: <class>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (status :: <lresult-type>);
end protocol <<win32-event-protocol>>;


define sealed method generate-trigger-event
    (port :: <win32-port>, sheet :: <sheet>) => ()
  let mirror = sheet-mirror(sheet);
  when (mirror)
    let handle = window-handle(mirror);
    // Use PostMessage instead of SendMessage so that we return
    // immmediately, rather than waiting for another thread's
    // event processing to complete
    PostMessage(handle, $WM-NULL, 0, 0)
  end
end method generate-trigger-event;


// One <LPMSG> object per thread in order to cut down on consing
define thread variable *lpmsg* :: false-or(<LPMSG>) = #f;

define sealed method process-next-event
    (_port :: <win32-port>, #key timeout)
 => (timed-out? :: <boolean>)
  //--- We should do something with the timeout
  ignore(timeout);
  let pMsg :: <LPMSG> = *lpmsg* | (*lpmsg* := make(<LPMSG>));
  if (GetMessage(pMsg,		// message structure
		 $NULL-HWND,	// handle of window receiving the message
		 0,		// lowest message to examine
		 0))		// highest message to examine
    let handle :: <HWND>   = pMsg.hwnd-value;
    let sheet              = handle-sheet(handle);
    let haccel :: <HACCEL> = (sheet & accelerator-table(sheet)) | $null-HACCEL;
    if (TranslateAccelerator(handle, haccel, pMsg) = $false)
      when (~sheet | ~process-dialog-message(sheet, pMsg))
	TranslateMessage(pMsg);	// translates virtual key codes
	DispatchMessage(pMsg)	// dispatches message to window
      end
    else
      duim-debug-message("Translated accelerator event for sheet %=: #x%x",
			 sheet, pMsg.message-value)
    end
  end;
  #f
end method process-next-event;

define sealed method process-dialog-message
    (sheet :: <sheet>, pMsg :: <LPMSG>) => (handled? :: <boolean>)
  let _port = port(sheet);
  let focus = sheet-input-focus((_port & port-input-focus(_port)) | sheet);
  if (sheet-handles-keyboard?(focus))
    #f
  else
    let top-mirror = top-level-mirror(sheet);
    block (return)
      local method maybe-process-dialog-message
		(mirror :: <top-level-mirror>) => ()
	      when (IsDialogMessage(window-handle(mirror), pMsg))
		return(#t)
	      end;
	      do(maybe-process-dialog-message,
		 mirror-registered-dialogs(mirror))
	    end method;
      top-mirror & maybe-process-dialog-message(top-mirror);
      #f
    end
  end
end method process-dialog-message;


// A null dialog procedure that just calls the default procedure
define sealed method null-callback-function
    (handle :: <HWND>,		// window handle
     message :: <message-type>,	// type of message
     wParam  :: <wparam-type>,	// additional information
     lParam  :: <lparam-type>)	// additional information
  => (result :: <lresult-type>)
  if (dialog-window?(handle))
    DefDlgProc(handle, message, wParam, lParam)
  else
    DefWindowProc(handle, message, wParam, lParam)
  end
end method null-callback-function;

define callback Null-Proc :: <DLGPROC> = null-callback-function;


// This is the callback function that is called by Windows to
// process a Windows message for DispatchMessage.
// Note: the hWnd's mirror can be #f for messages sent while still inside
// CreateWindow before we've had a chance to create the mirror object.
//---*** Maybe we should have a second callback function for dialogs,
//---*** but unfortunately the Win32 FFI only supports one at the moment
//---*** when using 'callback-definer'.
define sealed method window-callback-function
    (handle :: <HWND>,		// window handle
     message :: <message-type>,	// type of message
     wParam  :: <wparam-type>,	// additional information
     lParam  :: <lparam-type>)	// additional information
  => (result :: <lresult-type>)
  let sheet = handle-sheet(handle);
  if (sheet & handle-message(sheet, message, wParam, lParam))
    0
  else
    default-window-callback(handle, message, wParam, lParam)
  end
end method window-callback-function;

// Like 'next-method' for window callbacks...
define sealed inline method default-window-callback
    (handle :: <HWND>,		// window handle
     message :: <message-type>,	// type of message
     wParam  :: <wparam-type>,	// additional information
     lParam  :: <lparam-type>)	// additional information
  => (result :: <lresult-type>)
  if (dialog-window?(handle))
    DefDlgProc(handle, message, wParam, lParam)
  else
    DefWindowProc(handle, message, wParam, lParam)
  end
end method default-window-callback;

define callback WndProc :: <WNDPROC> = window-callback-function;

define method handle-message
    (sheet :: <sheet>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  let mirror = sheet-direct-mirror(sheet);
  let handle = mirror & window-handle(mirror);
  let handled?
    = block (return)
	select (message) 
	  $WM-COMMAND =>
	    //--- We might need this so that clicking on a menu button
	    //--- reverts the status bar back to its ordinary state
	    let frame = sheet-frame(sheet);
	    when (frame)
	      frame-status-bar-simple?(frame) := #f
	    end;
	    handle-wm-command(sheet, wParam, lParam);
	  $WM-NOTIFY =>
	    handle-wm-notify(sheet, wParam, lParam);
	  $WM-INITMENUPOPUP =>
	    //--- This needs to get handled immediately.  Does it matter
	    //--- that this then isn't running in the user process?
	    let menu-handle = as(<HMENU>, wParam);
	    let menu = handle-sheet(menu-handle);
	    if (menu)
	      handle-menu-update(menu)
	    else
	      warn("Ignoring WM_INITMENUPOPUP for unknown menu handle")
	    end;
	  $WM-MENUSELECT =>
	    //--- This needs to get handled immediately.  Does it matter
	    //--- that this then isn't running in the user process?
	    let frame = sheet-frame(sheet);
	    when (frame)
	      let menu-handle = as(<HMENU>, lParam);
	      let id      = LOWORD(wParam);
	      let fuFlags = HIWORD(wParam);
	      let menu-closed? = (fuFlags = #xFFFF) & null-pointer?(menu-handle);
	      let button
		= when (zero?(logand(fuFlags, $MF-POPUP)))
		    id->gadget(sheet, id)
		  end;
	      let documentation
		= unless (menu-closed?)
		    (button & gadget-documentation(button)) | ""
		  end;
	      update-frame-documentation(frame, documentation)
	    end;
	    #f;
	  $WM-ERASEBKGND =>
	    if (mirror)
	      let hdc = as(<HDC>, wParam);
	      erase-background(sheet, mirror, hdc)
	    else
	      warn("Ignoring WM_ERASEBKGND for unknown window handle")
	    end;
	    #t;
	  $WM-PAINT =>
	    // Note that Windows takes care of hiding and showing the caret for us...
	    if (mirror)
	      repaint-mirror(sheet, mirror)
	    else
	      warn("Ignoring WM_PAINT for unknown window handle")
	    end;
	    #t;
	  $WM-MOUSEMOVE =>
	    let pointer = port-pointer(_port);
	    let button-state :: <unsigned-int> = key-flags->button-state(wParam);
	    pointer-button-state(pointer) := button-state;
	    let (x, y) = LPARAM-TO-XY(lParam);
	    if (zero?(button-state))
	      distribute-event(_port, 
			       make(<pointer-motion-event>,
				    sheet: sheet,
				    pointer: port-pointer(_port),
				    x: x, y: y))
	    else
	      distribute-event(_port, 
			       make(<pointer-drag-event>,
				    sheet: sheet,
				    pointer: port-pointer(_port),
				    button: button-state,
				    x: x, y: y))
	    end;
	    #t;
	  $WM-KEYDOWN =>
	    handle-key-down(sheet, wParam, lParam);
	  $WM-KEYUP =>
	    handle-key-up(sheet, wParam, lParam);
	  $WM-CHAR =>
	    handle-char(sheet, wParam, lParam);
	  $WM-SYSKEYDOWN =>
	    handle-syskey-down(sheet, wParam, lParam);
	  $WM-SYSKEYUP =>
	    handle-syskey-up(sheet, wParam, lParam);
	  $WM-SYSCHAR =>
	    handle-syschar(sheet, wParam, lParam);
	  $WM-HOTKEY =>
	    let frame = sheet-frame(sheet);
	    when (frame & frame-keyboard-interrupt?(frame))
	      signal(make(<keyboard-interrupt>))
	    end;
	    #t;
	  $WM-LBUTTONDOWN =>
	    handle-button(sheet, $left-button,   <button-press-event>,
			  wParam, lParam);
	  $WM-MBUTTONDOWN =>
	    handle-button(sheet, $middle-button, <button-press-event>,
			  wParam, lParam);
	  $WM-RBUTTONDOWN =>
	    handle-button(sheet, $right-button,  <button-press-event>,
			  wParam, lParam);
	  $WM-LBUTTONUP =>
	    handle-button(sheet, $left-button,   <button-release-event>,
			  wParam, lParam);
	  $WM-MBUTTONUP =>
	    handle-button(sheet, $middle-button, <button-release-event>,
			  wParam, lParam);
	  $WM-RBUTTONUP =>
	    handle-button(sheet, $right-button,  <button-release-event>,
			  wParam, lParam);
	  $WM-LBUTTONDBLCLK =>
	    handle-button(sheet, $left-button,   <double-click-event>,
			  wParam, lParam);
	  $WM-MBUTTONDBLCLK =>
	    handle-button(sheet, $middle-button, <double-click-event>,
			  wParam, lParam);
	  $WM-RBUTTONDBLCLK =>
	    handle-button(sheet, $right-button,  <double-click-event>,
			  wParam, lParam);
	  $WM-HSCROLL, $WM-VSCROLL =>
	    handle-wm-scroll(sheet, wParam, lParam);
          $WM-MOUSEWHEEL =>
            handle-wm-mousewheel(sheet, wParam, lParam);
	  $WM-SIZE =>
	    let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
	    when (frame)
	      let width  = LOWORD(lParam);
	      let height = HIWORD(lParam);
	      select (wParam)
		$SIZE-RESTORED, $SIZE-MAXIMIZED =>
		  if (mirror)
		    when (~frame-iconified?(frame))
		      // When we've got a maximized frame that was iconified,
		      // and we're now deiconifying, don't do re-layout
		      handle-resize(sheet, mirror, width, height)
		    end;
		    when (frame-mapped?(frame))		// i.e., mapped or iconified
		      frame-maximized?(frame) := (wParam == $SIZE-MAXIMIZED);
		      frame-iconified?(frame) := #f
		    end;
		    #t
		  else
		    warn("Ignoring SIZE_RESTORED or SIZE_MAXIMIZED for unknown window handle")
		  end;
		$SIZE-MINIMIZED =>
		  if (mirror)
		    when (frame-mapped?(frame))		// i.e., mapped
		      frame-iconified?(frame) := #t
		    end;
		    #t
		  else
		    warn("Ignoring SIZE_MINIMIZED for unknown window handle")
		  end;
		otherwise =>
		  #f;
	      end
	    end;
	  $WM-MOVE =>
	    let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
	    // We only handle these messages for mapped frames, since
	    // we only care about the user moving the whole frame
	    when (frame & frame-mapped?(frame))
	      let (x, y) = LPARAM-TO-XY(lParam);
	      handle-move(sheet, mirror, x, y)
	    end;
	  $WM-GETMINMAXINFO =>	
	    // Return the min/max size of a window
	    when (instance?(sheet, <top-level-sheet>))
	      let size-info = make(<LPMINMAXINFO>, address: lParam);
	      let max-pos  = size-info.ptMaxPosition-value;
	      let max-size = size-info.ptMaxSize-value;
	      let min-track-size = size-info.ptMinTrackSize-value;
	      let max-track-size = size-info.ptMaxTrackSize-value;
	      let space-req = compose-space(sheet);
	      let (width, min-width, max-width, height, min-height, max-height)
		= space-requirement-components(sheet, space-req);
	      ignore(width, height);
	      let (display-width, display-height) = display-size(display(sheet));
	      when (min-width  >= $fill) min-width  := 1 end;
	      when (min-height >= $fill) min-height := 1 end;
	      when (max-width  >= $fill) max-width  := display-width  end;
	      when (max-height >= $fill) max-height := display-height end;
	      min!(min-width,  display-width);
	      min!(min-height, display-height);
	      min!(max-width,  display-width);
	      min!(max-height, display-height);
	      // Don't set the x and y position, since Windows should have
	      // passed in reasonable values for those
	      max-size.x-value := max-width;
	      max-size.y-value := max-height;
	      min-track-size.x-value := min-width;
	      min-track-size.y-value := min-height;
	      max-track-size.x-value := max-width;
	      max-track-size.y-value := max-height;
	      #t
	    end;
	  $WM-SETCURSOR =>
	    let pointer = port-pointer(_port);
	    let cursor = pointer-cursor(pointer);
	    unless (cursor == #"default")	// Windows does this case for us...
	      let hCursor :: <HCURSOR>
		= begin
		    if (cursor == _port.%last-cursor)
		      _port.%last-hCursor
		    else
		      let hCursor :: <HCURSOR> = realize-cursor(_port, cursor);
		      _port.%last-cursor  := cursor;
		      _port.%last-hCursor := hCursor;
		      hCursor
		    end
		  end;
	      SetCursor(hCursor)
	    end;
	  $WM-SETFOCUS =>
	    // Note that we never set 'frame-input-focus' -- that only gets
	    // done by user-level code
	    duim-debug-message("Got a WM_SETFOCUS for %= (accept it?: %=)",
			       sheet,
			       sheet-accepts-focus?(sheet));
	    if (instance?(sheet, <top-level-sheet>) & sheet.%needs-activation?)
	      sheet.%needs-activation? := #f;
	      note-sheet-activated(sheet)
	    else
	      let frame = sheet-frame(sheet);
	      when (frame)
		// Install the caret into the window with the focus
		let caret = sheet-caret(sheet);
		when (caret?(caret) & caret-visible?(caret))
		  let (width, height) = caret-size(caret);
		  check-result("CreateCaret",
			       CreateCaret(handle, $null-bitmap, width, height));
		  _port.%caret-sheet := sheet;
		  let (x, y) = caret-position(caret);
		  do-set-caret-position(caret, x, y);
		  do-show-caret(caret)
		end
	      end
	    end;
	    #t;
	  $WM-KILLFOCUS =>
	    // Remove the caret from the window that's losing the focus
	    duim-debug-message("Got a WM_KILLFOCUS for %=", sheet);
	    let caret = sheet-caret(sheet);
	    when (caret?(caret) & caret-visible?(caret))
	      do-hide-caret(caret);
	      // Don't error check, since we sometimes call this when
	      // there is no Windows caret around
	      DestroyCaret();
	      _port.%caret-sheet := #f;
	    end;
	    #t;
	  $WM-ACTIVATE =>
	    // Always reset the modifier and buttons states so that they
	    // aren't "sticky" across frames
	    let pointer = port-pointer(_port);
	    port-modifier-state(_port)    := 0;
	    pointer-button-state(pointer) := 0;
	    _port.%alt-key-state      := #f;
	    _port.%wm-char-state      := 0;
	    _port.%extended-key-state := #f;
	    // Notify the frame that its focus has been gained or lost
	    let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
	    when (frame)
	      let fActive = LOWORD(wParam);
	      select (fActive)
		$WA-INACTIVE =>
		  duim-debug-message("Got a WM_ACTIVATE to deactivate %=", frame);
		  pointer-grabbed?(pointer) := #f;
		  note-sheet-deactivated(sheet);
		otherwise    =>
		  duim-debug-message("Got a WM_ACTIVATE to activate %=", frame);
		  // We would like to call 'note-sheet-activated' here,
		  // but the problem is that Windows is going to try to
		  // manage the focus a bit for us, so we'll end up getting
		  // a WM_KILLFOCUS message on the window we just set the
		  // focus for!  Instead, note that we need to set the focus
		  // the first time we see WM_SETFOCUS on the top-level sheet.
		  sheet.%needs-activation? := #t;
	      end
	    end;
	    #f;				// Windows still has work to do...
	  $WM-ENABLE =>
	    // If enabling/disabling a frame, update the frame's state
	    let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
	    when (frame)
	      let enabled? = ~zero?(wParam);
	      frame-enabled?(frame) := enabled?
	    end;
	    #f;
	  $WM-CLOSE =>
	    // If the user asked to close via the window manager, just call
	    // 'exit-frame' on the frame.  'exit-frame' will take care of
	    // exiting the frame in an orderly way, destroying its sheets.
	    //---*** What if there's more than one "top level" sheet, e.g., MDI?
	    duim-debug-message("WM_CLOSE event received for %=", sheet);
	    let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
	    when (frame)
	      duim-debug-message("Exiting frame %=", frame);
	      exit-frame(frame, destroy?: #t)
	    end;
	    #t;
	  $WM-DESTROY =>
	    // If the 'top-level' frame has been destroyed (i.e. one with no owner)
	    // then we should quit the application.
	    duim-debug-message("Handling WM_DESTROY for %=", sheet);
	    let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
	    when (frame)
	      // At this point, 'sheet' is the frame's top-level sheet
	      unregister-keyboard-interrupt-handler(_port, sheet);
	      note-win32-frame-destroyed(frame);
	      note-mirror-destroyed(sheet, mirror)
	    end;
	    #t;
	  otherwise =>
	    #f;
	end
      cleanup
	when (mirror)
	  // If a DC was allocated during the processing of an event, release
	  // it before returning to the operating system.  This also has the
	  // effect of invalidating the drawing state cache.
	  release-DC(mirror)
	end
      end;
  handled? & #t
end method handle-message;

define sealed method handle-wm-command
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let handle  :: <HWND>    = as(<HWND>, lParam);
  let wmId    :: <integer> = LOWORD(wParam);
  let wmEvent :: <integer> = HIWORD(wParam);
  case
    null-handle?(handle) =>
      // Once upon a time, the second case used 'handle-accelerator-command',
      // but it was exactly the same as 'handle-command-for-id', so we flushed it
      select (wmEvent)
	0 => handle-command-for-id(sheet, wmId);
	1 => handle-command-for-id(sheet, wmId);
      end;
    otherwise =>
      let mirror = window-mirror(handle);
      if (mirror)
	let sheet = mirror-sheet(mirror);
        handle-command(sheet, mirror, wmId, wmEvent)
      else
	warn("Ignored WM_COMMAND #x%x for window with no mirror", wmEvent)
      end;
  end
end method handle-wm-command;
    
define sealed method handle-wm-notify
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  ignore(sheet);
  let nmhdr :: <LPNMHDR> = make(<LPNMHDR>, address: lParam);
  let handle = nmhdr.hwndFrom-value;
  let id     = nmhdr.idFrom-value;
  let code   = nmhdr.code-value;
  case
    null-handle?(handle) =>
      warn("Unexpectedly got a null handle from WM_NOTIFY");
      #f;
    otherwise =>
      let mirror = window-mirror(handle);
      if (mirror)
	let sheet = mirror-sheet(mirror);
        handle-notify(sheet, mirror, wParam, lParam, id, code)
      else
	warn("Ignored WM_NOTIFY #x%x for window with no mirror", code)
      end;
  end
end method handle-wm-notify;

define sealed method handle-wm-scroll
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let control = as(<HWND>, lParam);
  let scroll-code = LOWORD(wParam);
  let pos = HIWORD(wParam);
  let scroll-bar = handle-sheet(control);
  if (scroll-bar)
    handle-scrolling(scroll-bar, scroll-code, pos)
  else
    warn("Ignored WM_SCROLL event #x%x on %s for window with no mirror", 
	 scroll-code, sheet);
    #f
  end;
end method handle-wm-scroll;
    
define sealed method handle-wm-mousewheel
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  warn("Ignored WM_MOUSEWHEEL event for non-scrolling window %s",
       sheet);
end method handle-wm-mousewheel;
    
/* This implementation has several bugs:

   * HIWORD is supposed to return a signed value, not an unsigned value
   * It should query SPI_GETWHEELSCROLLLINES to figure out how many lines
     to scroll
   * It should use the GET_WHEEL_DELTA_PARAM macro to compute number of increments
   * It should support freely-rotating wheels
   * It should scroll three lines at once, not line per line

At the moment, we scroll three lines per event we get, and forget about the proper
computation. FIXME. 

--andreas, 20060504 */
define sealed method handle-wm-mousewheel
    (sheet :: <scrolling-sheet-mixin>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  if (sheet.sheet-vertical-scroll-bar)
    let distance = HIWORD(wParam);
    if (distance > 32768)
      scroll-down-line(sheet.sheet-vertical-scroll-bar);
      scroll-down-line(sheet.sheet-vertical-scroll-bar);
      scroll-down-line(sheet.sheet-vertical-scroll-bar);
    else
      scroll-up-line(sheet.sheet-vertical-scroll-bar);
      scroll-up-line(sheet.sheet-vertical-scroll-bar);
      scroll-up-line(sheet.sheet-vertical-scroll-bar);
    end;
    #t
  else
    #f
  end
end method handle-wm-mousewheel;
    
define method handle-command
    (sheet :: <sheet>, mirror :: <win32-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror);
  warn("No command handler for %= with event %x, id %x",
       sheet, event, id);
  #f		//---*** What to do here?
end method handle-command;

define method handle-notify
    (sheet :: <sheet>, mirror :: <win32-mirror>,
     wParam :: <wparam-type>, lParam :: <lparam-type>,
     id :: <integer>, code :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, wParam, lParam);
  warn("No WM_NOTIFY handler for %= with event %x, id %x",
       sheet, code, id);
  #f		//---*** What to do here?
end method handle-notify;

define sealed method handle-resize
    (sheet :: <sheet>, mirror :: <win32-mirror>,
     width :: <integer>, height :: <integer>)
 => (handled? :: <boolean>)
  let (old-width, old-height) = box-size(mirror.%region);
  unless (width = old-width & height = old-height)
    duim-debug-message("%= resized to %dx%d but WM_SIZE said %dx%d",
		       sheet, old-width, old-height, width, height)
  end;
  #t
end method handle-resize;

define sealed method repaint-mirror
    (sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  with-stack-structure (ps :: <LPPAINTSTRUCT>)
    let old-hDC = mirror.%DC;
    let hDC :: <HDC> = BeginPaint(handle, ps);
    check-result("BeginPaint", hDC);
    let lpRect :: <LPRECT> = ps.rcPaint-value;
    block ()
      //--- Maybe should do 'invalidate-cached-drawing-state' here?
      mirror.%DC := hDC;
      let region
	= make-bounding-box(lpRect.left-value,  lpRect.top-value,
			    lpRect.right-value, lpRect.bottom-value);
      duim-debug-message("Repainting %= in box (%d,%d):(%d,%d)",
			 sheet, 
			 lpRect.left-value,  lpRect.top-value,
			 lpRect.right-value, lpRect.bottom-value);
      // We call 'handle-event' instead of 'distribute-event' because we
      // want the repainting to happen between BeginPaint and EndPaint
      handle-event(event-handler(sheet),
		   make(<window-repaint-event>,
			sheet: sheet,
			region: region))
    cleanup
      EndPaint(handle, ps);
      unless (hDC = old-hDC)
	let medium = sheet-has-medium?(sheet) & sheet-medium(sheet);
	when (medium)
	  medium-drawing-state-cache(medium) := 0
	end;
	mirror.%DC := old-hDC;
      end
    end
  end
end method repaint-mirror;


define sealed method handle-button
    (sheet :: <sheet>, button :: <integer>, event-class :: <class>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (status :: <lresult-type>)
  let _port = port(sheet);
  let pointer = port-pointer(_port);
  let modifier-state :: <unsigned-int> = key-flags->modifier-state(wParam);
  let button-state   :: <unsigned-int> = key-flags->button-state(wParam);
  port-modifier-state(_port)    := modifier-state;
  pointer-button-state(pointer) := button-state;
  let (x, y) = LPARAM-TO-XY(lParam);
  distribute-event(_port,
		   make(event-class,
			sheet: sheet,
			pointer: pointer,
			button: button,
			modifier-state: modifier-state,
			x: x, y: y));
  0
end method handle-button;

define function key-flags->modifier-state
    (flags :: <unsigned-int>) => (modifier-state :: <unsigned-int>)
  let modifier-state :: <unsigned-int> = 0;
  when (~zero?(logand(flags, $MK-CONTROL)))
    modifier-state := $control-key
  end;
  when (~zero?(logand(flags, $MK-SHIFT)))
    modifier-state := logior(modifier-state, $shift-key)
  end;
  modifier-state
end function key-flags->modifier-state;

define function key-flags->button-state
    (flags :: <unsigned-int>) => (button-state :: <unsigned-int>)
  let button-state :: <unsigned-int> = 0;
  when (~zero?(logand(flags, $MK-LBUTTON)))
    button-state := logior(button-state, $left-button)
  end;
  when (~zero?(logand(flags, $MK-MBUTTON)))
    button-state := logior(button-state, $middle-button)
  end;
  when (~zero?(logand(flags, $MK-RBUTTON)))
    button-state := logior(button-state, $right-button)
  end;
  button-state
end function key-flags->button-state;
