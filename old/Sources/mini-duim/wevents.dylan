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
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>),
     message :: <unsigned-int>, wParam :: <unsigned-int>, lParam :: <unsigned-int>) 
 => (handled? :: <boolean>);
  function handle-scrolling
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>),
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>);
  function handle-command
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>),
     mirror :: <win32-mirror>, id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>);
  function handle-menu-command
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>), id :: <integer>)
 => (handled? :: <boolean>);
  function handle-accelerator-command
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>), id :: <integer>)
 => (handled? :: <boolean>);
  function handle-button
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>),
     button :: <integer>, event-class :: <class>, down? :: <boolean>,
     wParam :: <unsigned-int>, lParam :: <unsigned-int>)
 => (status :: <integer>);
end protocol <<win32-event-protocol>>;


define method process-next-event
    (_port :: <win32-port>, #key timeout)
 => (exit? :: <boolean>, status-code :: <signed-int>)
  // debug-message("Processing Win32 event for %=", _port);
  //---*** Multiple values don't make it out of with-stack-structure in Webster-8000
  //---*** so we've hacked around it for now.
  let exit? = #f;
  let status-code = 0;
  with-stack-structure (pMsg :: <PMSG>)
    if (GetMessage(pMsg,	// message structure
		   $NULL-HWND,	// handle of window receiving the message
		   0,		// lowest message to examine
		   0))		// highest message to examine
      if (#t /*---*** TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 */)
	TranslateMessage(pMsg);	// translates virtual key codes
	DispatchMessage(pMsg) 	// dispatches message to window
      end;
    else
      exit? := #t;
      status-code := pMsg.wParam-value
    end
  end;
  values(exit?, status-code)
end method process-next-message;

//---*** Do we need this?
define method process-pending-events
    (_port :: <win32-port>)
 => (exit? :: <boolean>, status-code :: <signed-int>)
  // Get and dispatch all messages on the queue
  with-stack-structure (pMsg :: <PMSG>)
    while (PeekMessage(pMsg,	// message structure
		       $NULL-HWND,// handle of window receiving the message
		       0,	// lowest message to examine
		       0,	// highest message to examine
		       $PM-REMOVE))
      if (#t /*---*** TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 */)
        TranslateMessage(pMsg);	// translates virtual key codes
        DispatchMessage(pMsg) 	// dispatches message to window
      end
    end;
    pMsg.wParam-value
  end;
  values(#f, 0)
end method process-pending-events;

/*---*** Old event handling... remove when sure of the new stuff
define method port-event-loop
    (_port :: <win32-port>) => (exit-status :: <signed-int>)
  ignore(_port);
  // Get and dispatch messages until a WM_QUIT message is received
  with-stack-structure (pMsg :: <PMSG>)
    while (GetMessage(pMsg,	// message structure
		      $NULL-HWND,// handle of window receiving the message
		      0,	// lowest message to examine
		      0))	// highest message to examine
      if (#t /*---*** TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 */)
	TranslateMessage(pMsg);	// translates virtual key codes
	DispatchMessage(pMsg) 	// dispatches message to window
      end
    end;
    pMsg.wParam-value
  end
end method port-event-loop;

//---*** This awaits PeekMessage to be imported from Win32
define method process-messages
    (_port :: <win32-port>) => (exit-status :: <signed-int>)
  ignore(_port);
  // Get and dispatch all messages on the queue
  /*
  with-stack-structure (pMsg :: <PMSG>)
    while (PeekMessage(pMsg,	// message structure
		       $NULL-HWND,// handle of window receiving the message
		       0,	// lowest message to examine
		       0,	// highest message to examine
		       $PM-REMOVE))
      if (#t /*---*** TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 */)
        TranslateMessage(pMsg);	// translates virtual key codes
        DispatchMessage(pMsg) 	// dispatches message to window
      end
    end;
    pMsg.wParam-value
  end;
  */
  0
end method process-messages;

//---*** Flush this when we have got PeekMessage imported
define method process-next-message
    (_port :: <win32-port>)
 => (finished? :: <boolean>, exit-status :: <signed-int>)
  ignore(_port);
  with-stack-structure (pMsg :: <PMSG>)
    if (GetMessage(pMsg,	// message structure
		   $NULL-HWND,	// handle of window receiving the message
		   0,		// lowest message to examine
		   0))		// highest message to examine
      if (#t /*---*** TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 */)
	TranslateMessage(pMsg);	// translates virtual key codes
	DispatchMessage(pMsg) 	// dispatches message to window
      end;
      values(#f, 0)
    else
      values(#t, pMsg.wParam-value)
    end
  end
end method process-next-message;
*/

// This is the callback function that is called by Windows to
// process a Windows message for DispatchMessage.
// Note: the hWnd's mirror can be #f for messages sent while still inside
// CreateWindow before we've had a chance to create the mirror object.
define method window-callback-function
    (hWnd :: <HWND>,		// window handle
     message :: <unsigned-int>,	// type of message
     wParam  :: <unsigned-int>,	// additional information
     lParam  :: <unsigned-int>)	// additional information
  => (value :: <integer>)
  //--- 'listener-condition-handler' is IDVM only...
  // let handler (<serious-condition>) = listener-condition-handler;
  let mirror = window-mirror(hWnd);
  let sheet = mirror & mirror-sheet(mirror);
  // debug-message("Window callback function for %=: message=#x%x, wparam=%=, lparam=%=",
  //		sheet, message, wParam, lParam);
  if (sheet & handle-message(sheet, message, wParam, lParam))
    0
  else
    DefWindowProc(hWnd, message, wParam, lParam)
  end
end method window-callback-function;

define callback WndProc :: <WNDPROC> = window-callback-function;

define method handle-wm-command
    (sheet :: <sheet>, wParam :: <unsigned-int>, lParam :: <unsigned-int>)
 => (handled? :: <boolean>)
  let hWnd    :: <HWND> = as(<HWND>, lParam);
  let wmId    :: <signed-int> = LOWORD(wParam);
  let wmEvent :: <signed-int> = HIWORD(wParam);
  case
    null-handle?(hWnd) =>
      select (wmEvent)
	0 => handle-menu-command(sheet, wmId);
	1 => handle-accelerator-command(sheet, wmId);
      end;
    otherwise =>
      let mirror = window-mirror(hWnd);
      if (mirror)
        handle-command(mirror-sheet(mirror), mirror, wmId, wmEvent)
      else
	warn("Ignored WM_COMMAND #x%x for window with no mirror", wmEvent)
      end;
  end
end method handle-wm-command;
    
define method handle-accelerator-command
    (sheet :: <sheet>, id :: <integer>)
 => (handled? :: <boolean>)
  ignore(sheet, id);
  error("Accelerator %= received for %=, but handling not implemented!",
	id, sheet);
end method handle-accelerator-command;

define method handle-command
    (sheet :: <sheet>, mirror :: <win32-mirror>, id :: <integer>, 
     event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror);
  warn("No command handler for %= with event %x, id %x",
       sheet, event, id);
  #f		//---*** What to do here?
end method handle-command;

define method handle-message
    (sheet :: <sheet>, message :: <unsigned-int>,
     wParam :: <unsigned-int>, lParam :: <unsigned-int>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  let mirror = sheet-direct-mirror(sheet);
  let hWnd = mirror & mirror.%window-handle;
  block (return)
    select (message) 
      $WM-COMMAND =>
	handle-wm-command(sheet, wParam, lParam);
      $WM-PAINT => 
	with-stack-structure (ps :: <PPAINTSTRUCT> )
	  let oldDC = mirror.%DC;
	  let hDC :: <HDC> = BeginPaint(hWnd, ps);
	  check-result("BeginPaint", hDC);
	  let lpRect :: <LPRECT> = ps.rcPaint-value;
	  block ()
	    //--- Maybe should do 'invalidate-drawing-state' here?
	    mirror.%DC := hDC;
	    let region = make-bounding-box(lpRect.left-value, lpRect.top-value,
					   lpRect.right-value, lpRect.bottom-value);
	    handle-event(sheet,
			 make(<window-repaint-event>,
			      sheet: sheet,
			      region: region))
	  cleanup
	    EndPaint(hWnd, ps);
	    unless (hDC = oldDC)
	      let medium = sheet-medium(sheet);
	      if (medium)
		drawing-state-cached?(medium) := #f;
	      end;
	      mirror.%DC := oldDC;
	    end
	  end
	end;
        return(#t);	//--- work around flow analysis bug in SCC
      $WM-KEYDOWN =>
	distribute-event(_port,
			 make(<key-press-event>,
			      sheet: sheet,
			      key-name:  virtual-key->keysym(wParam),
			      character: virtual-key->character(wParam)));
      $WM-KEYUP =>
	distribute-event(_port,
			 make(<key-release-event>,
			      sheet: sheet,
			      key-name:  virtual-key->keysym(wParam),
			      character: virtual-key->character(wParam)));
      $WM-LBUTTONDOWN =>
	handle-button(sheet, $left-button,   <button-press-event>, #t, wParam, lParam);
      $WM-MBUTTONDOWN =>
	handle-button(sheet, $middle-button, <button-press-event>, #t, wParam, lParam);
      $WM-RBUTTONDOWN =>
	handle-button(sheet, $right-button,  <button-press-event>, #t, wParam, lParam);
      $WM-LBUTTONUP =>
	handle-button(sheet, $left-button,   <button-release-event>, #f, wParam, lParam);
      $WM-MBUTTONUP =>
	handle-button(sheet, $middle-button, <button-release-event>, #f, wParam, lParam);
      $WM-RBUTTONUP =>
	handle-button(sheet, $right-button,  <button-release-event>, #f, wParam, lParam);
      $WM-LBUTTONDBLCLK =>
	handle-button(sheet, $left-button,   <double-click-event>, #f, wParam, lParam);
      $WM-MBUTTONDBLCLK =>
	handle-button(sheet, $middle-button, <double-click-event>, #f, wParam, lParam);
      $WM-RBUTTONDBLCLK =>
	handle-button(sheet, $right-button,  <double-click-event>, #f, wParam, lParam);
      $WM-MOUSEMOVE =>
        let pointer = port-pointer(_port);
	if (zero?(pointer-button-state(pointer)))
	  distribute-event(_port, 
			   make(<pointer-motion-event>,
				sheet: sheet,
				pointer: pointer,
				x: LOWORD(lParam), y: HIWORD(lParam)))
	else
	  distribute-event(_port, 
			   make(<pointer-drag-event>,
				sheet: sheet,
				pointer: pointer,
				button: pointer-button-state(pointer),
				x: LOWORD(lParam), y: HIWORD(lParam)))
	end;
      $WM-HSCROLL, $WM-VSCROLL =>
	let control = as(<HWND>, lParam);
	let scroll-code = LOWORD(wParam);
	let pos = HIWORD(wParam);
	let mirror = window-mirror(control);
        let scroll-bar = mirror & mirror-sheet(mirror);
        if (scroll-bar)
	  handle-scrolling(scroll-bar, scroll-code, pos)
	else
	  warn("Ignored WM_SCROLL event #x%x for window with no mirror", scroll-code)
	end;
      $WM-SIZE =>
        let region = make-bounding-box(0, 0, LOWORD(lParam), HIWORD(lParam));
	distribute-event(_port,
			 make(<window-configuration-event>,
			      sheet: sheet,
			      region: region));
      /*---*** This comes in before we have the sheet mirror relationship
        ---*** so how can we fix this?
      $WM-GETMINMAXINFO =>	
        // Return the min/max size of a window
        //---*** Is this correct?
	let space-req = compose-space(sheet);
        let size-info = as(<LPMINMAXINFO>, lparam);
	let max-size = size-info.ptMaxSize-value;
	let min-track-size = size-info.ptMinTrackSize-value;
	let max-track-size = size-info.ptMaxTrackSize-value;
        let (width, min-width, max-width, height, min-height, max-height)
          = space-requirement-components(space-req);
        ignore(width, height);
	max-size.x-value := max-width;
	max-size.y-value := max-height;
	min-track-size.x-value := min-width;
	min-track-size.y-value := min-height;
	max-track-size.x-value := max-width;
	max-track-size.y-value := max-height;
	#t;
      */
      $WM-SETFOCUS =>
	distribute-focus-in-callback(sheet);
        #t;
      $WM-KILLFOCUS =>
	distribute-focus-out-callback(sheet);
        #t;
      $WM-CREATE => 
	//---*** What should we do here?
        //---*** Is there where we should establish the mirror<->HWND link?
	#f;
      $WM-CLOSE =>
        // If the user asked to close via the window manager, just call
        // 'frame-exit' on the frame.  'frame-exit' will take care of
        // exiting the frame in an orderly way, destroying its sheets.
        //---*** What if there's more than one "top level" sheet, e.g., MDI?
        if (instance?(sheet, <top-level-sheet>))
	  let frame = sheet-frame(sheet);
	  frame-exit(frame)
	end;
        #t;
      $WM-DESTROY =>
        // If the 'top-level' frame has been destroyed (i.e. one with no owner)
        // then we should quit the application.
        let frame = instance?(sheet, <top-level-sheet>) & sheet-frame(sheet);
        case
	  ~frame =>
	    #f;
	  ~frame-owner(frame) =>
	    PostQuitMessage(0);
	  otherwise =>
	    let _port = port(sheet);
	    distribute-event(_port,
			     make(<frame-destroyed-event>, frame: frame))
	end;
        note-mirror-destroyed(sheet, mirror);
        #t;
      $WM-PALETTECHANGED =>
	//--- Explicitly ignored; experimental hack to try to stop propagation
	#f;
      otherwise =>
	return(#f);
    end;
    #t
  cleanup
    // If a DC was allocated during the processing of an event, release
    // it before returning to the operating system.  This also has the
    // effect of invalidating the drawing state cache.
    release-DC(mirror)
  end
end method handle-message;

define method handle-button
    (sheet :: <sheet>, button :: <integer>, event-class :: <class>,
     down? :: <boolean>, wParam :: <unsigned-int>, lParam :: <unsigned-int>)
 => (status :: <integer>)
  let modifier-state :: <unsigned-int> = 0;
  if (~zero?(logand(wParam, $MK-CONTROL)))
    modifier-state := $control-key;
  end;
  if (~zero?(logand(wParam, $MK-SHIFT)))
    modifier-state := logior(modifier-state, $shift-key);
  end;
  let _port = port(sheet);
  let pointer = port-pointer(_port);
  port-modifier-state(_port) := modifier-state;
  if (down?)
    pointer-button-state(pointer) := button
  else
    pointer-button-state(pointer) := 0
  end;
  distribute-event(_port,
		   make(event-class,
			sheet: sheet,
			pointer: port-pointer(port(sheet)),
			button: button,
			modifier-state: modifier-state,
			x: LOWORD(lParam), y: HIWORD(lParam)));
  0
end method handle-button;

define method do-queue-repaint
    (_port :: <win32-port>, sheet :: <sheet>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(region);
  //---*** Transform LTRB to proper coordinates
  InvalidateRect(sheet.%window-handle, $NULL-RECT, #f);
end method do-queue-repaint;

