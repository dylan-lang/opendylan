Module:    motif-duim
Synopsis:  Motif event processing implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif events

define method install-event-handlers
    (mirror :: <motif-mirror>) => ()
  let widget = mirror-widget(mirror)
  xt/XtAddEventHandler(width, #[#"button-press"],   #f, button-press-callback,   mirror);
  xt/XtAddEventHandler(width, #[#"button-release"], #f, button-release-callback, mirror);
  xt/XtAddEventHandler(width, #[#"pointer-motion"], #f, pointer-motion-callback, mirror);
  xt/XtAddEventHandler(width, #[#"enter-window"],   #f, enter-window-callback,   mirror);
  xt/XtAddEventHandler(width, #[#"leave-window"],   #f, leave-window-callback,   mirror);
  xt/XtAddEventHandler(width, #[#"key-press"],      #f, key-press-callback,      mirror);
  xt/XtAddEventHandler(width, #[#"exposure"],       #f, exposure-callback,       mirror);
end method install-event-handlers;

define method install-event-handlers
    (mirror :: <window-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror)
  xt/XtAddEventHandler(width, #[#"structure-notify"], #f, state-change-callback, mirror);
end method install-event-handlers;

define method install-event-handlers
    (mirror :: <top-level-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror);
  let shell  = mirror-shell-widget(mirror);
  xt/XtAddEventHandler(widget, #[#"structure-notify"], #f, state-change-no-config-callback, mirror);
  xt/XtAddEventHandler(shell,  #[#"structure-notify"], #f, state-change-config-callback, mirror);
end method install-event-handlers;

//---*** WHAT TO DO???
define sealed method generate-trigger-event
    (port :: <motif-port>, sheet :: <sheet>) => ()
  let mirror = sheet-mirror(sheet);
  when (mirror)
    let handle = window-handle(mirror);
    // Use PostMessage instead of SendMessage so that we return
    // immmediately, rather than waiting for another thread's
    // event processing to complete
    PostMessage(handle, $WM-NULL, 0, 0)
  end
end method generate-trigger-event;


//---*** DO WE EVEN NEED THIS IN THE X EVENT MODEL?
define sealed method process-next-event
    (_port :: <motif-port>, #key timeout)
 => (timed-out? :: <boolean>)
  //--- We should do something with the timeout
  ignore(timeout);
  with-stack-structure (pMsg :: <LPMSG>)
    if (GetMessage(pMsg,	// message structure
		   $NULL-HWND,	// handle of window receiving the message
		   0,		// lowest message to examine
		   0))		// highest message to examine
      let handle :: <HWND>   = pMsg.hwnd-value;
      let mirror             = window-mirror(handle);
      let sheet              = mirror & mirror-sheet(mirror);
      let haccel :: <HACCEL> = (sheet & accelerator-table(sheet)) | $null-HACCEL;
      if (TranslateAccelerator(handle, haccel, pMsg) = $false)
	when (~sheet | ~process-dialog-message(sheet, pMsg))
	  TranslateMessage(pMsg);	// translates virtual key codes
	  DispatchMessage(pMsg) 	// dispatches message to window
	end
      else
	duim-debug-message("Translated accelerator event for sheet %=: #x%x",
			   sheet, pMsg.message-value)
      end
    end
  end;
  #f
end method process-next-event;


define xt/xt-event-handler pointer-motion-callback
    (widget, mirror, event)
  ignore(widget);
  handle-pointer-motion(mirror, event)
  #t
end xt/xt-event-handler pointer-motion-callback;

define constant $motion-event-modifier-mask :: <integer>
  //---*** WHERE DOES THIS FUNCTION COME FROM?
  = x/translate-to-modifiers-mask(#"button1", #"button2", #"button3",
				  #"mod1", #"mod2", #"mod3", #"mod4", #"mod5",
				  #"shift", #"control");

define sealed method handle-pointer-motion
    (mirror :: <motif-mirror>, event :: x/<XPointerMovedEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let native-x  = event.x/x-value;
    let native-y  = event.x/y-value;
    let state     = event.x/state-value;
    let modifiers = x-state->duim-state(_port, logand(state, $motion-event-modifier-mask));
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    distribute-event(_port,
		     make(<pointer-motion-event>,
			  sheet: sheet,
			  pointer: port-pointer(_port),
			  modifier-state: modifiers,
			  x: x, y: y))
  end
end method handle-pointer-motion;


define xt/xt-event-handler enter-window-callback
    (widget, mirror, event)
  ignore(widget);
  handle-crossing-event(mirror, event, <pointer-enter-event>)
  #t
end xt/xt-event-handler enter-window-callback;

define xt/xt-event-handler leave-window-callback
    (widget, mirror, event)
  ignore(widget);
  handle-crossing-event(mirror, event, <pointer-exit-event>)
  #t
end xt/xt-event-handler leave-window-callback;

define constant $crossing-event-modifier-mask :: <integer>
  = x/translate-to-modifiers-mask(#"button1", #"button2", #"button3",
				  #"mod1", #"mod2", #"mod3", #"mod4", #"mod5",
				  #"shift", #"control");

// Watch out, because leave events show up after window have been killed!
define sealed method handle-crossing-event
    (mirror :: <motif-mirror>,
     event :: type-union(x/<XEnterWindowEvent>, x/<XLeaveWindowEvent>),
     event-class :: subclass(<pointer-motion-event>)) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let native-x  = event.x/x-value;
    let native-y  = event.x/y-value;
    let state     = event.x/state-value;
    let modifiers = x-state->duim-state(_port, logand(state, $crossing-event-modifier-mask));
    let detail    = event.x/detail-value;
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    distribute-event(_port,
		     make(event-class,
			  sheet: sheet,
			  pointer: port-pointer(_port),
			  kind: x-detail->duim-crossing-kind(detail),
			  modifier-state: modifiers,
			  x: x, y: y))
  end
end method handle-crossing-event;

define function x-detail->duim-crossing-kind
    (detail :: <integer>) => (kind :: <symbol>)
  select (detail)
    x/$NotifyAncestor         => #"ancestor";
    x/$NotifyVirtual          => #"virtual";
    x/$NotifyInferior         => #"inferior";
    x/$NotifyNonlinear        => #"nonlinear";
    x/$NotifyNonlinearVirtual => #"nonlinear-virtual";
  end
end function x-button->duim-button;


define xt/xt-event-handler button-press-callback
    (widget, mirror, event)
  ignore(widget);
  handle-button-press(mirror, event)
  #t
end xt/xt-event-handler button-press-callback;

define variable *last-button-click-time*  :: false-or(<integer>) = #f;
define variable *last-button-click-state* :: false-or(<integer>) = #f;

define constant $button-event-modifier-mask :: <integer>
  = x/translate-to-modifiers-mask(#"mod1", #"mod2", #"mod3", #"mod4", #"mod5",
				  #"shift", #"control");

define sealed method handle-button-press
    (mirror :: <motif-mirror>, event :: x/<XButtonPressedEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let native-x  = event.x/x-value;
    let native-y  = event.x/y-value;
    let button    = x-button->duim-button(event.x/button-value);
    let state     = event.x/state-value;
    let modifiers = x-state->duim-state(_port, logand(state, $button-event-modifier-mask));
    let time      = event.x/time-value;
    let timeout     = xt/XtGetMultiClickTime(_port.%display);
    let interval    = #f;
    let same-state? = #f;
    // Deal with possible second click
    if (*last-button-click-time*)
      interval    := time - *last-button-click-time*;
      same-state? := (state = *last-button-click-state*)
    else
      interval    := timeout;
      same-state? := #f
    end;
    *last-button-click-time*  := time;
    *last-button-click-state* := state;
    let event-class
      = if (same-state? & interval < timeout) <double-click-event>
	else <button-press-event> end;
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    port-modifier-state(_port)    := modifiers;
    pointer-button-state(pointer) := button;
    distribute-event(_port,
		     make(event-class,
			  sheet: sheet,
			  pointer: port-pointer(_port),
			  button: button,
			  modifier-state: modifiers,
			  x: x, y: y))
  end
end method handle-button-press;

define function x-button->duim-button
    (x-button :: <integer>) => (duim-button :: <integer>)
  select (x-button)
    x/$Button1 => $left-button;
    x/$Button2 => $middle-button;
    x/$Button3 => $right-button;
  end
end function x-button->duim-button;


define xt/xt-event-handler button-release-callback
    (widget, mirror, event)
  ignore(widget);
  handle-button-release(mirror, event)
  #t
end xt/xt-event-handler button-release-callback;

define sealed method handle-button-release
    (mirror :: <motif-mirror>, event :: x/<XButtonReleasedEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let native-x  = event.x/x-value;
    let native-y  = event.x/y-value;
    let button    = x-button->duim-button(event.x/button-value);
    let state     = event.x/state-value;
    let modifiers = x-state->duim-state(_port, logand(state, $button-event-modifier-mask));
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    port-modifier-state(_port)    := modifiers;
    pointer-button-state(pointer) := button;
    distribute-event(_port,
		     make(<button-release-event>,
			  sheet: sheet,
			  pointer: port-pointer(_port),
			  button: button,
			  modifier-state: modifiers,
			  x: x, y: y))
  end
end method handle-button-release;


define xt/xt-event-handler key-press-callback
    (widget, mirror, event)
  ignore(widget);
  handle-key-event(mirror, event, <key-press-event>)
  #t
end xt/xt-event-handler key-press-callback;

define xt/xt-event-handler key-release-callback
    (widget, mirror, event)
  ignore(widget);
  handle-key-event(mirror, event, <key-release-event>)
  #t
end xt/xt-event-handler key-release-callback;

define constant $key-event-modifier-mask :: <integer>
  = x/translate-to-modifiers-mask(#"mod1", #"mod2", #"mod3", #"mod4", #"mod5",
				  #"shift", #"control");

define sealed method handle-key-event
    (mirror :: <motif-mirror>,
     event :: type-union(x/<XKeyPressedEvent>, x/<XKeyReleasedEvent>),
     event-class :: subclass(<keyboard-event>)) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let keycode   = event.x/keycode-value;
    let state     = event.x/state-value;
    let modifiers = x-state->duim-state(_port, logand(state, $key-event-modifier-mask));
    let (keysym-modifiers, keysym)
      = xt/XtTranslateKeycode(_port.%display, keycode, state);
    let char   = x-keysym->character(keysym, logand(state, $key-event-modifier-mask));
    let keysym = x-keysym->keysym(keysym);
    port-modifier-state(_port) := modifiers;
    distribute-event(_port,
		     make(event-class,
			  sheet: sheet,
			  keysym: keysym,
			  character: char,
			  modifier-state: modifiers,
  end
end method handle-key-event;


define xt/xt-event-handler exposure-callback
    (widget, mirror, event)
  ignore(widget);
  handle-exposure-event(mirror, event)
  #t
end xt/xt-event-handler exposure-callback;

define sealed method handle-exposure-event
    (mirror :: <motif-mirror>, event :: x/<XExposeEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let native-x  = event.x/x-value;
    let native-y  = event.x/y-value;
    let native-dx = event.x/width-value;
    let native-dy = event.x/height-value;
    let (x, y)
      = untransform-position(sheet-native-transform(sheet), native-x, native-y);
    let (width, height)
      = untransform-distance(sheet-native-transform(sheet), native-dx, native-dy);
    let region = make-bounding-box(x, y, x + width, y + height);
    distribute-event(_port,
		     make(<window-repaint-event>,
			  sheet:  sheet,
			  region: region))
  end
end method handle-exposure-event;


define xt/xt-event-handler state-change-callback
    (widget, mirror, event)
  ignore(widget);
  handle-state-change-event(mirror, event)
  #t
end xt/xt-event-handler state-change-callback;

define sealed method handle-state-change-event
    (mirror :: <motif-mirror>, event :: x/<XEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let type = event.x/type-value;
    select (type)
      #"configure-notify" =>
	handle-configuration-change-event(_port, sheet, event);
      #"map-notify"       =>
	note-mirror-enabled/disabled(_port, sheet, #t);
      #"unmap-notify"     =>
	note-mirror-enabled/disabled(_port, sheet, #f);
      #"circulate-notify" => #f;
      #"destroy-notify"   => #f;
      #"gravity-notify"   => #f;
      #"reparent-notify"  => #f;
    end
  end
end method handle-state-change-event;


define xt/xt-event-handler state-change-config-callback
    (widget, mirror, event)
  ignore(widget);
  handle-state-change-config-event(mirror, event)
  #t
end xt/xt-event-handler state-change-config-callback;

define sealed method handle-state-change-config-event
    (mirror :: <motif-mirror>, event :: x/<XEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let type = event.x/type-value;
    select (type)
      #"configure-notify" =>
	handle-configuration-change-event(_port, sheet, event);
      #"map-notify"       => #f;
      #"unmap-notify"     => #f;
      #"circulate-notify" => #f;
      #"destroy-notify"   => #f;
      #"gravity-notify"   => #f;
      #"reparent-notify"  => #f;
    end
  end
end method handle-state-change-config-event;


define xt/xt-event-handler state-change-no-config-callback
    (widget, mirror, event)
  ignore(widget);
  handle-state-change-no-config-event(mirror, event)
  #t
end xt/xt-event-handler state-change-no-config-callback;

define sealed method handle-state-change-no-config-event
    (mirror :: <motif-mirror>, event :: x/<XEvent>) => ()
  let sheet = mirror-sheet(mirror);
  let _port = port(sheet);
  when (_port)
    let type = event.x/type-value;
    select (type)
      #"map-notify"       =>
	note-mirror-enabled/disabled(_port, sheet, #t);
      #"unmap-notify"     =>
	note-mirror-enabled/disabled(_port, sheet, #f);
      #"configure-notify" => #f;
      #"circulate-notify" => #f;
      #"destroy-notify"   => #f;
      #"gravity-notify"   => #f;
      #"reparent-notify"  => #f;
    end
  end
end method handle-state-change-no-config-event;

define sealed method handle-configuration-change-event
    (_port :: <motif-port>, sheet :: <sheet>, event :: x/<XEvent>) => ()
  let native-x  = event.x/x-value;
  let native-y  = event.x/y-value;
  let native-dx = event.x/width-value;
  let native-dy = event.x/height-value;
  let (x, y)
    = untransform-position(sheet-native-transform(sheet), native-x, native-y);
  let (width, height)
    = untransform-distance(sheet-native-transform(sheet), native-dx, native-dy);
  let region = make-bounding-box(x, y, x + width, y + height);
  distribute-event(_port,
		   make(<window-configuration-event>,
			sheet:  sheet,
			region: region))
end method handle-configuration-change-event;

define sealed method note-mirror-enabled/disabled
    (_port :: <motif-port>, sheet :: <sheet>, enabled? :: <boolean>) => ()
  sheet-enabled?(sheet) := enabled?
end method note-mirror-enabled/disabled;

define sealed method note-mirror-enabled/disabled
    (_port :: <motif-port>, sheet :: <top-level-sheet>, enabled? :: <boolean>) => ()
  when (enabled?)
    sheet-enabled?(sheet) := enabled?
  end
end method note-mirror-enabled/disabled;
