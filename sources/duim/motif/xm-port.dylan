Module:    motif-duim
Synopsis:  Motif port implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Motif constants

define constant $caret-width :: <integer> = 2;

define constant $primitive-resources
  = #["*buttonFontList:		*times-bold-r-normal--14*",
      "*labelFontList:  	*times-bold-r-normal--14*",
      "DUIM port*foreground:	Black",
      "DUIM port*background:	White"];


/// Motif ports

define sealed class <motif-port> (<basic-port>)
  sealed slot %display :: false-or(x/<Display>) = #f;
  sealed slot %app-context = #f;
  sealed slot %app-shell   = #f;
  sealed slot %modifier-map :: <simple-object-vector> = #[];
  // Cache for image cursors
  sealed slot %cursor-cache :: <object-table> = make(<object-table>);
  keyword focus-policy: = #"sheet-under-pointer";
end class <motif-port>;

define sealed method initialize
    (_port :: <motif-port>, #key server-path) => ()
  next-method();
  let type    = head(server-path);
  let display = get-property(tail(server-path), #"display",
			     default: environment-variable("DISPLAY"));
  ignore(type);
  let (shell, context, unused-args)
    = construct-application("DUIM port",	// class name -- defines resources
			    display-name: display,
			    app-context-name: format-to-string("DUIM port on %s", display),
			    fallback-resources: $primitive-resources);
  ignore(unused-args);
  _port.%display      := xt/XtDisplay(shell);
  _port.%app-shell    := shell;
  _port.%app-context  := context;
  _port.%modifier-map := initialize-modifier-map(_port.%display);
  install-default-palette(_port);
  install-default-text-style-mappings(_port);
end method initialize;

define sideways method class-for-make-port
    (type == #"motif", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<motif-port>, concatenate(initargs, #(event-processor-type:, #"n")))
end method class-for-make-port;

define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"motif", initargs)
end method class-for-make-port;

define sealed method port-type
    (_port :: <motif-port>) => (type :: <symbol>)
  #"motif"
end method port-type;

define sealed method port-name
    (_port :: <motif-port>) => (name :: false-or(<string>))
  xt/XtAppContentName(_port.%app-context)
end method port-name;

define sealed method destroy-port
    (_port :: <motif-port>) => ()
  next-method();
  release-default-text-style-mappings(_port);
  when (port-process(port))
    kill-event-loop(_port.%app-context);
    xt/XtDestroyApplicationContext(_port.%app-context)
  end
end method destroy-port;

define function shutdown-motif-duim ()
  let ports :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-ports(method (_port)
	     when (instance?(_port, <motif-port>))
	       add!(ports, _port)
	     end
	   end method);
  do(destroy-port, ports)
end function shutdown-motif-duim;


/// Beeping, etc

define sealed method beep
    (_port :: <motif-port>) => ()
  x/XBell(_port.%display, 0)
end method beep;


/// Pointer position hacking

define sealed method do-pointer-position
    (_port :: <motif-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let x-display = _port.%display;
  let x-window  = xt/XtWindow(mirror-widget(sheet-mirror(sheet)));
  let (x-win-root, x-win-child, root-x, root-y, child-x, child-y)
    = x/XQueryPointer(x-display, x-window);
  ignore(x-win-root, x-win-child, child-x, child-y);
  values(child-x, child-y)
end method do-pointer-position;

define sealed method do-pointer-position
    (_port :: <motif-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  let x-display = _port.%display;
  let x-window  = xt/XtWindow(mirror-widget(sheet-mirror(sheet)));
  let (x-win-root, x-win-child, root-x, root-y, child-x, child-y)
    = x/XQueryPointer(x-display, x-window);
  ignore(x-win-root, x-win-child, root-x, root-y);
  values(root-x, root-y)
end method do-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <motif-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>) => ()
  let x-display = _port.%display;
  let x-window  = xt/XtWindow(mirror-widget(sheet-mirror(sheet)));
  x/XWarpPointer(x-display, #f, x-window,
		 0, 0,		// src_x, src_y
		 2000, 2000,	// src-width, src_height
		 x, y)
end method do-set-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <motif-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  let x-display = _port.%display;
  let x-window  = xt/XtWindow(mirror-widget(sheet-mirror(sheet)));
  x/XWarpPointer(x-display, #f, x-window,
		 0, 0,		// src_x, src_y
		 2000, 2000,	// src-width, src_height
		 x, y)
end method do-set-pointer-position;


/// Pointer cursor hacking

define table $cursor-table :: <table>
  = { #"default"           => x/$XC-TOP-LEFT-ARROW,
      #"busy"              => x/$XC-WATCH,
      #"vertical-scroll"   => x/$XC-SB-V-DOUBLE-ARROW,
      #"horizontal-scroll" => x/$XC-SB-H-DOUBLE-ARROW,
      #"scroll-up"         => x/$XC-SB-UP-ARROW,
      #"scroll-down"       => x/$XC-SB-DOWN-ARROW,
      #"scroll-left"       => x/$XC-SB-LEFT-ARROW,
      #"scroll-right"      => x/$XC-SB-RIGHT-ARROW,
      #"upper-left"        => x/$XC-TOP-LEFT-CORNER,
      #"upper-right"       => x/$XC-TOP-RIGHT-CORNER,
      #"lower-left"        => x/$XC-BOTTOM-LEFT-CORNER,
      #"lower-right"       => x/$XC-BOTTOM-RIGHT-CORNER,
      #"vertical-thumb"    => x/$XC-SB-RIGHT-ARROW,
      #"horizontal-thumb"  => x/$XC-SB-UP-ARROW,
      #"button"            => x/$XC-TOP-LEFT-ARROW,
      #"prompt"            => x/$XC-QUESTION-ARROW,
      #"move"              => x/$XC-FLEUR,
      #"position"          => x/$XC-CROSSHAIR,
      #"i-beam"            => x/$XC-SB-UP-ARROW,
      #"cross"             => x/$XC-CROSSHAIR,
      #"starting"          => x/$XC-CLOCK,
      #"hand"              => x/$XC-I-BEAM };

define sealed method do-set-pointer-cursor
    (_port :: <motif-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  let sheet  = display(pointer-sheet(pointer));
  let window = xt/XtWindowOfObject(mirror-widget(sheet-mirror(sheet)));
  x/XDefineCursor(port.%display, window, realize-cursor(_port, cursor))
end method do-set-pointer-cursor;

define sealed method do-set-sheet-cursor
    (_port :: <motif-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  let window = xt/XtWindowOfObject(mirror-widget(sheet-mirror(sheet)));
  x/XDefineCursor(port.%display, window, realize-cursor(_port, cursor))
end method do-set-sheet-cursor;


define method grab-pointer
    (_port :: <motif-port>, pointer :: <pointer>, sheet :: <sheet>) => ()
  let mirror   = sheet-mirror(sheet);
  let widget   = mirror & mirror-widget(mirror);
  let grabbed? = #f;
  when (widget)
    grabbed?
      := x/XGrabPointer(widget,
			#f,			// owner events
			#[#"pointer-motion", #"button-press", #"button-release"],
			#"grab-mode-asynch",	// pointer mode
			#"grab-mode-asynch",	// keyboard mode
			#"none",		// confine to
			#"none",		// cursor
			#"current-time");
  end;
  grabbed? & #t
end method grab-pointer;

define method ungrab-pointer
    (_port :: <motif-port>, pointer :: <pointer>) => ()
  let sheet  = pointer-grabbed?(pointer);
  let mirror = sheet-mirror(sheet);
  let widget = mirror & mirror-widget(mirror);
  x/XUngrabPointer(widget, #"current-time")
end method ungrab-pointer;


define sealed method realize-cursor
    (_port :: <motif-port>, cursor :: <symbol>) => (x-cursor)
  realize-cursor(_port, gethash($cursor-table, cursor) | x/$XC-TOP-LEFT-ARROW)
end method realize-cursor;

define sealed method realize-cursor
    (_port :: <motif-port>, cursor :: <integer>) => (x-cursor)
  gethash(_port.%cursor-cache, cursor)
  | begin
      let x-cursor = x/XCreateFontCursor(_port.%display, cursor);
      gethash(_port.%cursor-cache, cursor) := x-cursor;
      x-cursor
    end
end method realize-cursor;


/// Focus and carets

define constant $caret-width :: <integer> = 2;

define sealed class <motif-caret> (<basic-caret>)
end class <motif-caret>;

define sealed method make-caret
    (_port :: <motif-port>, sheet :: <sheet>, #key x, y, width, height)
 => (caret :: <motif-caret>)
  make(<motif-caret>,
       port: _port, sheet: sheet,
       x: x | 0, y: y | 0,
       width:  width  | $caret-width,
       height: height | (sheet-line-height(sheet) + sheet-line-spacing(sheet)))
end method make-caret;

define sealed method do-set-caret-position
    (caret :: <motif-caret>, x :: <integer>, y :: <integer>) => ()
  let transform = sheet-device-transform(caret-sheet(caret));
  with-device-coordinates (transform, x, y)
    //---*** SET THE CARET POSITION, e.g., SetCaretPos
  end
end method do-set-caret-position;

define sealed method do-set-caret-size
    (caret :: <motif-caret>, width :: <integer>, height :: <integer>) => ()
  //---*** SET THE CARET SIZE
end method do-set-caret-size;

define sealed method do-show-caret
    (caret :: <motif-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  let sheet  = caret-sheet(caret);
  let widget = sheet & mirror-widget(sheet-mirror(sheet))
  when (widget)
    //---*** SHOW THE CARET, e.g., ShowCaret
  end
end method do-show-caret;

define sealed method do-hide-caret
    (caret :: <motif-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  let sheet  = caret-sheet(caret);
  let widget = sheet & mirror-widget(sheet-mirror(sheet))
  when (widget)
    //---*** SHOW THE CARET, e.g., HideCaret
  end
end method do-hide-caret;


/// Input focus handling

define sealed method note-focus-in
    (_port :: <motif-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-in")
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <motif-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-out")
end method note-focus-out;
