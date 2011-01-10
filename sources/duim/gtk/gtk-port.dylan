Module:       gtk-duim
Synopsis:     GTK port implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic GTK constants

define constant $caret-width :: <integer> = 2;


/// GTK ports

define sealed class <gtk-port> (<basic-port>)
  sealed slot %app-context = #f;
  sealed slot %app-shell   = #f;
  sealed slot %modifier-map :: <simple-object-vector> = #[];
  // Cache for image cursors
  sealed slot %cursor-cache :: <object-table> = make(<object-table>);
  keyword focus-policy: = #"sheet-under-pointer";
end class <gtk-port>;

define sealed method initialize
    (_port :: <gtk-port>, #key server-path) => ()
  initialize-gtk();
  next-method();
end method initialize;

register-port-class(#"gtk", <gtk-port>, default?: #t);

define sideways method class-for-make-port
    (type == #"gtk", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<gtk-port>,
         concatenate(initargs,
                     list(event-processor-type:,
                          if ($os-name == #"win32") #"n" else #"n+1" end)))
end method class-for-make-port;

define sealed method port-type
    (_port :: <gtk-port>) => (type :: <symbol>)
  #"gtk"
end method port-type;

define sealed method port-name
    (_port :: <gtk-port>) => (name :: false-or(<string>))
  "No Port Name"
end method port-name;

define sealed method destroy-port
    (_port :: <gtk-port>) => ()
  next-method();
  // release-default-text-style-mappings(_port);
  ignoring("destroy-port")
end method destroy-port;

define function shutdown-gtk-duim ()
  let ports :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-ports(method (_port)
	     when (instance?(_port, <gtk-port>))
	       add!(ports, _port)
	     end
	   end method);
  do(destroy-port, ports)
end function shutdown-gtk-duim;


/// Beeping, etc

define sealed method beep
    (_port :: <gtk-port>) => ()
  gdk-beep()
end method beep;


/// Pointer position hacking

define sealed method do-pointer-position
    (_port :: <gtk-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  ignoring("do-pointer-position");
  values(0, 0)
end method do-pointer-position;

define sealed method do-pointer-position
    (_port :: <gtk-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  ignoring("do-pointer-position");
  values(0, 0)
end method do-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <gtk-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>)
 => ()
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <gtk-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;


/// Pointer cursor hacking

/*---*** Need a GTK version of this...
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
*/

define sealed method do-set-pointer-cursor
    (_port :: <gtk-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  ignoring("do-set-pointer-cursor")
end method do-set-pointer-cursor;

define sealed method do-set-sheet-cursor
    (_port :: <gtk-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  ignoring("do-set-sheet-cursor")
end method do-set-sheet-cursor;


define method grab-pointer
    (_port :: <gtk-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (success? :: <boolean>)
  let mirror = sheet-mirror(sheet);
  let widget = mirror & mirror-widget(mirror);
  let result :: <integer> = 0;
  when (widget)
    //---*** Get real current time...
    let current-time = 0;
    result
      := with-gdk-lock gdk-pointer-grab(widget,
                                        0,		// owner events
                                        logior($GDK-POINTER-MOTION-MASK,
                                               $GDK-BUTTON-PRESS-MASK,
                                               $GDK-BUTTON-RELEASE-MASK),
                                        null-pointer(<GdkWindow>),		// confine to
                                        null-pointer(<GdkCursor>),		// cursor
                                        current-time) end;
  end;
  result ~= 0
end method grab-pointer;

define method ungrab-pointer
    (_port :: <gtk-port>, pointer :: <pointer>)
 => (success? :: <boolean>)
  let sheet  = pointer-grabbed?(pointer);
  let mirror = sheet-mirror(sheet);
  let widget = mirror & mirror-widget(mirror);
  let result = #f;
  if (widget)
    //---*** How do we get the current time?
    let current-time = 0;
    with-gdk-lock gdk-pointer-ungrab(current-time) end;
    #t
  end
end method ungrab-pointer;


define sealed method realize-cursor
    (_port :: <gtk-port>, cursor :: <symbol>) => (gtk-cursor)
  ignoring("realize-cursor")
end method realize-cursor;

define sealed method realize-cursor
    (_port :: <gtk-port>, cursor :: <integer>) => (gtk-cursor)
  gethash(_port.%cursor-cache, cursor)
  | begin
      ignoring("realize-cursor")
    end
end method realize-cursor;


/// Focus and carets

define sealed class <gtk-caret> (<basic-caret>)
end class <gtk-caret>;

define sealed method make-caret
    (_port :: <gtk-port>, sheet :: <sheet>, #key x, y, width, height)
 => (caret :: <gtk-caret>)
  make(<gtk-caret>,
       port: _port, sheet: sheet,
       x: x | 0, y: y | 0,
       width:  width  | $caret-width,
       height: height | (sheet-line-height(sheet) + sheet-line-spacing(sheet)))
end method make-caret;
define sealed method do-set-caret-position
    (caret :: <gtk-caret>, x :: <integer>, y :: <integer>) => ()
  ignoring("do-set-caret-position");
end method do-set-caret-position;

define sealed method do-set-caret-size
    (caret :: <gtk-caret>, width :: <integer>, height :: <integer>) => ()
  ignoring("do-set-caret-size");
end method do-set-caret-size;

define sealed method do-show-caret
    (caret :: <gtk-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  let sheet  = caret-sheet(caret);
  let widget = sheet & mirror-widget(sheet-mirror(sheet));
  let (x, y) = caret-position(caret);
  let (width, height) = caret-size(caret);
  when (widget)
    draw-rectangle(sheet-medium(sheet), x, y, x + width, y + height, filled?: #t);
  end
end method do-show-caret;

define sealed method do-hide-caret
    (caret :: <gtk-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  let sheet  = caret-sheet(caret);
  let widget = sheet & mirror-widget(sheet-mirror(sheet));
  when (widget)
    let (x, y) = caret-position(caret);
    let (width, height) = caret-size(caret);
    clear-box(sheet-medium(sheet), x, y, x + width, y + height);
    // queue-repaint(sheet, make(<general-box>, left: x, right: x + width, top: y + height, bottom: y));
  end
end method do-hide-caret;


/// Input focus handling

define sealed method note-focus-in
    (_port :: <gtk-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-in")
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <gtk-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-out")
end method note-focus-out;


/// Port defaults

define method port-default-foreground
    (_port :: <gtk-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>))
  query-widget-for-color(sheet, #"foreground")
end method port-default-foreground;

// Most sheets should show up with the standard 3d gray background...
define method port-default-background
    (_port :: <gtk-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>));
  query-widget-for-color(sheet, #"background")
end method port-default-background;

// ...but drawing panes should defaultly have a white background
define method port-default-background
    (_port :: <gtk-port>, sheet :: <drawing-pane>)
 => (background :: false-or(<ink>));
  $white
end method port-default-background;

define method query-widget-for-color
    (sheet :: <sheet>, key :: one-of(#"foreground", #"background"))
 => (color :: false-or(<ink>))
  ignoring("query-widget-for-color");
  let mirror = sheet-mirror(sheet);
  let widget = mirror & mirror-widget(mirror);
  when (widget)
    #f
    // query-pixel-for-color(xt/XtGetValues(widget, key), port-default-palette(_port))
  end
end method query-widget-for-color;


//---*** WHAT TO DO ABOUT THIS?

// FYI, the normal size on GTK is 8-points
// We arrange to map this to something close to ANSI_VAR_FONT
define constant $gtk-default-text-style
    = make(<text-style>,
	   family: #"sans-serif", weight: #"normal",
	   slant: #"roman", size: #"normal");

// Note that this "default default" text style is _not_ the one that we use
// for gadgets.  There's another method for that on <gtk-gadget-mixin>.
define method port-default-text-style
    (_port :: <gtk-port>, sheet :: <sheet>)
 => (text-style :: false-or(<text-style>))
  $gtk-default-text-style
end method port-default-text-style;
