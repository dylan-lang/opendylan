Module:       carbon-duim
Synopsis:     Macintosh port implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic MAC constants

define constant $caret-width :: <integer> = 2;


/// MAC ports

define sealed class <carbon-port> (<basic-port>)
  sealed slot %qd          = #f;
  sealed slot %app-context = #f;
  sealed slot %app-shell   = #f;
  sealed slot %modifier-map :: <simple-object-vector> = #[];
  // Cache for image cursors
  sealed slot %cursor-cache :: <object-table> = make(<object-table>);
  keyword focus-policy: = #"sheet-under-pointer";
end class <carbon-port>;

define sealed method initialize
    (_port :: <carbon-port>, #key server-path) => ()
  next-method();
  let qd = make(<QDGlobals*>);
  _port.%qd := qd;
  with-stack-structure (theWorld :: <SysEnvRec*>)
    SysEnvirons(1, theWorld);
    if (~c-type-cast(<boolean>, theWorld.hasColorQD-value))
      SysBeep(50);
      ExitToShell()
    end
  end;
  InitGraf(qd.thePort-address);
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(null-pointer(<C-void*>));
  InitCursor();
  GetDateTime(qd.randSeed-address);
  /*---*** What to do here?
  install-default-palette(_port);
  install-default-text-style-mappings(_port);
  */
end method initialize;

define sideways method class-for-make-port
    (type == #"carbon", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<carbon-port>, concatenate(initargs, #(event-processor-type:, #"n")))
end method class-for-make-port;

define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"carbon", initargs)
end method class-for-make-port;

define sealed method port-type
    (_port :: <carbon-port>) => (type :: <symbol>)
  #"carbon"
end method port-type;

define sealed method port-name
    (_port :: <carbon-port>) => (name :: false-or(<string>))
  "Carbon Port"
end method port-name;

define sealed method destroy-port
    (_port :: <carbon-port>) => ()
  next-method();
  // release-default-text-style-mappings(_port);
  ignoring("destroy-port")
end method destroy-port;

define function shutdown-carbon-duim ()
  let ports :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-ports(method (_port)
	     when (instance?(_port, <carbon-port>))
	       add!(ports, _port)
	     end
	   end method);
  do(destroy-port, ports)
end function shutdown-carbon-duim;


/// Beeping, etc

define sealed method beep
    (_port :: <carbon-port>) => ()
  ignoring("beep")
end method beep;


/// Pointer position hacking

define sealed method do-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  ignoring("do-pointer-position");
  values(0, 0)
end method do-pointer-position;

define sealed method do-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  ignoring("do-pointer-position");
  values(0, 0)
end method do-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>)
 => ()
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;

define sealed method do-set-pointer-position
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;


/// Pointer cursor hacking

/*---*** Need a MAC version of this...
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
    (_port :: <carbon-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  ignoring("do-set-pointer-cursor")
end method do-set-pointer-cursor;

define sealed method do-set-sheet-cursor
    (_port :: <carbon-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  ignoring("do-set-sheet-cursor")
end method do-set-sheet-cursor;


define method grab-pointer
    (_port :: <carbon-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (success? :: <boolean>)
  let mirror = sheet-mirror(sheet);
  let window = mirror & mirror-window(mirror);
  let result :: <integer> = 0;
  when (window)
    ignoring("grab-pointer")
  end;
  result ~= 0
end method grab-pointer;

define method ungrab-pointer
    (_port :: <carbon-port>, pointer :: <pointer>)
 => (success? :: <boolean>)
  let sheet  = pointer-grabbed?(pointer);
  let mirror = sheet-mirror(sheet);
  let window = mirror & mirror-window(mirror);
  let result = #f;
  if (window)
    ignoring("ungrab-pointer");
    #t
  end
end method ungrab-pointer;


define sealed method realize-cursor
    (_port :: <carbon-port>, cursor :: <symbol>) => (carbon-cursor)
  ignoring("realize-cursor")
end method realize-cursor;

define sealed method realize-cursor
    (_port :: <carbon-port>, cursor :: <integer>) => (carbon-cursor)
  gethash(_port.%cursor-cache, cursor)
  | begin
      ignoring("realize-cursor")
    end
end method realize-cursor;


/// Focus and carets

define sealed class <carbon-caret> (<basic-caret>)
end class <carbon-caret>;

define sealed method make-caret
    (_port :: <carbon-port>, sheet :: <sheet>, #key x, y, width, height)
 => (caret :: <carbon-caret>)
  make(<carbon-caret>,
       port: _port, sheet: sheet,
       x: x | 0, y: y | 0,
       width:  width  | $caret-width,
       height: height | (sheet-line-height(sheet) + sheet-line-spacing(sheet)))
end method make-caret;

define sealed method do-set-caret-position
    (caret :: <carbon-caret>, x :: <integer>, y :: <integer>) => ()
  let transform = sheet-device-transform(caret-sheet(caret));
  with-device-coordinates (transform, x, y)
    ignoring("do-set-caret-position")
  end
end method do-set-caret-position;

define sealed method do-set-caret-size
    (caret :: <carbon-caret>, width :: <integer>, height :: <integer>) => ()
  ignoring("do-set-caret-size")
end method do-set-caret-size;

define sealed method do-show-caret
    (caret :: <carbon-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  let sheet  = caret-sheet(caret);
  let window = sheet & mirror-window(sheet-mirror(sheet));
  when (window)
    ignoring("do-show-caret")
  end
end method do-show-caret;

define sealed method do-hide-caret
    (caret :: <carbon-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  let sheet  = caret-sheet(caret);
  let window = sheet & mirror-window(sheet-mirror(sheet));
  when (window)
    ignoring("do-hide-caret")
  end
end method do-hide-caret;


/// Input focus handling

define sealed method note-focus-in
    (_port :: <carbon-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-in")
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <carbon-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-out")
end method note-focus-out;


/// Port defaults

define method port-default-foreground
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>))
  query-window-for-color(sheet, #"foreground")
end method port-default-foreground;

// Most sheets should show up with the standard 3d gray background...
define method port-default-background
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>));
  query-window-for-color(sheet, #"background")
end method port-default-background;

// ...but drawing panes should defaultly have a white background
define method port-default-background
    (_port :: <carbon-port>, sheet :: <drawing-pane>)
 => (background :: false-or(<ink>));
  $white
end method port-default-background;

define method query-window-for-color
    (sheet :: <sheet>, key :: one-of(#"foreground", #"background"))
 => (color :: false-or(<ink>))
  ignoring("query-window-for-color");
  let mirror = sheet-mirror(sheet);
  let window = mirror & mirror-window(mirror);
  when (window)
    #f
    // query-pixel-for-color(xt/XtGetValues(window, key), port-default-palette(_port))
  end
end method query-window-for-color;


//---*** WHAT TO DO ABOUT THIS?
define constant $carbon-default-text-style
    = make(<text-style>,
	   family: #"sans-serif", weight: #"normal",
	   slant: #"roman", size: #"normal");

// Note that this "default default" text style is _not_ the one that we use
// for gadgets.  There's another method for that on <carbon-gadget-mixin>.
define method port-default-text-style
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (text-style :: false-or(<text-style>))
  $carbon-default-text-style
end method port-default-text-style;
