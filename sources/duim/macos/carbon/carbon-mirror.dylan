Module:       carbon-duim
Synopsis:     Macintosh mirror implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MAC panes

define open abstract class <carbon-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <carbon-pane-mixin>;

// Returns #t, meaning that the port will take care of repainting
define method port-handles-repaint?
    (_port :: <carbon-port>, sheet :: <mirrored-sheet-mixin>)
 => (true? :: <boolean>)
  #t
end method port-handles-repaint?;


/// MAC mirrors

define open abstract class <carbon-mirror> (<mirror>)
  sealed slot mirror-sheet :: <sheet>,
    required-init-keyword: sheet:;
end class <carbon-mirror>;

define method initialize
    (mirror :: <carbon-mirror>, #key) => ()
  next-method();
  sheet-direct-mirror(mirror-sheet(mirror)) := mirror;
end method initialize;

define protocol <<carbon-mirror-protocol>> ()
  function make-carbon-mirror
    (sheet :: <abstract-sheet>) => (mirror :: <carbon-mirror>);
  function install-event-handlers
    (sheet :: <abstract-sheet>, mirror :: <carbon-mirror>) => ();
  function update-mirror-attributes
    (sheet :: <abstract-sheet>, mirror :: <carbon-mirror>) => ();
  function set-mirror-parent
    (mirror :: <carbon-mirror>, parent :: <carbon-mirror>) => ();
  function move-mirror
    (parent :: <carbon-mirror>, mirror :: <carbon-mirror>, 
     x :: <integer>, y :: <integer>)
 => ();
  function size-mirror
    (parent :: <carbon-mirror>, mirror :: <carbon-mirror>, 
     width :: <integer>, height :: <integer>)
 => ();
end protocol <<carbon-mirror-protocol>>;

define constant $mirror-window-table :: <object-table> = make(<table>);

define sealed method do-make-mirror
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (mirror :: <carbon-mirror>)
  let parent = sheet-device-parent(sheet);
  let mirror = make-carbon-mirror(sheet);
  install-event-handlers(sheet, mirror);
  update-mirror-attributes(sheet, mirror);
  set-mirror-parent(mirror, sheet-direct-mirror(parent));
  mirror
end method do-make-mirror;

define sealed method window-mirror
    (window :: <C-pointer>) => (mirror :: false-or(<carbon-mirror>))
  element($mirror-window-table, pointer-address(window), default: #f)
end method window-mirror;

define sealed method window-mirror-setter
    (mirror :: <carbon-mirror>, window :: <C-pointer>)
 => (mirror :: <carbon-mirror>)
  element($mirror-window-table, pointer-address(window)) := mirror
end method window-mirror-setter;

define sealed method window-mirror-setter
    (mirror == #f, window :: <C-pointer>) => (mirror == #f)
  remove-key!($mirror-window-table, pointer-address(window));
  #f
end method window-mirror-setter;


/// Empty methods on non-window mirrors

define sealed method mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, 100, 100)	//--- kludge city
end method mirror-edges;

// The real methods are on more specific classes, such as <window-mirror>
define sealed method set-mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  #f
end method set-mirror-edges;

// Ditto...
define sealed method map-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  #f
end method map-mirror;

// Ditto...
define sealed method unmap-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  #f
end method unmap-mirror;

// Ditto...
define sealed method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

// Ditto...
define method install-event-handlers
    (sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  #f
end method install-event-handlers;

// Ditto...
define method update-mirror-attributes
    (sheet :: <sheet>, mirror :: <carbon-mirror>) => ()
  #f
end method update-mirror-attributes;


/// Mirror creation and destruction

define abstract class <window-mirror> (<carbon-mirror>)
  sealed slot mirror-window :: false-or(<WindowPtr>) = #f,
    init-keyword: window:;
  sealed slot %region :: <bounding-box>,
    required-init-keyword: region:;
end class <window-mirror>;

define sealed domain make (singleton(<window-mirror>));
define sealed domain initialize (<window-mirror>);

define sealed inline method make
    (mirror :: subclass(<window-mirror>), #rest args, #key sheet)
 => (mirror :: <window-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  apply(next-method, mirror,
	region: make-bounding-box(left, top, right, bottom),
	args)
end method make;

define method initialize
    (mirror :: <window-mirror>, #key) => ()
  next-method();
  let window = mirror-window(mirror);
  when (window)
    window-mirror(window) := mirror
  end
end method initialize;

define sealed method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let window = mirror-window(mirror);
  mirror-window(mirror) := #f;
  ignoring("destroy-mirror")
end method destroy-mirror;

//---*** WHAT ABOUT THIS?  WHO IS SUPPOSED TO CALL IT?
// Called by main WM_DESTROY handler
define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <window-mirror>) => ()
  ignoring("note-mirror-destroyed")
  // let handle :: <HWND> = window-handle(mirror);
  // window-mirror(handle) := #f;
  // window-handle(mirror) := $NULL-HWND
end method note-mirror-destroyed;


/// Mirror manipulation

// For non-top-level sheets, we just show the window
define sealed method map-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let window = mirror-window(mirror);
  debug-message("Showing %=", sheet);
  ignoring("map-mirror")
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let window = mirror-window(mirror);
  ignoring("unmap-mirror")
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>,
     #key activate? = #t)
 => ()
  if (activate?)
    ignoring("activate? keyword to raise-mirror")
  end;
  let window = mirror-window(mirror);
  ignoring("raise-mirror")
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let window = mirror-window(mirror);
  ignoring("lower-mirror")
end method lower-mirror;

define sealed method mirror-visible? 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (visible? :: <boolean>)
  let window = mirror-window(mirror);
  ignoring("mirror-visible?");
  #t
end method mirror-visible?;


/// Window mirrors

define sealed method mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

define sealed method set-mirror-edges
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <window-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => ()
  let parent = sheet-device-parent(sheet);
  let parent-mirror = sheet-direct-mirror(parent);
  let width  = right - left;
  let height = bottom - top;
  let old-region = mirror.%region;
  let (old-left, old-top)     = box-position(old-region);
  let (old-width, old-height) = box-size(old-region);
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom);
  if (left ~== old-left | top ~== old-top)
    move-mirror(parent-mirror, mirror, left, top)
  end;
  if (width ~== old-width | height ~== old-height)
    size-mirror(parent-mirror, mirror, width, height)
  end
end method set-mirror-edges;


// Returns the position of the sheet in "absolute" (screen) coordinates
define sealed method sheet-screen-position
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let ancestor  = sheet-device-parent(sheet);
  let transform = sheet-delta-transform(sheet, ancestor);
  // Get the position of the sheet in its mirrored parent's coordinates
  let (x, y) = transform-position(transform, 0, 0);
  let mirror = sheet-direct-mirror(ancestor);
  client-to-screen-position(mirror, x, y)
end method sheet-screen-position;

// Given a position (x, y) within a mirror, convert it to a position on the screen
define sealed method client-to-screen-position
    (mirror :: <window-mirror>, x :: <integer>, y :: <integer>)
 => (screen-x :: <integer>, screen-y :: <integer>)
  ignoring("client-to-screen-position");
  values(x, y)
end method client-to-screen-position;


/// Fixed container mirrors
///
/// The class of mirror that can contain other mirrors

define class <fixed-container-mirror> (<window-mirror>)
end class <fixed-container-mirror>;

define class <drawing-area-mirror> (<window-mirror>)
end class <drawing-area-mirror>;

define method make-carbon-mirror
    (sheet :: <mirrored-sheet-mixin>)
 => (mirror :: <window-mirror>)
  not-yet-implemented("make-carbon-mirror")
end method make-carbon-mirror;

define method set-mirror-parent
    (child :: <window-mirror>, parent :: <fixed-container-mirror>)
 => ()
  ignoring("set-mirror-parent");
end method set-mirror-parent;
    
define method move-mirror
    (parent :: <fixed-container-mirror>, child :: <window-mirror>,
     x :: <integer>, y :: <integer>)
 => ()
  ignoring("move-mirror");
end method move-mirror;

define method size-mirror
    (parent :: <fixed-container-mirror>, child :: <window-mirror>,
     width :: <integer>, height :: <integer>)
 => ()
  ignore(parent);
  ignoring("size-mirror");
end method size-mirror;
