Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Display

define constant <display-units> = one-of(#"device", #"pixels", #"mm");

define protocol <<display-protocol>> ()
  // Making displays
  function port-display-class (_port :: <port>) => (class);
  function display-matches-characteristics?
    (display :: <abstract-display>, #key, #all-keys)
 => (true? :: <boolean>);
  function initialize-display
    (port :: <abstract-port>, display :: <abstract-display>) => ();
  // Display characteristics
  function display-width
    (display :: <abstract-display>, #key units) => (width);
  function display-height
    (display :: <abstract-display>, #key units) => (height);
  function display-depth
    (display :: <abstract-display>) => (depth);
  getter display-units
    (display :: <abstract-display>) => (units :: <display-units>);
  getter display-orientation
    (display :: <abstract-display>) => (orientation);
  getter display-mm-height
    (display :: <abstract-display>) => (number);
  setter display-mm-height-setter
    (height, display :: <abstract-display>) => (number);
  getter display-mm-width
    (display :: <abstract-display>) => (number);
  setter display-mm-width-setter
    (width, display :: <abstract-display>) => (number);
  getter display-pixel-width
    (display :: <abstract-display>) => (integer);
  setter display-pixel-width-setter
    (width, display :: <abstract-display>) => (integer);
  getter display-pixel-height
    (display :: <abstract-display>) => (integer);
  setter display-pixel-height-setter
    (height, display :: <abstract-display>) => (integer);
  getter display-pixels-per-point
    (display :: <abstract-display>) => (number);
  setter display-pixels-per-point-setter
    (pixels, display :: <abstract-display>) => (number);
  // Attach top-level sheets to the display
  function sheet-attached?
    (sheet :: <abstract-sheet>) => (attached? :: <boolean>);
  function attach-sheet
    (display :: <abstract-display>, sheet :: <abstract-sheet>,
     #rest pane-options, #key frame-manager, sheet-class, #all-keys)
    => (top-sheet :: <abstract-sheet>);
  function note-sheet-attached (sheet :: <abstract-sheet>) => ();
  function do-note-sheet-attached (sheet :: <abstract-sheet>) => ();
  function detach-sheet
    (display :: <abstract-display>, sheet :: <abstract-sheet>)
 => (top-sheet :: false-or(<abstract-sheet>));
  function note-sheet-detached (sheet :: <abstract-sheet>) => ();
  function do-note-sheet-detached (sheet :: <abstract-sheet>) => ();
end protocol <<display-protocol>>;


define open abstract primary class <basic-display>
    (<mirrored-sheet-mixin>,
     <multiple-child-mixin>,
     <basic-sheet>,
     <display>)
  //--- If we fix sheets to have a %display slot, we'll need this slot here
  // slot %port :: false-or(<port>) = #f, ...
  sealed constant slot display-lock :: <simple-lock> = make(<simple-lock>);
  sealed slot display-orientation = #"default",
    init-keyword: orientation:;
  sealed slot display-depth = 8;
  sealed slot display-units :: <display-units> = #"device",
    init-keyword: units:;
  sealed slot display-pixel-width  = 1024;
  sealed slot display-pixel-height =  768;
  sealed slot display-mm-width = #f  = 1024;
  sealed slot display-mm-height = #f =  768;
  sealed slot display-pixels-per-point = 1;
end class <basic-display>;

define sealed class <standard-display> (<basic-display>)
end class <standard-display>;

define sealed domain make (singleton(<standard-display>));
define sealed domain initialize (<standard-display>);

define sealed inline method make
    (class == <display>, #rest initargs, #key)
 => (display :: <standard-display>)
  apply(make, <standard-display>, initargs)
end method make;

define method initialize (_display :: <display>, #key port: _port)
  next-method();
  add!(port-displays(_port), _display);
  initialize-display(_port, _display)
end method initialize;

define method display (_display :: <basic-display>) => (display :: <display>)
  _display
end method display;


// Some ports might not have a mirror for the display, so provide a
// reasonable base device transform for the display
define method sheet-device-transform
    (_display :: <basic-display>) => (transform :: <transform>)
  $identity-transform
end method sheet-device-transform;

define method display-width
    (_display :: <basic-display>, #key units = #"device") => (width)
  select (units)
    #"device" => box-width(sheet-region(_display));
    #"pixels" => display-pixel-width(_display);
    #"mm"     => display-mm-width(_display);
  end
end method display-width;

define method display-height
    (_display :: <basic-display>, #key units = #"device") => (height)
  select (units)
    #"device" => box-height(sheet-region(_display));
    #"pixels" => display-pixel-height(_display);
    #"mm"     => display-mm-height(_display);
  end
end method display-height;


//--- Maybe this should override 'sheet-state' instead?
define method sheet-mapped?
    (_display :: <basic-display>) => (mapped? :: <boolean>)
  #t
end method sheet-mapped?;

define method sheet-mapped?-setter
    (mapped? :: <boolean>, _display :: <basic-display>, #key do-repaint?, clear?)
 => (mapped? :: <boolean>)
  ignore(do-repaint?, clear?);
  unless (mapped?)
    error("Attempting to unmap the display %=!", _display)
  end;
  mapped?
end method sheet-mapped?-setter;


define method sheet-layed-out?
    (display :: <basic-display>) => (layed-out? :: <boolean>)
  #t
end method sheet-layed-out?;

define method sheet-layed-out?-setter
    (layed-out? :: <boolean>, display :: <basic-display>)
 => (layed-out? :: <boolean>)
  layed-out?
end method sheet-layed-out?-setter;


/// Making displays

define macro with-display-locked
  { with-display-locked (?object:expression) ?:body end }
    => { begin
	   let _display = display(?object);
	   with-lock (display-lock(_display))
	     ?body;
	   failure
	     error("Couldn't get display lock for %=", _display);
	   end
	 end }
end macro with-display-locked;

define inline function do-displays (function :: <function>, _port :: <port>) => ()
  dynamic-extent(function);
  do(function, port-displays(_port))
end function do-displays;

define method find-display
    (#key server-path, port: _port,
     orientation = #"default", units = #"device") => (display :: <display>)
  block (return)
    unless (_port)
      _port := default-port(server-path: server-path)
    end;
    local method match-display (_display) => ()
	    when (display-matches-characteristics?
		    (_display, orientation: orientation, units: units))
	      return(_display)
	    end
	  end method;
    dynamic-extent(match-display);
    do-displays(match-display, _port);
    make(port-display-class(_port),
	 port: _port,
	 orientation: orientation, units: units)
  end
end method find-display;

define method display-matches-characteristics?
    (_display :: <display>, #key orientation, units) => (true? :: <boolean>)
  ignore(orientation, units);
  #t
end method display-matches-characteristics?;


// Displays are always mirrored, so we don't need to propagate the
// region and transformation invalidations down the sheet hierarchy
define method invalidate-cached-regions (display :: <display>) => ()
  invalidate-cached-region(display)
end method invalidate-cached-regions;

define method invalidate-cached-transforms (display :: <display>) => ()
  invalidate-cached-transform(display)
end method invalidate-cached-transforms;


define method update-mirror-region
    (_port :: <port>, sheet :: <basic-display>, mirror) => ()
  // We don't currently ever ever change the region of a display...
  #f
end method update-mirror-region;

define method update-mirror-transform
    (_port :: <port>, sheet :: <basic-display>, mirror) => ()
  // We don't currently ever ever change the transformation of a display...
  #f
end method update-mirror-transform;


define method port-display-class (_port :: <port>) => (class)
  <standard-display>
end method port-display-class;


/// Attaching sheets to displays

// Attach a sheet to a display, interposing a top-level sheet
define method attach-sheet
    (_display :: <display>, sheet :: <sheet>,
     #rest pane-options,
     #key frame-manager: framem, sheet-class)
 => (top-sheet :: <sheet>)
  dynamic-extent(pane-options);
  assert(framem,
	 "You need to supply a frame manager for the time being");
  let top-sheet
    = with-frame-manager (framem)
        with-keywords-removed (pane-options = pane-options, #[sheet-class:])
	  apply(make-pane,
		sheet-class,
		region: begin
			  let (width, height) = box-size(sheet);
			  make-bounding-box(0, 0, width, height)
			end,
		parent: _display,
		pane-options)
        end
      end;
  // Attaching the top-level pane given by the layout to the top-level
  // sheet has the effect of attaching the whole tree to the display,
  // which in turn mirrors the whole tree.
  add-child(top-sheet, sheet);
  top-sheet
end method attach-sheet;

// Note that the sheet as been attached to the display
define method note-sheet-attached (sheet :: <sheet>) => ()
  do-note-sheet-attached(sheet)
end method note-sheet-attached;

define method do-note-sheet-attached (sheet :: <sheet>) => ()
  #f
end method do-note-sheet-attached;


// Detach a sheet from its display
define method detach-sheet
    (_display :: <display>, sheet :: <sheet>)
 => (top-sheet :: false-or(<sheet>))
  let top-sheet = top-level-sheet(sheet);
  when (top-sheet)
    assert(sheet-parent(top-sheet) == _display,
	   "The parent of the top-level sheet should be the display");
    remove-child(sheet-parent(top-sheet), top-sheet)
  end;
  top-sheet
end method detach-sheet;

// Note that the sheet as been detached from the display
define method note-sheet-detached (sheet :: <sheet>) => ()
  do-note-sheet-detached(sheet)
end method note-sheet-detached;

define method do-note-sheet-detached (sheet :: <sheet>) => ()
  #f
end method do-note-sheet-detached;


// Is the sheet attached to the display?
define sealed inline method sheet-attached?
    (sheet :: <basic-sheet>) => (mapped? :: <boolean>)
  // We can tell if it's attached by seeing if its port is set
  // A slower way to do this is 'display?(parent) | sheet-attached?(parent)'
  port(sheet) & #t
end method sheet-attached?;
