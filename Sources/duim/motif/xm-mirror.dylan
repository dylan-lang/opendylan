Module:    motif-duim
Synopsis:  Motif mirror implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif mirrors

define open abstract class <motif-mirror> (<mirror>)
  sealed slot mirror-sheet :: <sheet>,
    required-init-keyword: sheet:;
  sealed slot mirror-widget = #f,
    init-keyword: widget:;
end class <motif-mirror>;

define protocol <<motif-mirror-protocol>> ()
  function install-event-handlers
    (mirror :: <motif-mirror>) => ();
end protocol <<motif-mirror-protocol>>;


/// Empty methods on non-window mirrors

define sealed method mirror-edges
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <motif-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, 100, 100)	//--- kludge city
end method mirror-edges;

// The real methods are on more specific classes, such as <window-mirror>
define sealed method set-mirror-edges
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <motif-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  #f
end method set-mirror-edges;

// Ditto...
define sealed method map-mirror
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <motif-mirror>) => ()
  #f
end method map-mirror;

// Ditto...
define sealed method unmap-mirror
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <motif-mirror>) => ()
  #f
end method unmap-mirror;

// Ditto...
define sealed method destroy-mirror 
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <motif-mirror>) => ()
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;


/// Mirror creation and destruction

define sealed class <window-mirror> (<motif-mirror>)
end class <window-mirror>;

define sealed domain make (singleton(<window-mirror>));
define sealed domain initialize (<window-mirror>);

// A scrolled widget contains a work widget (list-pane or text-editor)
// which represent a DUIM gadget.  The scrolled widget surrounding the work
// widget is what needs to reflect the desired size of the DUIM gadget.  To
// keep everything consistent for 'compose-space' and 'allocate-space', the
// widget is made the scrolled widget.  The work-widget is only referenced
// in the backend, so we can be careful.
define sealed class <scrolled-window-mirror> (<window-mirror>)
  sealed slot mirror-work-widget = #f,
    init-keyword: work-widget:;
end class <scrolled-mirror>;

define sealed domain make (singleton(<scrolled-window-mirror>));
define sealed domain initialize (<scrolled-window-mirror>);

// This is intentionally _not_ sealed
define sealed method do-make-mirror
    (_port :: <motif-port>, sheet :: <sheet>)
 => (mirror :: <window-mirror>)
  let frame   = sheet-frame(sheet);
  let pmirror = sheet-direct-mirror(sheet-device-parent(sheet));
  let pwidget = mirror-widget(dmirror);
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let width   = right - left;
  let height  = bottom - top;
  let pane-name = if (viewport?(sheet)) sheet-child(sheet) else sheet end;
  let resource  = make-resource-name(pane-name);
  let widget
    = xt/XtCreateManagedWidget(resource, 	// "DUIMSheet",
			       xm/<drawing-area>,
			       pwidget,
			       resources:
				 vector(mapped-when-managed: #t,
					resize-policy:, xm/$XmRESIZE-NONE,
					x:, x, y:, y,
					width: width, height: height,
					margin-width: 0, margin-height: 0,
					border-width: 0));
  let mirror
    = make(<window-mirror>,
	   widget: widget,
	   sheet:  sheet);
  install-event-handlers(mirror);
  xt/add-widget-destroy-callback(widget, destroy-mirror-callback, mirror);
  xt/XtRealizeWidget(widget);
  mirror
end method do-make-mirror;

define function destroy-mirror-callback (mirror) => ()
  mirror-widget(mirror) := #f
end function destroy-mirror-callback;


// This is only ever called on mirrored sheets
define method erase-background
    (sheet :: <sheet>, mirror :: <window-mirror>, hDC :: <HDC>) => ()
  let (width, height) = sheet-size(sheet);
  //---*** Now clear the rectangle (0,0, width,height)
  //---*** presumably by drawing in the background color
  //---*** Do we need to restore the DC afterwards?
end method erase-background;

define sealed method destroy-mirror 
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let widget = mirror-widget(mirror);
  mirror-widget(mirror) := #f;
  xt/XtDestroyWidget(widget)
end method destroy-mirror;

//---*** WHAT ABOUT THIS?  WHO IS SUPPOSED TO CALL IT?
// Called by main WM_DESTROY handler
define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let handle :: <HWND> = window-handle(mirror);
  window-mirror(handle) := #f;
  window-handle(mirror) := $NULL-HWND
end method note-mirror-destroyed;


/// Mirror manipulation

// For non-top-level sheets, we just show the window
define sealed method map-mirror
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let widget = mirror-widget(mirror);
  xt/XtMapWidget(widget)
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let widget = mirror-widget(mirror);
  xt/XtUnmapWidget(widget)
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let widget = mirror-widget(mirror);
  x/XRaiseWindow(_port.%display, x/XtWindow(widget))
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let widget = mirror-widget(mirror);
  x/XLowerWindow(_port.%display, x/XtWindow(widget))
end method lower-mirror;

define sealed method mirror-visible? 
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (visible? :: <boolean>)
  let widget = mirror-widget(mirror);
  xt/XtIsRealized(widget) & #t
end method mirror-visible?;


/// Window mirrors

define sealed method mirror-edges
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let widget = mirror-widget(mirror);
  let (x, y)          = xt/XtGetValues(widget, #"x", #"y");
  let (width, height) = xt/XtGetValues(widget, #"width", #"height");
  values(x, y, x + width, y + height)
end method mirror-edges;

define sealed method set-mirror-edges
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <window-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  let widget = mirror-widget(mirror);
  let width  = right  - left;
  let height = bottom - top;
  let (ol, ot, or, ob) = mirror-edges(_port, sheet, mirror);
  let (ow, oh) = values(or - ol, ob - ot);
  if (width = ow & height = oh)
    xt/XtMoveWidget(widget, left, top)
  else
    xt/XtConfigureWidget(widget, left, top, width, height, 0)
  end
end method set-mirror-edges;


// Returns the position of the sheet in "absolute" (screen) coordinates
define sealed method sheet-screen-position
    (_port :: <motif-port>, sheet :: <sheet>)
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
  //---*** DO THIS FOR REAL
  values(x, y)
end method client-to-screen-position;


/// Motif panes

define open abstract class <motif-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <motif-pane-mixin>;

// Returns #t, meaning that the port will take care of repainting gadgets
define method port-handles-repaint?
    (_port :: <motif-port>, sheet :: <motif-pane-mixin>) => (true? :: <boolean>)
  #t
end method port-handles-repaint?;

// Returns #f, because even though Motif generates repaint events for drawing panes,
// we still need to ensure the contents get drawn
define method port-handles-repaint?
    (_port :: <motif-port>, sheet :: <drawing-pane>) => (true? :: <boolean>)
  #f
end method port-handles-repaint?;


/// Port defaults

define method port-default-foreground
    (_port :: <motif-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>))
  query-widget-for-color(_port, sheet, #"foreground")
end method port-default-foreground;

// Most sheets should show up with the standard 3d gray background...
define method port-default-background
    (_port :: <motif-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>));
  query-widget-for-color(_port, sheet, #"background")
end method port-default-background;

// ...but drawing panes should defaultly have a white background
define method port-default-background
    (_port :: <motif-port>, sheet :: <drawing-pane>)
 => (background :: false-or(<ink>));
  $white
end method port-default-background;

define method query-widget-for-color
    (sheet :: <sheet>, key :: one-of(#"foreground", #"background"))
 => (color :: false-or(<ink>))
  let mirror = sheet-mirror(sheet);
  let widget = mirror & mirror-widget(mirror);
  when (widget)
    query-pixel-for-color(xt/XtGetValues(widget, key), port-default-palette(_port))
  end
end method query-widget-for-color;


//---*** WHAT TO DO ABOUT THIS?

// FYI, the normal size on Motif is 8-points
// We arrange to map this to something close to ANSI_VAR_FONT
define constant $motif-default-text-style
    = make(<text-style>,
	   family: #"sans-serif", weight: #"normal",
	   slant: #"roman", size: #"normal");

// Note that this "default default" text style is _not_ the one that we use
// for gadgets.  There's another method for that on <motif-gadget-mixin>.
define method port-default-text-style
    (_port :: <motif-port>, sheet :: <sheet>)
 => (text-style :: false-or(<text-style>))
  $motif-default-text-style
end method port-default-text-style;
