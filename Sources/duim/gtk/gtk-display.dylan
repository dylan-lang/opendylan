Module:       gtk-duim
Synopsis:     GTK display implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK displays, i.e., the screen

define constant $points-per-mm = 72.0 / 25.4;

define sealed class <display-mirror> (<gtk-mirror>)
end class <display-mirror>;

// Mirror the display, set its region, and set its characteristics
define sealed method initialize-display 
    (_port :: <gtk-port>, _display :: <standard-display>) => ()
  let mirror
    = make(<display-mirror>,
	   sheet:  _display);
  let mm-width     = gdk-screen-width-mm();
  let mm-height    = gdk-screen-height-mm();
  let pixel-width  = gdk-screen-width();
  let pixel-height = gdk-screen-height();
  display-pixel-width(_display)  := pixel-width;
  display-pixel-height(_display) := pixel-height;
  display-mm-width(_display)     := mm-width;
  display-mm-height(_display)    := mm-height;
  display-pixels-per-point(_display)
    := sqrt(  (pixel-width  / (mm-width  * $points-per-mm))
	    * (pixel-height / (mm-height * $points-per-mm)));
  sheet-region(_display)
    := set-box-edges(sheet-region(_display),
		     0, 0, pixel-width, pixel-height);
  sheet-direct-mirror(_display) := mirror;
  /*---*** Not doing palettes yet...
  let palette  = port-default-palette(_port);
  let drawable = xt/XtWindow(top-shell);
  palette.%default-drawable := drawable;
  palette.%gcontext         := x/XCreateGC(_port.%display, drawable)
  */
end method initialize-display;

define method set-mirror-parent
    (child :: <top-level-mirror>, parent :: <display-mirror>)
 => ()
  ignoring("set-mirror-parent for <top-level-mirror>")
end method set-mirror-parent;
    
define method move-mirror
    (parent :: <display-mirror>, child :: <top-level-mirror>,
     x :: <integer>, y :: <integer>)
 => ()
  let widget = GTK-WIDGET(mirror-widget(child));
  //---*** This causes problems!
  // gtk-widget-set-uposition(widget, x, y)
end method move-mirror;

define method size-mirror
    (parent :: <display-mirror>, child :: <top-level-mirror>,
     width :: <integer>, height :: <integer>)
 => ()
  let widget = GTK-WIDGET(mirror-widget(child));
  //--- This may not work after the sheet is mapped...
  //---*** This causes the window to grow and grow...
  // gtk-window-set-default-size(widget, width, height)
end method size-mirror;
