Module:    motif-duim
Synopsis:  Motif display implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif displays, i.e., the screen

define sealed class <display-mirror> (<window-mirror>)
  slot mirror-screen :: false-or(x/<Screen>) = #f,
    init-keyword: screen:;
end class <display-mirror>;

// Mirror the display, set its region, and set its characteristics
define sealed method initialize-display 
    (_port :: <motif-port>, _display :: <standard-display>) => ()
  let app-shell = _port.%app-shell;
  let top-shell = xt/XtCreatePopupShell("DUIM display", xt/<top-level-shell>, app-shell,
					resources: #[]);
  let screen = xt/XtScreen(top-shell);
  let mirror = make(<display-mirror>,
		    sheet:  _display,
		    screen: screen,
		    widget: top-shell);
  let mm-width     = x/XWidthMMOfScreen(screen);
  let mm-height    = x/XHeightMMOfScreen(screen);
  let pixel-width  = x/XWidthOfScreen(screen);
  let pixel-height = x/XHeightOfScreen(screen);
  display-pixel-width(_display)  := pixel-width;
  display-pixel-height(_display) := pixel-height;
  display-mm-width(_display)     := mm-width;
  display-mm-height(_display)    := mm-height;
  display-pixels-per-point(_display)
    := sqrt(  (pixel-width  / (mm-width  * (72.0 / 25.4)))
	    * (pixel-height / (mm-height * (72.0 / 25.4))));
  sheet-region(_display) :=
    set-box-edges(sheet-region(_display),
		  0, 0, pixel-width, pixel-height);
  sheet-direct-mirror(_display) := mirror;
  xt/XtRealizeWidget(top-shell);
  let palette  = port-default-palette(_port);
  let drawable = xt/XtWindow(top-shell);
  palette.%default-drawable := drawable;
  palette.%gcontext         := x/XCreateGC(_port.%display, drawable)
end method initialize-display;
