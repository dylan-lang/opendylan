Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method initialize-display 
    (port :: <capi-port>, _display :: <display>) => ()
  let screen = convert-to-screen();
  let region = make-bounding-box(0, 0, 
                                 screen-width(screen), screen-height(screen));
  sheet-region(_display) := region;
  sheet-direct-mirror(_display) := screen;
  //--- Set up display-pixels-per-point, too
  display-pixel-width(_display)  := screen-width(screen);
  display-pixel-height(_display) := screen-height(screen);
  display-mm-width(_display)  := screen-width-in-millimeters(screen);
  display-mm-height(_display) := screen-height-in-millimeters(screen);
end method initialize-display;
