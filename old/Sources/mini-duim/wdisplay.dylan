Module:    win32-duim
Synopsis:  Win32 display implementation
Author:    David Gray, Scott McKay, Andrew Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Mirror the display, set its region, and set its characteristics
define method initialize-display 
    (_port :: <win32-port>, _display :: <standard-display>) => ()
  let client-area-width     = GetSystemMetrics($SM-CXFULLSCREEN);
  let client-area-height    = GetSystemMetrics($SM-CYFULLSCREEN);
  sheet-region(_display) :=
    make-bounding-box(0, 0, client-area-width, client-area-height);
  //---*** Not sure what to do for this one:
  //--- sheet-direct-mirror(_display) := --screen--;
  display-pixel-width(_display)  := client-area-width;
  display-pixel-height(_display) := client-area-height;
  let hDC :: <HDC> = CreateCompatibleDC($null-hdc);
  let mm-width  = GetDeviceCaps(hDC, $HORZSIZE);
  let mm-height = GetDeviceCaps(hDC, $VERTSIZE);
  let pixel-width  = GetSystemMetrics($SM-CXSCREEN);
  let pixel-height = GetSystemMetrics($SM-CYSCREEN);
  display-mm-width(_display) :=
    round/(mm-width * client-area-width, pixel-width);
  display-mm-height(_display) :=
    round/(mm-height * client-area-height, pixel-height);
  let piy :: <integer> = GetDeviceCaps(hDC, $LOGPIXELSY);
  let metrics = port-metrics(_port);
  win32-pixels-per-inch(metrics) := piy;
  display-pixels-per-point(_display)
    := (GetDeviceCaps(hDC, $LOGPIXELSX) + piy) / 144;
  DeleteDC(hDC);
  values()
end method initialize-display;
