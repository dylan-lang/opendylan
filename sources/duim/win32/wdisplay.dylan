Module:    win32-duim
Synopsis:  Win32 display implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 displays, i.e., the screen

// Mirror the display, set its region, and set its characteristics
define sealed method initialize-display 
    (_port :: <win32-port>, _display :: <standard-display>) => ()
  let (display-width, display-height) = display-size(_display);
  //---*** Not sure what to do for this one:
  //--- sheet-direct-mirror(_display) := --screen--;
  //--- fill in _port.%display, too
  //---*** Need to do the equivalent of 'w::init-screen-device'
  display-pixel-width(_display)  := display-width;
  display-pixel-height(_display) := display-height;
  let hDC :: <hDC> = _port.%memory-hDC;
  let mm-width  = GetDeviceCaps(hDC, $HORZSIZE);
  let mm-height = GetDeviceCaps(hDC, $VERTSIZE);
  let pixel-width  = GetSystemMetrics($SM-CXSCREEN);
  let pixel-height = GetSystemMetrics($SM-CYSCREEN);
  display-mm-width(_display) :=
    floor/(mm-width * display-width, pixel-width);
  display-mm-height(_display) :=
    floor/(mm-height * display-height, pixel-height);
  let piy :: <integer> = GetDeviceCaps(hDC, $LOGPIXELSY);
  let metrics = port-metrics(_port);
  win32-pixels-per-inch(metrics) := piy;
  display-pixels-per-point(_display)
    := floor/(GetDeviceCaps(hDC, $LOGPIXELSX) + piy, 144)
end method initialize-display;

// Compute the display size dynamically, since Windows can change the
// values when the taskbar moves around
//---*** We should really make the display's region update dynamically
define sealed method display-size
    (_display :: <standard-display>)
 => (width :: <integer>, height :: <integer>)
  let display-width  = GetSystemMetrics($SM-CXFULLSCREEN);
  let display-height = GetSystemMetrics($SM-CYFULLSCREEN);
  sheet-region(_display) :=
    set-box-edges(sheet-region(_display),
                  0, 0, display-width, display-height);
  values(display-width, display-height)
end method display-size;
