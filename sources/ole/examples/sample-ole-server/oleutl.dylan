Module:    sample-OLE-server
Synopsis:  Utility functions adapted from "samples/ole/olestd/oleutl.c".
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// ====  coordinate conversions  ====

define constant $HIMETRIC-PER-INCH = 2540; // millimeters per 100 inches

define function MAP-PIX-TO-LOGHIM(x :: <integer>, ppli :: <integer>)
 => (x :: <integer>);
  round/($HIMETRIC-PER-INCH * x, ppli)
end;


define variable *pixels-per-inch-x* :: <integer> = 72;
define variable *pixels-per-inch-y* :: <integer> = 72;

define function ole-util-init() => ();
  let hDC = GetDC($NULL-HWND);
  *pixels-per-inch-x* := GetDeviceCaps(hDC, $LOGPIXELSX);
  *pixels-per-inch-y* := GetDeviceCaps(hDC, $LOGPIXELSY);
  ReleaseDC($NULL-HWND, hDC);
  values()
end;


define function pixels-to-himetric (width :: <integer>, height :: <integer> )
 => (width :: <integer>, height :: <integer> );
  values(MAP-PIX-TO-LOGHIM(width,  *pixels-per-inch-x*),
	 MAP-PIX-TO-LOGHIM(height, *pixels-per-inch-y*))
end pixels-to-himetric;

/* // not needed
define function MAP-LOGHIM-TO-PIX(x :: <integer>, ppli :: <integer>)
 => (x :: <integer>);
  round/(ppli * x, $HIMETRIC-PER-INCH)
end;

define function himetric-to-pixels (width :: <integer>, height :: <integer> )
 => (width :: <integer>, height :: <integer> );
  values(MAP-LOGHIM-TO-PIX(width,  *pixels-per-inch-x*),
	 MAP-LOGHIM-TO-PIX(height, *pixels-per-inch-y*))
end himetric-to-pixels;
*/

