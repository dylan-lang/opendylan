Module:    win32-duim
Synopsis:  Win32 color and palette implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Palettes

//---*** Palettes need to be fleshed out
define sealed class <win32-palette> (<basic-palette>)
  slot port :: false-or(<port>),
    required-init-keyword: port:,
    setter: %port-setter;
end class <win32-palette>;

define method make-palette
    (_port :: <win32-port>, #key color?, dynamic?, #all-keys)
 => (palette :: <palette>)
  make(<win32-palette>, port: _port, color?: color?, dynamic?: dynamic?)
end method make-palette;


define method port-default-foreground
    (_port :: <win32-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>));
  native-color->color(GetSysColor($COLOR-WINDOWTEXT), sheet-medium(sheet))
end method port-default-foreground;

define method port-default-background
    (_port :: <win32-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>));
  native-color->color(GetSysColor($COLOR-WINDOW), sheet-medium(sheet))
end method port-default-background;


/// Win32 color management

define constant <native-color> = limited(<integer>, min: 0, max: #x03FFFFFF);

define constant $max-int-color   :: <integer> = #xFF;
define constant $max-float-color :: <single-float> = as(<single-float>, $max-int-color);

define constant $native-black :: <native-color> = RGB(#x00, #x00, #x00);
define constant $native-white :: <native-color> = RGB(#xFF, #xFF, #xFF);


// Given a DUIM color, returns a native pixel value
define function color->native-color
    (color :: <color>, medium :: <win32-medium>) => (native-color :: <native-color>)
  //--- Note:  Need to think some more about this cache stuff -- it may
  //--- make more sense to be caching brush handles instead of colors.
  //--- Also, the cache probably ought to be for the display instead of
  //--- for each medium.
  let cache = medium.%ink-cache;
  gethash(cache, color)
  | begin
      let (red, green, blue) = color-rgb(color);
      let value = RGB(round(red   * $max-int-color),
		      round(green * $max-int-color),
		      round(blue  * $max-int-color));
      gethash(cache, color) := value;
      value
    end
end function color->native-color;

// Given a native color, returns a DUIM color
define function native-color->color 
    (native-color :: <native-color>, medium :: <win32-medium>)
 => (color :: <color>)
  let cache = medium.%ink-cache;
  gethash(cache, native-color)
  | begin
      let value =
	make-rgb-color(GetRValue(native-color) / $max-float-color,
		       GetGValue(native-color) / $max-float-color,
		       GetBValue(native-color) / $max-float-color);
      gethash(cache, native-color) := value;
      value
    end
end function native-color->color;

// Return DUIM color corresponding to a Windows brush handle (assumed solid)
//--- This is not currently used and may not be needed
define function native-brush->color
    (hBrush :: <HBRUSH>, medium :: <win32-medium>) => (color :: <ink>)
  with-stack-structure (logbrush :: <LPLOGBRUSH>)
    let struct-size = 12;	//---*** not yet exported:  size-of(<LOGBRUSH>);
    if (GetObject(hBrush, struct-size, logbrush) ~= struct-size)
      report-error("GetObject for LOGBRUSH")
    else
      native-color->color(logbrush.lbColor-value, medium)
    end
  end
end function native-brush->color;
