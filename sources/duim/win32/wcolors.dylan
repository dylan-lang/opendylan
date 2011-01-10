Module:    win32-duim
Synopsis:  Win32 color and palette implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Palettes

//---*** Palettes need to be fleshed out
define sealed class <win32-palette> (<basic-palette>)
  sealed slot port :: false-or(<port>),
    required-init-keyword: port:,
    setter: %port-setter;
  sealed slot %DC :: <HDC> = $null-hDC,
    init-keyword: dc:;
  sealed slot %logical-palette = #f,
    init-keyword: logical-palette:;
end class <win32-palette>;

define sealed domain make (singleton(<win32-palette>));
define sealed domain initialize (<win32-palette>);

define sealed method make-palette
    (_port :: <win32-port>, #key color?, dynamic?, dc, logical-palette, #all-keys)
 => (palette :: <win32-palette>)
  make(<win32-palette>,
       port: _port, color?: color?, dynamic?: dynamic?,
       dc: dc, logical-palette: logical-palette)
end method make-palette;

define sealed method install-default-palette
    (_port :: <win32-port>) => ()
  //---*** Do this (see clim/win32/port.lisp)
end method install-default-palette;


/// Win32 color management

define constant $max-int-color   :: <integer> = #xFF;
define constant $max-float-color :: <single-float> = as(<single-float>, $max-int-color);

define constant $native-black :: <native-color> = RGB(#x00, #x00, #x00);
define constant $native-white :: <native-color> = RGB(#xFF, #xFF, #xFF);


// Given a DUIM color, returns a native pixel value and caches the result
define function color->native-color
    (color :: <rgb-color>, medium :: <win32-medium>)
 => (native-color :: <native-color>)
  //--- Note:  Need to think some more about this cache stuff -- it may
  //--- make more sense to be caching brush handles instead of colors.
  //--- Also, the cache probably ought to be for the display instead of
  //--- for each medium.
  let cache = medium.%ink-cache;
  gethash(cache, color)
  | begin
      let value = %color->native-color(color);
      gethash(cache, color) := value;
      value
    end
end function color->native-color;

define inline function %color->native-color
    (color :: <rgb-color>) => (native-color :: <native-color>)
  let (red, green, blue) = color-rgb(color);
  RGB(truncate(red   * $max-int-color),
      truncate(green * $max-int-color),
      truncate(blue  * $max-int-color))
end function %color->native-color;
  

// Given a native color, returns a DUIM color and caches the result
define function native-color->color 
    (native-color :: <native-color>, medium :: <win32-medium>)
 => (color :: <rgb-color>)
  let cache = medium.%ink-cache;
  gethash(cache, native-color)
  | begin
      let value = %native-color->color(native-color);
      gethash(cache, native-color) := value;
      value
    end
end function native-color->color;

define inline function %native-color->color
    (native-color :: <native-color>) => (color :: <rgb-color>)
  make-rgb-color(GetRValue(native-color) / $max-float-color,
                 GetGValue(native-color) / $max-float-color,
                 GetBValue(native-color) / $max-float-color)
end function %native-color->color;


// Return DUIM color corresponding to a Windows brush handle (assumed solid)
define function native-brush->color
    (hBrush :: <HBRUSH>, medium :: <win32-medium>) => (color :: <ink>)
  with-stack-structure (logbrush :: <LPLOGBRUSH>)
    let struct-size = safe-size-of(<LOGBRUSH>);
    if (GetObject(hBrush, struct-size, logbrush) ~= struct-size)
      report-error("GetObject for LOGBRUSH")
    else
      native-color->color(logbrush.lbColor-value, medium)
    end
  end
end function native-brush->color;


/// Some very useful color constants

define variable $default-face-color :: <rgb-color>
  = %native-color->color(GetSysColor($COLOR-3DFACE));
define variable $default-shadow-color :: <rgb-color>
  = %native-color->color(GetSysColor($COLOR-3DSHADOW));
define variable $default-highlight-color :: <rgb-color>
  = %native-color->color(GetSysColor($COLOR-3DHIGHLIGHT));
