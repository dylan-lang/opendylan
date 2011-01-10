Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM back-end for Deuce graphics

/// Colors

define variable $color-table :: <object-table> = make(<table>);

define sealed method make-duim-color
    (medium :: false-or(<medium>), color)
 => (color :: type-union(<color>, <brush>))
  case
    color == $default-foreground =>
      if (medium) medium-foreground(medium) else $black end;
    color == $default-background =>
      if (medium) medium-background(medium) else $white end;
    color =>
      gethash($color-table, color)
      | begin
	  let r = deuce/color-red(color);
	  let g = deuce/color-green(color);
	  let b = deuce/color-blue(color);
	  let c = make-rgb-color(r / 255.0, g / 255.0, b / 255.0);
	  gethash($color-table, color) := c;
	  c
	end;
    otherwise =>
      if (medium) medium-foreground(medium) else $black end;
  end
end method make-duim-color;

define inline function establish-color
    (window :: <deuce-pane>, medium :: <medium>, color) => ()
  unless (color == window-color(window))
    let brush = make-duim-color(medium, color);
    medium-brush(medium) := brush;
    window-color(window) := color
  end
end function establish-color;


/// Fonts

define variable $font-table :: <object-table> = make(<table>);

define sealed method make-duim-text-style
    (medium :: false-or(<medium>), font)
 => (text-style :: <text-style>)
  let default = if (medium) medium-default-text-style(medium)
		else $default-text-style end;
  if (font)
    gethash($font-table, font)
    | begin
	let style
	  = merge-text-styles(make-text-style(font-family(font), font-name(font),
                                              font-weight(font), font-slant(font),
					      font-size(font)),
			      default);
	gethash($font-table, font) := style;
	style
      end
  else
    default
  end
end method make-duim-text-style;

define sealed method make-font-from-duim-text-style
    (text-style :: <text-style>)
 => (font :: <font>)
  let (family, name, weight, slant, point-size)
    = text-style-components(text-style);
  make-font(family, name, weight, slant, point-size)
end method make-font-from-duim-text-style;

define inline function establish-font
    (window :: <deuce-pane>, medium :: <medium>, font) => ()
  unless (font == window-font(window))
    let style = make-duim-text-style(medium, font);
    medium-text-style(medium) := style;
    window-font(window) := font
  end
end function establish-font;

define sealed method deuce/font-metrics
    (window :: <deuce-pane>, font :: false-or(<font>))
 => (width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  let medium = sheet-medium(window);
  let style  = make-duim-text-style(medium, font | window-default-font(window));
  let (duim-font, width, height, ascent, descent)
    = font-metrics(style, port(medium));
  ignore(duim-font);
  values(width, height, ascent, descent)
end method deuce/font-metrics;

define method set-default-font-size
    (window :: <deuce-pane>, font-size) => ()
  ignore(font-size);
  next-method();
  let medium = sheet-medium(window);
  let style  = make-duim-text-style(medium, window-default-font(window));
  default-text-style(window) := style;
  when (medium)
    medium-default-text-style(medium) := style
  end
end method set-default-font-size;


/// Graphics

define sealed method deuce/draw-string
    (window :: <deuce-pane>,
     string :: <string>, x :: <integer>, y :: <integer>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
	  color = $default-foreground, font = window-default-font(window), do-tabs? = #t,
	  align-x = #"left", align-y = #"top") => ()
  let medium = sheet-medium(window);
  establish-color(window, medium, color);
  establish-font(window, medium, font);
  draw-text(medium, string, x, y,
	    start: _start, end: _end,
	    align-x: align-x, align-y: align-y, do-tabs?: do-tabs?)
end method deuce/draw-string;

define sealed method deuce/string-size
    (window :: <deuce-pane>, string :: <string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
	  font = window-default-font(window), do-tabs? = #t)
 => (width :: <integer>, height :: <integer>, baseline :: <integer>)
  let medium = sheet-medium(window);
  let style = make-duim-text-style(medium, font);
  let (width, height, cursor-x, cursor-y, baseline)
    = text-size(medium, string, start: _start, end: _end,
		text-style: style, do-tabs?: do-tabs?);
  ignore(cursor-x, cursor-y);
  values(width, height, baseline)
end method deuce/string-size;


define sealed method deuce/draw-line
    (window :: <deuce-pane>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>,
     #key color = $default-foreground, thickness = 1) => ()
  let medium = sheet-medium(window);
  establish-color(window, medium, color);
  medium-pen(medium) := make(<pen>, width: thickness);
  draw-line(medium, x1, y1, x2, y2)
end method deuce/draw-line;

define sealed method deuce/draw-rectangle
    (window :: <deuce-pane>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>,
     #key color = $default-foreground, thickness = 1, filled? = #t) => ()
  let medium = sheet-medium(window);
  establish-color(window, medium, color);
  unless (filled?)
    medium-pen(medium) := make(<pen>, width: thickness)
  end;
  draw-rectangle(medium, left, top, right, bottom, filled?: filled?)
end method deuce/draw-rectangle;

define sealed method deuce/draw-image
    (window :: <deuce-pane>, image, x :: <integer>, y :: <integer>) => ()
  let medium = sheet-medium(window);
  draw-image(medium, image, x, y)
end method deuce/draw-image;

define sealed method deuce/clear-area
    (window :: <deuce-pane>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  let medium = sheet-medium(window);
  clear-box(medium, left, top, right, bottom)
end method deuce/clear-area;

define sealed method deuce/copy-area
    (window :: <deuce-pane>,
     from-x :: <integer>, from-y :: <integer>, width :: <integer>, height :: <integer>,
     to-x :: <integer>, to-y :: <integer>) => ()
  let medium = sheet-medium(window);
  copy-area(medium, from-x, from-y, width, height, to-x, to-y)
end method deuce/copy-area;
