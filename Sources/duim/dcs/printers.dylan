Module:       duim-dcs-internals
Synopsis:     'print-object' methods for DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (color :: <color>, stream :: <stream>) => ()
  printing-object (color, stream, type?: #t, identity?: #t)
    let (red, green, blue, opacity) = color-rgb(color);
    ignore(opacity);
    format(stream, "r=%d g=%d b=%d", red, green, blue)
  end
end method print-object;


define method print-object
    (color :: <foreground>, stream :: <stream>) => ()
  printing-object (color, stream, type?: #f, identity?: #f)
    format(stream, "foreground ink")
  end
end method print-object;

define method print-object
    (color :: <background>, stream :: <stream>) => ()
  printing-object (color, stream, type?: #f, identity?: #f)
    format(stream, "background ink")
  end
end method print-object;


define method print-object
    (color :: <contrasting-color>, stream :: <stream>) => ()
  printing-object (color, stream, type?: #t, identity?: #t)
    format(stream, "%d of %d", color.%which-one, color.%how-many)
  end
end method print-object;


define method print-object
    (color :: <dynamic-color>, stream :: <stream>) => ()
  printing-object (color, stream, type?: #t, identity?: #t)
    format(stream, "%=", dynamic-color-color(color))
  end
end method print-object;


define method print-object
    (pen :: <pen>, stream :: <stream>) => ()
  printing-object (pen, stream, type?: #t, identity?: #t)
    format(stream, "width %d (%=), dashes %=, joint %=, cap %=",
	   pen-width(pen), pen-units(pen), pen-dashes(pen),
	   pen-joint-shape(pen), pen-cap-shape(pen))
  end
end method print-object;


define method print-object
    (stencil :: <stencil>, stream :: <stream>) => ()
  printing-object (stencil, stream, type?: #t, identity?: #t)
    format(stream, "%dX%d",
           dimension(stencil.%array, 1), dimension(stencil.%array, 0))
  end
end method print-object;

define method print-object
    (pattern :: <pattern>, stream :: <stream>) => ()
  printing-object (pattern, stream, type?: #t, identity?: #t)
    format(stream, "%dX%d n=%d",
           dimension(pattern.%array, 1), dimension(pattern.%array, 0),
           size(pattern.%colors))
  end
end method print-object;


define method print-object
    (style :: <standard-text-style>, stream :: <stream>) => ()
  printing-object (style, stream, type?: #t, identity?: #t)
    format(stream, "%=.%=.%=.%=",
	   text-style-family(style), text-style-weight(style),
	   text-style-slant(style), text-style-size(style))
  end
end method print-object;

define method print-object
    (font :: <device-font>, stream :: <stream>) => ()
  printing-object (font, stream, type?: #t, identity?: #t)
    format(stream, "%=", device-font-font(font))
  end
end method print-object;
