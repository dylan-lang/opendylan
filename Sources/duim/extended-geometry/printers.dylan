Module:       duim-extended-geometry-internals
Synopsis:     'print-object' methods for DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (transform :: <general-transform>, stream :: <stream>) => ()
  printing-object (transform, stream, type?: #t, identity?: #t)
    if (zero?(transform.%mxy) & zero?(transform.%myx))
      format(stream, "scale (%d,%d) translate (%d,%d)",
	     transform.%mxx, transform.%myy, transform.%tx, transform.%ty)
    else
      format(stream, "[%d %d %d %d] %d %d", 
	     transform.%mxx, transform.%mxy, transform.%myx, transform.%myy,
	     transform.%tx, transform.%ty)
    end
  end
end method print-object;


define method print-object
    (line :: <line>, stream :: <stream>) => ()
  printing-object (line, stream, type?: #t, identity?: #t)
    let (start-x, start-y) = line-start-position(line);
    let (end-x, end-y) = line-end-position(line);
    format(stream, "(%d,%d):(%d,%d)", start-x, start-y, end-x, end-y)
  end
end method print-object;

define method print-object
    (rectangle :: <standard-rectangle>, stream :: <stream>) => ()
  printing-object (rectangle, stream, type?: #t, identity?: #t)
    let (left, top, right, bottom) = rectangle-edges(rectangle);
    format(stream, "(%d,%d):(%d,%d)", left, top, right, bottom)
  end
end method print-object;
