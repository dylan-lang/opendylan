Module:       duim-geometry-internals
Synopsis:     'print-object' methods for DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (transform :: <identity-transform>, stream :: <stream>) => ()
  printing-object (transform, stream, type?: #f, identity?: #f)
    format(stream, "identity transform")
  end
end method print-object;

define method print-object
    (transform :: <translation-transform>, stream :: <stream>) => ()
  printing-object (transform, stream, type?: #t, identity?: #t)
    format(stream, "(%d,%d)", transform.%tx, transform.%ty)
  end
end method print-object;


define method print-object
    (point :: <point>, stream :: <stream>) => ()
  printing-object (point, stream, type?: #t, identity?: #t)
    format(stream, "(%d,%d)", point-x(point), point-y(point))
  end
end method print-object;


define method print-object
    (box :: <general-box>, stream :: <stream>) => ()
  printing-object (box, stream, type?: #t, identity?: #t)
    format(stream, "(%d,%d):(%d,%d)",
	   box.%left, box.%top, box.%right, box.%bottom)
  end
end method print-object;

define method print-object
    (box :: <simple-box>, stream :: <stream>) => ()
  printing-object (box, stream, type?: #t, identity?: #t)
    format(stream, "(%d,%d):(%d,%d)",
	   0, 0, box.%width, box.%height)
  end
end method print-object;
