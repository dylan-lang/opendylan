Module:       duim-layouts-internals
Synopsis:     'print-object' methods for DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (space-req :: <null-space-requirement>, stream :: <stream>) => ()
  printing-object (space-req, stream, type?: #t, identity?: #t)
    format(stream, "width 0 height 0")
  end
end method print-object;

define method print-object
    (space-req :: <fixed-space-requirement>, stream :: <stream>) => ()
  printing-object (space-req, stream, type?: #t, identity?: #t)
    format(stream, "width %= height %=",
	   space-req.%width, space-req.%height)
  end
end method print-object;

define method print-object
    (space-req :: <unbounded-space-requirement>, stream :: <stream>) => ()
  printing-object (space-req, stream, type?: #t, identity?: #t)
    format(stream, "width %=-0+$fill height %=-0+$fill",
	   space-req.%width, space-req.%height)
  end
end method print-object;

define method print-object
    (space-req :: <general-space-requirement>, stream :: <stream>) => ()
  printing-object (space-req, stream, type?: #t, identity?: #t)
    format(stream, "width %=-%=+%= height %=-%=+%=",
	   space-req.%width,  space-req.%min-width,  space-req.%max-width,
	   space-req.%height, space-req.%min-height, space-req.%max-height)
  end
end method print-object;

define method print-object
    (space-req :: <label-space-requirement>, stream :: <stream>) => ()
  printing-object (space-req, stream, type?: #t, identity?: #t)
    format(stream, "label %=", space-req.%label)
  end
end method print-object;
