Module:    mini-duim
Synopsis:  Mini-DUIM graphics
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Drawing functions

define method draw-point* (medium :: <basic-medium>, x, y)
  do-draw-point*(medium, x, y)
end method draw-point*;

define method draw-points* (medium :: <basic-medium>, coord-seq)
  do-draw-points*(medium, coord-seq)
end method draw-points*;

define method draw-line* (medium :: <basic-medium>, x1, y1, x2, y2)
  do-draw-line*(medium, x1, y1, x2, y2)
end method draw-line*;

define method draw-lines* (medium :: <basic-medium>, coord-seq)
  do-draw-lines*(medium, coord-seq)
end method draw-lines*;

define method draw-rectangle* (medium :: <basic-medium>, x1, y1, x2, y2,
			       #rest keys, #key filled? = #t)
  ignore(filled?);
  apply(do-draw-rectangle*, medium, x1, y1, x2, y2, keys)
end method draw-rectangle*;

define method draw-rectangles* (medium :: <basic-medium>, coord-seq,
				#rest keys, #key filled? = #t)
  ignore(filled?);
  apply(do-draw-rectangles*, medium, coord-seq, keys)
end method draw-rectangles*;

define method draw-rounded-rectangle* (medium :: <basic-medium>, x1, y1, x2, y2,
				       #rest keys, #key filled? = #t, radius)
  ignore(filled?, radius);
  apply(do-draw-rounded-rectangle*, medium, x1, y1, x2, y2, keys)
end method draw-rounded-rectangle*;

define method draw-polygon* (medium :: <basic-medium>, coord-seq,
			     #rest keys, #key closed? = #t, filled? = #t)
  ignore(filled?, closed?);
  apply(do-draw-polygon*, medium, coord-seq, keys)
end method draw-polygon*;

define method draw-ellipse* (medium :: <basic-medium>, center-x, center-y,
			     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
			     #rest keys, #key start-angle, end-angle, filled? = #t)
  ignore(start-angle, end-angle, filled?);
  apply(do-draw-ellipse*, medium, center-x, center-y,
	radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
end method draw-ellipse*;

define method draw-text* (medium :: <basic-medium>, string-or-char, x, y,
			  #rest keys, #key start: _start, end: _end, align-x, align-y)
  ignore(_start, _end, align-x, align-y);
  apply(do-draw-text*, medium, string-or-char, x, y, keys)
end method draw-text*;


/// Sheet -> medium trampolines

define method draw-point* (sheet :: <basic-sheet>, x, y)
  draw-point*(sheet-medium(sheet), x, y)
end method draw-point*;

define method draw-points* (sheet :: <basic-sheet>, coord-seq)
  draw-points*(sheet-medium(sheet), coord-seq)
end method draw-points*;

define method draw-line* (sheet :: <basic-sheet>, x1, y1, x2, y2)
  draw-line*(sheet-medium(sheet), x1, y1, x2, y2)
end method draw-line*;

define method draw-lines* (sheet :: <basic-sheet>, coord-seq)
  draw-lines*(sheet-medium(sheet), coord-seq)
end method draw-lines*;

define method draw-rectangle* (sheet :: <basic-sheet>, x1, y1, x2, y2, #rest keys, #key)
  apply(draw-rectangle*, sheet-medium(sheet), x1, y1, x2, y2, keys)
end method draw-rectangle*;

define method draw-rectangles* (sheet :: <basic-sheet>, coord-seq, #rest keys, #key)
  apply(draw-rectangles*, sheet-medium(sheet), coord-seq, keys)
end method draw-rectangles*;

define method draw-rounded-rectangle* (sheet :: <basic-sheet>, x1, y1, x2, y2, #rest keys, #key)
  apply(draw-rounded-rectangle*, sheet-medium(sheet), x1, y1, x2, y2, keys)
end method draw-rounded-rectangle*;

define method draw-polygon* (sheet :: <basic-sheet>, coord-seq, #rest keys, #key)
  apply(draw-polygon*, sheet-medium(sheet), coord-seq, keys)
end method draw-polygon*;

define method draw-ellipse* (sheet :: <basic-sheet>, center-x, center-y,
			     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
			     #rest keys, #key)
  apply(draw-ellipse*, sheet-medium(sheet), center-x, center-y,
	radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
end method draw-ellipse*;

define method draw-text* (sheet :: <basic-sheet>, string-or-char, x, y, #rest keys, #key)
  apply(draw-text*, sheet-medium(sheet), string-or-char, x, y, keys)
end method draw-text*;


/// 'clear-box'

define method clear-box* (sheet :: <basic-sheet>, left, top, right, bottom) => ()
  let medium = sheet-medium(sheet);
  if (medium)
    clear-box*(medium, left, top, right, bottom)
  end
end method clear-box*;

define function clear-box (drawable, region) => ()
  let (left, top, right, bottom) = box-edges(region);
  clear-box*(drawable, left, top, right, bottom)
end function clear-box;


/// Utility functions

define method elliptical-arc-box
    (center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
 => (left, top, right, bottom)
  let dx = abs(radius-1-dx) + abs(radius-2-dx);
  let dy = abs(radius-1-dy) + abs(radius-2-dy);
  values(center-x - dx, center-y - dy,
	 center-x + dx, center-y + dy)
end method elliptical-arc-box;

define method compute-text-adjustment
    (medium :: <medium>, string, text-style, align-x, align-y,
     #key start: _start, end: _end)
 => (x-adjust, y-adjust)
  values(0, 0)
end method compute-text-adjustment;
