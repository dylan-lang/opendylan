Module:       carbon-duim
Synopsis:     Macintosh drawing implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MAC graphics

define constant $2pi-in-64ths-of-degree :: <integer> = 360 * 64;
define constant $supports-titled-ellipses = #f;

define sealed method draw-point
    (medium :: <carbon-medium>, x, y) => (record)
  update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let thickness = pen-width(medium-pen(medium));
    if (thickness < 2)
      MoveTo(x, y);
      Line(0, 0)
    else 
      let radius = truncate/(thickness, 2);
      let left   = x - radius;
      let top    = y - radius;
      let right  = x + radius;
      let bottom = y + radius;
      with-stack-structure (Rect :: <Rect*>)
	SetRect(Rect, left, top, right, bottom);
	PaintOval(Rect)
      end
    end
  end;
  #f
end method draw-point;

define sealed method draw-points
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>) => (record)
  update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  let thickness = pen-width(medium-pen(medium));
  if (thickness < 2)
    do-coordinates
      (method (x, y)
	 with-device-coordinates (transform, x, y)
	   MoveTo(x, y);
	   Line(0, 0)
	 end
       end,
       coord-seq)
  else
    let thickness/2 = truncate/(thickness, 2);
    with-stack-structure (Rect :: <Rect*>)
      do-coordinates
	(method (x, y)
	   with-device-coordinates (transform, x, y)
	     let radius = truncate/(thickness, 2);
	     let left   = x - radius;
	     let top    = y - radius;
	     let right  = x + radius;
	     let bottom = y + radius;
	     SetRect(Rect, left, top, right, bottom);
	     PaintOval(Rect)
	   end
	 end,
	 coord-seq)
    end
  end;
  #f
end method draw-points;


/// Pixel graphics

//---*** Do an efficient version of this
define sealed method set-pixel
    (medium :: <carbon-medium>, color :: <rgb-color>, x, y) => (record)
  with-drawing-options (medium, brush: color)
    draw-point(medium, x, y)
  end;
  #f
end method set-pixel;

//---*** Do an efficient version of this
define sealed method set-pixels
    (medium :: <carbon-medium>, color :: <rgb-color>, 
     coord-seq :: <coordinate-sequence>)
 => (record)
  with-drawing-options (medium, brush: color)
    draw-points(medium, coord-seq)
  end;
  #f
end method set-pixels;


define sealed method draw-line
    (medium :: <carbon-medium>, x1, y1, x2, y2) => (record)
  update-drawing-state(medium, pen: medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    MoveTo(x1, y1);
    LineTo(x2, y2)
  end;
  #f
end method draw-line;

define sealed method draw-lines
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>) => (record)
  update-drawing-state(medium, pen: medium-pen(medium));
  let transform = medium-device-transform(medium);
  do-endpoint-coordinates
    (method (x1, y1, x2, y2)
       with-device-coordinates (transform, x1, y1, x2, y2)
	 MoveTo(x1, y1);
	 LineTo(x2, y2)
       end
     end,
     coord-seq);
  #f
end method draw-lines;

define sealed method draw-rectangle
    (medium :: <carbon-medium>, x1, y1, x2, y2,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    //---*** Could do this directly...
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    with-stack-structure (Rect :: <Rect*>)
      with-device-coordinates (transform, x1, y1, x2, y2)
	SetRect(Rect, x1, y1, x2, y2);
	MoveTo(x1, y1);
	if (filled?)
	  PaintRect(Rect)
	else
	  FrameRect(Rect)
	end
      end
    end
  end;
  #f
end method draw-rectangle;

define sealed method draw-rectangles
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    draw-transformed-rectangles(medium, coord-seq, filled?: filled?)
  else
    update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    let transform = medium-device-transform(medium);
    with-stack-structure (Rect :: <Rect*>)
      do-endpoint-coordinates
	(method (x1, y1, x2, y2)
	   with-device-coordinates (transform, x1, y1, x2, y2)
	     SetRect(Rect, x1, y1, x2, y2);
	     MoveTo(x1, y1);
	     if (filled?)
	       PaintRect(Rect)
	     else
	       FrameRect(Rect)
	     end
	   end
	 end,
	 coord-seq)
    end
  end;
  #f
end method draw-rectangles;

define sealed method draw-transformed-rectangles
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let ncoords :: <integer> = size(coord-seq);
  assert(zero?(modulo(ncoords, 4)),
	 "The coordinate sequence has the wrong number of elements");
  local method draw-one (x1, y1, x2, y2) => ()
	  with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
	    apply(draw-polygon, medium, coords, closed?: #t, keys)
	  end
        end method;
  dynamic-extent(draw-one);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 4, until: i = ncoords)
      draw-one(coord-seq[i + 0], coord-seq[i + 1],
	       coord-seq[i + 2], coord-seq[i + 3])
    end
  end;
  #f
end method draw-transformed-rectangles;

define sealed method draw-rounded-rectangle
    (medium :: <carbon-medium>, x1, y1, x2, y2,
     #key filled? = #t, radius) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    //---*** This isn't right...
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    with-stack-structure (Rect :: <Rect*>)
      with-device-coordinates (transform, x1, y1, x2, y2)
	unless (radius)
	  let width  = x2 - x1;
	  let height = y2 - y1;
	  radius := max(truncate/(min(width, height), 3), 2)
	end;
	SetRect(Rect, x1, y1, x2, y2);
	MoveTo(x1, y1);
	if (filled?)
	  PaintRoundRect(Rect, radius, radius)
	else
	  FrameRoundRect(Rect, radius, radius)
	end
      end
    end
  end;
  #f
end method draw-rounded-rectangle;

define sealed method draw-polygon
    (medium :: <carbon-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  let scoords :: <integer> = size(coord-seq);
  let ncoords :: <integer> = size(coord-seq);
  let npoints :: <integer> = floor/(ncoords, 2) + if (closed? & ~filled?) 1 else 0 end;
  without-bounds-checks
    let polygon = OpenPoly();
    for (i :: <integer> from 0 below ncoords by 2,
	 j :: <integer> from 0)
      let x = coord-seq[i + 0];
      let y = coord-seq[i + 1];
      with-device-coordinates (transform, x, y)
        LineTo(x, y)
      end
    end;
    if (closed?)
      ClosePoly()
    end;
    if (filled?)
      PaintPoly(polygon)
    else
      FramePoly(polygon)
    end
  end;
  #f
end method draw-polygon;

define sealed method draw-ellipse
    (medium :: <carbon-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      let (angle-2, x-radius, y-radius, angle-1)
	= singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
      if (~$supports-titled-ellipses
	  | x-radius = abs(y-radius)		// a circle - rotations are irrelevant
	  | zero?(angle-1))			// axis-aligned ellipse
	let (angle, delta-angle)
	  = if (start-angle & end-angle)
	      let start-angle = modulo(start-angle, $2pi);
	      let end-angle   = modulo(end-angle, $2pi);
	      when (end-angle < start-angle)
		end-angle := end-angle + $2pi
	      end;
	      values(round($2pi-in-64ths-of-degree * (($2pi - start-angle) / $2pi)),
		     round($2pi-in-64ths-of-degree * ((start-angle - end-angle) / $2pi)))
	    else
	      values(0, $2pi-in-64ths-of-degree)
	    end;
	x-radius := abs(x-radius);
	y-radius := abs(y-radius);
	let left   = center-x - x-radius;
	let top    = center-y - y-radius;
	let right  = center-x + x-radius;
	let bottom = center-y + y-radius;
	with-stack-structure (Rect :: <Rect*>)
	  SetRect(Rect, left, top, right, bottom);
	  MoveTo(left, top);
	  if (filled?)
	    PaintOval(Rect)
	  else
	    FrameOval(Rect)
	  end
	end
      else
	ignoring("draw-ellipse for tilted ellipses");
	#f
      end;
      // SelectObject(hDC, old-object)
    end
  end;
  #f
end method draw-ellipse;

// MAC bitmaps and icons are handled separately
define sealed method draw-image
    (medium :: <carbon-medium>, image :: <image>, x, y) => (record)
  update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let width  = image-width(image);
    let height = image-height(image);
    ignoring("draw-image");
    //---*** DRAW THE IMAGE, BUT FOR NOW DRAW A RECTANGLE
    // let (pixel, fill-style, operation, pattern)
    //   = convert-ink-to-DC-components(medium, hDC, image);
    // let old-object :: <HANDLE> = SelectObject(hDC, $null-hpen);
    // Rectangle(hDC, x, y, x + width, y + height);
    // SelectObject(hDC, old-object)
  end;
  #f
end method draw-image;


/// Path graphics

define sealed method start-path
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method start-path;

define sealed method end-path
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method end-path;

define sealed method abort-path
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method abort-path;

define sealed method close-path
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method close-path;

define sealed method stroke-path
    (medium :: <carbon-medium>, #key filled?) => (record)
  ignoring("MAC does not support path-based graphics")
end method stroke-path;

define sealed method fill-path
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method fill-path;

define sealed method clip-from-path
    (medium :: <carbon-medium>, #key function = $boole-and) => (record)
  ignoring("MAC does not support path-based graphics")
end method clip-from-path;

define sealed method save-clipping-region
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method save-clipping-region;

define sealed method restore-clipping-region
    (medium :: <carbon-medium>) => (record)
  ignoring("MAC does not support path-based graphics")
end method restore-clipping-region;

define sealed method move-to
    (medium :: <carbon-medium>, x, y) => (record)
  ignoring("MAC does not support path-based graphics")
end method move-to;

define sealed method line-to
    (medium :: <carbon-medium>, x, y) => (record)
  ignoring("MAC does not support path-based graphics")
end method line-to;

define sealed method arc-to
    (medium :: <carbon-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle) => (record)
  ignoring("MAC does not support path-based graphics")
end method arc-to;

define sealed method curve-to
    (medium :: <carbon-medium>, x1, y1, x2, y2, x3, y3) => (record)
  ignoring("MAC does not support path-based graphics")
end method curve-to;


/// 'draw-pixmap', etc

define sealed method draw-pixmap
    (medium :: <carbon-medium>, pixmap :: <pixmap>, x, y,
     #key function = $boole-1) => (record)
  do-copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
	       medium, x, y)
end method draw-pixmap;

define sealed method clear-box
    (medium :: <carbon-medium>, left, top, right, bottom) => ()
  with-stack-structure (Rect :: <Rect*>)
    let transform = medium-device-transform(medium);
    with-device-coordinates (transform, left, top, right, bottom)
      SetRect(Rect, left, top, right, bottom);
      EraseRect(Rect)
    end
  end
end method clear-box;


/// Text drawing

define sealed method draw-text
    (medium :: <carbon-medium>, character :: <character>, x, y,
     #rest keys,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  ignore(_start, _end, align-x, align-y, do-tabs?,
          towards-x, towards-y, transform-glyphs?);
  let string = make(<string>, size: 1, fill: character);
  apply(draw-text, medium, string, x, y, keys)
end method draw-text;

//---*** What do we do about Unicode strings?
define sealed method draw-text
    (medium :: <carbon-medium>, string :: <string>, x, y,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let text-style :: <text-style> = medium-merged-text-style(medium);
  let font :: <carbon-font> = text-style-mapping(port(medium), text-style);
  let length :: <integer> = size(string);
  update-drawing-state(medium, font: font);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    when (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    //---*** What about x and y alignment?
    if (do-tabs?)
      ignoring("draw-text with do-tabs?: #t");
      /*---*** Not yet implemented!
      let tab-width  = text-size(medium, " ") * 8;
      let tab-origin = if (do-tabs? == #t) x else do-tabs? end;
      let x = 0;
      let s = _start;
      block (break)
	while (#t)
	  let e = position(string, '\t', start: s, end: _end);
	  //---*** It would be great if 'with-c-string' took start & end!
	  let substring = copy-sequence(string, start: s, end: e);
	  MoveTo(x, y);
	  with-pascal-string (text = substring)
	    DrawString(text)
	  end;
	  if (e = _end)
	    break()
	  else
	    let (x1, y1, x2, y2) = GET-STRING-EXTENT(drawable, string, font, s, e);
	    ignore(x1, y1, y2);
	    x := floor/(x + x2 + tab-width, tab-width) * tab-width;
	    s := min(e + 1, _end)
	  end
	end
      end
      */
    else
      let substring
	= if (_start = 0 & _end = length) string
	  else copy-sequence(string, start: _start, end: _end) end;
      MoveTo(x, y);
      with-pascal-string (text = substring)
        DrawString(text)
      end
    end
  end
end method draw-text;
