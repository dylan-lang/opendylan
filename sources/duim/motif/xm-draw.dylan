Module:    motif-duim
Synopsis:  Motif drawing implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif graphics

define sealed method draw-point
    (medium :: <motif-medium>, x, y) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let thickness = pen-width(medium-pen(medium));
    if (thickness < 2)
      x/XDrawPoint(display, drawable, gcontext, x, y)
    else 
      let thickness/2 = truncate/(thickness, 2);
      x/XDrawArc(display, drawable, gcontext,
		 x - thickness/2, y - thickness/2, thickness, thickness,
		 0, $2pi-in-64ths-degrees)
    end
  end;
  #f
end method draw-point;

define sealed method draw-points
    (medium :: <motif-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  let thickness = pen-width(medium-pen(medium));
  if (thickness < 2)
    do-coordinates
      (method (x, y)
	 with-device-coordinates (transform, x, y)
	   //---*** Use x/XDrawPoints
	   x/XDrawPoint(display, drawable, gcontext, x, y)
	 end
       end,
       coord-seq)
  else
    let thickness/2 = truncate/(thickness, 2);
    do-coordinates
      (method (x, y)
	 with-device-coordinates (transform, x, y)
	   //---*** Use x/XDrawArcs
	   x/XDrawArc(display, drawable, gcontext,
		      x - thickness/2, y - thickness/2, thickness, thickness,
		      0, $2pi-in-64ths-degrees)
	 end
       end,
       coord-seq)
  end;
  #f
end method draw-points;

define sealed method draw-line
    (medium :: <motif-medium>, x1, y1, x2, y2) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium, pen: medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    x/XDrawLine(display, drawable, gcontext, x1, y1, x2, y2)
  end;
  #f
end method draw-line;

define sealed method draw-lines
    (medium :: <motif-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium, pen: medium-pen(medium));
  let transform = medium-device-transform(medium);
  //---*** Use x/XDrawSegments
  do-endpoint-coordinates
    (method (x1, y1, x2, y2)
       with-device-coordinates (transform, x1, y1, x2, y2)
	 x/XDrawLine(display, drawable, gcontext, x1, y1, x2, y2)
       end
     end,
     coord-seq);
  #f
end method draw-lines;

define sealed method draw-rectangle
    (medium :: <motif-medium>, x1, y1, x2, y2,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
      = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    //---*** Might need to use 'x/XSetTSOrigin' to set tile/stipple origin to x1/y1
    with-device-coordinates (transform, x1, y1, x2, y2)
      if (filled?)
	x/XFillRectangle(display, drawable, gcontext,
			 x1, y1, x2 - x1, y2 - y1)
      else
	x/XDrawRectangle(display, drawable, gcontext,
			 x1, y1, x2 - x1, y2 - y1)
      end
    end
  end;
  #f
end method draw-rectangle;

define sealed method draw-rectangles
    (medium :: <motif-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    draw-transformed-rectangles(medium, coord-seq, filled?: filled?)
  else
    let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
      = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    let transform = medium-device-transform(medium);
    //---*** Use x/XFillRectangles or x/XDrawRectangles
    do-endpoint-coordinates
      (method (x1, y1, x2, y2)
	 with-device-coordinates (transform, x1, y1, x2, y2)
	   if (filled?)
	     x/XFillRectangle(display, drawable, gcontext,
			      x1, y1, x2 - x1, y2 - y1)
	   else
	     x/XDrawRectangle(display, drawable, gcontext,
			      x1, y1, x2 - x1, y2 - y1)
	   end
	 end
       end,
       coord-seq);
  end;
  #f
end method draw-rectangles;

define sealed method draw-transformed-rectangles
    (medium :: <motif-medium>, coord-seq :: <coordinate-sequence>,
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
    (medium :: <motif-medium>, x1, y1, x2, y2,
     #key filled? = #t, radius) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    unless (radius)
      let width  = x2 - x1;
      let height = y2 - y1;
      radius := max(truncate/(min(width, height), 3), 2)
    end;
    //---*** DO THIS FOR REAL
    draw-rectangle(medium, x1, y1, x2, y2, filled?: filled?)
  end;
  #f
end method draw-rounded-rectangle;

define sealed method draw-polygon
    (medium :: <motif-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  let scoords :: <integer> = size(coord-seq);
  let ncoords :: <integer> = if (closed? & ~filled?) scoords + 2 else scoords end;
  //---*** Can't we use a stack-allocated FFI structure for this?
  let points :: <simple-object-vector> = make(<vector>, size: ncoords);
  without-bounds-checks
    for (i :: <integer> from 0 below ncoords by 2)
      let x = coord-seq[i + 0];
      let y = coord-seq[i + 1];
      with-device-coordinates (transform, x, y)
	points[i + 0] := x;
	points[i + 1] := y
      end;
    finally
      when (closed? & ~filled?)
	let x = coord-seq[0];
	let y = coord-seq[1];
	with-device-coordinates (transform, x, y)
	  points[ncoords - 2] := x;
	  points[ncoords - 1] := y
	end
      end
    end;
  if (filled?)
    let shape = if (scoords <= 6) $Convex else $Complex end;
    x/XFillPolygon(display, drawable, gcontext, points, shape)
  else
    when (closed?)
      points[scoords + 0] := points[0];
      points[scoords + 1] := points[1]
    end;
    x/XDrawLines(display, drawable, gcontext, points)
  end;
  #f
end method draw-polygon;

define constant $2pi-in-64ths-of-degree :: <integer> = 360 * 64;

define sealed method draw-ellipse
    (medium :: <motif-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      let (angle-2, x-radius, y-radius, angle-1)
	= singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
      if (#t					//---*** remove when tilted ellipses work
	  | x-radius = abs(y-radius)		// a circle - rotations are irrelevant
	  | zero?(angle-1))			// axis-aligned ellipse
	let (angle, delta-angle)
	  = if (start-angle & end-angle)
	      let start-angle = mod(start-angle, $2pi);
	      let end-angle   = mod(end-angle, $2pi);
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
	if (filled?)
	  x/XFillArc(display, drawable, gcontext,
		     center-x - x-radius, center-y - y-radius,
		     x-radius * 2, y-radius * 2, angle, delta-angle)
	else
	  x/XDrawArc(display, drawable, gcontext,
		     center-x - x-radius, center-y - y-radius,
		     x-radius * 2, y-radius * 2, angle, delta-angle)
	end
      else
	#f					//---*** do tilted ellipses here
      end;
      SelectObject(hDC, old-object)
    end
  end;
  #f
end method draw-ellipse;

// Motif bitmaps and icons are handled separately
define sealed method draw-image
    (medium :: <motif-medium>, image :: <image>, x, y) => (record)
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let width  = image-width(image);
    let height = image-height(image);
    let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
      = get-gcontext(medium);
    //---*** DRAW THE IMAGE, BUT FOR NOW DRAW A RECTANGLE
    // let (pixel, fill-style, operation, pattern)
    //   = convert-ink-to-DC-components(medium, hDC, image);
    let old-object :: <HANDLE> = SelectObject(hDC, $null-hpen);
    Rectangle(hDC, x, y, x + width, y + height);
    SelectObject(hDC, old-object)
  end;
  #f
end method draw-image;


/// Path graphics

define sealed method start-path
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method start-path;

define sealed method end-path
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method end-path;

define sealed method abort-path
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method abort-path;

define sealed method close-path
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method close-path;

define sealed method stroke-path
    (medium :: <motif-medium>, #key filled?) => (record)
  nyi("X does not support path-based graphics")
end method stroke-path;

define sealed method fill-path
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method fill-path;

define sealed method clip-from-path
    (medium :: <motif-medium>, #key function = $boole-and) => (record)
  nyi("X does not support path-based graphics")
end method clip-from-path;

define sealed method save-clipping-region
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method save-clipping-region;

define sealed method restore-clipping-region
    (medium :: <motif-medium>) => (record)
  nyi("X does not support path-based graphics")
end method restore-clipping-region;

define sealed method move-to
    (medium :: <motif-medium>, x, y) => (record)
  nyi("X does not support path-based graphics")
end method move-to;

define sealed method line-to
    (medium :: <motif-medium>, x, y) => (record)
  nyi("X does not support path-based graphics")
end method line-to;

define sealed method arc-to
    (medium :: <motif-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle) => (record)
  nyi("X does not support path-based graphics")
end method arc-to;

define sealed method curve-to
    (medium :: <motif-medium>, x1, y1, x2, y2, x3, y3) => (record)
  nyi("X does not support path-based graphics")
end method curve-to;


/// 'draw-pixmap', etc

define sealed method draw-pixmap
    (medium :: <motif-medium>, pixmap :: <pixmap>, x, y,
     #key function = $boole-1) => (record)
  do-copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
	       medium, x, y)
end method draw-pixmap;

define sealed method clear-box
    (medium :: <motif-medium>, left, top, right, bottom) => ()
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = get-gcontext(medium);
  let sheet = medium-sheet(medium);
  let transform = sheet-device-transform(sheet);
  with-device-coordinates (transform, left, top, right, bottom)
    x/XSetWindowBackground(display, drawable, medium.%background-pixel);
    x/XClearArea(display, drawable, left, top, right - left, bottom - top, #f)
  end
end method clear-box;


/// Text drawing

define sealed method draw-text
    (medium :: <motif-medium>, character :: <character>, x, y,
     #rest keys,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  ignore(_start, _end, align-x, align-y, do-tabs?,
          towards-x, towards-y, transform-glyphs?);
  let string = make(<string>, size: 1, fill: char);
  apply(draw-text, string, x, y, keys)
end method draw-text;

//---*** What do we do about Unicode strings?
define sealed method draw-text
    (medium :: <motif-medium>, string :: <string>, x, y,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let text-style :: <text-style> = medium-merged-text-style(medium);
  let font :: <motif-font> = text-style-mapping(port(medium), text-style);
  let (display :: x/<Display>, drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = update-drawing-state(medium, font: font);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    when (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    //---*** What about x and y alignment?
    if (do-tabs?)
      let tab-width  = text-size(medium, " ") * 8;
      let tab-origin = if (do-tabs? == #t) x else do-tabs? end;
      let x = 0;
      let s = _start;
      block (break)
	while (#t)
	  let e = find-character(string, '\t', start: s, end: _end);
	  //---*** It would be great if 'with-c-string' took start & end!
	  let substring = copy-sequence(string, start: s, end: e);
	  with-c-string (c-string = substring)
	    x/XDrawString(display, drawable, gcontext,
			  tab-origin + x, y, string, e - s)
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
    else
      //---*** It would be great if 'with-c-string' took start & end!
      let substring
	= if (_start = 0 & _end = length) string
	  else copy-sequence(string, start: _start, end: _end) end;
      with-c-string (c-string = substring)
	x/XDrawString(display, drawable, gcontext,
		      x, y, string, _end - _start)
      end
    end
  end
end method draw-text;
