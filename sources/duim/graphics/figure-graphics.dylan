Module:       duim-graphics-internals
Synopsis:     DUIM graphics
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Figure-based Graphics

//--- These could become limited collections...
define constant <coordinate-sequence> = <sequence>;
define constant <point-sequence> = <sequence>;

define protocol <<figure-graphics-protocol>> ()
  // API level functions
  function draw-point
    (drawable :: <drawable>, x, y) => (record);
  function draw-points
    (drawable :: <drawable>, coord-seq :: <coordinate-sequence>) => (record);
  function draw-line
    (drawable :: <drawable>, x1, y1, x2, y2) => (record);
  function draw-lines
    (drawable :: <drawable>, coord-seq :: <coordinate-sequence>) => (record);
  function draw-arrow
    (drawable :: <drawable>, x1, y1, x2, y2,
     #key from-head?, to-head?, head-length, head-width) => (record);
  function draw-rectangle
    (drawable :: <drawable>, x1, y1, x2, y2,
     #key filled?) => (record);
  function draw-rectangles
    (drawable :: <drawable>, coord-seq :: <coordinate-sequence>,
     #key filled?) => (record);
  function draw-rounded-rectangle
    (drawable :: <drawable>, x1, y1, x2, y2,
     #key filled?, radius) => (record);
  function draw-polygon
    (drawable :: <drawable>, coord-seq :: <coordinate-sequence>,
     #key closed?, filled?) => (record);
  function draw-triangle
    (drawable :: <drawable>, x1, y1, x2, y2, x3, y3,
     #key filled?) => (record);
  function draw-ellipse
    (drawable :: <drawable>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled?) => (record);
  function draw-oval
    (drawable :: <drawable>, center-x, center-y, x-radius, y-radius,
     #rest keys, #key filled? = #t, #all-keys) => (record);
  function draw-bezier-curve
    (drawable :: <drawable>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record);
  // Image drawing
  function draw-image
    (drawable :: <drawable>, image :: <image>, x, y) => (record);
  // Text drawing
  function draw-text
    (drawable :: <drawable>, string-or-char, x, y,
     #key start, end: _end, align-x, align-y, do-tabs?,
          towards-x, towards-y, transform-glyphs?) => (record);
  // Pixel drawing
  function set-pixel
    (drawable :: <drawable>, color :: <rgb-color>, x, y) => (record);
  function set-pixels
    (drawable :: <drawable>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>) => (record);
end protocol <<figure-graphics-protocol>>;


/// DRAW-POINT

define method draw-point
    (sheet :: <basic-sheet>, x, y) => (record)
  with-sheet-medium (medium = sheet)
    draw-point(medium, x, y)
  end
end method draw-point;

define method draw-point 
    (sheet :: <permanent-medium-mixin>, x, y) => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  draw-point(medium, x, y)
end method draw-point;

define function draw-point* 
    (drawable :: <drawable>, point :: <standard-point>) => (record)
  draw-point(drawable, point-x(point), point-y(point))
end function draw-point*;


/// DRAW-POINTS

define method draw-points 
    (sheet :: <basic-sheet>, coord-seq :: <coordinate-sequence>) => (record)
  with-sheet-medium (medium = sheet)
    draw-points(medium, coord-seq)
  end
end method draw-points;

define method draw-points 
    (sheet :: <permanent-medium-mixin>, coord-seq :: <coordinate-sequence>) => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  draw-points(medium, coord-seq)
end method draw-points;

define function draw-points* 
    (drawable :: <drawable>, points :: <point-sequence>) => (record)
  draw-points(drawable, spread-point-sequence(points))
end function draw-points*;


/// DRAW-LINE and friends

define method draw-line 
    (sheet :: <basic-sheet>, x1, y1, x2, y2) => (record)
  with-sheet-medium (medium = sheet)
    draw-line(medium, x1, y1, x2, y2)
  end
end method draw-line;

define method draw-line 
    (sheet :: <permanent-medium-mixin>, x1, y1, x2, y2) => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  draw-line(medium, x1, y1, x2, y2)
end method draw-line;

define function draw-line* 
    (drawable :: <drawable>, point1 :: <standard-point>, point2 :: <standard-point>) => (record)
  draw-line(drawable, point-x(point1), point-y(point1),
		      point-x(point2), point-y(point2))
end function draw-line*;


define method draw-arrow 
    (sheet :: <basic-sheet>, x1, y1, x2, y2,
     #rest keys, #key from-head?, to-head? = #t, head-length, head-width) => (record)
  ignore(from-head?, to-head?, head-length, head-width);
  with-sheet-medium (medium = sheet)
    apply(draw-arrow, medium, x1, y1, x2, y2, keys)
  end
end method draw-arrow;

define method draw-arrow 
    (sheet :: <permanent-medium-mixin>, x1, y1, x2, y2,
     #rest keys, #key from-head?, to-head? = #t, head-length, head-width) => (record)
  ignore(from-head?, to-head?, head-length, head-width);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-arrow, medium, x1, y1, x2, y2, keys)
end method draw-arrow;

define method draw-arrow
    (medium :: <basic-medium>, x1, y1, x2, y2,
     #key from-head?, to-head? = #t, head-length, head-width) => (record)
  case
    ~head-length & ~head-width =>
      let pw = max(pen-width(medium-pen(medium)), 1);
      head-width  := pw * 4 + pw;
      head-length := head-width * 2;
    ~head-length =>
      head-length := head-width * 2;
    ~head-width =>
      head-width  := truncate/(head-length, 2);
  end;
  let dx = x2 - x1;
  let dy = y2 - y1;
  let norm
    = if (zero?(dx))
        if (zero?(dy)) 0.0 else 1.0 / abs(dy) end
      else
        if (zero?(dy)) 1.0 / abs(dx) else 1.0 / sqrt(dx * dx + dy * dy) end
      end;
  when (norm > 0)
    let length-norm = head-length * norm;
    let ldx = dx * length-norm;
    let ldy = dy * length-norm;
    let base-norm = head-width * norm * 0.5;
    let bdx = dy * base-norm;
    let bdy = dx * base-norm;
    when (from-head?)
      let xa = x1 + ldx;
      let ya = y1 + ldy;
      with-stack-vector (coords = x1, y1, xa + bdx, ya - bdy, xa - bdx, ya + bdy)
	draw-polygon(medium, coords, filled?: #t)
      end;
      x1 := xa;
      y1 := ya
    end;
    when (to-head?)
      let xa = x2 - ldx;
      let ya = y2 - ldy;
      with-stack-vector (coords = x2, y2, xa + bdx, ya - bdy, xa - bdx, ya + bdy)
	draw-polygon(medium, coords, filled?: #t)
      end;
      x2 := xa;
      y2 := ya
    end;
    // Draw the line after drawing the arrowheads so that the shortening 
    // by the length of the heads has its useful effect
    draw-line(medium, x1, y1, x2, y2)
  end
end method draw-arrow;

define function draw-arrow*
    (drawable :: <drawable>, point1 :: <standard-point>, point2 :: <standard-point>,
     #rest keys) => (record)
  dynamic-extent(keys);
  apply(draw-arrow, drawable,
	point-x(point1), point-y(point1),
	point-x(point2), point-y(point2), keys)
end function draw-arrow*;


/// DRAW-LINES

define method draw-lines 
    (sheet :: <basic-sheet>, coord-seq :: <coordinate-sequence>) => (record)
  with-sheet-medium (medium = sheet)
    draw-lines(medium, coord-seq)
  end
end method draw-lines;

define method draw-lines 
    (sheet :: <permanent-medium-mixin>, coord-seq :: <coordinate-sequence>) => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  draw-lines(medium, coord-seq)
end method draw-lines;

define function draw-lines* 
    (drawable :: <drawable>, points :: <point-sequence>) => (record)
  draw-lines(drawable, spread-point-sequence(points))
end function draw-lines*;


/// DRAW-RECTANGLE

define method draw-rectangle
    (sheet :: <basic-sheet>, x1, y1, x2, y2,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  with-sheet-medium (medium = sheet)
    apply(draw-rectangle, medium, x1, y1, x2, y2, keys)
  end
end method draw-rectangle;

define method draw-rectangle
    (sheet :: <permanent-medium-mixin>, x1, y1, x2, y2,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-rectangle, medium, x1, y1, x2, y2, keys)
end method draw-rectangle;

define function draw-rectangle*
    (drawable :: <drawable>, point1 :: <standard-point>, point2 :: <standard-point>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  apply(draw-rectangle, drawable,
	point-x(point1), point-y(point1),
	point-x(point2), point-y(point2), keys)
end function draw-rectangle*;


/// DRAW-RECTANGLES

define method draw-rectangles
    (sheet :: <basic-sheet>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  with-sheet-medium (medium = sheet)
    apply(draw-rectangles, medium, coord-seq, keys)
  end
end method draw-rectangles;

define method draw-rectangles
    (sheet :: <permanent-medium-mixin>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-rectangles, medium, coord-seq, keys)
end method draw-rectangles;

define function draw-rectangles*
    (drawable :: <drawable>, points :: <point-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  apply(draw-rectangles, drawable, spread-point-sequence(points), keys)
end function draw-rectangles*;


/// DRAW-ROUNDED-RECTANGLE

define method draw-rounded-rectangle
    (sheet :: <basic-sheet>, x1, y1, x2, y2,
     #rest keys, #key filled? = #t, radius) => (record)
  dynamic-extent(keys);
  ignore(filled?, radius);
  with-sheet-medium (medium = sheet)
    apply(draw-rounded-rectangle, medium, x1, y1, x2, y2, keys)
  end
end method draw-rounded-rectangle;

define method draw-rounded-rectangle
    (sheet :: <permanent-medium-mixin>, x1, y1, x2, y2,
     #rest keys, #key filled? = #t, radius) => (record)
  dynamic-extent(keys);
  ignore(filled?, radius);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-rounded-rectangle, medium, x1, y1, x2, y2, keys)
end method draw-rounded-rectangle;

define function draw-rounded-rectangle*
    (drawable :: <drawable>, point1 :: <standard-point>, point2 :: <standard-point>,
     #rest keys, #key filled? = #t, radius) => (record)
  dynamic-extent(keys);
  ignore(filled?, radius);
  apply(draw-rounded-rectangle, drawable,
	point-x(point1), point-y(point1),
	point-x(point2), point-y(point2), keys)
end function draw-rounded-rectangle*;


/// DRAW-POLYGON and friends

define method draw-polygon
    (sheet :: <basic-sheet>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key closed? = #t, filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(closed?, filled?);
  with-sheet-medium (medium = sheet)
    apply(draw-polygon, medium, coord-seq, keys)
  end
end method draw-polygon;

define method draw-polygon
    (sheet :: <permanent-medium-mixin>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key closed? = #t, filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(closed?, filled?);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-polygon, medium, coord-seq, keys)
end method draw-polygon;

define function draw-polygon*
    (drawable :: <drawable>, points :: <point-sequence>,
     #rest keys, #key closed? = #t, filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(closed?, filled?);
  apply(draw-polygon, drawable, spread-point-sequence(points), keys)
end function draw-polygon*;


define method draw-regular-polygon
    (drawable :: <drawable>, x1, y1, x2, y2, nsides :: <integer>,
     #rest keys,
     #key handedness = #"left", closed? = #t, filled? = #t,
     #all-keys) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let theta
    = ($2pi / nsides)
        * select (handedness)
	    #"left" => 1;
	    #"right" => -1
	  end;
  let transform = make-rotation-transform(theta);
  let coords :: <simple-object-vector>
    = make(<simple-vector>, size: (if (closed?) nsides + 1 else nsides end) * 2);
  let index = 4;
  without-bounds-checks
    coords[0] := x1; coords[1] := y1;
    coords[2] := x2; coords[3] := y2;
    let dx = x2 - x1;
    let dy = y2 - y1;
    let next-x = x2;
    let next-y = y2;
    for (i :: <integer> from 0 below nsides - 2)
      transform-distances!(transform, dx, dy);
      inc!(next-x, dx);
      inc!(next-y, dy);
      coords[index + 0] := next-x;
      coords[index + 1] := next-y;
      inc!(index, 2)
    end;
    when (closed?)
      coords[index + 0] := x1;
      coords[index + 1] := y1
    end
  end;
  with-keywords-removed (keys = keys, #[handedness:])
    apply(draw-polygon, drawable, coords, keys)
  end
end method draw-regular-polygon;

define function draw-regular-polygon*
    (drawable :: <drawable>, point1 :: <standard-point>, point2 :: <standard-point>, 
     nsides :: <integer>,
     #rest keys) => (record)
  dynamic-extent(keys);
  apply(draw-regular-polygon, drawable,
	point-x(point1), point-y(point1),
	point-x(point2), point-y(point2), nsides, keys)
end function draw-regular-polygon*;


define method draw-triangle
    (drawable :: <drawable>, x1, y1, x2, y2, x3, y3,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  with-stack-vector (coords = x1, y1, x2, y2, x3, y3)
    apply(draw-polygon, drawable, coords, closed?: #t, keys)
  end
end method draw-triangle;

define function draw-triangle*
    (drawable :: <drawable>,
     p1 :: <standard-point>, p2 :: <standard-point>, p3 :: <standard-point>,
     #rest keys) => (record)
  dynamic-extent(keys);
  with-stack-vector (points = p1, p2, p3)
    apply(draw-polygon*, drawable, points, closed?: #t, keys)
  end
end function draw-triangle*;


/// DRAW-ELLIPSE and friends

define method draw-ellipse
    (sheet :: <basic-sheet>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #rest keys, #key start-angle, end-angle, filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(start-angle, end-angle, filled?);
  with-sheet-medium (medium = sheet)
    apply(draw-ellipse, medium, center-x, center-y,
	  radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
  end
end method draw-ellipse;

define method draw-ellipse
    (sheet :: <permanent-medium-mixin>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #rest keys, #key start-angle, end-angle, filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(start-angle, end-angle, filled?);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-ellipse, medium, center-x, center-y,
	radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
end method draw-ellipse;

define function draw-ellipse*
    (drawable :: <drawable>, center :: <standard-point>, 
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #rest keys, #key start-angle, end-angle, filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(start-angle, end-angle, filled?);
  apply(draw-ellipse, drawable, point-x(center), point-y(center),
	radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
end function draw-ellipse*;


define method draw-circle
    (drawable :: <drawable>, center-x, center-y, radius, #rest keys) => (record)
  dynamic-extent(keys);
  apply(draw-ellipse, drawable, center-x, center-y,
	radius, 0, 0, radius, keys)
end method draw-circle;

define function draw-circle*
    (drawable :: <drawable>, center :: <standard-point>, radius, #rest keys) => (record)
  dynamic-extent(keys);
  apply(draw-ellipse, drawable, point-x(center), point-y(center),
	radius, 0, 0, radius, keys)
end function draw-circle*;


define method draw-oval
    (drawable :: <drawable>, center-x, center-y, x-radius, y-radius,
     #rest keys, #key filled? = #t, #all-keys) => (record)
  dynamic-extent(keys);
  let left  = center-x - x-radius;
  let right = center-x + x-radius;
  let top    = center-y - y-radius;
  let bottom = center-y + y-radius;
  case
    x-radius = y-radius | zero?(x-radius) =>
      apply(draw-ellipse, drawable, center-x, center-y,
	    y-radius, 0, 0, y-radius, keys);
    zero?(y-radius) =>
      apply(draw-ellipse, drawable, center-x, center-y,
	    x-radius, 0, 0, x-radius, keys);
    x-radius > y-radius =>
      let rect-left  = left + y-radius;
      let rect-right = right - y-radius;
      case
        filled? =>
          draw-rectangle
            (drawable, rect-left, top, rect-right, bottom, filled?: #t);
        otherwise =>
          draw-line(drawable, rect-left, top, rect-right, top);
          draw-line(drawable, rect-left, bottom, rect-right, bottom)
      end;
      let north = $pi/2;
      let south = $pi/2 * 3;
      apply(draw-ellipse, drawable, rect-left, center-y,
	    y-radius, 0, 0, y-radius, start-angle: north, end-angle: south, keys);
      apply(draw-ellipse, drawable, rect-right, center-y,
	    y-radius, 0, 0, y-radius, start-angle: south, end-angle: north, keys);
    otherwise =>
      let rect-top = top + x-radius;
      let rect-bottom = bottom - x-radius;
      case
        filled? =>
          draw-rectangle
            (drawable, left, rect-top, right, rect-bottom, filled?: #t);
        otherwise =>
          draw-line(drawable, left, rect-top, left, rect-bottom);
          draw-line(drawable, right, rect-top, right, rect-bottom)
      end;
      let east = 0.0;
      let west = $pi;
      apply(draw-ellipse, drawable, center-x, rect-top,
	    x-radius, 0, 0, x-radius, start-angle: west, end-angle: east, keys);
      apply(draw-ellipse, drawable, center-x, rect-bottom,
	    x-radius, 0, 0, x-radius, start-angle: east, end-angle: west, keys)
  end
end method draw-oval;

define function draw-oval*
    (drawable :: <drawable>, center :: <standard-point>, x-radius, y-radius, 
     #rest keys) => (record)
  dynamic-extent(keys);
  apply(draw-oval, drawable, point-x(center), point-y(center),
	x-radius, y-radius, keys)
end function draw-oval*;


/// DRAW-IMAGE

define method draw-image
    (sheet :: <basic-sheet>, image :: <image>, x, y) => (record)
  with-sheet-medium (medium = sheet)
    apply(draw-image, medium, image, x, y)
  end
end method draw-image;

define method draw-image
    (sheet :: <permanent-medium-mixin>, image :: <image>, x, y) => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  draw-image(medium, image, x, y)
end method draw-image;

define function draw-image*
    (drawable :: <drawable>, image :: <image>, point :: <standard-point>) => (record)
  draw-image(drawable, image, point-x(point), point-y(point))
end function draw-image*;

// The default implementation of 'draw-image' is just a special case of
// 'draw-rectangle', believe it or not...
define method draw-image
    (medium :: <basic-medium>, image :: <image>, x, y) => (record)
  let width  = image-width(image);
  let height = image-height(image);
  with-drawing-options (medium, brush: image)
    draw-rectangle(medium, x, y, x + width, y + height, filled?: #t)
  end
end method draw-image;


/// The rest of CLEAR-BOX

// Some mediums can do better than this...
// Note that the coordinates are unaffected by the medium transformation!
define sideways method clear-box
    (medium :: <basic-medium>, left, top, right, bottom) => ()
  dynamic-bind (medium-brush(medium) = medium-background(medium),
		medium-transform(medium) = $identity-transform)
    draw-rectangle(medium, left, top, right, bottom, filled?: #t)
  end
end method clear-box;


/// DRAW-TEXT

define method draw-text
    (sheet :: <basic-sheet>, string-or-char, x, y,
     #rest keys,
     #key start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs?,
          towards-x, towards-y, transform-glyphs?) => (record)
  dynamic-extent(keys);
  ignore(start, _end, align-x, align-y, towards-x, towards-y, transform-glyphs?, do-tabs?);
  with-sheet-medium (medium = sheet)
    apply(draw-text, medium, string-or-char, x, y, keys)
  end
end method draw-text;

define method draw-text
    (sheet :: <permanent-medium-mixin>, string-or-char, x, y,
     #rest keys,
     #key start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs?,
          towards-x, towards-y, transform-glyphs?) => (record)
  dynamic-extent(keys);
  ignore(start, _end, align-x, align-y, towards-x, towards-y, transform-glyphs?, do-tabs?);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-text, medium, string-or-char, x, y, keys)
end method draw-text;

define function draw-text*
    (drawable :: <drawable>, string-or-char, point :: <standard-point>,
     #rest keys, #key towards-point, #all-keys) => (record)
  dynamic-extent(keys);
  with-keywords-removed (keys = keys, #[towards-point:])
    apply(draw-text, drawable, string-or-char, point-x(point), point-y(point),
          towards-x: towards-point & point-x(towards-point),
          towards-y: towards-point & point-y(towards-point), keys)
  end
end function draw-text*;


/// DRAW-BEZIER-CURVE

define method draw-bezier-curve
    (sheet :: <basic-sheet>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  with-sheet-medium (medium = sheet)
    apply(draw-bezier-curve, medium, coord-seq, keys)
  end
end method draw-bezier-curve;

define method draw-bezier-curve
    (sheet :: <permanent-medium-mixin>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let medium :: <basic-medium> = sheet-medium(sheet);
  apply(draw-bezier-curve, medium, coord-seq, keys)
end method draw-bezier-curve;

define function draw-bezier-curve*
    (drawable :: <drawable>, points :: <point-sequence>, #rest keys) => (record)
  dynamic-extent(keys);
  apply(draw-bezier-curve, drawable, spread-point-sequence(points), keys)
end function draw-bezier-curve*;

// Primary method for mediums that can't do this natively...
define method draw-bezier-curve
    (medium :: <basic-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let npoints :: <integer> = size(coord-seq);
  let points  :: <stretchy-object-vector> = make(<stretchy-vector>);
  let distance = 1;
  assert(zero?(modulo(truncate/(npoints, 2) - 4, 3)),
         "Incorrect number of points for Bezier curve drawing");
  local method collect (x, y) => ()
	  add!(points, x);
	  add!(points, y)
	end method;
  dynamic-extent(collect);
  collect(coord-seq[0], coord-seq[1]);
  for (i :: <integer> = 0 then i + 6,
       until: i = npoints - 2)
    render-bezier-curve
      (collect,
       coord-seq[i + 0], coord-seq[i + 1],
       coord-seq[i + 2], coord-seq[i + 3],
       coord-seq[i + 4], coord-seq[i + 5],
       coord-seq[i + 6], coord-seq[i + 7], distance);
    collect(coord-seq[i + 6], coord-seq[i + 7])
  end;
  draw-polygon(medium, points, closed?: #f, filled?: filled?)
end method draw-bezier-curve;

define method render-bezier-curve
    (function :: <function>, x0, y0, x1, y1, x2, y2, x3, y3, distance) => ()
  local method split-bezier-curve (x0, y0, x1, y1, x2, y2, x3, y3)
	  let r1/2 :: <single-float> = 1.0 / 2.0;
	  let r1/4 :: <single-float> = 1.0 / 4.0;
	  let r1/8 :: <single-float> = 1.0 / 8.0;
	  let r3/8 :: <single-float> = 3.0 / 8.0;
	  // We should write a matrix multiplication macro
	  values
	    (// The first 1/2
	     x0, y0,
	     x0 * r1/2 + x1 * r1/2, y0 * r1/2 + y1 * r1/2,
	     x0 * r1/4 + x1 * r1/2 + x2 * r1/4, y0 * r1/4 + y1 * r1/2 + y2 * r1/4,
	     x0 * r1/8 + x1 * r3/8 + x2 * r3/8 + x3 * r1/8,
	     y0 * r1/8 + y1 * r3/8 + y2 * r3/8 + y3 * r1/8,
	     // The second 1/2
	     x0 * r1/8 + x1 * r3/8 + x2 * r3/8 + x3 * r1/8,
	     y0 * r1/8 + y1 * r3/8 + y2 * r3/8 + y3 * r1/8,
	     x1 * r1/4 + x2 * r1/2 + x3 * r1/4, y1 * r1/4 + y2 * r1/2 + y3 * r1/4,
	     x2 * r1/2 + x3 * r1/2, y2 * r1/2 + y3 * r1/2,
	     x3, y3)
	end method,
        method distance-from-line (x0, y0, x1, y1, x, y)
	  let dx = x1 - x0;
	  let dy = y1 - y0;
	  let r-p-x = x - x0;
	  let r-p-y = y - y0;
	  let dot-v = dx * dx + dy * dy;
	  let dot-r-v = r-p-x * dx + r-p-y * dy;
	  let dp = as(<single-float>, dot-r-v) / as(<single-float>, dot-v);
	  let closest-x = x0 + dp * dx;
	  let closest-y = y0 + dp * dy;
	  let ax = x - closest-x;
	  let ay = y - closest-y;
	  values(ax * ax + ay * ay, closest-x, closest-y)
	end method;
  dynamic-extent(split-bezier-curve, distance-from-line);
  let d1 = distance-from-line(x0, y0, x3, y3, x1, y1);
  let d2 = distance-from-line(x0, y0, x3, y3, x2, y2);
  when (d1 >= distance | d2 >= distance)
    let (x00, y00, x10, y10, x20, y20, x30, y30,
	 x01, y01, x11, y11, x21, y21, x31, y31)
      = split-bezier-curve(x0, y0, x1, y1, x2, y2, x3, y3);
    render-bezier-curve
      (function, x00, y00, x10, y10, x20, y20, x30, y30, distance);
    function(x30, y30);
    render-bezier-curve
      (function, x01, y01, x11, y11, x21, y21, x31, y31, distance)
  end
end method render-bezier-curve;


/// SET-PIXEL

define method set-pixel
    (sheet :: <basic-sheet>, color :: <rgb-color>, x, y) => (record)
  with-sheet-medium (medium = sheet)
    set-pixel(medium, color, x, y)
  end
end method set-pixel;

define method set-pixel 
    (sheet :: <permanent-medium-mixin>, color :: <rgb-color>, x, y) => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  set-pixel(medium, color, x, y)
end method set-pixel;

define function set-pixel* 
    (drawable :: <drawable>, color :: <rgb-color>, point :: <standard-point>) => (record)
  set-pixel(drawable, color, point-x(point), point-y(point))
end function set-pixel*;


/// SET-PIXELS

define method set-pixels 
    (sheet :: <basic-sheet>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  with-sheet-medium (medium = sheet)
    set-pixels(medium, color, coord-seq)
  end
end method set-pixels;

define method set-pixels 
    (sheet :: <permanent-medium-mixin>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  let medium :: <basic-medium> = sheet-medium(sheet);
  set-pixels(medium, color, coord-seq)
end method set-pixels;

define function set-pixels* 
    (drawable :: <drawable>, color :: <rgb-color>, points :: <point-sequence>)
 => (record)
  set-pixels(drawable, color, spread-point-sequence(points))
end function set-pixels*;
