Module:       duim-geometry-internals
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Geometry utilities

define inline function radians->degrees
    (radians :: <real>) => (degrees :: <float>)
  radians * (360 / $2pi)
end function radians->degrees;

define inline function degrees->radians
    (degrees :: <real>) => (radians :: <float>)
  degrees * ($2pi / 360)
end function degrees->radians;

define inline function square (x :: <real>) x * x end;

// This runs when we already know that the point is inside the bounding box.
// By using perpendicular-distance from line instead of slope and intercept
// we don't have to worry about divide by zero in slope and we're also more
// robust against roundoff error.
define sealed method position-close-to-line?
    (x :: <real>, y :: <real>,
     from-x :: <real>, from-y :: <real>, to-x :: <real>, to-y :: <real>,
     #key thickness = 1) => (true? :: <boolean>)
  let distance = ceiling/(thickness, 2) + 1;
  let dx = to-x - from-x;
  let dy = to-y - from-y;
  (zero?(dx) & zero?(dy))
  | (square((y * dx - x * dy) - (from-y * to-x - from-x * to-y))
     <= square(distance) * (square(dx) + square(dy)))
end method position-close-to-line?;

// This algorithm counts the number of edge segments that intersect
// the ray from (X,Y) to (+infinity,Y).  If there are an odd number of
// crossings, (X,Y) is considered to be inside the polygon.
define sealed method position-inside-polygon?
    (x :: <real>, y :: <real>, position-seq :: <sequence>,
     #key closed? :: <boolean> = #t) => (true? :: <boolean>)
  let crossings :: <integer> = 0;
  let ncoords :: <integer> = size(position-seq) - 1;
  let x = as(<single-float>, x);	// force floating point computations below
  let y = as(<single-float>, y);
  let x1 :: <real> = position-seq[0];
  let y1 :: <real> = position-seq[1];
  let xi :: <real> = x1;
  let yi :: <real> = y1;
  let i :: <integer> = 1;
  local method do-segment (x1 :: <real>, y1 :: <real>, x2 :: <real>, y2 :: <real>) => ()
	  when ((y <= y1) == (y > y2))
	    // Segment crosses ray
	    when (~(y1 = y2)		// ignore horizontal segment
		  & (x - x1) - ((y - y1) * (x2 - x1)) / (y2 - y1) < 0)
	      inc!(crossings)		// point is to the left
	    end
	  end
	end method;
  if (instance?(position-seq, <list>))
    let positions = position-seq;
    until (empty?(positions))
      do-segment(xi, yi,
                 xi := pop!(positions), yi := pop!(positions))
    end
  else
    let positions :: <vector> = position-seq;
    until (i = ncoords)
      do-segment(xi, yi,
                 xi := positions[inc!(i)], yi := positions[inc!(i)]);
    end
  end;
  when (closed?)
    do-segment(xi, yi, x1, y1)
  end;
  odd?(crossings)
end method position-inside-polygon?;

// Computes whether a point is inside an ellipse whose center is (0,0).
// This calculation is exact.
define sealed method position-inside-ellipse?
    (x :: <real>, y :: <real>, 
     radius-1-dx :: <real>, radius-1-dy :: <real>,
     radius-2-dx :: <real>, radius-2-dy :: <real>) => (true? :: <boolean>)
  (square(radius-2-dy * x - radius-2-dx * y)
   + square(radius-1-dx * y - radius-1-dy * x))
  <= square(radius-1-dx * radius-2-dy - radius-1-dy * radius-2-dx)
end method position-inside-ellipse?;

// Computes whether a point is on a stroked ellipse whose center is (0,0).
// This calculation is not exact - the envelope of an ellipse is not an ellipse
// and an "average radius" is used - but it should be ok for thickness small
// compared to radii.  The calculation is exact for circles.
define sealed method position-on-thick-ellipse?
    (x :: <real>, y :: <real>,
     radius-1-dx :: <real>, radius-1-dy :: <real>,
     radius-2-dx :: <real>, radius-2-dy :: <real>,
     #key thickness = 1) => (true? :: <boolean>)
  let det = abs((radius-1-dx * radius-2-dy) - (radius-1-dy * radius-2-dx));
  let avrad*delta = sqrt(det) * thickness;
  square(det - avrad*delta)
    <= square(radius-2-dy * x - radius-2-dx * y)
       + square(radius-1-dx * y - radius-1-dy * x)
  & square(det - avrad*delta) <= square(det + avrad*delta)
end method position-on-thick-ellipse?;

// Find the singular value decomposition of a 2 by 2 matrix: M = R1.D.R2
// where R's are rotations and D is diagonal.  The four values returned
// are the first angle, the two diagonal elements, and the second angle.
// Used to convert internal representation of ellipses to various window
// systems' representations.
define sealed method singular-value-decomposition-2x2 
    (a :: <real>, b :: <real>, c :: <real>, d :: <real>)
 => (theta1 :: <real>, d1 :: <real>, d2 :: <real>, theta2 :: <real>)
  case
    zero?(b) & zero?(c) =>
      values(0.0, a, d, 0.0);
    zero?(a) & zero?(d) =>
      values($pi/2, b, -c, 0.0);
    otherwise =>
      let d+a = d + a;
      let a-d = a - d;
      let c+b = c + b;
      let c-b = c - b;
      let sx+sy = sqrt(square(d+a) + square(c-b));
      let sx-sy = sqrt(square(a-d) + square(c+b));
      let sx = 0.5 * (sx+sy + sx-sy);
      let sy = 0.5 * (sx+sy - sx-sy);
      let t1+t2
        = if (zero?(c-b) & zero?(d+a)) 0.0 else atan2(c-b, d+a) end;
      let t1-t2
        = if (zero?(c+b) & zero?(a-d)) 0.0 else atan2(c+b, a-d) end;
      let t1 = 0.5 * (t1+t2 + t1-t2);
      let t2 = 0.5 * (t1+t2 - t1-t2);
      values(t2, sx, sy, t1)
  end
end method singular-value-decomposition-2x2;

define sealed method coordinate-sequence-box 
    (coordinates, #key thickness :: <integer> = 0)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let min-x :: <integer> = $largest-coordinate;
  let min-y :: <integer> = $largest-coordinate;
  let max-x :: <integer> = $smallest-coordinate;
  let max-y :: <integer> = $smallest-coordinate;
  local method compute-corners (x, y) => ()
	  min!(min-x, floor(x));
	  min!(min-y, floor(y));
	  max!(max-x, ceiling(x));
	  max!(max-y, ceiling(y))
	end method;
  do-coordinates(compute-corners, coordinates);
  values(min-x - thickness, min-y - thickness,
	 max-x + thickness, max-y + thickness)
end method coordinate-sequence-box;

// For a complete ellipse, the box is actually the rectangle that bounds
// the parallelogram that bounds the ellipse.  That means it's a little
// bigger than the tightest possible bounding box when the ellipse is
// not axis-aligned.  It's not worth computing anything tighter because
// the refined highlighting test will be faster than the computation of
// a tighter box.
define sealed method elliptical-arc-box
    (center-x :: <real>, center-y :: <real>, 
     radius-1-dx :: <real>, radius-1-dy :: <real>,
     radius-2-dx :: <real>, radius-2-dy :: <real>,
     #key start-angle, end-angle, thickness = 0)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let filled? = ~thickness;
  let thickness = thickness | 0;
  let lthickness = truncate/(thickness, 2);
  let rthickness = thickness - lthickness;
  if (~start-angle & ~end-angle)
    let dx = abs(radius-1-dx) + abs(radius-2-dx);
    let dy = abs(radius-1-dy) + abs(radius-2-dy);
    fix-box(center-x - dx - lthickness, center-y - dy - lthickness,
	    center-x + dx + rthickness, center-y + dy + rthickness)
  else
    start-angle := modulo(start-angle | 0.0, $2pi);
    end-angle   := modulo(end-angle | $2pi, $2pi);
    let (x-radius, y-radius)
      = case
          radius-1-dx = 0 & radius-2-dy = 0 =>
            values(abs(radius-2-dx), abs(radius-1-dy));
          radius-2-dx = 0 & radius-1-dy = 0 =>
            values(abs(radius-1-dx), abs(radius-2-dy));
          otherwise =>
            let s-1 = radius-1-dx * radius-1-dx + radius-1-dy * radius-1-dy;
            let s-2 = radius-2-dx * radius-2-dx + radius-2-dy * radius-2-dy;
            if (s-1 = s-2)
              // Degrade to drawing a rectilinear ellipse
              let r = truncate(sqrt(s-1));
              values(r, r)
            else
              values(truncate(sqrt(s-1)), truncate(sqrt(s-2)))
            end
        end;
    let cos1 = cos(start-angle);
    let sin1 = sin(start-angle);
    let cos2 = cos(end-angle);
    let sin2 = sin(end-angle);
    let x1 = center-x + x-radius * cos1;
    let y1 = center-y + y-radius * sin1;
    let x2 = center-x + x-radius * cos2;
    let y2 = center-y + y-radius * sin2;
    let left = min(x1, x2);
    let top = min(y1, y2);
    let right = max(x1, x2);
    let bottom = max(y1, y2);
    when (angle-between-angles?($pi, start-angle, end-angle))
      min!(left, center-x - x-radius)
    end;
    when (angle-between-angles?($pi/2 * 3, start-angle, end-angle))
      min!(top, center-y - y-radius)
    end;
    when (angle-between-angles?(0, start-angle, end-angle))
      max!(right, center-x + x-radius)
    end;
    when (angle-between-angles?($pi/2, start-angle, end-angle))
      max!(bottom, center-y + y-radius)
    end;
    when (filled?)
      min!(left, center-x);
      min!(top, center-y);
      max!(right, center-x);
      max!(bottom, center-y)
    end;
    fix-box(left - lthickness,  top - lthickness,
            right + rthickness, bottom + rthickness)
  end
end method elliptical-arc-box;

define sealed method angle-between-angles?
    (theta :: <real>, start-angle :: <real>, end-angle :: <real>)
 => (true? :: <boolean>)
  unless (start-angle < end-angle)
    inc!(end-angle, $2pi)
  end;
  unless (start-angle < theta)
    inc!(theta, $2pi)
  end;
  theta < end-angle
end method angle-between-angles?;
