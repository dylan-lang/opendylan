Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Lines

define protocol <<line-protocol>> (<<polygon-protocol>>)
  function line-start-point
    (line :: <line>) => (point :: <point>);
  function line-end-point
    (line :: <line>) => (point :: <point>);
  function line-start-position
    (line :: <line>) => (x :: <real>, y :: <real>);
  function line-end-position
    (line :: <line>) => (x :: <real>, y :: <real>);
end protocol <<line-protocol>>;

define sealed class <standard-line> (<line>)
  sealed constant slot %start-x :: <real>,
    required-init-keyword: start-x:;
  sealed constant slot %start-y :: <real>,
    required-init-keyword: start-y:;
  sealed constant slot %end-x :: <real>,
    required-init-keyword: end-x:;
  sealed constant slot %end-y :: <real>,
    required-init-keyword: end-y:;
  sealed slot %points :: false-or(<vector>) = #f,
    init-keyword: points:;
end class <standard-line>;

define sealed domain make (singleton(<standard-line>));
define sealed domain initialize (<standard-line>);

define inline function make-line
    (start-x, start-y, end-x, end-y) => (line :: <standard-line>)
  make(<standard-line>,
       start-x: start-x, start-y: start-y,
       end-x: end-x, end-y: end-y)
end function make-line;

define inline function make-line*
    (start-point, end-point) => (line :: <standard-line>)
  make(<standard-line>,
       start-x: point-x(start-point), start-y: point-y(start-point),
       end-x: point-x(end-point), end-y: point-y(end-point),
       points: vector(start-point, end-point))
end function make-line*;

define sealed inline method make
    (class == <line>, #key start-point, end-point)
 => (line :: <standard-line>)
  make-line*(start-point, end-point);
end method make;

define method line-start-position
    (line :: <standard-line>) => (x :: <real>, y :: <real>)
  values(line.%start-x, line.%start-y)
end method line-start-position;

define method line-end-position
    (line :: <standard-line>) => (x :: <real>, y :: <real>)
  values(line.%end-x, line.%end-y)
end method line-end-position;

define method polygon-points (line :: <standard-line>) => (points :: <vector>)
  line.%points
  | (line.%points := vector(make-point(line.%start-x, line.%start-y),
			    make-point(line.%end-x, line.%end-y)))
end method polygon-points;

define method line-start-point
    (line :: <standard-line>) => (point :: <standard-point>)
  polygon-points(line)[0]
end method line-start-point;

define method line-end-point
    (line :: <standard-line>) => (point :: <standard-point>)
  polygon-points(line)[1]
end method line-end-point;

define method polyline-closed? (line :: <standard-line>) => (true? :: <boolean>)
  #f
end method polyline-closed?;

define method do-polygon-coordinates
    (function :: <function>, line :: <standard-line>) => ()
  function(line.%start-x, line.%start-y);
  function(line.%end-x, line.%end-y)
end method do-polygon-coordinates;

define method do-polygon-segments
    (function :: <function>, line :: <standard-line>) => ()
  function(line.%start-x, line.%start-y, line.%end-x, line.%end-y)
end method do-polygon-segments;

define method transform-region
    (transform :: <transform>, line :: <standard-line>) => (line :: <standard-line>)
  let (sx, sy) = transform-position(transform, line.%start-x, line.%start-y);
  let (ex, ey) = transform-position(transform, line.%end-x, line.%end-y);
  make-line(sx, sy, ex, ey)
end method transform-region;

define method box-edges
    (line :: <standard-line>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  fix-box(min(line.%start-x, line.%end-x), min(line.%start-y, line.%end-y),
          max(line.%start-x, line.%end-x), max(line.%start-y, line.%end-y))
end method box-edges;

define method region-equal
    (line1 :: <standard-line>, line2 :: <standard-line>) => (true? :: <boolean>)
  (line1.%start-x = line2.%start-x
   & line1.%start-y = line2.%start-y
   & line1.%end-x = line2.%end-x
   & line1.%end-y = line2.%end-y)
  | (line1.%start-x = line2.%end-x
     & line1.%start-y = line2.%end-y
     & line1.%end-x = line2.%start-x
     & line1.%end-y = line2.%start-y)
end method region-equal;

define method region-contains-position?
    (line :: <standard-line>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  let (left, top, right, bottom) = box-edges(line);
  ltrb-contains-position?(left, top, right, bottom,
			  fix-coordinate(x), fix-coordinate(y))
  & position-close-to-line?(x, y,
			    line.%start-x, line.%start-y,
			    line.%end-x, line.%end-y)
end method region-contains-position?;

define method region-contains-region?
    (line1 :: <standard-line>, line2 :: <standard-line>) => (true? :: <boolean>)
  region-contains-position?(line1, line2.%start-x, line2.%start-y)
  & region-contains-position?(line1, line2.%end-x, line2.%end-y)
end method region-contains-region?;

define method region-intersects-region?
    (line1 :: <standard-line>, line2 :: <standard-line>) => (true? :: <boolean>)
  line-intersects-line?(line1.%start-x, line1.%start-y, line1.%end-x, line1.%end-y,
			line2.%start-x, line2.%start-y, line2.%end-x, line2.%end-y)
end method region-intersects-region?;

define method region-intersection
    (line1 :: <standard-line>, line2 :: <standard-line>) => (region :: <region>)
  if (region-intersects-region?(line1, line2))
    make-line
      (max(line1.%start-x, line2.%start-x),
       max(line1.%start-y, line2.%start-y), min(line1.%end-x, line2.%end-x),
       min(line1.%end-y, line2.%end-y))
  else
    $nowhere
  end
end method region-intersection;


/// Line geometry

define method line-intersects-line?
    (sx1 :: <real>, sy1 :: <real>, ex1 :: <real>, ey1 :: <real>,
     sx2 :: <real>, sy2 :: <real>, ex2 :: <real>, ey2 :: <real>)
 => (true? :: <boolean>)
  max(sx2, ex2) >= min(sx1, ex1)
  & max(sx1, ex1) >= min(sx2, ex2)
  & begin
      let dx1 = ex1 - sx1;
      let dy1 = ey1 - sy1;
      let dx2 = ex2 - sx2;
      let dy2 = ey2 - sy2;
      dx1 * dy2 = dx2 * dy1		// slopes equal
      & dx1 * (sy1 - sy2) = dy1 * (sx1 - sx2)
    end
end method line-intersects-line?;

// Returns either the new line endpoints, or (#f,#f,#f,#f) if the line is gone
define method clip-line-to-box
    (x0 :: <real>, y0 :: <real>, x1 :: <real>, y1 :: <real>,
     left :: <real>, top :: <real>, right :: <real>, bottom :: <real>)
 => (x0 :: false-or(<real>), y0 :: false-or(<real>),
     x1 :: false-or(<real>), y1 :: false-or(<real>))
  block (return)
    local method clip-bound (value, lower, upper) => (how)
	    case
	      value < lower => #"below";
	      value > upper => #"above";
	      otherwise => #f
	    end
	  end method,
          method interpolate (u0, v0, u1, v1, u) => (u1)
	    ((u1 - u) * v0 + (u - u0) * v1) / as(<single-float>, u1 - u0)
	  end method;
    let bx0 = clip-bound(x0, left, right);
    let by0 = clip-bound(y0, top, bottom);
    let bx1 = clip-bound(x1, left, right);
    let by1 = clip-bound(y1, top, bottom);
    when ((bx0 & (bx0 == bx1)) | (by0 & (by0 == by1)))
      return(#f, #f, #f, #f)
    end;
    let cy0 = by0;
    let cy1 = by1;
    when (bx0)
      if (bx0 == #"below")
	y0 := interpolate(x0, y0, x1, y1, left);
	x0 := left
      else	// bx0 == #"above"
	y0 := interpolate(x0, y0, x1, y1, right);
	x0 := right
      end;
      cy0 := clip-bound(y0, top, bottom);
      when (cy0 & (cy0 == cy1))
	return(#f, #f, #f, #f)
      end
    end;
    when (bx1)
      if (bx1 == #"below")
	y1 := interpolate(x0, y0, x1, y1, left);
	x1 := left
      else	// bx1 == #"above"
	y1 := interpolate(x0, y0, x1, y1, right);
	x1 := right
      end;
      cy1 := clip-bound(y1, top, bottom);
      when (cy1 & (cy0 == cy1))
	return(#f, #f, #f, #f)
      end
    end;
    when (cy0)
      if (cy0 == #"below")
	x0 := interpolate(y0, x0, y1, x1, top);
	y0 := top
      else	// cy0 == #"above"
	x0 := interpolate(y0, x0, y1, x1, bottom);
	y0 := bottom
      end
    end;
    when (cy1)
      if (cy1 == #"below")
	x1 := interpolate(y0, x0, y1, x1, top);
	y1 := top
      else	// cy0 == #"above"
	x1 := interpolate(y0, x0, y1, x1, bottom);
	y1 := bottom
      end
    end;
    values(x0, y0, x1, y1)
  end
end method clip-line-to-box;
