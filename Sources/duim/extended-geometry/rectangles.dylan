Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Rectangles

define protocol <<rectangle-protocol>> (<<polygon-protocol>>)
  function rectangle-edges
    (rectangle :: <rectangle>)
 => (min-x :: <real>, min-y :: <real>, max-x :: <real>, max-y :: <real>);
  function rectangle-min-point
    (rectangle :: <rectangle>) => (point :: <point>);
  function rectangle-max-point
    (rectangle :: <rectangle>) => (point :: <point>);
  function rectangle-min-position
    (rectangle :: <rectangle>) => (x :: <real>, y :: <real>);
  function rectangle-max-position
    (rectangle :: <rectangle>) => (x :: <real>, y :: <real>);
  function rectangle-size
    (rectangle :: <rectangle>) => (width :: <real>, height :: <real>);
  function rectangle-width
    (rectangle :: <rectangle>) => (width :: <real>);
  function rectangle-height
    (rectangle :: <rectangle>) => (height :: <real>);
end protocol <<rectangle-protocol>>;

define sealed class <standard-rectangle> (<rectangle>)
  sealed constant slot %min-x :: <real>,
    required-init-keyword: min-x:;
  sealed constant slot %min-y :: <real>,
    required-init-keyword: min-y:;
  sealed constant slot %max-x :: <real>,
    required-init-keyword: max-x:;
  sealed constant slot %max-y :: <real>,
    required-init-keyword: max-y:;
  sealed slot %points :: false-or(<vector>) = #f,
    init-keyword: points:;
end class <standard-rectangle>;

define sealed domain make (singleton(<standard-rectangle>));
define sealed domain initialize (<standard-rectangle>);

define method rectangle-min-position
    (rectangle :: <standard-rectangle>) => (x :: <real>, y :: <real>)
  values(rectangle.%min-x, rectangle.%min-y)
end method rectangle-min-position;

define method rectangle-max-position
    (rectangle :: <standard-rectangle>) => (x :: <real>, y :: <real>)
  values(rectangle.%max-x, rectangle.%max-y)
end method rectangle-max-position;

define inline function make-rectangle
    (x1, y1, x2, y2) => (rectangle :: <standard-rectangle>)
  when (x1 > x2)
    swap!(x1, x2)
  end;
  when (y1 > y2)
    swap!(y1, y2)
  end;
  make(<standard-rectangle>,
       min-x: x1, min-y: y1, max-x: x2, max-y: y2)
end function make-rectangle;

define function make-rectangle*
    (min-point, max-point) => (rectangle :: <standard-rectangle>)
  let (min-x, min-y) = point-position(min-point);
  let (max-x, max-y) = point-position(max-point);
  assert(min-x <= max-x & min-y <= max-y,
	 "The min point must be to the upper-left of the max point");
  make(<standard-rectangle>,
       min-x: min-x, min-y: min-y,
       max-x: max-x, max-y: max-y,
       points: vector(min-point, make-point(min-x, max-y), 
                      max-point, make-point(max-x, min-y)))
end function make-rectangle*;

define sealed inline method make
    (class == <rectangle>, #key min-point, max-point)
 => (rectangle :: <standard-rectangle>)
  make-rectangle*(min-point, max-point);
end method make;

define method rectangle-edges
    (rectangle :: <standard-rectangle>)
 => (min-x :: <real>, min-y :: <real>, max-x :: <real>, max-y :: <real>)
  values(rectangle.%min-x, rectangle.%min-y,
         rectangle.%max-x, rectangle.%max-y)
end method rectangle-edges;

define method polygon-points
    (rectangle :: <standard-rectangle>) => (points :: <vector>)
  rectangle.%points
  | (rectangle.%points
       := vector(make-point(rectangle.%min-x, rectangle.%min-y),	// min
		 make-point(rectangle.%min-x, rectangle.%max-y),
		 make-point(rectangle.%max-x, rectangle.%max-y),	// max
		 make-point(rectangle.%max-x, rectangle.%min-y)))
end method polygon-points;

define method rectangle-min-point
    (rectangle :: <standard-rectangle>) => (point :: <standard-point>)
  polygon-points(rectangle)[0]
end method rectangle-min-point;

define method rectangle-max-point
    (rectangle :: <standard-rectangle>) => (point :: <standard-point>)
  polygon-points(rectangle)[2]
end method rectangle-max-point;

define method rectangle-width
    (rectangle :: <standard-rectangle>) => (width :: <real>)
  rectangle.%max-x - rectangle.%min-x
end method rectangle-width;

define method rectangle-height
    (rectangle :: <standard-rectangle>) => (height :: <real>)
  rectangle.%max-y - rectangle.%min-y
end method rectangle-height;

define method rectangle-size
    (rectangle :: <standard-rectangle>) => (width :: <real>, height :: <real>)
  values
    (rectangle.%max-x - rectangle.%min-x, rectangle.%max-y - rectangle.%min-y)
end method rectangle-size;

define method do-polygon-coordinates
    (function :: <function>, rectangle :: <standard-rectangle>) => ()
  function(rectangle.%min-x, rectangle.%min-y);
  function(rectangle.%min-x, rectangle.%max-y);
  function(rectangle.%max-x, rectangle.%max-y);
  function(rectangle.%max-x, rectangle.%min-y)
end method do-polygon-coordinates;

define method do-polygon-segments
    (function :: <function>, rectangle :: <standard-rectangle>) => ()
  let min-x = rectangle.%min-x;
  let min-y = rectangle.%min-x;
  let max-x = rectangle.%max-x;
  let max-y = rectangle.%max-x;
  function(min-x, min-y, min-x, max-y);
  function(min-x, max-y, max-x, max-y);
  function(max-x, max-y, max-x, min-y);
  function(max-x, min-y, min-x, min-y);
  #f
end method do-polygon-segments;

define method transform-region
    (transform :: <transform>, rectangle :: <standard-rectangle>)
 => (region :: type-union(<standard-rectangle>, <standard-polygon>))
  if (rectilinear-transform?(transform))
    let (x1, y1)
      = transform-position(transform, rectangle.%min-x, rectangle.%min-y);
    let (x2, y2)
      = transform-position(transform, rectangle.%max-x, rectangle.%max-y);
    make-rectangle(x1, y1, x2, y2)
  else
    let coords :: <stretchy-object-vector> = make(<stretchy-vector>);
    local method transform-coord (x, y) => ()
	    let (nx, ny) = transform-position(transform, x, y);
	    add!(coords, ny);
	    add!(coords, nx)
	  end method;
    do-polygon-coordinates(transform-coord, rectangle);
    make-polygon(coords)
  end
end method transform-region;

define method box-edges
    (rectangle :: <standard-rectangle>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  fix-box(min(rectangle.%min-x, rectangle.%max-x),
          min(rectangle.%min-y, rectangle.%max-y),
          max(rectangle.%min-x, rectangle.%max-x),
          max(rectangle.%min-y, rectangle.%max-y))
end method box-edges;

define method region-equal
    (rect1 :: <standard-rectangle>, rect2 :: <standard-rectangle>) => (true? :: <boolean>)
  rect1.%min-x = rect2.%min-x
  & rect1.%min-y = rect2.%min-y
  & rect1.%max-x = rect2.%max-x
  & rect1.%max-y = rect2.%max-y
end method region-equal;

define method region-contains-position?
    (rectangle :: <standard-rectangle>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  rectangle.%min-x <= x & rectangle.%min-y <= y
  & rectangle.%max-x >= x & rectangle.%max-y >= y
end method region-contains-position?;

define method region-contains-region?
    (rect1 :: <standard-rectangle>, rect2 :: <standard-rectangle>) => (true? :: <boolean>)
  rect1.%min-x <= rect2.%min-x
  & rect1.%min-y <= rect2.%min-y
  & rect1.%max-x >= rect2.%max-x
  & rect1.%max-y >= rect2.%max-y
end method region-contains-region?;

define method region-intersects-region?
    (rect1 :: <standard-rectangle>, rect2 :: <standard-rectangle>) => (true? :: <boolean>)
  let left = max(rect1.%min-x, rect2.%min-x);
  let top  = max(rect1.%min-y, rect2.%min-y);
  let right  = min(rect1.%max-x, rect2.%max-x);
  let bottom = min(rect1.%max-y, rect2.%max-y);
  right >= left & bottom >= top
end method region-intersects-region?;


/// Lines x Rectangles

define method region-contains-region?
    (rect :: <rectangle>, line :: <standard-line>) => (true? :: <boolean>)
  region-contains-position?(rect, line.%start-x, line.%start-y)
  & region-contains-position?(rect, line.%end-x, line.%end-y)
end method region-contains-region?;

define method region-intersects-region?
    (rect :: <rectangle>, line :: <standard-line>) => (true? :: <boolean>)
  let (left, top, right, bottom) = rectangle-edges(rect);
  let (x0, y0, x1, y1)
    = clip-line-to-box(line.%start-x, line.%start-y, line.%end-x, line.%end-y,
		       left, top, right, bottom);
  ignore(y0, x1, y1);
  x0 & #t
end method region-intersects-region?;

define method region-intersects-region?
    (line :: <standard-line>, rect :: <rectangle>) => (true? :: <boolean>)
  let (left, top, right, bottom) = rectangle-edges(rect);
  let (x0, y0, x1, y1)
    = clip-line-to-box(line.%start-x, line.%start-y, line.%end-x, line.%end-y,
		       left, top, right, bottom);
  ignore(y0, x1, y1);
  x0 & #t
end method region-intersects-region?;
