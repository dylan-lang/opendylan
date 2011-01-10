Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Polygons and polylines

define protocol <<polygon-protocol>> (<<region-protocol>>)
  function polygon-points
    (polygon) => (points :: <sequence>);
  function do-polygon-coordinates
    (function :: <function>, polygon) => ();
  function do-polygon-segments
    (function :: <function>, polygon) => ();
  function polyline-closed?
    (polyline) => (true? :: <boolean>);
end protocol <<polygon-protocol>>;


define abstract class <polygon-mixin> (<object>)
  sealed slot %coords :: false-or(<vector>) = #f,
    init-keyword: coordinates:;
  sealed slot %points :: false-or(<vector>) = #f,
    init-keyword: points:;
end class <polygon-mixin>;

define method polygon-points
    (polygon :: <polygon-mixin>) => (points :: <vector>)
  polygon.%points
  | begin
      // If %points empty, %coords will not be
      let coords = polygon.%coords;
      let npoints :: <integer> = truncate/(size(coords), 2);
      let points :: <simple-object-vector> = make(<simple-vector>, size: npoints);
      without-bounds-checks
	for (i :: <integer> from 0 below npoints)
	  points[i] := make-point(coords[i * 2 + 0], coords[i * 2 + 1])
	end
      end;
      polygon.%points := points
    end
end method polygon-points;

define method polygon-coordinates
    (polygon :: <polygon-mixin>) => (coords :: <vector>)
  polygon.%coords
  | begin
      // If %coords empty, %points will not be
      let points = polygon.%points;
      let npoints :: <integer> = size(points);
      let coords :: <simple-object-vector> = make(<simple-vector>, size: npoints * 2);
      without-bounds-checks
	for (i :: <integer> from 0 below npoints)
	  coords[i * 2 + 0] := point-x(points[i]);
	  coords[i * 2 + 1] := point-y(points[i])
	end
     end;
      polygon.%coords := coords
    end
end method polygon-coordinates;

define method do-polygon-coordinates
    (function :: <function>, polygon :: <polygon-mixin>) => ()
  if (polygon.%coords)
    let coords = polygon.%coords;
    let ncoords :: <integer> = size(coords) - 1;
    let i :: <integer> = -1;
    until (i = ncoords)
      function(coords[inc!(i)], coords[inc!(i)])
    end
  else
    local method do-coords (point) => ()
	    function(point-x(point), point-y(point))
	  end method;
    do(do-coords, polygon.%points)
  end
end method do-polygon-coordinates;

define method do-polygon-segments
    (function :: <function>, polygon :: <polygon-mixin>) => ()
  if (polygon.%coords)
    let coords = polygon.%coords;
    let ncoords :: <integer> = size(coords) - 1;
    let x1 = coords[0];
    let y1 = coords[1];
    let x = x1;
    let y = y1;
    let i :: <integer> = 1;
    until (i = ncoords)
      function(x, y,
	       x := coords[inc!(i)], y := coords[inc!(i)])
    end;
    when (polyline-closed?(polygon))
      function(x, y, x1, y1)
    end
  else
    let (x1, y1) = point-position(polygon.%points[0]);
    let x = x1;
    let y = y1;
    let points = polygon.%points;
    let npoints :: <integer> = size(points);
    for (i :: <integer> from 0 below npoints - 1)
      let (nx, ny) = point-position(points[i + 1]);
      function(x, y, nx, ny);
      x := nx;
      y := ny
    end;
    when (polyline-closed?(polygon))
      function(x, y, x1, y1)
    end
  end
end method do-polygon-segments;

define method box-edges
    (polygon :: <polygon-mixin>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let min-x = $largest-coordinate;
  let min-y = $largest-coordinate;
  let max-x = $smallest-coordinate;
  let max-y = $smallest-coordinate;
  local method add-coord (x, y) => ()
	  min!(min-x, x);
	  min!(min-y, y);
	  max!(max-x, x);
	  max!(max-y, y)
	end method;
  do-polygon-coordinates(add-coord, polygon);
  fix-box(min-x, min-y, max-x, max-y)
end method box-edges;


define sealed class <standard-polyline> (<polygon-mixin>, <polyline>)
  sealed constant slot polyline-closed? :: <boolean> = #f,
    init-keyword: closed?:;
end class <standard-polyline>;

define sealed domain make (singleton(<standard-polyline>));
define sealed domain initialize (<standard-polyline>);

define inline function make-polyline
    (coord-seq, #key closed?) => (polyline :: <standard-polyline>)
  assert(even?(size(coord-seq)),
	 "There must be an even number of coordinates in %=", coord-seq);
  make(<standard-polyline>,
       coordinates: as(<simple-vector>, coord-seq),
       closed?: closed?)
end function make-polyline;

define inline function make-polyline*
    (point-seq, #key closed?) => (polyline :: <standard-polyline>)
  make(<standard-polyline>,
       points: as(<simple-vector>, point-seq), closed?: closed?)
end function make-polyline*;

define sealed inline method make
    (class == <polyline>, #key points, closed?)
 => (polyline :: <standard-polyline>)
  make-polyline*(points, closed?: closed?)
end method make;

define method transform-region
    (transform :: <transform>, polyline :: <standard-polyline>)
 => (polyline :: <standard-polyline>)
  let coords :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method transform-coord (x, y) => ()
	  let (nx, ny) = transform-position(transform, x, y);
	  add!(coords, ny);
	  add!(coords, nx)
	end method;
  do-polygon-coordinates(transform-coord, polyline);
  make-polyline(coords, closed?: polyline-closed?(polyline))
end method transform-region;

define method region-equal
    (p1 :: <standard-polyline>, p2 :: <standard-polyline>) => (true? :: <boolean>)
  polygon-coordinates(p1) = polygon-coordinates(p2)
  & polyline-closed?(p1) = polyline-closed?(p2)
end method region-equal;


define sealed class <standard-polygon> (<polygon-mixin>, <polygon>)
end class <standard-polygon>;

define sealed domain make (singleton(<standard-polygon>));
define sealed domain initialize (<standard-polygon>);

define inline function make-polygon
    (coord-seq) => (polygon :: <standard-polygon>)
  assert(even?(size(coord-seq)),
	 "There must be an even number of coordinates in %=", coord-seq);
  make(<standard-polygon>,
       coordinates: as(<simple-vector>, coord-seq))
end function make-polygon;

define inline function make-polygon*
    (point-seq) => (polygon :: <standard-polygon>)
  make(<standard-polygon>,
       points: as(<simple-vector>, point-seq))
end function make-polygon*;

define sealed inline method make
    (class == <polygon>, #key points)
 => (polygon :: <standard-polygon>)
  make-polygon*(points);
end method make;

define method polyline-closed? (polygon :: <standard-polygon>) => (true? :: <boolean>)
  #t
end method polyline-closed?;

define method transform-region
    (transform :: <transform>, polygon :: <standard-polygon>)
 => (polygon :: <standard-polygon>)
  let coords :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method transform-coord (x, y) => ()
	  let (nx, ny) = transform-position(transform, x, y);
	  add!(coords, ny);
	  add!(coords, nx)
	end method;
  do-polygon-coordinates(transform-coord, polygon);
  make-polygon(coords)
end method transform-region;

define method region-equal
    (p1 :: <standard-polygon>, p2 :: <standard-polygon>) => (true? :: <boolean>)
  polygon-coordinates(p1) = polygon-coordinates(p2)
end method region-equal;

define method region-contains-position?
    (polygon :: <standard-polygon>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  let (left, top, right, bottom) = box-edges(polygon);
  ltrb-contains-position?(left, top, right, bottom,
			  fix-coordinate(x), fix-coordinate(y))
  & position-inside-polygon?(x, y, polygon-coordinates(polygon), closed?: #t)
end method region-contains-position?;
