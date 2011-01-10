Module:       duim-geometry-internals
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Coordinate hacking

define constant $largest-coordinate  :: <integer> = $maximum-integer;
define constant $smallest-coordinate :: <integer> = $minimum-integer;

// Coerce a coordinate to an integer
define inline function fix-coordinate
    (x :: <real>) => (x :: <integer>)
  truncate(x)
end function fix-coordinate;

define inline function fix-box
    (left :: <real>, top :: <real>, right :: <real>, bottom :: <real>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(floor(left), floor(top), ceiling(right), ceiling(bottom))
end function fix-box;


/// Transforming positions and distances

// Translate the coordinate pairs by dx/dy
// translate-coordinates!(dx, dy, x1, y1, x2, y2, ...);
define macro translate-coordinates!
  { translate-coordinates! (?dx:expression, ?dy:expression) }
    => { }
  { translate-coordinates!
      (?dx:expression, ?dy:expression, ?x:expression, ?y:expression, ?more:*) }
    => { ?x := ?x + ?dx;
         ?y := ?y + ?dy;
         translate-coordinates!(?dx, ?dy, ?more); }
end macro translate-coordinates!;

define method translate-coordinate-sequence!
    (dx, dy, coords :: <sequence>) => (coords :: <sequence>)
  unless (zero?(dx) & zero?(dy))
    let ncoords :: <integer> = size(coords);
    without-bounds-checks
      for (i :: <integer> from 0 below ncoords by 2)
	coords[i + 0] := coords[i + 0] + dx;
	coords[i + 1] := coords[i + 1] + dy
      end
    end
  end;
  coords
end method translate-coordinate-sequence!;

define method translate-coordinate-sequence!
    (dx, dy, coords :: <simple-object-vector>) => (coords :: <simple-object-vector>)
  unless (zero?(dx) & zero?(dy))
    let ncoords :: <integer> = size(coords);
    without-bounds-checks
      for (i :: <integer> from 0 below ncoords by 2)
	coords[i + 0] := coords[i + 0] + dx;
	coords[i + 1] := coords[i + 1] + dy
      end
    end
  end;
  coords
end method translate-coordinate-sequence!;

define method translate-coordinate-sequence!
    (dx, dy, coords :: <list>) => (coords :: <list>)
  unless (zero?(dx) & zero?(dy))
    let new-coords = coords;
    while (~empty?(new-coords))
      head(new-coords) := head(new-coords) + dx;
      pop!(new-coords);
      head(new-coords) := head(new-coords) + dy;
      pop!(new-coords)
    end
  end;
  coords
end method translate-coordinate-sequence!;

// Apply a general transformation to the coordinate pairs
// transform-coordinates!(transform, x1, y1, x2, y2, ...);
define macro transform-coordinates!
  { transform-coordinates! (?transform:expression) }
    => { }
  { transform-coordinates!
      (?transform:variable, ?x:expression, ?y:expression, ?more:*) }
    => { let (_x, _y) = transform-position(?transform, ?x, ?y);
         ?x := _x;
         ?y := _y;
         transform-coordinates!(?transform, ?more); }
  { transform-coordinates!
      (?transform:expression, ?x:expression, ?y:expression, ?more:*) }
    => { let _transform = ?transform;
         let (_x, _y) = transform-position(_transform, ?x, ?y);
         ?x := _x;
         ?y := _y;
         transform-coordinates!(_transform, ?more); }
end macro transform-coordinates!;

// Apply a general transformation to the dx/dy pairs
// transform-distances!(transform, dx1, dy1, dx2, dy2, ...);
define macro transform-distances!
  { transform-distances! (?transform:expression) }
    => { }
  { transform-distances!
      (?transform:variable, ?dx:expression, ?dy:expression, ?more:*) }
    => { let (_dx, _dy) = transform-distance(?transform, ?dx, ?dy);
         ?dx := _dx;
         ?dy := _dy;
         transform-distances!(?transform, ?more); }
  { transform-distances!
      (?transform:expression, ?dx:expression, ?dy:expression, ?more:*) }
    => { let _transform = ?transform;
         let (_dx, _dy) = transform-distance(_transform, ?dx, ?dy);
         ?dx := _dx;
         ?dy := _dy;
         transform-distances!(_transform, ?more); }
end macro transform-distances!;


/// Fixing coordinates and distances

// fix-coordinates!(x1, y1, x2, y2, ...);
// Side-effects x1, y1, ...
define macro fix-coordinates!
  { fix-coordinates! (?x:expression, ?y:expression) }
    => { ?x := floor(?x);
         ?y := floor(?y); }
  { fix-coordinates! (?x:expression, ?y:expression, ?more:*) }
    => { ?x := floor(?x);
         ?y := floor(?y);
         fix-coordinates!(?more); }
end macro fix-coordinates!;


/// Device transformations

// Use this in preference to 'convert-to-device-coordinates!', since it
// the coordinates to device coordinates having tighter type declarations
// with-device-coordinates (transform, x1, y1, x2, y2, ...) body end;
define macro with-device-coordinates
  { with-device-coordinates (?transform:expression)
      ?:body
    end}
    => { ?body }
  { with-device-coordinates (?transform:expression, ?x:name, ?y:name, ?more:*)
      ?:body
    end }
    => { begin
	   let (?x :: <integer>, ?y :: <integer>)
	     = if (?transform == $identity-transform)	// for speed...
		 values(floor(?x), floor(?y))
	       else
		 let (_x, _y) = transform-position(?transform, ?x, ?y);
		 values(floor(_x), floor(_y))
	       end;
	   with-device-coordinates (?transform, ?more)
	     ?body
	   end 
	 end }
end macro with-device-coordinates;

// with-device-distances (transform, dx1, dy1, dx2, dy2, ...) body end;
define macro with-device-distances
  { with-device-distances (?transform:expression)
      ?:body
    end}
    => { ?body }
  { with-device-distances (?transform:expression, ?dx:name, ?dy:name, ?more:*)
      ?:body
    end }
    => { begin
	   let (?dx :: <integer>, ?dy :: <integer>)
	     = if (?transform == $identity-transform)	// for speed...
		 values(floor(?dx), floor(?dy))
	       else
		 let (_dx, _dy) = transform-distance(?transform, ?dx, ?dy);
		 values(floor(_dx), floor(_dy))
	       end;
	   with-device-distances (?transform, ?more)
	     ?body
	   end
	 end }
end macro with-device-distances;


// convert-to-device-coordinates!(transform, x1, y1, x2, y2, ...);
define macro convert-to-device-coordinates!
  { convert-to-device-coordinates! (?transform:expression) }
    => { }
  { convert-to-device-coordinates!
      (?transform:expression, ?x:expression, ?y:expression, ?more:*) }
    => { let (_x, _y) = transform-position(?transform, ?x, ?y);
	 ?x := floor(_x);
         ?y := floor(_y);
         convert-to-device-coordinates!(?transform, ?more); }
end macro convert-to-device-coordinates!;

// convert-to-device-distances!(transform, dx1, dy1, dx2, dy2, ...);
define macro convert-to-device-distances!
  { convert-to-device-distances! (?transform:expression) }
    => { }
  { convert-to-device-distances!
      (?transform:expression, ?dx:expression, ?dy:expression, ?more:*) }
    => { let (_dx, _dy) = transform-distance(?transform, ?dx, ?dy);
	 ?dx := floor(_dx);
         ?dy := floor(_dy);
         convert-to-device-distances!(?transform, ?more); }
end macro convert-to-device-distances!;


/// Mapping over coordinate sequences

define method do-coordinates
    (function :: <function>, coordinates :: <sequence>) => ()
  dynamic-extent(function);
  let ncoords :: <integer> = size(coordinates);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 2,
	 until: i >= ncoords)
      function(coordinates[i], coordinates[i + 1])
    end
  end
end method do-coordinates;

define method do-coordinates
    (function :: <function>, coordinates :: <simple-object-vector>) => ()
  dynamic-extent(function);
  let ncoords :: <integer> = size(coordinates);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 2,
	 until: i >= ncoords)
      function(coordinates[i], coordinates[i + 1])
    end
  end
end method do-coordinates;

define method do-coordinates
    (function :: <function>, coordinates :: <list>) => ()
  dynamic-extent(function);
  until (empty?(coordinates))
    let x = pop!(coordinates);
    let y = pop!(coordinates);
    function(x, y)
  end
end method do-coordinates;


define method do-endpoint-coordinates
    (function :: <function>, coordinates :: <vector>) => ()
  dynamic-extent(function);
  let ncoords :: <integer> = size(coordinates);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 4,
	 until: i >= ncoords)
      function(coordinates[i],     coordinates[i + 1],
	       coordinates[i + 2], coordinates[i + 3])
    end
  end
end method do-endpoint-coordinates;

define method do-endpoint-coordinates
    (function :: <function>, coordinates :: <simple-object-vector>) => ()
  dynamic-extent(function);
  let ncoords :: <integer> = size(coordinates);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 4,
	 until: i >= ncoords)
      function(coordinates[i],     coordinates[i + 1],
	       coordinates[i + 2], coordinates[i + 3])
    end
  end
end method do-endpoint-coordinates;

define method do-endpoint-coordinates
    (function :: <function>, coordinates :: <list>) => ()
  dynamic-extent(function);
  until (empty?(coordinates))
    let x1 = pop!(coordinates);
    let y1 = pop!(coordinates);
    let x2 = pop!(coordinates);
    let y2 = pop!(coordinates);
    function(x1, y1, x2, y2)
  end
end method do-endpoint-coordinates;


define method spread-point-sequence
    (sequence :: <sequence>) => (coords :: <simple-object-vector>)
  let length = size(sequence);
  let result :: <simple-object-vector> = make(<simple-vector>, size: 2 * length);
  let i :: <integer> = -1;
  without-bounds-checks
    for (point :: <standard-point> in sequence)
      result[inc!(i)] := point-x(point);
      result[inc!(i)] := point-y(point)
    end
  end;
  result
end method spread-point-sequence;
