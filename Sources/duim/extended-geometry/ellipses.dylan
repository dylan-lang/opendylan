Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Ellipses and elliptical arcs

define protocol <<ellipse-protocol>> (<<region-protocol>>)
  function ellipse-center-point
    (ellipse) => (point :: <point>);
  function ellipse-center-position
    (ellipse) => (x :: <real>, y :: <real>);
  function ellipse-radii
    (ellipse)
 => (radius-1-dx :: <real>, radius-1-dy :: <real>,
     radius-2-dx :: <real>, radius-2-dy :: <real>);
  function ellipse-start-angle
    (ellipse) => (angle :: false-or(<real>));
  function ellipse-end-angle
    (ellipse) => (angle :: false-or(<real>));
end protocol <<ellipse-protocol>>;


define abstract class <ellipse-mixin> (<object>)
  sealed constant slot %center-x :: <real>,
    required-init-keyword: center-x:;
  sealed constant slot %center-y :: <real>,
    required-init-keyword: center-y:;
  sealed slot %center-point :: false-or(<standard-point>) = #f,
    init-keyword: center-point:;
  sealed constant slot %radius-1-dx :: <real>,
    required-init-keyword: radius-1-dx:;
  sealed constant slot %radius-1-dy :: <real>,
    required-init-keyword: radius-1-dy:;
  sealed constant slot %radius-2-dx :: <real>,
    required-init-keyword: radius-2-dx:;
  sealed constant slot %radius-2-dy :: <real>,
    required-init-keyword: radius-2-dy:;
  sealed constant slot ellipse-start-angle :: false-or(<single-float>) = #f,
    init-keyword: start-angle:;
  sealed constant slot ellipse-end-angle :: false-or(<single-float>) = #f,
    init-keyword: end-angle:;
end class <ellipse-mixin>;

define method ellipse-center-position
    (ellipse :: <ellipse-mixin>) => (x :: <real>, y :: <real>)
  values(ellipse.%center-x, ellipse.%center-y)
end method ellipse-center-position;

define method ellipse-center-point
    (ellipse :: <ellipse-mixin>) => (point :: <standard-point>)
  ellipse.%center-point
  | (ellipse.%center-point := make-point(ellipse.%center-x, ellipse.%center-y))
end method ellipse-center-point;

define method ellipse-radii
    (ellipse :: <ellipse-mixin>)
 => (radius-1-dx :: <real>, radius-1-dy :: <real>,
     radius-2-dx :: <real>, radius-2-dy :: <real>);
  values(ellipse.%radius-1-dx, ellipse.%radius-1-dy,
         ellipse.%radius-2-dx, ellipse.%radius-2-dy)
end method ellipse-radii;


define sealed class <standard-elliptical-arc>
    (<ellipse-mixin>, <elliptical-arc>)
end class <standard-elliptical-arc>;

define sealed domain make (singleton(<standard-elliptical-arc>));
define sealed domain initialize (<standard-elliptical-arc>);

//--- Should signal <ellipse-not-well-defined> if the axes are collinear
define inline function make-elliptical-arc
    (center-x :: <real>, center-y :: <real>,
     radius-1-dx :: <real>, radius-1-dy :: <real>, radius-2-dx :: <real>, radius-2-dy :: <real>,
     #key start-angle, end-angle)
 => (arc :: <standard-elliptical-arc>)
  make(<standard-elliptical-arc>,
       center-x: center-x, center-y: center-y,
       radius-1-dx: radius-1-dx, radius-1-dy: radius-1-dy,
       radius-2-dx: radius-2-dx, radius-2-dy: radius-2-dy,
       start-angle: case
                      start-angle => as(<single-float>, start-angle);
                      end-angle => 0.0;
                      otherwise => #f
                    end,
       end-angle: case
                    end-angle => as(<single-float>, end-angle);
                    start-angle => $2pi;
                    otherwise => #f
                  end)
end function make-elliptical-arc;

//--- Should signal <ellipse-not-well-defined> if the axes are collinear
define inline function make-elliptical-arc*
    (center-point :: <standard-point>,
     radius-1-dx :: <real>, radius-1-dy :: <real>, radius-2-dx :: <real>, radius-2-dy :: <real>,
     #key start-angle, end-angle)
 => (arc :: <standard-elliptical-arc>)
  make(<standard-elliptical-arc>,
       center-point: center-point,
       center-x: point-x(center-point), center-y: point-y(center-point),
       radius-1-dx: radius-1-dx, radius-1-dy: radius-1-dy,
       radius-2-dx: radius-2-dx, radius-2-dy: radius-2-dy,
       start-angle: case
                      start-angle => as(<single-float>, start-angle);
                      end-angle => 0.0;
                      otherwise => #f
                    end,
       end-angle: case
                    end-angle => as(<single-float>, end-angle);
                    start-angle => $2pi;
                    otherwise => #f
                  end)
end function make-elliptical-arc*;

define sealed inline method make
    (class == <elliptical-arc>,
     #key center-point, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
	  start-angle, end-angle)
 => (arc :: <standard-elliptical-arc>)
  make-elliptical-arc*(center-point, 
		       radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
		       start-angle: start-angle, end-angle: end-angle)
end method make;

define method transform-region
    (transform :: <transform>, ellipse :: <standard-elliptical-arc>)
 => (arc :: <standard-elliptical-arc>)
  let (cx, cy)
    = transform-position(transform, ellipse.%center-x, ellipse.%center-y);
  let (r1-dx, r1-dy)
    = transform-distance
        (transform, ellipse.%radius-1-dx, ellipse.%radius-1-dy);
  let (r2-dx, r2-dy)
    = transform-distance
        (transform, ellipse.%radius-2-dx, ellipse.%radius-2-dy);
  let start-angle = ellipse-start-angle(ellipse);
  let end-angle   = ellipse-end-angle(ellipse);
  when (start-angle)		// non-#f => end angle is non-#f
    let (sa, ea) = transform-angles(transform, start-angle, end-angle);
    start-angle := sa;
    end-angle   := ea
  end;
  make-elliptical-arc(cx, cy, r1-dx, r1-dy, r2-dx, r2-dy,
		      start-angle: start-angle, end-angle: end-angle)
end method transform-region;

define method box-edges
    (ellipse :: <standard-elliptical-arc>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  elliptical-arc-box(ellipse.%center-x, ellipse.%center-y,
                     ellipse.%radius-1-dx, ellipse.%radius-1-dy,
                     ellipse.%radius-2-dx, ellipse.%radius-2-dy,
                     start-angle: ellipse-start-angle(ellipse),
		     end-angle: ellipse-end-angle(ellipse))
end method box-edges;

define method region-equal
    (e1 :: <standard-elliptical-arc>, e2 :: <standard-elliptical-arc>) => (true? :: <boolean>)
  e1.%center-x = e2.%center-x
  & e1.%center-y = e2.%center-y
  & e1.%radius-1-dx = e2.%radius-1-dx
  & e1.%radius-1-dy = e2.%radius-1-dy
  & e1.%radius-2-dx = e2.%radius-2-dx
  & e1.%radius-2-dy = e2.%radius-2-dy
  & ellipse-start-angle(e1) = ellipse-start-angle(e2)
  & ellipse-end-angle(e1) = ellipse-end-angle(e2)
end method region-equal;

define method region-contains-position?
    (ellipse :: <standard-elliptical-arc>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  let (left, top, right, bottom) = box-edges(ellipse);
  ltrb-contains-position?(left, top, right, bottom,
			  fix-coordinate(x), fix-coordinate(y))
  & position-on-thick-ellipse?(x - ellipse.%center-x, y - ellipse.%center-y,
			       ellipse.%radius-1-dx, ellipse.%radius-1-dy,
			       ellipse.%radius-2-dx, ellipse.%radius-2-dy)
end method region-contains-position?;


define sealed class <standard-ellipse>
    (<ellipse-mixin>, <ellipse>)
end class <standard-ellipse>;

define sealed domain make (singleton(<standard-ellipse>));
define sealed domain initialize (<standard-ellipse>);

define inline function make-ellipse
    (center-x :: <real>, center-y :: <real>,
     radius-1-dx :: <real>, radius-1-dy :: <real>, radius-2-dx :: <real>, radius-2-dy :: <real>,
     #key start-angle, end-angle)
 => (ellipse :: <standard-ellipse>)
  make(<standard-ellipse>,
       center-x: center-x, center-y: center-y,
       radius-1-dx: radius-1-dx, radius-1-dy: radius-1-dy,
       radius-2-dx: radius-2-dx, radius-2-dy: radius-2-dy,
       start-angle: case
                      start-angle => as(<single-float>, start-angle);
                      end-angle => 0.0;
                      otherwise => #f
                    end,
       end-angle: case
                    end-angle => as(<single-float>, end-angle);
                    start-angle => $2pi;
                    otherwise => #f
                  end)
end function make-ellipse;

define inline function make-ellipse*
    (center-point :: <standard-point>,
     radius-1-dx :: <real>, radius-1-dy :: <real>, radius-2-dx :: <real>, radius-2-dy :: <real>,
     #key start-angle, end-angle)
 => (ellipse :: <standard-ellipse>)
  make(<standard-ellipse>,
       center-point: center-point,
       center-x: point-x(center-point), center-y: point-y(center-point),
       radius-1-dx: radius-1-dx, radius-1-dy: radius-1-dy,
       radius-2-dx: radius-2-dx, radius-2-dy: radius-2-dy,
       start-angle: case
                      start-angle => as(<single-float>, start-angle);
                      end-angle => 0.0;
                      otherwise => #f
                    end,
       end-angle: case
                    end-angle => as(<single-float>, end-angle);
                    start-angle => $2pi;
                    otherwise => #f
                  end)
end function make-ellipse*;

define sealed inline method make
    (class == <ellipse>,
     #key center-point, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
	 start-angle, end-angle)
 => (ellipse :: <standard-ellipse>)
  make-ellipse*(center-point, 
		radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
		start-angle: start-angle, end-angle: end-angle)
end method make;

define method transform-region
    (transform :: <transform>, ellipse :: <standard-ellipse>)
 => (ellipse :: <standard-ellipse>)
  let (cx, cy)
    = transform-position(transform, ellipse.%center-x, ellipse.%center-y);
  let (r1-dx, r1-dy)
    = transform-distance(transform, ellipse.%radius-1-dx, ellipse.%radius-1-dy);
  let (r2-dx, r2-dy)
    = transform-distance(transform, ellipse.%radius-2-dx, ellipse.%radius-2-dy);
  let start-angle = ellipse-start-angle(ellipse);
  let end-angle   = ellipse-end-angle(ellipse);
  when (start-angle)		// non-#f => end angle is non-#f
    let (sa, ea) = transform-angles(transform, start-angle, end-angle);
    start-angle := sa;
    end-angle   := ea
  end;
  make-ellipse(cx, cy, r1-dx, r1-dy, r2-dx, r2-dy,
	       start-angle: start-angle, end-angle: end-angle)
end method transform-region;

define method box-edges
    (ellipse :: <standard-ellipse>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  elliptical-arc-box(ellipse.%center-x, ellipse.%center-y,
                     ellipse.%radius-1-dx, ellipse.%radius-1-dy,
                     ellipse.%radius-2-dx, ellipse.%radius-2-dy,
                     start-angle: ellipse-start-angle(ellipse),
		     end-angle: ellipse-end-angle(ellipse),
		     thickness: #f)	// filled...
end method box-edges;

define method region-equal
    (e1 :: <standard-ellipse>, e2 :: <standard-ellipse>) => (true? :: <boolean>)
  e1.%center-x = e2.%center-x
  & e1.%center-y = e2.%center-y
  & e1.%radius-1-dx = e2.%radius-1-dx
  & e1.%radius-1-dy = e2.%radius-1-dy
  & e1.%radius-2-dx = e2.%radius-2-dx
  & e1.%radius-2-dy = e2.%radius-2-dy
  & ellipse-start-angle(e1) = ellipse-start-angle(e2)
  & ellipse-end-angle(e1) = ellipse-end-angle(e2)
end method region-equal;

define method region-contains-position?
    (ellipse :: <standard-ellipse>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  let (left, top, right, bottom) = box-edges(ellipse);
  ltrb-contains-position?(left, top, right, bottom,
			  fix-coordinate(x), fix-coordinate(y))
  & position-inside-ellipse?(x - ellipse.%center-x, y - ellipse.%center-y,
			     ellipse.%radius-1-dx, ellipse.%radius-1-dy,
			     ellipse.%radius-2-dx, ellipse.%radius-2-dy)
end method region-contains-position?;
