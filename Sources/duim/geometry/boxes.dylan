Module:       duim-geometry-internals
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bounding boxes

/// Box protocol

define protocol <<box-protocol>> ()
  function bounding-box
    (region, #key into) => (box :: <bounding-box>);
  function box-edges
    (region)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  function set-box-edges
    (region, left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
 => (region);
  function box-position (region) => (x :: <integer>, y :: <integer>);
  function set-box-position (region, x :: <integer>, y :: <integer>) => (region);
  function box-size (region) => (width :: <integer>, height :: <integer>);
  function set-box-size (region, width :: <integer>, height :: <integer>) => (region);
  function box-width  (region) => (width :: <integer>);
  function box-height (region) => (height :: <integer>);
end protocol <<box-protocol>>;


/// Box creation

// Canonicalizes x1/y1/x2/y2 into left/top/right/bottom
// Note that boxes are in an _inverted_ cartesian coordinate system,
// with the positive Y axis running _downward_.  This matches the
// notion of most window systems
define sealed method make-bounding-box
    (x1 :: <real>, y1 :: <real>, x2 :: <real>, y2 :: <real>)
 => (box :: <bounding-box>)
  let (x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
    = fix-box(x1, y1, x2, y2);
  when (x1 > x2)
    swap!(x1, x2)
  end;
  when (y1 > y2)
    swap!(y1, y2)
  end;
  if (zero?(x1) & zero?(y1))
    make(<simple-box>, width: x2, height: y2)
  else
    make(<general-box>, left: x1, top: y1, right: x2, bottom: y2)
  end
end method make-bounding-box;

define sealed inline method make
    (class == <bounding-box>, #key left, top, right, bottom)
 => (box :: <bounding-box>)
  make-bounding-box(left, top, right, bottom)
end method make;

// Seal the constructors and initializers for all bounding boxes
define sealed domain make (subclass(<bounding-box>));
define sealed domain initialize (<bounding-box>);


/// General bounding boxes

define sealed class <general-box> (<bounding-box>)
  sealed slot %left :: <integer>,
    required-init-keyword: left:;
  sealed slot %top  :: <integer>,
    required-init-keyword: top:;
  sealed slot %right  :: <integer>,
    required-init-keyword: right:;
  sealed slot %bottom :: <integer>,
    required-init-keyword: bottom:;
end class <general-box>;

define sealed inline method box-edges
    (box :: <general-box>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(box.%left, box.%top, box.%right, box.%bottom)
end method box-edges;

define sealed method transform-region 
    (transform :: <transform>, box :: <general-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = transform-box(transform, box.%left, box.%top, box.%right, box.%bottom);
  make-bounding-box(x1, y1, x2, y2)
end method transform-region;

define sealed method untransform-region 
    (transform :: <transform>, box :: <general-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = untransform-box(transform, box.%left, box.%top, box.%right, box.%bottom);
  make-bounding-box(x1, y1, x2, y2)
end method untransform-region;

define sealed method transform-region!
    (transform :: <transform>, box :: <general-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = transform-box(transform, box.%left, box.%top, box.%right, box.%bottom);
  when (x1 > x2)
    swap!(x1, x2)
  end;
  when (y1 > y2)
    swap!(y1, y2)
  end;
  box.%left := x1;
  box.%top  := y1;
  box.%right  := x2;
  box.%bottom := y2;
  box
end method transform-region!;

define sealed method untransform-region!
    (transform :: <transform>, box :: <general-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = untransform-box(transform, box.%left, box.%top, box.%right, box.%bottom);
  when (x1 > x2)
    swap!(x1, x2)
  end;
  when (y1 > y2)
    swap!(y1, y2)
  end;
  box.%left := x1;
  box.%top  := y1;
  box.%right  := x2;
  box.%bottom := y2;
  box
end method untransform-region!;

define sealed method region-empty?
    (box :: <general-box>) => (true? :: <boolean>)
    box.%right <= box.%left
  | box.%bottom <= box.%top
end method region-empty?;

// Set the edges of the box, and return the box as the value
// Note that we don't downgrade a <general-box> to a <simple-box>,
// because that seems more likely to cause oscillating consathons
define sealed method set-box-edges
    (box :: <general-box>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
 => (box :: <general-box>)
  box.%left := left;
  box.%top  := top;
  box.%right  := right;
  box.%bottom := bottom;
  box
end method set-box-edges;

// Set the position of the box, and return the box as the value
define sealed method set-box-position
    (box :: <general-box>, x :: <integer>, y :: <integer>)
 => (box :: <general-box>)
  let width  :: <integer> = box.%right  - box.%left;
  let height :: <integer> = box.%bottom - box.%top;
  box.%left := x;
  box.%top  := y;
  box.%right  := x + width;
  box.%bottom := y + height;
  box
end method set-box-position;

// Set the size of the box, and return the box as the value
define sealed method set-box-size
    (box :: <general-box>, width :: <integer>, height :: <integer>)
 => (box :: <general-box>)
  let new-right  :: <integer> = box.%left + width;
  let new-bottom :: <integer> = box.%top + height;
  box.%right  := new-right;
  box.%bottom := new-bottom;
  box
end method set-box-size;

define sealed method invalidate-box!
    (box :: <general-box>) => ()
  box.%left := $largest-coordinate
end method invalidate-box!;

define sealed method box-invalidated?
    (box :: <general-box>) => (invalid? :: <boolean>)
  box.%left = $largest-coordinate
end method box-invalidated?;


/// Simple bounding boxes

define sealed class <simple-box> (<bounding-box>)
  sealed slot %width  :: <integer>,
    required-init-keyword: width:;
  sealed slot %height :: <integer>,
    required-init-keyword: height:;
end class <simple-box>;

define sealed inline method box-edges
    (box :: <simple-box>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, box.%width, box.%height)
end method box-edges;

define sealed method transform-region 
    (transform :: <transform>, box :: <simple-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = transform-box(transform, 0, 0, box.%width, box.%height);
  make-bounding-box(x1, y1, x2, y2)
end method transform-region;

define sealed method untransform-region 
    (transform :: <transform>, box :: <simple-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = untransform-box(transform, 0, 0, box.%width, box.%height);
  make-bounding-box(x1, y1, x2, y2)
end method untransform-region;

define sealed method transform-region!
    (transform :: <transform>, box :: <simple-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = transform-box(transform, 0, 0, box.%width, box.%height);
  when (x1 > x2)
    swap!(x1, x2)
  end;
  when (y1 > y2)
    swap!(y1, y2)
  end;
  if (zero?(x1) & zero?(y1))
    box.%width  := x2;
    box.%height := y2;
    box
  else
    make(<general-box>, left: x1, top: y1, right: x2, bottom: y2)
  end
end method transform-region!;

define sealed method untransform-region!
    (transform :: <transform>, box :: <simple-box>) => (box :: <bounding-box>)
  let (x1, y1, x2, y2)
    = untransform-box(transform, 0, 0, box.%width, box.%height);
  when (x1 > x2)
    swap!(x1, x2)
  end;
  when (y1 > y2)
    swap!(y1, y2)
  end;
  if (zero?(x1) & zero?(y1))
    box.%width  := x2;
    box.%height := y2;
    box
  else
    make(<general-box>, left: x1, top: y1, right: x2, bottom: y2)
  end
end method untransform-region!;

define sealed method region-empty?
    (box :: <simple-box>) => (true? :: <boolean>)
  box.%width <= 0
  | box.%height <= 0
end method region-empty?;

// Set the edges of the box, and return the box as the value
define sealed method set-box-edges
    (box :: <simple-box>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
 => (box :: <bounding-box>)
  if (zero?(left) & zero?(top))
    box.%width  := right;
    box.%height := bottom;
    box
  else
    make-bounding-box(left, top, right, bottom)
  end
end method set-box-edges;

// Set the position of the box, and return the box as the value
define sealed method set-box-position
    (box :: <simple-box>, x :: <integer>, y :: <integer>)
 => (box :: <bounding-box>)
  if (zero?(x) & zero?(y))
    box
  else
    make-bounding-box(x, y, x + box.%width, y + box.%height)
  end
end method set-box-position;

// Set the size of the box, and return the box as the value
define sealed method set-box-size
    (box :: <simple-box>, width :: <integer>, height :: <integer>)
 => (box :: <simple-box>)
  box.%width  := width;
  box.%height := height;
  box
end method set-box-size;

define sealed method invalidate-box!
    (box :: <simple-box>) => ()
  box.%width := $smallest-coordinate
end method invalidate-box!;

define sealed method box-invalidated?
    (box :: <simple-box>) => (invalid? :: <boolean>)
  box.%width = $smallest-coordinate
end method box-invalidated?;


/// The rest of the box protocol...

define sealed method region-equal
    (box1 :: <bounding-box>, box2 :: <bounding-box>) => (true? :: <boolean>)
  let (left1, top1, right1, bottom1) = box-edges(box1);
  let (left2, top2, right2, bottom2) = box-edges(box2);
  ltrb-equals-ltrb?(left1, top1, right1, bottom1,
		    left2, top2, right2, bottom2)
end method region-equal;

define sealed method region-contains-position? 
    (box :: <bounding-box>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  let (left, top, right, bottom) = box-edges(box);
  ltrb-contains-position?(left, top, right, bottom,
			  fix-coordinate(x), fix-coordinate(y))
end method region-contains-position?;

define sealed method region-contains-region? 
    (box1 :: <bounding-box>, box2 :: <bounding-box>) => (true? :: <boolean>)
  let (left1, top1, right1, bottom1) = box-edges(box1);
  let (left2, top2, right2, bottom2) = box-edges(box2);
  ltrb-contains-ltrb?(left1, top1, right1, bottom1,
		      left2, top2, right2, bottom2)
end method region-contains-region?;

define sealed method region-intersects-region?
    (box1 :: <bounding-box>, box2 :: <bounding-box>) => (true? :: <boolean>)
  let (left1, top1, right1, bottom1) = box-edges(box1);
  let (left2, top2, right2, bottom2) = box-edges(box2);
  ltrb-intersects-ltrb?(left1, top1, right1, bottom1,
			left2, top2, right2, bottom2)
end method region-intersects-region?;


define method box-position
    (region) => (x :: <integer>, y :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(right, bottom);
  values(left, top)
end method box-position;


define method box-width
    (region) => (width :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(top, bottom);
  right - left
end method box-width;

define method box-height
    (region) => (height :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, right);
  bottom - top
end method box-height;

define method box-size
    (region) => (width :: <integer>, height :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  values(right - left, bottom - top)
end method box-size;


define function box-positions-equal
    (region1, region2) => (true? :: <boolean>)
  let (x1, y1) = box-position(region1);
  let (x2, y2) = box-position(region2);
  x1 = x2 & y1 = y2
end function box-positions-equal;

define function box-edges-equal
    (region1, region2) => (true? :: <boolean>)
  let (left1, top1, right1, bottom1) = box-edges(region1);
  let (left2, top2, right2, bottom2) = box-edges(region2);
  left1 = left2 & top1 = top2 & right1 = right2 & bottom1 = bottom2
end function box-edges-equal;

// This should only be used to compare integer coordinates
define inline function position-difference
    (x1 :: <integer>, y1 :: <integer>,
     x2 :: <integer>, y2 :: <integer>)
 => (dx :: <integer>, dy :: <integer>)
  values(x1 - x2, y1 - y2)
end function position-difference;

define function box-position-difference
    (region1, region2) => (dx :: <integer>, dy :: <integer>)
  let (x1, y1) = box-position(region1);
  let (x2, y2) = box-position(region2);
  position-difference(x1, y1, x2, y2)
end function box-position-difference;

define function box-sizes-equal
    (region1, region2) => (true? :: <boolean>)
  let (left1, top1, right1, bottom1) = box-edges(region1);
  let (left2, top2, right2, bottom2) = box-edges(region2);
  ltrb-size-equal?(left1, top1, right1, bottom1,
		   left2, top2, right2, bottom2)
end function box-sizes-equal;


/// Convenience functions

// Guaranteed to cons a new box unless INTO is supplied
define sealed method bounding-box
    (region, #key into) => (box :: <bounding-box>)
  let (left, top, right, bottom) = box-edges(region);
  if (into)
    select (into by instance?)
      <simple-box> =>
	assert(zero?(left) & zero?(top),
	       "The simple box %= cannot be modified this way", into);
	into.%width  := right;
	into.%height := bottom;
	into;
      <general-box> =>
	into.%left := left;
	into.%top  := top;
	into.%right  := right;
	into.%bottom := bottom;
	into;
    end
  else
    make-bounding-box(left, top, right, bottom)
  end
end method bounding-box;

// Make a new bounding box for the region, and shift its position by DX,DY,
// and return the new box
define sealed method shift-box-position
    (region, dx :: <integer>, dy :: <integer>, #key into)
 => (box :: <bounding-box>)
  let (left, top, right, bottom) = box-edges(region);
  let box :: <general-box>
    = if (into & instance?(into, <general-box>))
	into
      else
	make(<general-box>,
	     left: left, top: top, right: right, bottom: bottom)
      end;
  box.%left := box.%left + dx;
  box.%top  := box.%top + dy;
  box.%right  := box.%right + dx;
  box.%bottom := box.%bottom + dy;
  box
end method shift-box-position;


define method box-center
    (region) => (x :: <integer>, y :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  values(left + floor/((right - left), 2), top + floor/((bottom - top), 2))
end method box-center;

define method box-center*
    (region) => (point :: <standard-point>)
  let (left, top, right, bottom) = box-edges(region);
  make-point(left + floor/((right - left), 2), top + floor/((bottom - top), 2))
end method box-center*;


/// The following are always implemented in terms of 'box-edges'

define inline function box-left
    (region) => (left :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(top, right, bottom);
  left
end function box-left;

define inline function box-top
    (region) => (top :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, right, bottom);
  top
end function box-top;

define inline function box-right
    (region) => (right :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, top, bottom);
  right
end function box-right;

define inline function box-bottom
    (region) => (bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, top, right);
  bottom
end function box-bottom;
