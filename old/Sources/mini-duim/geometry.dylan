Module:    mini-duim
Synopsis:  Mini-DUIM geometry
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Basic regions

define open abstract class <region> (<object>)
end class <region>;


define sealed class <nowhere> (<region>)
end class <nowhere>;

define constant $nowhere :: <nowhere> = make(<nowhere>);


define sealed class <everywhere> (<region>)
end class <everywhere>;

define constant $everywhere :: <everywhere> = make(<everywhere>);


/// Points

define sealed class <point> (<region>)
  slot point-x :: <integer>,
    required-init-keyword: x:;
  slot point-y :: <integer>,
    required-init-keyword: y:;
end class <standard-point>;

define function make-point
    (x :: <real>, y :: <real>) => (point :: <point>)
  make(<point>, x: truncate(x), y: truncate(y))
end function make-point;

define method point-position
    (point :: <point>) => (x :: <integer>, y :: <integer>)
  values(point-x(point), point-y(point))
end method point-position;

define method box-edges
    (point :: <point>) => (left, top, right, bottom)
  values(point-x(point), point-y(point),
	 point-x(point), point-y(point))
end method box-edges;


/// Boxes

define sealed class <bounding-box> (<region>)
  slot %left :: <integer>,
    required-init-keyword: left:;
  slot %top  :: <integer>,
    required-init-keyword: top:;
  slot %right  :: <integer>,
    required-init-keyword: right:;
  slot %bottom :: <integer>,
    required-init-keyword: bottom:;
end class <bounding-box>;

define function make-bounding-box
    (x1 :: <real>, y1 :: <real>, x2 :: <real>, y2 :: <real>)
 => (box :: <bounding-box>)
  if (x1 > x2) swap!(x1, x2) end;
  if (y1 > y2) swap!(y1, y2) end;
  make(<bounding-box>,
       left:  truncate(x1), top:    truncate(y1),
       right: truncate(x2), bottom: truncate(y2))
end function make-bounding-box;

define method box-edges
    (box :: <bounding-box>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(box.%left, box.%top, box.%right, box.%bottom)
end method box-edges;


/// Bounding box protocol

define method box-left (region) => (left :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(top, right, bottom);
  left
end method box-left;

define method box-top (region) => (top :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, right, bottom);
  top
end method box-top;

define method box-right (region) => (right :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, top, bottom);
  right
end method box-right;

define method box-bottom (region) => (bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, top, right);
  bottom
end method box-bottom;

define method box-size (region) => (width :: <integer>, height :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  values(right - left, bottom - top)
end method box-size;

define method box-width (region) => (width :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(top, bottom);
  right - left
end method box-width;

define method box-height (region) => (height :: <integer>)
  let (left, top, right, bottom) = box-edges(region);
  ignore(left, right);
  bottom - top
end method box-height;


/// LTRB's

define function ltrb-well-formed?
    (left :: <coordinate>, top :: <coordinate>,
     right :: <coordinate>, bottom :: <coordinate>) => (true? :: <boolean>)
  right >= left & bottom >= top
end function ltrb-well-formed?;

define function ltrb-contains-position?
    (left  :: <coordinate>, top    :: <coordinate>,
     right :: <coordinate>, bottom :: <coordinate>,
     x :: <coordinate>, y :: <coordinate>) => (true? :: <boolean>)
  left <= x & top <= y
  & right >= x & bottom >= y
end function ltrb-contains-position?;

define function ltrb-contains-ltrb?
    (left1  :: <coordinate>, top1    :: <coordinate>,
     right1 :: <coordinate>, bottom1 :: <coordinate>,
     left2  :: <coordinate>, top2    :: <coordinate>,
     right2 :: <coordinate>, bottom2 :: <coordinate>) => (true? :: <boolean>)
  left1 <= left2 & top1 <= top2
  & right1 >= right2 & bottom1 >= bottom2
end function ltrb-contains-ltrb?;

define function ltrb-intersects-ltrb? 
    (left1  :: <coordinate>, top1    :: <coordinate>,
     right1 :: <coordinate>, bottom1 :: <coordinate>,
     left2  :: <coordinate>, top2    :: <coordinate>,
     right2 :: <coordinate>, bottom2 :: <coordinate>)
 => (valid? :: <boolean>,
     left  :: <coordinate>, top    :: <coordinate>,
     right :: <coordinate>, bottom :: <coordinate>)
  let left = max(left1, left2);
  let top  = max(top1, top2);
  let right  = min(right1, right2);
  let bottom = min(bottom1, bottom2);
  if (ltrb-well-formed?(left, top, right, bottom))
    values(#t, left, top, right, bottom)
  end
end function ltrb-intersects-ltrb?;


/// Transforms

define sealed class <transform> (<object>)
  slot %tx :: <integer>,
    required-init-keyword: tx:;
  slot %ty :: <integer>,
    required-init-keyword: ty:;
end class <transform>;

define function make-translation-transform
    (tx :: <integer>, ty :: <integer>) => (transform :: <transform>)
  make(<transform>, tx: tx, ty: ty)
end function make-translation-transform;

define constant $identity-transform = make-translation-transform(0,0);


define method compose-transforms
    (t1 :: <transform>, t2 :: <transform>) => (transform :: <transform>)
  make(<transform>,
       tx: t1.%tx + t2.%tx, ty: t1.%ty + t2.%ty)
end method compose-transforms;

define method invert-transform
    (transform :: <transform>) => (transform :: <transform>)
  make(<transform>,
       tx: -transform.%tx, ty: -transform.%ty)
end method invert-transform;


define method compose-translation-with-transform
    (transform :: <transform>, tx :: <integer>, ty :: <integer>)
 => (transform :: <transform>)
  if (tx = 0 & ty = 0)
    transform
  else
    let tx = tx + transform.%tx;
    let ty = ty + transform.%ty;
    if (tx = 0 & ty = 0)
      $identity-transform
    else
      make-translation-transform(tx, ty)
    end
  end
end method compose-translation-with-transform;


define method transform-position
    (transform :: <transform>, x, y) => (x, y)
  values(x + transform.%tx, y + transform.%ty)
end method transform-position;

define method untransform-position
    (transform :: <transform>, x, y) => (x, y)
  values(x - transform.%tx, y - transform.%ty)
end method untransform-position;


define method transform-distance
    (transform :: <transform>, dx, dy) => (dx, dy)
  ignore(transform);
  values(dx, dy)
end method transform-distance;

define method untransform-distance
    (transform :: <transform>, dx, dy) => (dx, dy)
  ignore(transform);
  values(dx, dy)
end method untransform-distance;


define method transform-box
    (transform :: <transform>, left, top, right, bottom)
 => (left, top, right, bottom)
  values(left  + transform.%tx, top    + transform.%ty,
	 right + transform.%tx, bottom + transform.%ty)
end method transform-box;

define method untransform-box
    (transform :: <transform>, left, top, right, bottom)
 => (left, top, right, bottom)
  values(left  - transform.%tx, top    - transform.%ty,
	 right - transform.%tx, bottom - transform.%ty)
end method untransform-box;


define method transform-region
    (transform :: <transform>, point :: <point>) => (point :: <point>)
  let (x, y) = point-position(point);
  let (nx, ny) = transform-position(transform, x, y);
  make-point(nx, ny)
end method transform-region;

define method transform-region
    (transform :: <transform>, box :: <bounding-box>) => (box :: <bounding-box>)
  let (left, top, right, bottom) = box-edges(box);
  let (nl, nt) = transform-position(transform, left, top);
  let (nr, nb) = transform-position(transform, right, bottom);
  make-bounding-box(nl, nt, nr, nb)
end method transform-region;


define method untransform-region
    (transform :: <transform>, point :: <point>) => (point :: <point>)
  let (x, y) = point-position(point);
  let (nx, ny) = untransform-position(transform, x, y);
  make-point(nx, ny)
end method untransform-region;

define method untransform-region
    (transform :: <transform>, box :: <bounding-box>) => (box :: <bounding-box>)
  let (left, top, right, bottom) = box-edges(box);
  let (nl, nt) = untransform-position(transform, left, top);
  let (nr, nb) = untransform-position(transform, right, bottom);
  make-bounding-box(nl, nt, nr, nb)
end method untransform-region;
