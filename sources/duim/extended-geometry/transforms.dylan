Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Extended transformation protocol

/// General transformations

define sealed class <general-transform> (<transform>)
  sealed constant slot %mxx :: <single-float>,
    required-init-keyword: mxx:;
  sealed constant slot %mxy :: <single-float>,
    required-init-keyword: mxy:;
  sealed constant slot %myx :: <single-float>,
    required-init-keyword: myx:;
  sealed constant slot %myy :: <single-float>,
    required-init-keyword: myy:;
  sealed constant slot %tx  :: <single-float>,
    required-init-keyword: tx:;
  sealed constant slot %ty  :: <single-float>,
    required-init-keyword: ty:;
  sealed slot %inverse :: false-or(<transform>) = #f;
end class <general-transform>;

define sealed domain make (singleton(<general-transform>));
define sealed domain initialize (<general-transform>);

define method transform-components
    (transform :: <general-transform>)
 => (mxx :: <single-float>, mxy :: <single-float>, myx :: <single-float>, myy :: <single-float>,
     tx :: <single-float>, ty :: <single-float>);
  values(transform.%mxx, transform.%mxy,
	 transform.%myx, transform.%myy,
	 transform.%tx, transform.%ty)
end method transform-components;


/// Conditions

define sealed class <transform-underspecified> (<transform-error>)
  sealed constant slot %underspecified-points,
    required-init-keyword: points:;
end class <transform-underspecified>;

define method condition-to-string
    (condition :: <transform-underspecified>) => (string :: <string>)
  format-to-string("You can't make a transformation from the three collinear points "
		   "(%d,%d), (%d,%d), and (%d,%d)",
		   condition.%underspecified-points[0],
		   condition.%underspecified-points[1],
		   condition.%underspecified-points[2],
		   condition.%underspecified-points[3],
		   condition.%underspecified-points[4],
		   condition.%underspecified-points[5])
end method condition-to-string;

define sealed class <reflection-underspecified> (<transform-underspecified>)
end class <reflection-underspecified>;

define method condition-to-string
    (condition :: <reflection-underspecified>) => (string :: <string>)
  format-to-string("You can't make a reflection from the two coincident points "
		   "(%d,%d) and (%d,%d)",
		   condition.%underspecified-points[0],
		   condition.%underspecified-points[1],
		   condition.%underspecified-points[2],
		   condition.%underspecified-points[3])
end method condition-to-string;


/// Constructors

// External interface ensures everything is a single float
define sideways method make-transform
    (mxx :: <real>, mxy :: <real>, myx :: <real>, myy :: <real>,
     tx :: <real>,  ty :: <real>)
 => (transform :: <transform>)
  make-transform-1
    (as(<single-float>, mxx), as(<single-float>, mxy),
     as(<single-float>, myx), as(<single-float>, myy),
     as(<single-float>, tx),  as(<single-float>, ty))
end method make-transform;

//--- 'sideways' because <transform> is defined in DUIM-Geometry
define sealed inline sideways method make 
    (class == <transform>,
     #key mxx = 1.0, mxy = 0.0, myx = 0.0, myy = 1.0, tx = 0.0, ty = 0.0)
 => (transform :: <transform>)
  make-transform(mxx, mxy, myx, myy, tx, ty)
end method make;

// Internal version receives single floats
define method make-transform-1
    (mxx :: <single-float>, mxy :: <single-float>, myx :: <single-float>, myy :: <single-float>,
     tx :: <single-float>,  ty :: <single-float>)
 => (transform :: <transform>)
  case
    ~(mxx = 1.0 & mxy = 0.0 & myx = 0.0 & myy = 1.0) =>
      make(<general-transform>,
           mxx: mxx, mxy: mxy, myx: myx, myy: myy, tx: tx, ty: ty);
    ~(zero?(tx) & zero?(ty)) =>
      if (integral?(tx) & integral?(ty))
        make(<integer-translation-transform>,
             tx: truncate(tx), ty: truncate(ty))
      else
        make(<float-translation-transform>,
	     tx: as(<single-float>, tx), ty: as(<single-float>, ty))
      end;
    otherwise =>
      $identity-transform
  end
end method make-transform-1;

define sideways method make-rotation-transform
    (angle :: <real>, #key origin-x = 0, origin-y = 0)
 => (transform :: <transform>)
  let angle = as(<single-float>, modulo(angle, $2pi));
  if (angle = 0.0)
    $identity-transform
  else
    let cos = cos(angle);
    let sin = sin(angle);
    let one-minus-cos = 1.0 - cos;
    let origin-x = as(<single-float>, origin-x);
    let origin-y = as(<single-float>, origin-y);
    make(<general-transform>,
         mxx: cos, mxy: -sin,
         myx: sin, myy: cos,
         tx: one-minus-cos * origin-x + sin * origin-y,
         ty: one-minus-cos * origin-y - sin * origin-x)
  end
end method make-rotation-transform;

define sideways method make-rotation-transform*
    (angle :: <real>, #key origin) => (transform :: <transform>)
  if (origin)
    make-rotation-transform(angle,
			    origin-x: point-x(origin),
			    origin-x: point-y(origin))
  else
    make-rotation-transform(angle)
  end
end method make-rotation-transform*;

define sideways method make-scaling-transform
    (mx :: <real>, my :: <real>, #key origin-x = 0, origin-y = 0)
 => (transform :: <transform>)
  let mx = as(<single-float>, mx);
  let my = as(<single-float>, my);
  if (mx = 1.0 & my = 1.0)
    $identity-transform
  else
    make(<general-transform>,
         mxx: mx, mxy: 0.0,
         myx: 0.0, myy: my,
         tx: (1.0 - mx) * as(<single-float>, origin-x),
         ty: (1.0 - my) * as(<single-float>, origin-y))
  end
end method make-scaling-transform;

define sideways method make-scaling-transform*
    (mx :: <real>, my :: <real>, #key origin) => (transform :: <transform>)
  if (origin)
    make-scaling-transform(mx, my, 
			   origin-x: point-x(origin),
			   origin-y: point-y(origin))
  else
    make-scaling-transform(mx, my)
  end
end method make-scaling-transform*;

define sideways method make-reflection-transform
    (x1 :: <real>, y1 :: <real>, x2 :: <real>, y2 :: <real>)
 => (transform :: <transform>)
  when (x1 = x2 & y1 = y2)
    error(make(<reflection-underspecified>, points: list(x1, y1, x2, y2)))
  end;
  let x1 = as(<single-float>, x1);
  let y1 = as(<single-float>, y1);
  let x2 = as(<single-float>, x2);
  let y2 = as(<single-float>, y2);
  let nx = y1 - y2;
  let ny = x2 - x1;
  let nxx = nx * nx;
  let nxy = -(nx * ny);
  let nyy = ny * ny;
  // Now normalize...
  let norm = 2.0 / (nxx + nyy);
  nxx := nxx * norm;
  nxy := nxy * norm;
  nyy := nyy * norm;
  make(<general-transform>,
       mxx: 1.0 - nxx, mxy: nxy,
       myx: nxy, myy: 1.0 - nyy,
       tx: nxx * x1 - nxy * y1, ty: nyy * y1 - nxy * x1)
end method make-reflection-transform;

define sideways method make-reflection-transform*
    (point-1 :: <standard-point>, point-2 :: <standard-point>)
 => (transform :: <transform>)
  make-reflection-transform(point-x(point-1), point-y(point-1),
			    point-x(point-2), point-y(point-2))
end method make-reflection-transform*;

define sideways method make-3-point-transform
    (x1, y1, x2, y2, x3, y3,
     x1-image, y1-image, x2-image, y2-image, x3-image, y3-image)
 => (transform :: <transform>)
  let x1y2 = x1 * y2;
  let x2y1 = x2 * y1;
  let x2y3 = x2 * y3;
  let x3y2 = x3 * y2;
  let x3y1 = x3 * y1;
  let x1y3 = x1 * y3;
  let one/det = x1y2 + -x2y1 + x2y3 + -x3y2 + x3y1 + -x1y3;
  when (zero?(one/det))
    error(make(<transform-underspecified>,
               points: list(x1, y1, x2, y2, x3, y3)))
  end;
  one/det := 1.0 / one/det;
  let x2-x1 = x2 - x1;
  let y1-y2 = y1 - y2;
  let x3-x2 = x3 - x2;
  let y2-y3 = y2 - y3;
  let x1-x3 = x1 - x3;
  let y3-y1 = y3 - y1;
  let x1y2-x2y1 = x1y2 - x2y1;
  let x2y3-x3y2 = x2y3 - x3y2;
  let x3y1-x1y3 = x3y1 - x1y3;
  make-transform
    ((x1-image * y2-y3 + x2-image * y3-y1 + x3-image * y1-y2) * one/det,
     (x1-image * x3-x2 + x2-image * x1-x3 + x3-image * x2-x1) * one/det,
     (y1-image * y2-y3 + y2-image * y3-y1 + y3-image * y1-y2) * one/det,
     (y1-image * x3-x2 + y2-image * x1-x3 + y3-image * x2-x1) * one/det,
     (x1-image * x2y3-x3y2 + x2-image * x3y1-x1y3 + x3-image * x1y2-x2y1) * one/det,
     (y1-image * x2y3-x3y2 + y2-image * x3y1-x1y3 + y3-image * x1y2-x2y1) * one/det)
end method make-3-point-transform;

define sideways method make-3-point-transform*
    (point-1, point-2, point-3,
     point-1-image, point-2-image, point-3-image)
 => (transform :: <transform>)
  make-3-point-transform
    (point-x(point-1), point-y(point-1),
     point-x(point-2), point-y(point-2),
     point-x(point-3), point-y(point-3),
     point-x(point-1-image), point-y(point-1-image),
     point-x(point-2-image), point-y(point-2-image),
     point-x(point-3-image), point-y(point-3-image))
end method make-3-point-transform*;


/// Predicates

define method transform-equal
    (tr1 :: <general-transform>, tr2 :: <general-transform>) => (true? :: <boolean>)
  tr1.%mxx = tr2.%mxx
  & tr1.%mxy = tr2.%mxy
  & tr1.%myx = tr2.%myx
  & tr1.%myy = tr2.%myy
  & tr1.%tx = tr2.%tx
  & tr1.%ty = tr2.%ty
end method transform-equal;

define method identity-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  #f
end method identity-transform?;

define method translation-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  #f
end method translation-transform?;

define method invertible-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  ~zero?(transform.%mxx * transform.%myy - transform.%mxy * transform.%myx)
end method invertible-transform?;

define method reflection-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  negative?(transform.%mxx * transform.%myy - transform.%mxy * transform.%myx)
end method reflection-transform?;

define method rigid-transform? (transform :: <general-transform>) => (true? :: <boolean>)
    transform.%mxx * transform.%myy - transform.%mxy * transform.%myx = 1.0
  & transform.%mxx * transform.%mxy + transform.%myx * transform.%myy = 1.0
  & transform.%mxx * transform.%mxx + transform.%myx * transform.%myx = 1.0
end method rigid-transform?;

define method even-scaling-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  transform.%mxy = 0.0
  & transform.%myx = 0.0
  & transform.%mxx = transform.%myy
end method even-scaling-transform?;

define method scaling-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  transform.%mxy = 0.0
  & transform.%myx = 0.0
end method scaling-transform?;

define method rectilinear-transform? (transform :: <general-transform>) => (true? :: <boolean>)
  (transform.%mxy = 0.0 & transform.%myx = 0.0)
  | (transform.%mxx = 0.0 & transform.%myy = 0.0)
end method rectilinear-transform?;


/// Inversion

define method invert-transform
    (transform :: <general-transform>) => (transform :: <transform>)
  transform.%inverse
  | begin
      let one/det
	= transform.%mxx * transform.%myy - transform.%mxy * transform.%myx;
      when (zero?(one/det))
	error(make(<singular-transform>, transform: transform))
      end;
      one/det := 1.0 / one/det;
      let inverse :: <general-transform>
	= make(<general-transform>,
	       mxx: transform.%myy * one/det, mxy: -transform.%mxy * one/det,
	       myx: -transform.%myx * one/det, myy: transform.%mxx * one/det,
	       tx: (transform.%mxy * transform.%ty - transform.%myy * transform.%tx) * one/det,
	       ty: (transform.%myx * transform.%tx - transform.%mxx * transform.%ty) * one/det);
      // Link the transform to its inverse
      inverse.%inverse   := transform;
      transform.%inverse := inverse;
      inverse
    end
end method invert-transform;


/// Composition operators

define method compose-transforms
    (tr1 :: <general-transform>, tr2 :: <general-transform>)
 => (transform :: <transform>)
  make-transform-1
    (tr1.%mxx * tr2.%mxx + tr1.%mxy * tr2.%myx,
     tr1.%mxx * tr2.%mxy + tr1.%mxy * tr2.%myy,
     tr1.%myx * tr2.%mxx + tr1.%myy * tr2.%myx,
     tr1.%myx * tr2.%mxy + tr1.%myy * tr2.%myy,
     tr1.%tx + tr1.%mxx * tr2.%tx + tr1.%mxy * tr2.%ty,
     tr1.%ty + tr1.%myx * tr2.%tx + tr1.%myy * tr2.%ty)
end method compose-transforms;

define method compose-transforms
    (tr1 :: <general-transform>, tr2 :: <translation-transform>)
 => (transform :: <transform>)
  // NB: the translations can be integers or single floats here
  let (mxx2, mxy2, myx2, myy2, tx2, ty2) = transform-components(tr2);
  ignore(mxx2, mxy2, myx2, myy2);
  make(<general-transform>,
       mxx: tr1.%mxx, mxy: tr1.%mxy,
       myx: tr1.%myx, myy: tr1.%myy,
       // Floating point contagion will ensure tx/ty are OK
       tx: tr1.%tx + tr1.%mxx * tx2 + tr1.%mxy * ty2,
       ty: tr1.%ty + tr1.%myx * tx2 + tr1.%myy * ty2)
end method compose-transforms;

define method compose-transforms
    (tr1 :: <translation-transform>, tr2 :: <general-transform>)
 => (transform :: <transform>)
  // NB: the translations can be integers or single floats here
  let (mxx1, mxy1, myx1, myy1, tx1, ty1) = transform-components(tr1);
  ignore(mxx1, mxy1, myx1, myy1);
  make(<general-transform>,
       mxx: tr2.%mxx, mxy: tr2.%mxy,
       myx: tr2.%myx, myy: tr2.%myy,
       // Floating point contagion will ensure tx/ty are OK
       tx: tx1 + tr2.%tx, ty: ty1 + tr2.%ty)
end method compose-transforms;


/// Translation composition operators

define method compose-translation-with-transform
    (transform :: <general-transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>)
  let tx = as(<single-float>, tx);
  let ty = as(<single-float>, ty);
  if (tx = 0.0 & ty = 0.0)
    transform
  else
    make(<general-transform>,
         mxx: transform.%mxx, mxy: transform.%mxy,
         myx: transform.%myx, myy: transform.%myy,
         tx: transform.%tx + transform.%mxx * tx + transform.%mxy * ty,
         ty: transform.%ty + transform.%myx * tx + transform.%myy * ty)
  end
end method compose-translation-with-transform;


/// Scaling composition operators

define sideways method compose-scaling-with-transform
    (transform :: <identity-transform>, mx :: <real>, my :: <real>, #key origin)
 => (transform :: <transform>)
  make-scaling-transform*(mx, my, origin: origin)
end method compose-scaling-with-transform;

define sideways method compose-scaling-with-transform
    (transform :: <translation-transform>, mx :: <real>, my :: <real>, #key origin)
 => (transform :: <transform>)
  let mx = as(<single-float>, mx);
  let my = as(<single-float>, my);
  if (mx = 1.0 & my = 1.0)
    transform
  else
    let (mxx, mxy, myx, myy, tx, ty) = transform-components(transform);
    ignore(mxx, mxy, myx, myy);
    // NB: the translations can be integers or single floats here
    if (origin)
      make-transform-1
        (mx, 0.0, 0.0, my,
         tx + (1.0 - mx) * as(<single-float>, point-x(origin)),
         ty + (1.0 - my) * as(<single-float>, point-y(origin)))
    else
      make-transform-1(mx, 0.0, 0.0, my,
		       as(<single-float>, tx), as(<single-float>, ty))
    end
  end
end method compose-scaling-with-transform;

define method compose-scaling-with-transform
    (transform :: <general-transform>, mx :: <real>, my :: <real>, #key origin)
 => (transform :: <transform>)
  let mx = as(<single-float>, mx);
  let my = as(<single-float>, my);
  if (mx = 1.0 & my = 1.0)
    transform
  else
    if (origin)
      let tx2 = (1.0 - mx) * as(<single-float>, point-x(origin));
      let ty2 = (1.0 - my) * as(<single-float>, point-y(origin));
      make-transform-1
        (transform.%mxx * mx, transform.%mxy * my, transform.%myx * mx,
         transform.%myy * my,
         transform.%tx + transform.%mxx * tx2 + transform.%mxy * ty2,
         transform.%ty + transform.%myx * tx2 + transform.%myy * ty2)
    else
      make-transform-1
        (transform.%mxx * mx, transform.%mxy * my, transform.%myx * mx,
         transform.%myy * my, transform.%tx, transform.%ty)
    end
  end
end method compose-scaling-with-transform;

define sideways method compose-transform-with-scaling
    (transform :: <transform>, mx :: <real>, my :: <real>, #key origin)
 => (transform :: <transform>)
  compose-transforms(make-scaling-transform*(mx, my, origin: origin), transform)
end method compose-transform-with-scaling;


/// Rotation composition operators

define sideways method compose-rotation-with-transform
    (transform :: <identity-transform>, angle :: <real>, #key origin)
 => (transform :: <transform>)
  make-rotation-transform*(angle, origin: origin)
end method compose-rotation-with-transform;

define sideways method compose-rotation-with-transform
    (transform :: <translation-transform>, angle :: <real>, #key origin)
 => (transform :: <transform>)
  let angle :: <single-float> = modulo(angle, $2pi);
  if (angle = 0.0)
    transform
  else
    // NB: the translations can be integers or single floats here
    let (mxx, mxy, myx, myy, tx, ty) = transform-components(transform);
    ignore(mxx, mxy, myx, myy);
    let cos = cos(angle);
    let sin = sin(angle);
    let one-minus-cos :: <single-float> = 1.0 - cos;
    if (origin)
      let origin-x = as(<single-float>, point-x(origin));
      let origin-y = as(<single-float>, point-y(origin));
      make(<general-transform>,
           mxx: cos, mxy: -sin,
           myx: sin, myy: cos,
           tx: tx + one-minus-cos * origin-x + sin * origin-y,
           ty: ty + (one-minus-cos * origin-y - sin * origin-x))
    else
      make(<general-transform>,
           mxx: cos, mxy: -sin, myx: sin, myy: cos,
	   tx: as(<single-float>, tx), ty: as(<single-float>, ty))
    end
  end
end method compose-rotation-with-transform;

define method compose-rotation-with-transform
    (transform :: <general-transform>, angle :: <real>, #key origin)
 => (transform :: <transform>)
  let angle :: <single-float> = modulo(angle, $2pi);
  if (angle = 0.0)
    transform
  else
    let cos = cos(angle);
    let sin = sin(angle);
    let one-minus-cos = 1.0 - cos;
    let mxx = transform.%mxx * cos + transform.%mxy * sin;
    let mxy = transform.%mxy * cos - transform.%mxx * sin;
    let myx = transform.%myx * cos + transform.%myy * sin;
    let myy = transform.%myy * cos - transform.%myx * sin;
    if (origin)
      let origin-x = as(<single-float>, point-x(origin));
      let origin-y = as(<single-float>, point-y(origin));
      let tx2 = one-minus-cos * origin-x + sin * origin-y;
      let ty2 = one-minus-cos * origin-y - sin * origin-x;
      make-transform-1
        (mxx, mxy, myx, myy,
         transform.%tx + transform.%mxx * tx2 + transform.%mxy * ty2,
         transform.%ty + transform.%myx * tx2 + transform.%myy * ty2)
    else
      make-transform-1(mxx, mxy, myx, myy, transform.%tx, transform.%ty)
    end
  end
end method compose-rotation-with-transform;

define sideways method compose-transform-with-rotation
    (transform :: <transform>, angle :: <real>, #key origin)
 => (transform :: <transform>)
  compose-transforms(make-rotation-transform*(angle, origin: origin), transform)
end method compose-transform-with-rotation;


/// Transforming and untransforming of "spread" points

define method transform-position
    (transform :: <general-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x * transform.%mxx + y * transform.%mxy + transform.%tx,
         x * transform.%myx + y * transform.%myy + transform.%ty)
end method transform-position;

define method untransform-position
    (transform :: <general-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  transform-position(invert-transform(transform), x, y)
end method untransform-position;


/// Transforming and untransforming of distances

define method transform-distance
    (transform :: <general-transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>)
  values(dx * transform.%mxx + dy * transform.%mxy,
         dx * transform.%myx + dy * transform.%myy)
end method transform-distance;

define method untransform-distance
    (transform :: <general-transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>)
  transform-distance(invert-transform(transform), dx, dy)
end method untransform-distance;


/// Transforming and untransforming of angles

define method transform-angles
    (transform :: <general-transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>)
  when (reflection-transform?(transform))
    swap!(start-angle, end-angle)
  end;
  let sx = cos(start-angle);
  let sy = sin(start-angle);
  let ex = cos(end-angle);
  let ey = sin(end-angle);
  transform-distances!(transform, sx, sy, ex, ey);
  values(atan2(sy, sx), atan2(ey, ex))
end method transform-angles;

define method untransform-angles
    (transform :: <general-transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>)
  transform-angles(invert-transform(transform), start-angle, end-angle)
end method untransform-angles;


/// Transforming and untransforming of "spread" rectangles

define method transform-box
    (transform :: <general-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  assert(rectilinear-transform?(transform),
         "Bounding boxes can only be transformed by a rectilinear transform");
  let nx1 = x1 * transform.%mxx + y1 * transform.%mxy + transform.%tx;
  let ny1 = x1 * transform.%myx + y1 * transform.%myy + transform.%ty;
  let nx2 = x2 * transform.%mxx + y2 * transform.%mxy + transform.%tx;
  let ny2 = x2 * transform.%myx + y2 * transform.%myy + transform.%ty;
  fix-box(min(nx1, nx2), min(ny1, ny2),
	  max(nx1, nx2), max(ny1, ny2))
end method transform-box;

define method untransform-box
    (transform :: <general-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  transform-box(invert-transform(transform), x1, y1, x2, y2)
end method untransform-box;
