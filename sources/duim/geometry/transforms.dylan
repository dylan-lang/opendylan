Module:       duim-geometry-internals
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Transformation protocol

define protocol <<transform-protocol>> ()
  function transform-components
    (transform :: <transform>)
 => (mxx :: <real>, mxy :: <real>, myx :: <real>, myy :: <real>,
     tx :: <real>, ty :: <real>);
  function transform-coordinate-sequence
    (transform :: <transform>, coordinates :: <sequence>, #key copy?)
 => (coordinates :: <vector>);
  // Constructors
  function make-transform
    (mxx :: <real>, mxy :: <real>, myx :: <real>, myy :: <real>,
     tx :: <real>,  ty :: <real>) => (transform :: <transform>);
  function make-translation-transform
    (tx :: <real>, ty :: <real>) => (transform :: <transform>);
  function make-scaling-transform
    (mx :: <real>, my :: <real>, #key origin-x, origin-y)
 => (transform :: <transform>);
  function make-scaling-transform*
    (mx :: <real>, my :: <real>, #key origin) => (transform :: <transform>);
  function make-rotation-transform
    (angle :: <real>, #key origin-x, origin-y) => (transform :: <transform>);
  function make-rotation-transform*
    (angle :: <real>, #key origin) => (transform :: <transform>);
  function make-reflection-transform
    (x1 :: <real>, y1 :: <real>, x2 :: <real>, y2 :: <real>)
 => (transform :: <transform>);
  function make-reflection-transform*
    (point-1 :: <point>, point-2 :: <point>) => (transform :: <transform>);
  // Predicates
  function transform-equal
    (transform1 :: <transform>, transform2 :: <transform>)
 => (true? :: <boolean>);
  function identity-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function translation-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function invertible-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function reflection-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function rigid-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function even-scaling-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function scaling-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  function rectilinear-transform?
    (transform :: <transform>) => (true? :: <boolean>);
  // Composition
  function compose-transforms
    (transform1 :: <transform>, transform2 :: <transform>)
 => (transform :: <transform>);
  function compose-translation-with-transform
    (transform :: <transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>);
  function compose-transform-with-translation
    (transform :: <transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>);
  function compose-scaling-with-transform
    (transform :: <transform>, mx :: <real>, my :: <real>, #key origin)
 => (transform :: <transform>);
  function compose-transform-with-scaling
    (transform :: <transform>, mx :: <real>, my :: <real>, #key origin)
 => (transform :: <transform>);
  function compose-rotation-with-transform
    (transform :: <transform>, angle :: <real>, #key origin)
 => (transform :: <transform>);
  function compose-transform-with-rotation
    (transform :: <transform>, angle :: <real>, #key origin)
 => (transform :: <transform>);
  function invert-transform
    (transform :: <transform>) => (transform :: <transform>);
  // Simple transformations
  function transform-position
    (transform :: <transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>);
  function untransform-position
    (transform :: <transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>);
  function transform-distance
    (transform :: <transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>);
  function untransform-distance
    (transform :: <transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>);
  function transform-angles
    (transform :: <transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>);
  function untransform-angles
    (transform :: <transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>);
  // Boxes, by definition, have integer coordinates
  function transform-box
    (transform :: <transform>, x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  function untransform-box
    (transform :: <transform>, x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
=> (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
end protocol <<transform-protocol>>;


/// The identity transform

define sealed class <identity-transform> (<transform>)
end class <identity-transform>;

define sealed domain make (singleton(<identity-transform>));
define sealed domain initialize (<identity-transform>);

define sealed inline method transform-components
    (transform :: <identity-transform>)
 => (mxx :: <integer>, mxy :: <integer>, myx :: <integer>, myy :: <integer>,
     tx :: <integer>, ty :: <integer>);
  values(1, 0, 0, 1, 0, 0)
end method transform-components;

define constant $identity-transform :: <identity-transform> = make(<identity-transform>);


/// Translation transformations

define open abstract class <translation-transform> (<transform>)
  sealed slot %inverse :: false-or(<translation-transform>) = #f;
end class <translation-transform>;


/// Float translation transformations

define sealed class <float-translation-transform> (<translation-transform>)
  sealed slot %tx :: <single-float>,
    required-init-keyword: tx:;
  sealed slot %ty :: <single-float>,
    required-init-keyword: ty:;
end class <float-translation-transform>;

define sealed domain make (singleton(<float-translation-transform>));
define sealed domain initialize (<float-translation-transform>);

define sealed inline method transform-components
    (transform :: <float-translation-transform>)
 => (mxx :: <single-float>, mxy :: <single-float>, myx :: <single-float>, myy :: <single-float>,
     tx :: <single-float>, ty :: <single-float>);
  values(1.0, 0.0, 0.0, 1.0, transform.%tx, transform.%ty)
end method transform-components;


/// Integer translation transformations

define sealed class <integer-translation-transform> (<translation-transform>)
  sealed slot %tx :: <integer>,
    required-init-keyword: tx:;
  sealed slot %ty :: <integer>,
    required-init-keyword: ty:;
end class <integer-translation-transform>;

define sealed domain make (singleton(<integer-translation-transform>));
define sealed domain initialize (<integer-translation-transform>);

define sealed inline method transform-components
    (transform :: <integer-translation-transform>)
 => (mxx :: <integer>, mxy :: <integer>, myx :: <integer>, myy :: <integer>,
     tx :: <integer>, ty :: <integer>);
  values(1, 0, 0, 1, transform.%tx, transform.%ty)
end method transform-components;


/// Conditions

define open abstract class <transform-error> (<error>)
end class <transform-error>;

define sealed class <singular-transform> (<transform-error>)
  sealed constant slot %transform,
    required-init-keyword: transform:;
end class <singular-transform>;

define method condition-to-string
    (condition :: <singular-transform>) => (string :: <string>)
  format-to-string("The transformation %= is singular", condition.%transform)
end method condition-to-string;


/// Constructors

define sealed method make-translation-transform
    (tx :: <real>, ty :: <real>) => (transform :: <transform>)
  case
    zero?(tx) & zero?(ty) =>
      $identity-transform;
    integral?(tx) & integral?(ty) =>
      make(<integer-translation-transform>,
           tx: truncate(tx), ty: truncate(ty));
    otherwise =>
      make(<float-translation-transform>, 
	   tx: as(<single-float>, tx), ty: as(<single-float>, ty))
  end
end method make-translation-transform;

define sealed inline method make 
    (class == <translation-transform>, #key tx = 0, ty = 0)
 => (transform :: <transform>)
  make-translation-transform(tx, ty)
end method make;


/// Predicates

define method \=
    (tr1 :: <transform>, tr2 :: <transform>) => (true? :: <boolean>)
  tr1 == tr2
  | transform-equal(tr1, tr2)
end method \=;

define sealed method transform-equal
    (tr1 :: <identity-transform>, tr2 :: <identity-transform>) => (true? :: <boolean>)
  #t
end method transform-equal;

define sealed method transform-equal
    (tr1 :: <translation-transform>, tr2 :: <translation-transform>) => (true? :: <boolean>)
  // NB: the translations can be integers or single floats here
    tr1.%tx = tr2.%tx
  & tr1.%ty = tr2.%ty
end method transform-equal;

define method transform-equal
    (tr1 :: <transform>, tr2 :: <transform>) => (true? :: <boolean>)
  let (mxx1, mxy1, myx1, myy1, tx1, ty1) = transform-components(tr1);
  let (mxx2, mxy2, myx2, myy2, tx2, ty2) = transform-components(tr2);
  mxx1 = mxx2
  & mxy1 = mxy2
  & myx1 = myx2
  & myy1 = myy2
  & tx1 = tx2
  & ty1 = ty2
end method transform-equal;


// Identity transform?
define sealed method identity-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method identity-transform?;

define sealed method identity-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #f
end method identity-transform?;


// Translation transform?
define sealed method translation-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method translation-transform?;

define sealed method translation-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #t
end method translation-transform?;


// Invertible transform?
define sealed method invertible-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method invertible-transform?;

define sealed method invertible-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #t
end method invertible-transform?;


// Reflection transform?
define sealed method reflection-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #f
end method reflection-transform?;

define sealed method reflection-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #f
end method reflection-transform?;


// Rigid transform?
define sealed method rigid-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method rigid-transform?;

define sealed method rigid-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #t
end method rigid-transform?;


// Even scaling transform?
define sealed method even-scaling-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method even-scaling-transform?;

define sealed method even-scaling-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #t
end method even-scaling-transform?;


// Scaling transform?
define sealed method scaling-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method scaling-transform?;

define sealed method scaling-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #t
end method scaling-transform?;


// Rectilinear transform?
define sealed method rectilinear-transform?
    (transform :: <identity-transform>) => (true? :: <boolean>)
  #t
end method rectilinear-transform?;

define sealed method rectilinear-transform?
    (transform :: <translation-transform>) => (true? :: <boolean>)
  #t
end method rectilinear-transform?;


/// Inversion

define sealed method invert-transform
    (transform :: <identity-transform>) => (transform :: <transform>)
  transform
end method invert-transform;

define sealed method invert-transform
    (transform :: <translation-transform>) => (transform :: <transform>)
  transform.%inverse
  | begin
      // NB: the translations can be integers or single floats here, so this
      // call might make either a normal or an integer translation
      let inverse :: <translation-transform>
	= make-translation-transform(-transform.%tx, -transform.%ty);
      inverse.%inverse := transform;
      transform.%inverse := inverse;
      inverse
    end
end method invert-transform;


/// Composition operators

define method compose-transforms
    (tr1 :: <identity-transform>, tr2 :: <transform>) => (transform :: <transform>)
  tr2
end method compose-transforms;

define method compose-transforms
    (tr1 :: <transform>, tr2 :: <identity-transform>) => (transform :: <transform>)
  tr1
end method compose-transforms;

define method compose-transforms
    (tr1 :: <identity-transform>, tr2 :: <identity-transform>) => (transform :: <transform>)
  $identity-transform
end method compose-transforms;

define method compose-transforms
    (tr1 :: <translation-transform>, tr2 :: <translation-transform>)
 => (transform :: <transform>)
  // NB: the translations can be integers or single floats here
  let tx = tr1.%tx + tr2.%tx;
  let ty = tr1.%ty + tr2.%ty;
  make-translation-transform(tx, ty)
end method compose-transforms;

define method compose-transforms
    (tr1 :: <integer-translation-transform>, tr2 :: <integer-translation-transform>)
 => (transform :: <transform>)
  let tx = tr1.%tx + tr2.%tx;
  let ty = tr1.%ty + tr2.%ty;
  if (zero?(tx) & zero?(ty))
    $identity-transform
  else
    make(<integer-translation-transform>, tx: tx, ty: ty)
  end
end method compose-transforms;


/// Translation composition operators

define sealed method compose-translation-with-transform
    (transform :: <identity-transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>)
  make-translation-transform(tx, ty)
end method compose-translation-with-transform;

define sealed method compose-translation-with-transform
    (transform :: <translation-transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>)
  let tx = as(<single-float>, tx);
  let ty = as(<single-float>, ty);
  if (tx = 0.0 & ty = 0.0)
    transform
  else
    let tx = tx + transform.%tx;
    let ty = ty + transform.%ty;
    if (tx = 0.0 & ty = 0.0)
      $identity-transform
    else
      make-translation-transform(tx, ty)
    end
  end
end method compose-translation-with-transform;

define sealed method compose-translation-with-transform
    (transform :: <integer-translation-transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>)
  make-translation-transform(transform.%tx + tx, transform.%ty + ty)
end method compose-translation-with-transform;

define method compose-transform-with-translation
    (transform :: <transform>, tx :: <real>, ty :: <real>)
 => (transform :: <transform>)
  compose-transforms(make-translation-transform(tx, ty), transform)
end method compose-transform-with-translation;


/// Transforming and untransforming of "spread" points

define sealed inline method transform-position
    (transform :: <identity-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x, y)
end method transform-position;

define sealed inline method transform-position
    (transform :: <translation-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x + transform.%tx, y + transform.%ty)
end method transform-position;

define sealed inline method transform-position
    (transform :: <integer-translation-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x + transform.%tx, y + transform.%ty)
end method transform-position;

define sealed inline method transform-position
    (transform :: <integer-translation-transform>, x :: <integer>, y :: <integer>)
 => (x :: <integer>, y :: <integer>)
  values(x + transform.%tx, y + transform.%ty)
end method transform-position;

define sealed inline method untransform-position
    (transform :: <identity-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x, y)
end method untransform-position;

define sealed inline method untransform-position
    (transform :: <translation-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x - transform.%tx, y - transform.%ty)
end method untransform-position;

define sealed inline method untransform-position 
    (transform :: <integer-translation-transform>, x :: <real>, y :: <real>)
 => (x :: <real>, y :: <real>)
  values(x - transform.%tx, y - transform.%ty)
end method untransform-position;

define sealed inline method untransform-position
    (transform :: <integer-translation-transform>, x :: <integer>, y :: <integer>)
 => (x :: <integer>, y :: <integer>)
  values(x - transform.%tx, y - transform.%ty)
end method untransform-position;


/// Transforming and untransforming of distances

define sealed inline method transform-distance
    (transform :: <identity-transform>, dx :: <real>, dy :: <real>)
 => (x :: <real>, y :: <real>)
  values(dx, dy)
end method transform-distance;

define sealed inline method transform-distance
    (transform :: <translation-transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>)
  values(dx, dy)
end method transform-distance;

define sealed inline method transform-distance
    (transform :: <translation-transform>, dx :: <integer>, dy :: <integer>)
 => (dx :: <integer>, dy :: <integer>)
  values(dx, dy)
end method transform-distance;

define sealed inline method untransform-distance
    (transform :: <identity-transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>)
  values(dx, dy)
end method untransform-distance;

define sealed inline method untransform-distance
    (transform :: <translation-transform>, dx :: <real>, dy :: <real>)
 => (dx :: <real>, dy :: <real>)
  values(dx, dy)
end method untransform-distance;

define sealed inline method untransform-distance
    (transform :: <translation-transform>, dx :: <integer>, dy :: <integer>)
 => (dx :: <integer>, dy :: <integer>)
  values(dx, dy)
end method untransform-distance;


/// Transforming and untransforming of angles

define sealed inline method transform-angles
    (transform :: <identity-transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>)
  values(start-angle, end-angle)
end method transform-angles;

define sealed inline method transform-angles
    (transform :: <translation-transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>)
  values(start-angle, end-angle)
end method transform-angles;

define sealed inline method untransform-angles
    (transform :: <identity-transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>)
  values(start-angle, end-angle)
end method untransform-angles;

define sealed inline method untransform-angles
    (transform :: <translation-transform>, start-angle :: <real>, end-angle :: <real>)
 => (start-angle :: <real>, end-angle :: <real>)
  values(start-angle, end-angle)
end method untransform-angles;


/// Transforming and untransforming of "spread" rectangles

define sealed method transform-box
    (transform :: <identity-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(min(x1, x2), min(y1, y2),
         max(x1, x2), max(y1, y2))
end method transform-box;

define sealed method transform-box
    (transform :: <translation-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let nx1 = x1 + transform.%tx;
  let ny1 = y1 + transform.%ty;
  let nx2 = x2 + transform.%tx;
  let ny2 = y2 + transform.%ty;
  fix-box(min(nx1, nx2), min(ny1, ny2),
	  max(nx1, nx2), max(ny1, ny2))
end method transform-box;

define sealed method transform-box
    (transform :: <integer-translation-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  let nx1 = x1 + transform.%tx;
  let ny1 = y1 + transform.%ty;
  let nx2 = x2 + transform.%tx;
  let ny2 = y2 + transform.%ty;
  values(min(nx1, nx2), min(ny1, ny2),
	 max(nx1, nx2), max(ny1, ny2))
end method transform-box;

define sealed method untransform-box
    (transform :: <identity-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(min(x1, x2), min(y1, y2),
         max(x1, x2), max(y1, y2))
end method untransform-box;

define sealed method untransform-box
    (transform :: <translation-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let nx1 = x1 - transform.%tx;
  let ny1 = y1 - transform.%ty;
  let nx2 = x2 - transform.%tx;
  let ny2 = y2 - transform.%ty;
  fix-box(min(nx1, nx2), min(ny1, ny2),
	  max(nx1, nx2), max(ny1, ny2))
end method untransform-box;

define sealed method untransform-box
    (transform :: <integer-translation-transform>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let nx1 = x1 - transform.%tx;
  let ny1 = y1 - transform.%ty;
  let nx2 = x2 - transform.%tx;
  let ny2 = y2 - transform.%ty;
  values(min(nx1, nx2), min(ny1, ny2),
	 max(nx1, nx2), max(ny1, ny2))
end method untransform-box;


// Transforms all of the coordinate pairs in the sequence.  This returns
// the original sequence if the transformation is the identity and COPY?
// is false, otherwise it returns a new vector containing the result.
define method transform-coordinate-sequence
    (transform :: <transform>, coordinates :: <vector>, #key copy?)
 => (coordinates :: <vector>)
  let length :: <integer> = size(coordinates);
  assert(even?(length),
	 "Coordinate sequences must have an even number of x/y pairs");
  if (transform == $identity-transform)
    if (copy?) copy-sequence(coordinates) else coordinates end
  else
    let result = make(<simple-vector>, size: length);
    transform-coordinates-into!(transform, coordinates, result)
  end
end method transform-coordinate-sequence;

define method transform-coordinate-sequence
    (transform :: <transform>, coordinates :: <sequence>, #key copy?)
 => (coordinates :: <vector>)
  ignore(copy?);
  let length :: <integer> = size(coordinates);
  assert(even?(length),
	 "Coordinate sequences must have an even number of x/y pairs");
  let result = as(<simple-vector>, coordinates);
  if (transform == $identity-transform)
    result
  else
    transform-coordinates-into!(transform, result, result)
  end
end method transform-coordinate-sequence;

define method transform-coordinates-into!
    (transform :: <transform>, coordinates :: <vector>, result :: <vector>)
 => (result :: <vector>)
  // Inline 'do-coordinates' for speed...
  let ncoords :: <integer> = size(coordinates);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 2,
	 until: i = ncoords)
      let x = coordinates[i];
      let y = coordinates[i + 1];
      transform-coordinates!(transform, x, y);
      result[i]     := x;
      result[i + 1] := y
    end
  end;
  result
end method transform-coordinates-into!;


/// Mutable integer translation transforms

define generic make-translation-transform-into!
    (tx :: <real>, ty :: <real>, into :: <transform>)
 => (into :: <transform>);
define generic compose-transform-into!
    (transform :: <transform>, into :: <transform>)
 => (into :: <transform>);
define generic compose-translation-into!
    (x :: <real>, y :: <real>, into :: <transform>)
 => (into :: <transform>);


define sealed class <mutable-translation-transform> (<integer-translation-transform>)
end class <mutable-translation-transform>;

define sealed domain make (singleton(<mutable-translation-transform>));
define sealed domain initialize (<mutable-translation-transform>);


define method make-translation-transform-into!
    (tx :: <real>, ty :: <real>, into :: <transform>) => (into :: <transform>)
  if (integral?(tx) & integral?(ty))
    make(<mutable-translation-transform>,
	 tx: truncate(tx), ty: truncate(ty))
  else
    make-translation-transform(tx, ty)
  end
end method make-translation-transform-into!;

define sealed method make-translation-transform-into!
    (tx :: <integer>, ty :: <integer>, into :: <mutable-translation-transform>)
 => (into :: <mutable-translation-transform>)
  into.%tx := tx;
  into.%ty := ty;
  into.%inverse := #f;
  into
end method make-translation-transform-into!;


define method compose-transform-into!
    (transform :: <transform>, into :: <transform>)
 => (into :: <transform>)
  compose-transforms(transform, into)
end method compose-transform-into!;

define method compose-transform-into!
    (transform :: <identity-transform>, into :: <transform>)
 => (into :: <transform>)
  into
end method compose-transform-into!;

define sealed method compose-transform-into!
    (transform :: <integer-translation-transform>, into :: <mutable-translation-transform>)
 => (into :: <mutable-translation-transform>)
  into.%tx := into.%tx + transform.%tx;
  into.%ty := into.%ty + transform.%ty;
  into.%inverse := #f;
  into
end method compose-transform-into!;


define method compose-translation-into!
    (tx :: <real>, ty :: <real>, into :: <transform>)
 => (into :: <transform>)
  compose-translation-with-transform(into, tx, ty)
end method compose-translation-into!;

define sealed method compose-translation-into!
    (tx :: <real>, ty :: <real>, into :: <mutable-translation-transform>)
 => (into :: <transform>)
  if (integral?(tx) & integral?(ty))
    into.%tx := into.%tx + truncate(tx);
    into.%ty := into.%ty + truncate(ty);
    into.%inverse := #f;
    into
  else
    compose-translation-with-transform(into, tx, ty)
  end
end method compose-translation-into!;
