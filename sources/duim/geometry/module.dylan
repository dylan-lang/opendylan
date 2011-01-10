Module:       Dylan-User
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-geometry
  // Coordinates
  create $largest-coordinate,
         $smallest-coordinate,
         do-coordinates,
         do-endpoint-coordinates,
         \fix-coordinate;

  // Boxes
  create <bounding-box>,
         bounding-box,
         bounding-box?,
         box-edges, set-box-edges, 
         box-position, set-box-position,
         box-size, set-box-size,
         box-left,
         box-top,
         box-right,
         box-bottom,
         box-width,
         box-height,
         make-bounding-box;

  // Simple regions
  create $everywhere,
         $nowhere,
         <area>,
         <path>,
         <point>,
         <region-set>,
         <region>,
         <standard-point>,
         area?,
         do-regions,
         make-point,
         path?,
         point-position,
         point-x,
         point-y,
         point?,
         region-contains-position?,
         region-contains-region?,
         region-difference,
         region-empty?,
         region-equal,
         region-intersection,
         region-intersects-region?,
         region-set-function,
         region-set-regions,
         region-set?,
         region-union,
         region?,
         transform-region,
         untransform-region;

  // Transforms
  create $identity-transform,
         <singular-transform>,
         <transform-error>,
         <transform>,
         compose-rotation-with-transform,
         compose-scaling-with-transform,
         compose-transform-with-rotation,
         compose-transform-with-scaling,
         compose-transform-with-translation,
         compose-transforms,
         compose-translation-with-transform,
         even-scaling-transform?,
         identity-transform?,
         invert-transform,
         invertible-transform?,
         make-reflection-transform, make-reflection-transform*,
         make-rotation-transform, make-rotation-transform*,
         make-scaling-transform, make-scaling-transform*,
         make-transform,
         make-translation-transform,
         rectilinear-transform?,
         reflection-transform?,
         rigid-transform?,
         scaling-transform?,
         transform-angles,
         transform-box,
         \transform-coordinates!,
         transform-coordinate-sequence,
         transform-distance,
         \transform-distances!,
         transform-equal,
         transform-position,
         transform?,
         translation-transform?,
         untransform-angles,
         untransform-box,
         untransform-distance,
         untransform-position;
end module duim-geometry;

define module duim-geometry-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry, export: all;

  // Coordinates
  export \convert-to-device-coordinates!,
         \convert-to-device-distances!,
	 fix-box,
         \fix-coordinates!,
         spread-point-sequence,
         \translate-coordinates!,
         translate-coordinate-sequence!,
	 \with-device-coordinates,
         \with-device-distances;

  // Boxes
  export <general-box>,
         <simple-box>,
	 box-center, box-center*,
         box-invalidated?,
         invalidate-box!,
         box-edges-equal,
         box-position-difference,
         box-positions-equal,
         box-sizes-equal,
         position-difference,
         shift-box-position;

  // LTRBs
  export ltrb-contains-ltrb?,
         ltrb-contains-position?,
         ltrb-difference,
         ltrb-equals-ltrb?,
         ltrb-intersection,
         ltrb-intersects-ltrb?,
         ltrb-size-equal?,
         ltrb-union,
         ltrb-well-formed?;

  // Random geometry hacking
  export radians->degrees,
         degrees->radians,
         position-close-to-line?,
         position-inside-polygon?,
         position-inside-ellipse?,
         position-on-thick-ellipse?,
         singular-value-decomposition-2x2,
         coordinate-sequence-box,
         elliptical-arc-box,
         angle-between-angles?;

  // Simple regions
  export <everywhere>,
         <nowhere>,
	 <region-difference>,
	 <region-intersection>,
	 <region-union>,
         everywhere?,
         point-x-setter,
         point-y-setter,
         transform-region!,
         untransform-region!;

  // Transforms
  export <integer-translation-transform>,
         <float-translation-transform>,
         <identity-transform>,
         <translation-transform>,
         transform-components;

  // Mutable translation transforms
  export <mutable-translation-transform>,
         compose-transform-into!,
         compose-translation-into!,
         make-translation-transform-into!;
end module duim-geometry-internals;
