Module:       Dylan-User
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-extended-geometry
  // Complex transforms
  create <reflection-underspecified>,
         <transform-underspecified>,
         make-3-point-transform, make-3-point-transform*;

  // Complex regions
  create <ellipse>,
         <elliptical-arc>,
         <line>,
         <polygon>,
         <polyline>,
         <rectangle>,
         do-polygon-coordinates,
         do-polygon-segments,
         ellipse-center-position,
         ellipse-center-point,
         ellipse-end-angle,
         ellipse-radii,
         ellipse-start-angle,
         ellipse?,
         elliptical-arc?,
         line-end-position,
         line-end-point,
         line-start-position,
         line-start-point,
         line?,
         make-ellipse, make-ellipse*,
         make-elliptical-arc, make-elliptical-arc*,
         make-line, make-line*,
         make-polygon, make-polygon*,
         make-polyline, make-polyline*,
         make-rectangle, make-rectangle*,
         polygon-coordinates,
         polygon-points,
         polygon?,
         polyline-closed?,
         polyline?,
         rectangle-edges,
         rectangle-height,
         rectangle-max-point,
         rectangle-max-position,
         rectangle-min-point,
         rectangle-min-position,
         rectangle-size,
         rectangle-width,
         rectangle?;

  // Region-based graphics
  create draw-design;
end module duim-extended-geometry;

define module duim-extended-geometry-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics-internals; 
  use duim-extended-geometry, export: all;

  // Complex transforms
  export <general-transform>;

  // Complex regions
  export <standard-ellipse>,
         <standard-elliptical-arc>,
         <standard-line>,
         <standard-polygon>,
         <standard-polyline>,
         <standard-rectangle>;
end module duim-extended-geometry-internals;
