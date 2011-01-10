Module:       Dylan-User
Synopsis:     DUIM graphics
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-graphics
  // Figure graphics
  create <coordinate-sequence>,
         <point-sequence>,
         draw-arrow, draw-arrow*,
         draw-bezier-curve, draw-bezier-curve*,
         draw-circle, draw-circle*,
         draw-ellipse, draw-ellipse*,
         draw-image, draw-image*,
         draw-line, draw-line*,
         draw-lines, draw-lines*,
         draw-oval, draw-oval*,
         draw-point, draw-point*,
         draw-points, draw-points*,
         draw-polygon, draw-polygon*,
         draw-rectangle, draw-rectangle*,
         draw-rectangles, draw-rectangles*,
         draw-rounded-rectangle, draw-rounded-rectangle*,
         draw-regular-polygon, draw-regular-polygon*,
         draw-triangle, draw-triangle*;

  // "Pixel graphics" :-)
  create set-pixel, set-pixel*,
	 set-pixels, set-pixels*;

  // Path graphics
  create abort-path,
         arc-to, arc-to*,
         clip-from-path,
         close-path,
         curve-to, curve-to*,
         end-path,
         fill-path,
         line-to, line-to*,
         move-to, move-to*,
         restore-clipping-region,
         save-clipping-region,
         start-path,
         stroke-path;

  // Text "graphics"
  create draw-text, draw-text*;

  // Pixmaps
  create <pixmap>,
         <pixmap-medium>,
         copy-area,
         copy-from-pixmap,
         copy-to-pixmap,
         destroy-pixmap,
         draw-pixmap, draw-pixmap*,
         make-pixmap,
         pixmap?,
         \with-double-buffering, do-with-double-buffering,
         \with-output-to-pixmap, do-with-output-to-pixmap;
end module duim-graphics;

define module duim-graphics-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics, export: all;

  // Pixmaps
  export <pixmap-sheet>,
         <basic-pixmap-medium>,
         do-copy-area,
         do-make-pixmap,
         make-pixmap-medium,
	 pixmap-drawable, pixmap-drawable-setter,
         pixmap-medium-pixmap;
end module duim-graphics-internals;
