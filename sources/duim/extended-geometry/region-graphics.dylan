Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Region class-based graphics

define open generic draw-design
    (drawable :: <drawable>, design :: <region>) => (record);


/// Easy cases

define sealed method draw-design
    (sheet :: <sheet>, region :: <everywhere>) => (record)
  // Just fill with the foreground color
  let (left, top, right, bottom) = box-edges(sheet-region(sheet));
  with-drawing-options (sheet, brush: $foreground)
    draw-rectangle(sheet, left, top, right, bottom, filled?: #t)
  end
end method draw-design;

define sealed method draw-design
    (medium :: <medium>, region :: <everywhere>) => (record)
  let sheet = medium-sheet(medium);
  let (left, top, right, bottom) = box-edges(sheet-region(sheet));
  with-drawing-options (medium, brush: $foreground)
    draw-rectangle(medium, left, top, right, bottom, filled?: #t)
  end
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, region :: <nowhere>) => (record)
  #f
end method draw-design;


/// Composite regions

define sealed method draw-design
    (drawable :: <drawable>, region :: <region-union>) => (record)
  do-regions(method (region) draw-design(region, drawable) end,
	     region)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, region :: <region-intersection>) => (record)
  //---*** Should draw the intersection, but I dunno how to do that in general
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, region :: <region-difference>) => (record)
  //---*** Should draw the difference, but I dunno how to do that in general
end method draw-design;


/// Simple figures

define sealed method draw-design
    (drawable :: <drawable>, point :: <standard-point>) => (record)
  let (x, y) = point-position(point);
  draw-point(drawable, x, y)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, line :: <standard-line>) => (record)
  let (x1, y1) = line-start-position(line);
  let (x2, y2) = line-end-position(line);
  draw-line(drawable, x1, y1, x2, y2)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, rectangle :: <standard-rectangle>) => (record)
  let (x1, y1, x2, y2) = rectangle-edges(rectangle);
  draw-rectangle(drawable, x1, y1, x2, y2, filled?: #t)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, polygon :: <standard-polygon>) => (record)
  let coords = polygon-coordinates(polygon);
  draw-polygon(drawable, coords, closed?: #t, filled?: #t)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, polyline :: <standard-polyline>) => (record)
  let coords = polygon-coordinates(polyline);
  draw-polygon(drawable, coords, closed?: #f, filled?: #f)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, ellipse :: <standard-ellipse>) => (record)
  let (center-x, center-y) = ellipse-center-position(ellipse);
  let (radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy) = ellipse-radii(ellipse);
  draw-ellipse(drawable,
	       center-x, center-y,
	       radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
	       start-angle: ellipse-start-angle(ellipse),
	       end-angle: ellipse-end-angle(ellipse),
	       filled?: #t)
end method draw-design;

define sealed method draw-design
    (drawable :: <drawable>, arc :: <standard-elliptical-arc>) => (record)
  let (center-x, center-y) = ellipse-center-position(arc);
  let (radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy) = ellipse-radii(arc);
  draw-ellipse(drawable,
	       center-x, center-y,
	       radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
	       start-angle: ellipse-start-angle(arc),
	       end-angle: ellipse-end-angle(arc),
	       filled?: #f)
end method draw-design;
