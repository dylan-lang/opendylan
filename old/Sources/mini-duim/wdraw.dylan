Module:    win32-duim
Synopsis:  Win32 drawing implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Win32 graphics

define method do-draw-point*
    (medium :: <win32-medium>, x, y) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, x, y);
    SetPixel(hDC, x, y, medium.%brush-color)
  end
end method do-draw-point*;

define method do-draw-points*
    (medium :: <win32-medium>, coord-seq) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    do-coordinates
      (method (x, y)
	 convert-to-device-coordinates!(transform, x, y);
	 SetPixel(hDC, x, y, medium.%brush-color)
       end,
       coord-seq)
  end
end method do-draw-points*;

define method do-draw-line*
    (medium :: <win32-medium>, x1, y1, x2, y2) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, x1, y1, x2, y2);
    MoveToEx(hDC, x1, y1, $NULL-POINT);
    LineTo(hDC, x2, y2)
  end
end method do-draw-line*;

define method do-draw-lines* (medium :: <win32-medium>, coord-seq) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    //--- Surely there's a better way to do this
    do-endpoint-coordinates
      (method (x1, y1, x2, y2)
	 convert-to-device-coordinates!(transform, x1, y1, x2, y2);
	 MoveToEx(hDC, x1, y1, $NULL-POINT);
	 LineTo(hDC, x2, y2)
       end,
       coord-seq)
  end
end method do-draw-lines*;

define method do-draw-rectangle*
    (medium :: <win32-medium>, left, top, right, bottom, #key filled? = #t) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, left, top, right, bottom);
    let old-object :: <HANDLE> =
      SelectObject(hDC, GetStockObject(if (filled?) $NULL-PEN else $NULL-BRUSH end));
    Rectangle(hDC, left, top, right, bottom);
    SelectObject(hDC, old-object)
  end
end method do-draw-rectangle*;

define method do-draw-rectangles*
    (medium :: <win32-medium>, coord-seq, #key filled? = #t) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    let old-object :: <HANDLE> =
      SelectObject(hDC, GetStockObject(if (filled?) $NULL-PEN else $NULL-BRUSH end));
    do-endpoint-coordinates
      (method (left, top, right, bottom)
	 convert-to-device-coordinates!(transform, left, top, right, bottom);
	 Rectangle(hDC, left, top, right, bottom);
       end,
       coord-seq);
    SelectObject(hDC, old-object)
  end
end method do-draw-rectangles*;

define method do-draw-rounded-rectangle*
    (medium :: <win32-medium>, left, top, right, bottom,
     #key filled? = #t, radius) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, left, top, right, bottom);
    unless (radius)
      let width  = right - left;
      let height = bottom - top;
      radius := max(floor/(min(width, height), 3), 2)
    end;
    let old-object :: <HANDLE> =
      SelectObject(hDC, GetStockObject(if (filled?) $NULL-PEN else $NULL-BRUSH end));
    RoundRect(hDC, left, top, right, bottom, radius, radius);
    SelectObject(hDC, old-object)
  end
end method do-draw-rounded-rectangle*;

define method do-draw-polygon*
    (medium :: <win32-medium>, coord-seq, #key closed? = #t, filled? = #t) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    let ncoords = floor/(size(coord-seq), 2);
    let npoints = if (closed? & ~filled?) ncoords + 1 else ncoords end;
    let old-object :: <HANDLE> =
      SelectObject(hDC, GetStockObject(if (filled?) $NULL-PEN else $NULL-BRUSH end));
    with-stack-structure (c-points :: <PPOINT>, element-count: npoints)
      let i = 0;
      for (j from 0 below ncoords)
	let x = coord-seq[i + 0];
	let y = coord-seq[i + 1];
	convert-to-device-coordinates!(transform, x, y);
	//---*** Should be...
	//---*** c-points[j].x-value := x;
	//---*** c-points[j].y-value := y;
 	pointer-value(c-points, index: j).x-value := x;
	pointer-value(c-points, index: j).y-value := y;
	i := i + 2
      finally
	if (closed? & ~filled?)
	  let x = coord-seq[0];
	  let y = coord-seq[1];
	  convert-to-device-coordinates!(transform, x, y);
	  //---*** Should be...
	  //---*** c-points[npoints - 1].x-value := x;
	  //---*** c-points[npoints - 1].y-value := y
	  pointer-value(c-points, index: npoints - 1).x-value := x;
	  pointer-value(c-points, index: npoints - 1).y-value := y
	end
      end;
      if (filled?)
	Polygon(hDC, c-points, npoints)
      else
	Polyline(hDC, c-points, npoints)
      end
    end;
    SelectObject(hDC, old-object)
  end
end method do-draw-polygon*;

define method do-draw-ellipse*
    (medium :: <win32-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, center-x, center-y);
    convert-to-device-distances!(transform,
				 radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy);
    let (x-radius, y-radius)
      = case
	  radius-1-dx = 0 & radius-2-dy = 0 =>
	    values(abs(radius-2-dx), abs(radius-1-dy));
	  radius-2-dx = 0 & radius-1-dy = 0 =>
	    values(abs(radius-1-dx), abs(radius-2-dy));
	  otherwise =>
	    not-yet-implemented("Tilted ellipses");
	end;
    let (left, top, right, bottom)
      = elliptical-arc-box(center-x, center-y,
			   radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy);
    let old-object :: <HANDLE> =
      SelectObject(hDC, GetStockObject(if (filled?) $NULL-PEN else $NULL-BRUSH end));
    if (start-angle | end-angle)
      //---*** What angle conventions does Windows use?
      start-angle := start-angle | 0.0;
      end-angle   := end-angle | $2pi;
      if (end-angle < start-angle)
	end-angle := end-angle + $2pi
      end;
      let (rx1, ry1) = values(cos(start-angle), sin(start-angle));
      let (rx2, ry2) = values(cos(end-angle),   sin(end-angle));
      Pie(hDC, left, top, right, bottom, rx1, ry1, rx2, ry2)
    else
      Ellipse(hDC, left, top, right, bottom)
    end;
    SelectObject(hDC, old-object)
  end
end method do-draw-ellipse*;

define method do-draw-image*
    (medium :: <win32-medium>, image :: <image>, x, y) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, x, y);
    let width  = image-width(image);
    let height = image-height(image);
    let hDC = get-DC(medium);
    if (hDC)
      let (pixel, fill-style, operation, pattern)
	= convert-ink-to-DC-components(medium, hDC, image);
      //---*** draw the image, but for now draw a rectangle
      let old-object :: <HANDLE> =
	SelectObject(hDC, GetStockObject($NULL-PEN));
      Rectangle(hDC, x, y, x + width, y + height);
      SelectObject(hDC, old-object)
    end
  end
end method do-draw-image*;


/// Path graphics

define method do-start-path (medium :: <win32-medium>) => ()
  let hDC :: <HDC> = get-DC(medium);
  BeginPath(hDC)
end method do-start-path;

define method do-end-path (medium :: <win32-medium>) => ()
  let hDC :: <HDC> = get-DC(medium);
  EndPath(hDC)
end method do-end-path;

define method do-abort-path (medium :: <win32-medium>) => ()
  let hDC :: <HDC> = get-DC(medium);
  AbortPath(hDC)
end method do-abort-path;

define method do-close-path (medium :: <win32-medium>) => ()
  let hDC :: <HDC> = get-DC(medium);
  CloseFigure(hDC)
end method do-close-path;

define method do-stroke-path (medium :: <win32-medium>, #key filled?) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    if (filled?)
      StrokeAndFillPath(hDC)
    else
      StrokePath(hDC)
    end
  end
end method do-stroke-path;

define method do-fill-path (medium :: <win32-medium>) => ()
  let hDC = update-drawing-state(medium);
  if (hDC)
    FillPath(hDC)
  end
end method do-fill-path;

define method do-clip-from-path
    (medium :: <win32-medium>, #key function = $boole-and) => ()
  let hDC :: <HDC> = get-DC(medium);
  let mode = select (function)
	       $boole-and   => $RGN-AND;
	       $boole-set   => $RGN-COPY;
	       $boole-ior   => $RGN-OR;
	       $boole-xor   => $RGN-XOR;
	       $boole-andc2 => $RGN-DIFF;
	     end;
  SelectClipPath(hDC, mode)
end method do-clip-from-path;

define method do-save-clipping-region (medium :: <win32-medium>) => ()
  //---*** push the clip region
end method do-save-clipping-region;

define method do-restore-clipping-region (medium :: <win32-medium>) => ()
  //---*** pop the clip region
end method do-restore-clipping-region;

define method do-move-to* (medium :: <win32-medium>, x, y) => ()
  let hDC :: <HDC> = get-DC(medium);
  let transform = sheet-device-transform(medium-sheet(medium));
  convert-to-device-coordinates!(transform, x, y);
  MoveToEx(hDC, x, y, $NULL-POINT)
end method do-move-to*;

define method do-line-to* (medium :: <win32-medium>, x, y) => ()
  let hDC :: <HDC> = get-DC(medium);
  let transform = sheet-device-transform(medium-sheet(medium));
  convert-to-device-coordinates!(transform, x, y);
  LineTo(hDC, x, y)
end method do-line-to*;

define method do-arc-to* (medium :: <win32-medium>, center-x, center-y,
			  radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
			  #key start-angle, end-angle) => ()
  let hDC :: <HDC> = get-DC(medium);
  let transform = sheet-device-transform(medium-sheet(medium));
  convert-to-device-coordinates!(transform, center-x, center-y);
  convert-to-device-distances!(transform,
			       radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy);
  let (x-radius, y-radius)
    = case
	radius-1-dx = 0 & radius-2-dy = 0 =>
	  values(abs(radius-2-dx), abs(radius-1-dy));
	radius-2-dx = 0 & radius-1-dy = 0 =>
	  values(abs(radius-1-dx), abs(radius-2-dy));
	otherwise =>
	  not-yet-implemented("Tilted ellipses");
      end;
  let (left, top, right, bottom)
    = elliptical-arc-box(center-x, center-y,
			 radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy);
  //---*** What angle conventions does Windows use?
  start-angle := start-angle | 0.0;
  end-angle   := end-angle | $2pi;
  if (end-angle < start-angle)
    end-angle := end-angle + $2pi
  end;
  let (rx1, ry1) = values(cos(start-angle), sin(start-angle));
  let (rx2, ry2) = values(cos(end-angle),   sin(end-angle));
  ArcTo(hDC, left, top, right, bottom, rx1, ry1, rx2, ry2)
end method do-arc-to*;

define method do-curve-to* (medium :: <win32-medium>, x1, y1, x2, y2, x3, y3) => ()
  let hDC :: <HDC> = get-DC(medium);
  let transform = sheet-device-transform(medium-sheet(medium));
  convert-to-device-coordinates!(transform, x1, y1, x2, y2, x3, y3);
  with-stack-structure (c-points :: <PPOINT>, element-count: 3)
    //---*** Should be...
    //---*** c-points[i].x-value := x;
    //---*** c-points[i].y-value := y;
    pointer-value(c-points, index: 0).x-value := x1;
    pointer-value(c-points, index: 0).y-value := y1;
    pointer-value(c-points, index: 1).x-value := x2;
    pointer-value(c-points, index: 1).y-value := y2;
    pointer-value(c-points, index: 2).x-value := x3;
    pointer-value(c-points, index: 2).y-value := y3;
    PolyBezierTo(hDC, c-points, 3)
  end
end method do-curve-to*;


/// 'draw-pixmap'

define method do-draw-pixmap*
    (medium :: <win32-medium>, pixmap :: <pixmap>, x, y,
     #key function = $boole-1) => ()
  // Coordinates will get transformed in 'copy-area'
  //---*** do-copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
  //---***              medium, x, y, function)
  //---*** for now just draw a rectangle
  let hDC = update-drawing-state(medium);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, x, y);
    let width  = image-width(pixmap);
    let height = image-height(pixmap);
    let old-object :: <HANDLE> =
      SelectObject(hDC, GetStockObject($NULL-PEN));
    Rectangle(hDC, x, y, x + width, y + height);
    SelectObject(hDC, old-object)
  end
end method do-draw-pixmap*;

define method clear-box*
    (medium :: <win32-medium>, left, top, right, bottom) => ()
  let hDC = get-DC(medium);
  if (hDC)
   //---*** Wrong -- this should set it to the background color
    let old-brush :: <HANDLE> =
      SelectObject(hDC, GetStockObject($WHITE-BRUSH));
    let old-pen :: <HANDLE> =
      SelectObject(hDC, GetStockObject($NULL-PEN));
    Rectangle(hDC, left, top, right, bottom);
    SelectObject(hDC, old-brush);
    SelectObject(hDC, old-pen)
  end
end method clear-box*;


/// Text drawing

define method do-draw-text*
    (medium :: <win32-medium>, string :: <string>, x, y,
     #key start: _start = 0, end: _end = size(string),
          align-x = #"left", align-y = #"baseline",
          towards-x, towards-y, transform-glyphs?) => ()
  let text-style :: <text-style> = medium-text-style(medium);
  let font :: <win32-font> = text-style-mapping(port(medium), text-style);
  let hDC = update-drawing-state(medium, font: font);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, x, y);
    if (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    //--- There is probably a better Windows-specific way to do this adjustment
    //--- See the function SetTextAlign.
    let (x-adjust, y-adjust)
      = compute-text-adjustment(medium, string, text-style, align-x, align-y,
				start: _start, end: _end);
    inc!(x, x-adjust);
    inc!(y, y-adjust);
    if (towards-x & towards-y)
      inc!(towards-x, x-adjust);
      inc!(towards-y, y-adjust)
    end;
    with-c-string (c-string = string, start: _start, end: _end)
      TextOut(hDC, x, y, c-string, _end - _start);
    end
  end
end method do-draw-text*;

define method do-draw-text*
    (medium :: <win32-medium>, character :: <character>, x, y,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline",
          towards-x, towards-y, transform-glyphs?) => ()
  ignore(_start, _end);
  let text-style :: <text-style> = medium-text-style(medium);
  let font :: <win32-font> = text-style-mapping(port(medium), text-style);
  let hDC = update-drawing-state(medium, font: font);
  if (hDC)
    let transform = sheet-device-transform(medium-sheet(medium));
    convert-to-device-coordinates!(transform, x, y);
    if (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    let (x-adjust, y-adjust)
      = compute-text-adjustment(medium, character, text-style, align-x, align-y);
    inc!(x, x-adjust);
    inc!(y, y-adjust);
    if (towards-x & towards-y)
      inc!(towards-x, x-adjust);
      inc!(towards-y, y-adjust)
    end;
    //---*** draw the character
  end
end method do-draw-text*;
