Module:    win32-duim
Synopsis:  Win32 drawing implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 graphics

define variable *rectangle-fudge-factor* :: <integer> = 0;

define method initialize-graphics
    (_port :: <win32-port>) => ()
  // For some reason, rectangles on NT are one pixel shorter than on Windows...
  *rectangle-fudge-factor* := if (_port.%os-name == #"Windows-NT") 1 else 0 end
end method initialize-graphics;

define macro with-temporary-gdi-object
  { with-temporary-gdi-object (?hDC:name = ?object:expression) ?body:body end }
    => { begin
           let object :: <HGDIOBJ> = ?object;
           let old-object = SelectObject(?hDC, object);
           ?body;
           SelectObject(?hDC, old-object)
         end }
end macro with-temporary-gdi-object;

define macro with-fill-selected
  { with-fill-selected (?hDC:name, ?filled?:expression) ?body:body end }
    => { with-temporary-gdi-object (?hDC = if (?filled?) $null-hpen else $null-hbrush end)
           ?body  
         end }
end macro with-fill-selected;


/// Figure graphics

define sealed method draw-point
    (medium :: <win32-medium>, x, y) => (record)
  let hDC :: <HDC> = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let thickness = pen-width(medium-pen(medium));
    if (thickness < 2)
      SetPixel(hDC, x, y, medium.%brush-color)
    else 
      let thickness/2 = truncate/(thickness, 2);
      Ellipse(hDC,
              x - thickness/2, y - thickness/2, 
              x + thickness/2, y + thickness/2)
    end
  end;
  #f
end method draw-point;

define sealed method draw-points
    (medium :: <win32-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let hDC :: <HDC> = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  let thickness = pen-width(medium-pen(medium));
  if (thickness < 2)
    do-coordinates
      (method (x, y)
         with-device-coordinates (transform, x, y)
           SetPixel(hDC, x, y, medium.%brush-color)
         end
       end,
       coord-seq)
  else
    let thickness/2 = truncate/(thickness, 2);
    do-coordinates
      (method (x, y)
         with-device-coordinates (transform, x, y)
           Ellipse(hDC,
                   x - thickness/2, y - thickness/2,
                   x + thickness/2, y + thickness/2)
         end
       end,
       coord-seq)
  end;
  #f
end method draw-points;

define sealed method draw-line
    (medium :: <win32-medium>, x1, y1, x2, y2) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    MoveToEx(hDC, x1, y1, $NULL-POINT);
    LineTo(hDC, x2, y2)
  end;
  #f
end method draw-line;

define sealed method draw-lines
    (medium :: <win32-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: medium-pen(medium));
  let transform = medium-device-transform(medium);
  //---*** Use PolyPolyLine
  do-endpoint-coordinates
    (method (x1, y1, x2, y2)
       with-device-coordinates (transform, x1, y1, x2, y2)
         MoveToEx(hDC, x1, y1, $NULL-POINT);
         LineTo(hDC, x2, y2)
       end
     end,
     coord-seq);
  #f
end method draw-lines;

define sealed method draw-rectangle
    (medium :: <win32-medium>, x1, y1, x2, y2,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    let hDC :: <HDC> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    let fudge-factor = *rectangle-fudge-factor*;
    with-device-coordinates (transform, x1, y1, x2, y2)
      with-fill-selected (hDC, filled?)
        Rectangle(hDC, x1, y1, x2 + fudge-factor, y2 + fudge-factor)
      end
    end
  end;
  #f
end method draw-rectangle;

define sealed method draw-rectangles
    (medium :: <win32-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    draw-transformed-rectangles(medium, coord-seq, filled?: filled?)
  else
    let hDC :: <HDC> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
    let transform = medium-device-transform(medium);
    let fudge-factor = *rectangle-fudge-factor*;
    with-fill-selected (hDC, filled?)
      //---*** Use PolyPolyLine
      do-endpoint-coordinates
        (method (x1, y1, x2, y2)
           with-device-coordinates (transform, x1, y1, x2, y2)
             Rectangle(hDC, x1, y1, x2 + fudge-factor, y2 + fudge-factor)
           end
         end,
         coord-seq)
    end
  end;
  #f
end method draw-rectangles;

define sealed method draw-transformed-rectangles
    (medium :: <win32-medium>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let ncoords :: <integer> = size(coord-seq);
  assert(zero?(modulo(ncoords, 4)),
         "The coordinate sequence has the wrong number of elements");
  local method draw-one (x1, y1, x2, y2) => ()
          with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
            apply(draw-polygon, medium, coords, closed?: #t, keys)
          end
        end method;
  dynamic-extent(draw-one);
  without-bounds-checks
    for (i :: <integer> = 0 then i + 4, until: i = ncoords)
      draw-one(coord-seq[i + 0], coord-seq[i + 1],
               coord-seq[i + 2], coord-seq[i + 3])
    end
  end;
  #f
end method draw-transformed-rectangles;

define sealed method draw-rounded-rectangle
    (medium :: <win32-medium>, x1, y1, x2, y2,
     #key filled? = #t, radius) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    unless (radius)
      let width  = x2 - x1;
      let height = y2 - y1;
      radius := max(truncate/(min(width, height), 3), 2)
    end;
    with-fill-selected (hDC, filled?)
      RoundRect(hDC, x1, y1, x2, y2, radius, radius)
    end
  end;
  #f
end method draw-rounded-rectangle;

define sealed method draw-polygon
    (medium :: <win32-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  let ncoords :: <integer> = truncate/(size(coord-seq), 2);
  let npoints :: <integer> = if (closed? & ~filled?) ncoords + 1 else ncoords end;
  with-fill-selected (hDC, filled?)
    with-stack-structure (c-points :: <LPPOINT>, element-count: npoints)
      without-bounds-checks
        let i :: <integer> = 0;
        for (j :: <integer> from 0 below ncoords)
          let x = coord-seq[i + 0];
          let y = coord-seq[i + 1];
          with-device-coordinates (transform, x, y)
            //---*** Should be (in some later version of Webster)...
            //---*** c-points[j].x-value := x;
            //---*** c-points[j].y-value := y;
            pointer-value-address(c-points, index: j).x-value := x;
            pointer-value-address(c-points, index: j).y-value := y;
            i := i + 2
          end
        finally
          when (closed? & ~filled?)
            let x = coord-seq[0];
            let y = coord-seq[1];
            with-device-coordinates (transform, x, y)
              //---*** Should be (in some later version of Webster)...
              //---*** c-points[npoints - 1].x-value := x;
              //---*** c-points[npoints - 1].y-value := y
              pointer-value-address(c-points, index: npoints - 1).x-value := x;
              pointer-value-address(c-points, index: npoints - 1).y-value := y
            end
          end
        end
      end;
      if (filled?)
        Polygon(hDC, c-points, npoints)
      else
        Polyline(hDC, c-points, npoints)
      end
    end
  end;
  #f
end method draw-polygon;

define sealed method draw-triangle
    (medium :: <win32-medium>, x1, y1, x2, y2, x3, y3,
     #key filled? = #t) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  let ncoords :: <integer> = 3;
  let npoints :: <integer> = if (~filled?) ncoords + 1 else ncoords end;
  with-device-coordinates (transform, x1, y1, x2, y2, x3, y3)
    with-fill-selected (hDC, filled?)
      with-stack-structure (c-points :: <LPPOINT>, element-count: npoints)
        pointer-value-address(c-points, index: 0).x-value := x1;
        pointer-value-address(c-points, index: 0).y-value := y1;
        pointer-value-address(c-points, index: 1).x-value := x2;
        pointer-value-address(c-points, index: 1).y-value := y2;
        pointer-value-address(c-points, index: 2).x-value := x3;
        pointer-value-address(c-points, index: 2).y-value := y3;
        when (~filled?)
          pointer-value-address(c-points, index: 3).x-value := x1;
          pointer-value-address(c-points, index: 3).y-value := y1;
        end;
        if (filled?)
          Polygon(hDC, c-points, npoints)
        else
          Polyline(hDC, c-points, npoints)
        end
      end
    end
  end;
  #f
end method draw-triangle;

define sealed method draw-ellipse
    (medium :: <win32-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: ~filled? & medium-pen(medium));
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      let (angle-2, x-radius, y-radius, angle-1)
        = singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
      with-fill-selected (hDC, filled?)
        if (#t                                  //---*** remove when tilted ellipses work
            | x-radius = abs(y-radius)          // a circle - rotations are irrelevant
            | zero?(angle-1))                   // axis-aligned ellipse
          x-radius := abs(x-radius);
          y-radius := abs(y-radius);
          let (left, top, right, bottom)
            = elliptical-arc-box(center-x, center-y,
                                 x-radius, 0, 0, y-radius,
                                 thickness: pen-width(medium-pen(medium)));
          if (start-angle & end-angle)
            let start-x = truncate(center-x +  x-radius * cos(end-angle));
            let start-y = truncate(center-y +  y-radius * sin(end-angle));
            let end-x   = truncate(center-x +  x-radius * cos(start-angle));
            let end-y   = truncate(center-y +  y-radius * sin(start-angle));
            let dx = start-x - end-x;
            let dy = start-y - end-y;
            if (filled?)
              Pie(hDC, left, top, right, bottom, start-x, start-y, end-x, end-y)
            else
              Arc(hDC, left, top, right, bottom, start-x, start-y, end-x, end-y)
            end
          else
            Ellipse(hDC, left, top, right, bottom)
          end
        else
          #f                                    //---*** do tilted ellipses here
        end
      end
    end
  end;
  #f
end method draw-ellipse;


/// Pixel graphics

define sealed method set-pixel
    (medium :: <win32-medium>, color :: <rgb-color>, x, y) => (record)
  let hDC :: <HDC> = get-DC(medium);
  let transform = medium-device-transform(medium);
  let color     = %color->native-color(color);
  with-device-coordinates (transform, x, y)
    SetPixel(hDC, x, y, color)
  end;
  #f
end method set-pixel;

define sealed method set-pixels
    (medium :: <win32-medium>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  let hDC :: <HDC> = update-drawing-state(medium);
  let transform = medium-device-transform(medium);
  let color     = %color->native-color(color);
  do-coordinates
    (method (x, y)
       with-device-coordinates (transform, x, y)
         SetPixel(hDC, x, y, color)
       end
     end,
     coord-seq);
  #f
end method set-pixels;


/// Pixmap graphics

// Win32 bitmaps and icons are handled separately
define sealed method draw-image
    (medium :: <win32-medium>, image :: <stencil>, x, y) => (record)
  let hDC :: <HDC> = update-drawing-state(medium);
  let width  = image-width(image);
  let height = image-height(image);
  let hDC    = get-DC(medium);
  let cache  = medium.%ink-cache;
  let pixmap
    = gethash(cache, image)
      | begin
          // Decode the pattern into a pixmap...
          let (array, colors) = decode-pattern(image);
          let ncolors :: <integer> = size(colors);
          let pixels  :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
          without-bounds-checks
            for (n :: <integer> from 0 below ncolors)
              let pixel = convert-ink-to-DC-components(medium, hDC, colors[n]);
              pixels[n] := pixel
            end;
            let pixmap
              = with-output-to-pixmap(medium, width: width, height: height)
                  let hDC = get-DC(medium);     // get the DC for the pixmap medium
                  for (y :: <integer> from 0 below height)
                    for (x :: <integer> from 0 below width)
                      SetPixel(hDC, x, y, pixels[array[y,x]])
                    end
                  end
                end;
            gethash(cache, image) := pixmap;
            pixmap
          end
        end;
  do-copy-area(pixmap, 0, 0, width, height,
               medium, x, y)
end method draw-image;


/// Path graphics

define sealed method start-path
    (medium :: <win32-medium>) => (record)
  let hDC :: <HDC> = get-DC(medium);
  BeginPath(hDC);
  #f
end method start-path;

define sealed method end-path
    (medium :: <win32-medium>) => (record)
  let hDC :: <HDC> = get-DC(medium);
  EndPath(hDC);
  #f
end method end-path;

define sealed method abort-path
    (medium :: <win32-medium>) => (record)
  let hDC :: <HDC> = get-DC(medium);
  AbortPath(hDC);
  #f
end method abort-path;

define sealed method close-path
    (medium :: <win32-medium>) => (record)
  let hDC :: <HDC> = get-DC(medium);
  CloseFigure(hDC);
  #f
end method close-path;

define sealed method stroke-path
    (medium :: <win32-medium>, #key filled?) => (record)
  let hDC :: <HDC> = update-drawing-state(medium, pen: medium-pen(medium));
  if (filled?)
    StrokeAndFillPath(hDC)
  else
    StrokePath(hDC)
  end;
  #f
end method stroke-path;

define sealed method fill-path
    (medium :: <win32-medium>) => (record)
  let hDC :: <HDC> = update-drawing-state(medium);
  FillPath(hDC);
  #f
end method fill-path;

define sealed method clip-from-path
    (medium :: <win32-medium>, #key function = $boole-and) => (record)
  let hDC :: <HDC> = get-DC(medium);
  let mode = select (function)
               $boole-and   => $RGN-AND;
               $boole-set   => $RGN-COPY;
               $boole-ior   => $RGN-OR;
               $boole-xor   => $RGN-XOR;
               $boole-andc2 => $RGN-DIFF;
             end;
  SelectClipPath(hDC, mode);
  #f
end method clip-from-path;

define sealed method save-clipping-region
    (medium :: <win32-medium>) => (record)
  //---*** Push the clipping region
  #f
end method save-clipping-region;

define sealed method restore-clipping-region
    (medium :: <win32-medium>) => (record)
  //---*** Pop the clipping region
  #f
end method restore-clipping-region;

define sealed method move-to
    (medium :: <win32-medium>, x, y) => (record)
  let hDC :: <HDC> = get-DC(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    MoveToEx(hDC, x, y, $NULL-POINT)
  end;
  #f
end method move-to;

define sealed method line-to
    (medium :: <win32-medium>, x, y) => (record)
  let hDC :: <HDC> = get-DC(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    LineTo(hDC, x, y)
  end;
  #f
end method line-to;

define sealed method arc-to
    (medium :: <win32-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle) => (record)
  let hDC :: <HDC> = get-DC(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
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
      when (end-angle < start-angle)
        end-angle := end-angle + $2pi
      end;
      let (rx1, ry1) = values(cos(start-angle), sin(start-angle));
      let (rx2, ry2) = values(cos(end-angle),   sin(end-angle));
      ArcTo(hDC, left, top, right, bottom, rx1, ry1, rx2, ry2)
    end
  end;
  #f
end method arc-to;

define sealed method curve-to
    (medium :: <win32-medium>, x1, y1, x2, y2, x3, y3) => (record)
  let hDC :: <HDC> = get-DC(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2, x3, y3)
    with-stack-structure (c-points :: <LPPOINT>, element-count: 3)
      //---*** Should be (in some later version of Webster)...
      //---*** c-points[i].x-value := x;
      //---*** c-points[i].y-value := y;
      pointer-value-address(c-points, index: 0).x-value := x1;
      pointer-value-address(c-points, index: 0).y-value := y1;
      pointer-value-address(c-points, index: 1).x-value := x2;
      pointer-value-address(c-points, index: 1).y-value := y2;
      pointer-value-address(c-points, index: 2).x-value := x3;
      pointer-value-address(c-points, index: 2).y-value := y3;
      PolyBezierTo(hDC, c-points, 3)
    end
  end;
  #f
end method curve-to;


/// 'clear-box'

/*---*** This doesn't work... let's just use the default method for now,
//---*** which uses draw-rectangle using the background brush.
define sealed method clear-box
    (medium :: <win32-medium>, left,  top, right, bottom) => ()
  let hDC = get-DC(medium);
  let sheet = medium-sheet(medium);
  let transform = sheet-device-transform(sheet);
  with-device-coordinates (transform, left, top, right, bottom)
    //---*** Wrong -- only do this for 3d canvases
    with-temporary-gdi-object (hDC = as(<HBRUSH>, $COLOR-3DFACE))
      with-temporary-gdi-object (hDC = $null-hpen)
        let fudge-factor = *rectangle-fudge-factor*;
        Rectangle(hDC, left, top, right + fudge-factor, bottom + fudge-factor);
      end
    end
  end
end method clear-box;
*/


/// Text drawing

define sealed method draw-text
    (medium :: <win32-medium>, character :: <character>, x, y,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  ignore(_start, _end);
  let text-style :: <text-style> = medium-merged-text-style(medium);
  let font :: <win32-font> = text-style-mapping(port(medium), text-style);
  let hDC :: <HDC> = update-drawing-state(medium, font: font);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    when (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    let old-alignment = SetTextAlign(hDC, windows-text-alignment(align-x, align-y));
    //---*** Should be using <C-unicode-string> when supported by following method
    with-stack-structure (c-string :: <C-string>, size: 2, fill: character)
      TextOut(hDC, x, y, c-string, 1)
    end;
    SetTextAlign(hDC, old-alignment)
  end;
  #f
end method draw-text;

//---*** What do we do about Unicode strings?
define sealed method draw-text
    (medium :: <win32-medium>, string :: <string>, x, y,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let text-style :: <text-style> = medium-merged-text-style(medium);
  let font :: <win32-font> = text-style-mapping(port(medium), text-style);
  let hDC :: <HDC> = update-drawing-state(medium, font: font);
  let transform = medium-device-transform(medium);
  let length :: <integer> = size(string);
  let _start :: <integer> = _start | 0;
  let _end   :: <integer> = _end   | length;
  assert(_end - _start < 32000,
         "'draw-text' cannot draw text strings longer than 32000 characters");
  with-device-coordinates (transform, x, y)
    when (towards-x & towards-y)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    //---*** It would be great if 'with-c-string' took start & end!
    let substring
      = if (_start = 0 & _end = length) string
        else copy-sequence(string, start: _start, end: _end) end;
    with-c-string (c-string = substring)
      let old-alignment = SetTextAlign(hDC, windows-text-alignment(align-x, align-y));
      //--- Unfortunately Windows can't do y-centering...
      when (align-y == #"center")
        let (font, width, height, ascent, descent)
          = windows-font-metrics(font, port(medium));
        ignore(font, width, ascent, descent);
        y := y - floor/(height, 2)
      end;
      if (do-tabs?)
        let tab-origin :: <integer> = if (do-tabs? == #t) x else do-tabs? end;
        TabbedTextOut(hDC, x, y, c-string, _end - _start,
                      0, null-pointer(<LPINT>), tab-origin)
      else
        TextOut(hDC, x, y, c-string, _end - _start)
      end;
      SetTextAlign(hDC, old-alignment)
    end
  end;
  #f
end method draw-text;

define inline function windows-text-alignment
    (align-x, align-y) => (flag :: <integer>)
  logior(select (align-x)
           #"left"     => $TA-LEFT;
           #"right"    => $TA-RIGHT;
           #"center"   => $TA-CENTER;
         end,
         select (align-y)
           #"top"      => $TA-TOP;
           #"bottom"   => $TA-BOTTOM;
           #"center"   => $TA-TOP;      //--- Windows can't do this so we hack it later
           #"baseline" => $TA-BASELINE;
         end)
end function windows-text-alignment;
