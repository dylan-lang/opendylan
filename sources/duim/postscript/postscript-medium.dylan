Module:       postscript-duim
Synopsis:     DUIM postscript backend
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// PostScript mediums

define method use-pen (medium :: <postscript-medium>, pen)
  let printer-stream = medium.%printer-stream;
  let thickness
    = select (pen-units(pen))
        #"normal" =>
          normal-pen-width(port(medium), pen-width(pen));
        #"point" =>
          pen-width(pen)
      end;
  let dashes = pen-dashes(pen);
  format(printer-stream, " %d setlinewidth\n", thickness);
  when (dashes)
    when (dashes == #t)
      dashes := #[4, 4]
    end;
    making-ps-array(printer-stream)
      let limit = size(dashes);
      for (i :: <integer> from 0 below limit)
        format(printer-stream, "%d", dashes[i]);
        unless (i + 1 >= limit)
          write(printer-stream, " ")
        end;
      end
    end;
    format(printer-stream, "0 setdash\n")
  end
end method use-pen;

define method use-clipping-region (medium :: <postscript-medium>, region)
  let printer-stream = medium.%printer-stream;
  let transform = medium-transform(medium);
  let (left, top, right, bottom) = box-edges(region);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, left, top, right, bottom)
  end;
  format(printer-stream, " newpath\n");
  ps-pos-op(medium, "moveto", left, top);
  ps-pos-op(medium, "lineto", right, top);
  ps-pos-op(medium, "lineto", right, bottom);
  ps-pos-op(medium, "lineto", left, bottom);
  format(printer-stream, " closepath clip\n")
end method use-clipping-region;


define method maybe-set-color
    (medium :: <postscript-medium>, brush :: <standard-brush>)
  maybe-set-color(medium, brush-foreground(brush))
end method maybe-set-color;

define method maybe-set-color
    (medium :: <postscript-medium>, brush :: <foreground>)
  maybe-set-color(medium, medium-foreground(medium) | $black)
end method maybe-set-color;

define method maybe-set-color
    (medium :: <postscript-medium>, brush :: <background>)
  maybe-set-color(medium, medium-background(medium) | $white)
end method maybe-set-color;

define method maybe-set-color
    (medium :: <postscript-medium>, brush :: <color>)
  let current-color = medium.%current-color;
  when (~current-color | current-color ~= brush)
    medium.%current-color := brush;
    let (r, g, b) = color-rgb(brush);
    format(medium.%printer-stream, " %= %= %= setrgbcolor\n", r, g, b)
  end
end method maybe-set-color;

define method maybe-set-color
    (medium :: <postscript-medium>, brush :: <contrasting-color>)
  maybe-set-color(medium, contrasting-color->color(brush))
end method maybe-set-color;


define macro with-postscript-drawing-options
  { with-postscript-drawing-options
        (?stream:variable = ?medium:expression, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let drawing-body = method (?stream) ?body end;
           do-with-postscript-drawing-options(?medium, drawing-body, ?options)
         end }
  { with-postscript-drawing-options
        (?medium:expression, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let drawing-body = method (_stream) ignore(_stream); ?body end;
           do-with-postscript-drawing-options(?medium, drawing-body, ?options)
         end }
end macro with-postscript-drawing-options;

define method do-with-postscript-drawing-options
    (medium :: <postscript-medium>, continuation :: <function>,
     #key pen, filled? = $unsupplied, epilogue, newpath? = #t) => (#rest values)
  let printer-stream = medium.%printer-stream;
  let brush = medium-brush(medium);
  let region = medium-clipping-region(medium);
  unless (region == $nowhere)
    block ()
      when (supplied?(filled?) & ~filled? & pen)
	use-pen(medium, pen)
      end;
      maybe-set-color(medium, brush);
      when (region ~== $everywhere)
	format(printer-stream, " gsave\n");
	use-clipping-region(medium, region)
      end;
      when (newpath?)
	format(printer-stream, " newpath\n")
      end;
      continuation(printer-stream)
    cleanup
      select (epilogue)
	#"default" => if (supplied?(filled?) & filled?)
			ps-fill(medium, printer-stream, brush)
		      else
			ps-stroke(medium, printer-stream, brush)
		      end;
	#"fill" => ps-fill(medium, printer-stream, brush);
	#"stroke" => ps-stroke(medium, printer-stream, brush);
	otherwise => #f
      end;
      when (region ~== $everywhere)
	format(printer-stream, " grestore\n")
      end
    end
  end
end method do-with-postscript-drawing-options;


/// Figure graphics

define method draw-point
    (medium :: <postscript-medium>, x, y) => (record)
  let transform = medium-device-transform(medium);
  let pen = medium-pen(medium);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, x, y)
  end;
  with-postscript-drawing-options (medium,
				   pen: pen, epilogue: #"stroke")
    ps-pos-op(medium, "m", x, y);
    ps-rel-pos-op(medium, "rlineto", 0, 0)
  end;
  #f
end method draw-point;

define method draw-points
    (medium :: <postscript-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let transform = medium-device-transform(medium);
  let pen = medium-pen(medium);
  with-postscript-drawing-options (medium,
				   pen: pen, epilogue: #"stroke")
    do-coordinates
      (method (x, y)
	 unless (transform == $identity-transform)	// for speed
	   transform-coordinates!(transform, x, y)
	 end;
         ps-pos-op(medium, "m", x, y);
         ps-rel-pos-op(medium, "rlineto", 0, 0)
       end,
       coord-seq)
  end;
  #f
end method draw-points;

define method draw-line
    (medium :: <postscript-medium>, x1, y1, x2, y2) => (record)
  let transform = medium-device-transform(medium);
  let pen = medium-pen(medium);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, x1, y1, x2, y2)
  end;
  with-postscript-drawing-options (medium,
				   pen: pen, epilogue: #"stroke")
    ps-pos-op(medium, "m", x1, y1);
    ps-pos-op(medium, "lineto", x2, y2)
  end;
  #f
end method draw-line;

define method draw-lines
    (medium :: <postscript-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let transform = medium-device-transform(medium);
  let pen = medium-pen(medium);
  with-postscript-drawing-options (medium,
				   pen: pen, epilogue: #"stroke")
    do-endpoint-coordinates
      (method (x1, y1, x2, y2)
	 unless (transform == $identity-transform)	// for speed
	   transform-coordinates!(transform, x1, y1, x2, y2)
	 end;
         ps-pos-op(medium, "m", x1, y1);
         ps-pos-op(medium, "lineto", x2, y2)
       end,
       coord-seq)
  end;
  #f
end method draw-lines;

define method draw-rectangle
    (medium :: <postscript-medium>, x1, y1, x2, y2,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    let pen = medium-pen(medium);
    unless (transform == $identity-transform)	// for speed
      transform-coordinates!(transform, x1, y1, x2, y2)
    end;
    with-postscript-drawing-options (printer-stream = medium,
				     filled?: filled?, pen: pen, epilogue: #"default")
      ps-pos-op(medium, "m", x1, y1);
      ps-pos-op(medium, "lineto", x2, y1);
      ps-pos-op(medium, "lineto", x2, y2);
      ps-pos-op(medium, "lineto", x1, y2);
      format(printer-stream, " closepath ")
    end
  end;
  #f
end method draw-rectangle;

define method draw-rectangles
    (medium :: <postscript-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    draw-transformed-rectangles(medium, coord-seq, filled?: filled?)
  else
    let pen = medium-pen(medium);
    with-postscript-drawing-options (printer-stream = medium,
				     filled?: filled?, pen: pen, epilogue: #"default")
      do-endpoint-coordinates
	(method (x1, y1, x2, y2)
	   unless (transform == $identity-transform)	// for speed
	     transform-coordinates!(transform, x1, y1, x2, y2)
	   end;
	   ps-pos-op(medium, "m", x1, y1);
	   ps-pos-op(medium, "lineto", x2, y1);
	   ps-pos-op(medium, "lineto", x2, y2);
	   ps-pos-op(medium, "lineto", x1, y2);
	   format(printer-stream, " closepath ")
	 end,
	 coord-seq)
    end
  end;
  #f
end method draw-rectangles;

define method draw-transformed-rectangles
    (medium :: <postscript-medium>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let len = size(coord-seq);
  assert(zero?(modulo(len, 4)));
  local method draw-one (x1, y1, x2, y2) => ()
	  with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
	    apply(draw-polygon, medium, coords, closed?: #t, keys)
	  end
        end method;
  dynamic-extent(draw-one);
  for (i :: <integer> = 0 then i + 4, until: i = len)
    draw-one(coord-seq[i + 0], coord-seq[i + 1],
	     coord-seq[i + 2], coord-seq[i + 3])
  end;
  #f
end method draw-transformed-rectangles;

//---*** Do this
define method draw-rounded-rectangle
    (medium :: <postscript-medium>, x1, y1, x2, y2,
     #key filled? = #t, radius) => (record)
  draw-rectangle(medium, x1, y1, x2, y2, filled?: filled?);
  #f
end method draw-rounded-rectangle;

define method draw-polygon
    (medium :: <postscript-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  let pen = medium-pen(medium);
  let minx = $maximum-integer;
  let miny = $maximum-integer;
  let length = size(coord-seq);
  with-stack-object (points :: <simple-object-vector>, size: length)
    replace-subsequence!(points, coord-seq);
    for (i :: <integer> = 0 then i + 2, until: i >= length)
      let x = points[i];
      let y = points[i + 1];
      unless (transform == $identity-transform)	// for speed
	transform-coordinates!(transform, x, y)
      end;
      points[i] := x;
      points[i + 1] := y;
      when (x < minx)
	minx := x
      end;
      when (y < miny)
	miny := y
      end;
    end;
    with-postscript-drawing-options
        (printer-stream = medium,
	 filled?: filled?, pen: pen, epilogue: #"default")
      let start-x = points[0];
      let start-y = points[1];
      ps-pos-op(medium, "m", start-x, start-y);
      for (i :: <integer> = 2 then i + 2, until: i >= length)
        let ex = points[i];
        let ey = points[i + 1];
        ps-pos-op(medium, "lineto", ex, ey);
      finally
        when (closed?)
          format(printer-stream, " closepath ")
        end;
      end
    end
  end;
  #f
end method draw-polygon;

define method draw-ellipse
    (medium :: <postscript-medium>, center-x, center-y, radius-1-dx,
     radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  maybe-send-feature(medium, #"ellipse", *ps-ellipse-code*);
  let transform = medium-device-transform(medium);
  let pen = medium-pen(medium);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, center-x, center-y);
    transform-distances!(transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
  end;
  when (start-angle)
    swap!(start-angle, end-angle)		// imitate left-handed coord system
  end;
  let (angle-2, x-radius, y-radius, angle-1)
    = singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
  ignore(angle-2);
  y-radius := abs(y-radius);
  pixels-to-points(x-radius, y-radius);
  let degrees = radians->degrees(angle-1);
  let (sa, ea)
    = if (start-angle)
	values(degrees - radians->degrees(start-angle),
	       degrees - radians->degrees(end-angle))
      else
	values(0, 360)
      end;
  with-postscript-drawing-options (medium,
				   filled?: filled?, pen: pen, epilogue: #"default")
    // If it's a filled arc, make sure it's a pie slice instead of a chord
    when (filled? & ~zero?(modulo(abs(sa - ea), 360)))
      ps-pos-op(medium, "m", center-x, center-y)
    end;
    ps-pos-op(medium, "ellipse",
	      center-x, center-y, x-radius, y-radius, sa, ea)
  end;
  #f
end method draw-ellipse;


/// Pixel drawing

define method set-pixel
    (medium :: <postscript-medium>, color :: <rgb-color>, x, y) => (record)
  let transform = medium-device-transform(medium);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, x, y)
  end;
  let current-color = medium.%current-color;
  when (~current-color | current-color ~= color)
    medium.%current-color := color;
    let (r, g, b) = color-rgb(color);
    format(medium.%printer-stream, " %= %= %= setrgbcolor\n", r, g, b)
  end;
  let printer-stream = medium.%printer-stream;
  let region = medium-clipping-region(medium);
  unless (region == $nowhere)
    when (region ~== $everywhere)
      format(printer-stream, " gsave\n");
      use-clipping-region(medium, region)
    end;
    format(printer-stream, " newpath\n");
    ps-pos-op(medium, "m", x, y);
    ps-rel-pos-op(medium, "rlineto", 0, 0);
    format(printer-stream, " stroke\n");
    when (region ~== $everywhere)
      format(printer-stream, " grestore\n")
    end
  end;
  #f
end method set-pixel;

define method set-pixels
    (medium :: <postscript-medium>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  let transform = medium-device-transform(medium);
  let current-color = medium.%current-color;
  when (~current-color | current-color ~= color)
    medium.%current-color := color;
    let (r, g, b) = color-rgb(color);
    format(medium.%printer-stream, " %= %= %= setrgbcolor\n", r, g, b)
  end;
  let printer-stream = medium.%printer-stream;
  let region = medium-clipping-region(medium);
  unless (region == $nowhere)
    when (region ~== $everywhere)
      format(printer-stream, " gsave\n");
      use-clipping-region(medium, region)
    end;
    do-coordinates
      (method (x, y)
	 unless (transform == $identity-transform)	// for speed
	   transform-coordinates!(transform, x, y)
	 end;
	 format(printer-stream, " newpath\n");
	 ps-pos-op(medium, "m", x, y);
	 ps-rel-pos-op(medium, "rlineto", 0, 0);
	 format(printer-stream, " stroke\n");
       end,
       coord-seq);
    when (region ~== $everywhere)
      format(printer-stream, " grestore\n")
    end
  end;
  #f
end method set-pixels;


/// Text drawing

// These 2 clones of DRAW-STRING would be much more modular if there
// were a reasonable way of passing arguments transparently, so that
// we might be able to share code.
define method draw-text
    (medium :: <postscript-medium>, string :: <string>, x, y,
     #key start: _start = 0, end: _end = size(string),
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let transform = medium-device-transform(medium);
  let text-style = medium-merged-text-style(medium);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, x, y);
    when (towards-x)
      transform-coordinates!(transform, towards-x, towards-y)
    end
  end;
  let (x-adjust, y-adjust)
    = compute-text-adjustment(medium, string, text-style, align-x, align-y,
			      start: _start, end: _end);
  inc!(x, x-adjust);
  inc!(y, y-adjust);
  when (towards-x)
    inc!(towards-x, x-adjust);
    inc!(towards-y, y-adjust)
  end;
  // do raster/ink stuff
  let font = find-postscript-font(port(medium), medium, text-style);
  set-font-if-needed(medium, font);
  with-postscript-drawing-options (printer-stream = medium,
				   epilogue: #f, newpath?: #f)
    ps-pos-op(medium, "m", x, y);
    carefully-output-ps-showstring
      (printer-stream, string, start: _start, end: _end)
  end;
  #f
end method draw-text;

define method draw-text
    (medium :: <postscript-medium>, character :: <character>, x, y,
     #key start: _start, end: _end,
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  ignore(_start, _end);
  let transform = medium-device-transform(medium);
  let text-style = medium-merged-text-style(medium);
  unless (transform == $identity-transform)	// for speed
    transform-coordinates!(transform, x, y);
    when (towards-x)
      transform-coordinates!(transform, towards-x, towards-y)
    end
  end;
  let (x-adjust, y-adjust)
    = compute-text-adjustment(medium, character, text-style, align-x, align-y);
  inc!(x, x-adjust);
  inc!(y, y-adjust);
  when (towards-x)
    inc!(towards-x, x-adjust);
    inc!(towards-y, y-adjust)
  end;
  // do raster/ink stuff
  medium.%ch1buf[0] := character;
  let font = find-postscript-font(port(medium), medium, text-style);
  set-font-if-needed(medium, font);
  with-postscript-drawing-options (printer-stream = medium,
				   epilogue: #f, newpath?: #f)
    ps-pos-op(medium, "m", x, y);
    carefully-output-ps-showstring
      (printer-stream, medium.%ch1buf, start: 0, end: 1)
  end;
  #f
end method draw-text;


// Provide a way for the "user" to start a new page.
// Should this have a different name?
// Should this functionality be invoked by writing the #\page character?
define method new-page (sheet :: <postscript-sheet>)
  let medium = sheet-medium(sheet);
  let printer-stream = medium.%printer-stream;
  format(printer-stream, "new-page\n");
end method new-page;

define method glyph-for-character
    (_port :: <postscript-port>, char :: <character>, text-style :: <text-style>, #key font)
 => (index :: <integer>, font,
     escapement-x :: <real>, escapement-y :: <real>,
     origin-x :: <real>, origin-y :: <real>, bb-x :: <real>, bb-y :: <real>)
  let (index, char-set) = index-and-character-set(char);
  ignore(char-set);
  let font = font | find-postscript-font(_port, #f, text-style);
  let cwt = font.psfont-width-table;
  let escapement-x = if (instance?(cwt, <number>)) cwt else cwt[index] end;
  let escapement-y = 0;
  let origin-x = 0;
  let origin-y = font.psfont-ascent;
  let bb-x = escapement-x;	// not available yet
  let bb-y = font.psfont-height;
  values(index, font, escapement-x, escapement-y, origin-x, origin-y, bb-x, bb-y)
end method glyph-for-character;

define method set-font-if-needed (medium :: <postscript-medium>, font) => ()
  unless (medium.%curfont == font)
    format(medium.%printer-stream, "%d f ", font.psfont-index);
    medium.%curfont := font
  end
end method set-font-if-needed;

define method carefully-output-ps-showstring
    (printer-stream, data, #key start, end: _end)
  assert(_end <= size(data));
  write(printer-stream, "(");
  block (return)
    while (#t)
      let next-special
        = position-if
            (data,
             method (char)
               char = '(' | char = ')' | char = '\\'
             end,
             start: start, end: _end);
      write(printer-stream, data, start: start, end: next-special | _end);
      when (next-special)
        write-element(printer-stream, '\\');
        write-element(printer-stream, data[next-special])
      end;
      unless (next-special)
        return()
      end;
      start := next-special + 1
    end
  end;
  format(printer-stream, ") show\n")
end method carefully-output-ps-showstring;

define method position-if
    (sequence :: <sequence>, predicate :: <function>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence))
 => (index :: false-or(<integer>))
  assert(_start >= 0 & _end <= size(sequence));
  block (return)
    for (i :: <integer> = _start then i + 1,
	 until: i = _end)
      if (predicate(sequence[i]))
	return(i)
      end
    end;
    #f
  end
end method position-if;


/// Font metrics

// PostScript's "default user space" is measured in printers' points.
// Should these force the stream to map the font and then get the
// information from the font?
define method font-width
    (text-style :: <text-style>, _port :: <postscript-port>, #key character-set)
 => (width :: <real>)
  // An 'M' is often square and of height approximating the point size of the font.
  //--- This should consult the real metrics.
  text-style-size-in-points(text-style, _port)
end method font-width;

define method font-height
    (text-style :: <text-style>, _port :: <postscript-port>, #key character-set)
 => (height :: <real>)
  text-style-size-in-points(text-style, _port)
end method font-height;

define method font-ascent
    (text-style :: <text-style>, _port :: <postscript-port>, #key character-set)
 => (ascent :: <real>)
  font-height(text-style, _port) * (1 - *ps-magic-baseline-factor*)
end method font-ascent;

define method font-descent
    (text-style :: <text-style>, _port :: <postscript-port>, #key character-set)
 => (descent :: <real>)
  font-height(text-style, _port) * *ps-magic-baseline-factor*
end method font-descent;

define method font-metrics
    (text-style :: <text-style>, _port :: <postscript-port>, #key character-set)
 => (font, width :: <real>, height :: <real>, ascent :: <real>, descent :: <real>)
  values(#f,
	 text-style-size-in-points(text-style, _port),
	 text-style-size-in-points(text-style, _port),
	 font-height(text-style, _port) * (1 - *ps-magic-baseline-factor*),
	 font-height(text-style, _port) * *ps-magic-baseline-factor*)
end method font-metrics;

//--- This can probably go away when we have standardized text styles.
define method text-style-size-in-points
    (text-style :: <text-style>, _port :: <postscript-port>)
  values(point-size-for-text-style(text-style))
end method text-style-size-in-points;

define method fixed-width-font?
    (text-style :: <text-style>, _port :: <postscript-port>, #key character-set)
 => (true? :: <boolean>)
  let font = find-postscript-font(_port, #f, text-style);
  instance?(font.psfont-width-table, <number>)
end method fixed-width-font?;


/*

/// PostScript streams

define method new-page (stream :: <postscript-stream>)
  let medium = sheet-medium(stream);
  let printer-stream = medium.%printer-stream;
  when (stream.%scale-to-fit?)
    error("'new-page' is only valid when 'scale-to-fit?' is #f")
  end;
  // Replay all the records we've collected so far before clearing the page
  with-output-recording-options (stream, record?: #f, draw?: #t)
    stream-replay(stream, #f)
  end;
  format(printer-stream, "new-page\n");
  // Simulate WINDOW-CLEAR
  when (stream-output-history(stream))
    clear-output-record(stream-output-history(stream))
  end;
  stream-text-output-record(stream) := #f;
  when (extended-output-stream?(stream))
    // Can we assume this?
    stream-set-caret-position(stream, 0, 0);
    stream-baseline(stream) := 0;
    stream-current-line-height(stream) := 0
  end;
end method new-page;

define method stream-scan-string-for-writing
    (stream :: <output-protocol-mixin>, medium :: <postscript-medium>, string,
     start, end: _end, style, cursor-x, max-x, #key glyph-buffer)
  with-postscript-glyph-for-character
    scanning-string-for-writing
      (stream, medium, string, start, _end, style, cursor-x, max-x,
       glyph-buffer)
  end
end method stream-scan-string-for-writing;

*/
