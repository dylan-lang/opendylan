Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Vanilla medium

define sealed class <vanilla-medium> (<basic-medium>)
  //---*** Shouldn't this be in DUIM if everybody needs it?
  sealed slot %clip-mask = #f;
end class <vanilla-medium>;


define method make-medium
    (_port :: <vanilla-port>, sheet :: <sheet>)
 => (medium :: <vanilla-medium>)
  make(<vanilla-medium>,
       port: _port, sheet: sheet)
end method make-medium;

define method do-attach-medium
    (sheet :: <sheet>, medium :: <vanilla-medium>) => ()
  //--- Initialize any medium-specific state, including 'medium-drawable'
  ignoring("do-attach-medium")
end method do-attach-medium;

define method do-detach-medium
    (sheet :: <sheet>, medium :: <vanilla-medium>) => ()
  //--- Deallocate any resources
  ignoring("do-detach-medium");
  medium-drawable(medium) := #f
end method do-detach-medium;

define method destroy-medium
    (medium :: <vanilla-medium>) => ()
  //--- Deallocate all window system resources
  next-method();
  ignoring("destroy-medium")
  medium-drawable(medium) := #f
end method destroy-medium;

define method deallocate-medium
    (_port :: <vanilla-port>, medium :: <vanilla-medium>) => ()
  next-method();
  //--- Deallocate any resources you don't feel like hanging on to
  ignoring("deallocate-medium")
  medium-drawable(medium) := #f
end method deallocate-medium;


define method invalidate-cached-drawing-state
    (medium :: <vanilla-medium>, new-state :: <integer>) => ()
  //--- Release any drawing state objects
  ignoring("invalidate-cached-drawing-state")
end method invalidate-cached-drawing-state;


define method medium-foreground-setter
    (fg :: <ink>, medium :: <vanilla-medium>) => (foreground :: <ink>)
  next-method();
  //--- Change any GC associated with foreground, and repaint as necesary
  ignoring("medium-foreground-setter");
  fg
end method medium-foreground-setter;

define method medium-background-setter
    (bg :: <ink>, medium :: <vanilla-medium>) => (background :: <ink>)
  next-method();
  //--- Change any GC associated with background, and repaint as necesary
  ignoring("medium-background-setter");
  bg
end method medium-background-setter;


/// Pen and brush support

// 'update-drawing-state' is a useful utility to be used by all of the
// drawing routines
define method update-drawing-state
    (medium :: <vanilla-medium>, #key font) => (drawable)
  let drawable = medium-drawable(medium);
  when (drawable)
    let old-cache :: <integer> = medium-drawing-state-cache(medium);
    let new-cache :: <integer> = 0;
    when (old-cache ~= $medium-fully-cached)
      // Establish a brush, unless it's already cached
      when (zero?(logand(old-cache, $medium-brush-cached)))
	let brush = medium-brush(medium);
	establish-brush(medium, brush, drawable);
	new-cache := logior(new-cache, $medium-brush-cached)
      end;
      // Establish a pen, unless it's already cached
      //--- Note that you may have to establish a new pen if the brush changed
      //--- on platforms in which the pen contains color information
      when (zero?(logand(old-cache, $medium-pen-cached)))
	let pen = medium-pen(medium);
	establish-pen(medium, pen, drawable);
	new-cache := logior(new-cache, $medium-pen-cached)
      end;
      // Establish a font only if requested, unless it's already cached
      //--- Note that on some platforms, you may still have to set the text color
      //--- if the brush changed, even if the font didn't change
      when (zero?(logand(old-cache, $medium-font-cached)))
	let text-style = medium-merged-text-style(medium);
	let font = text-style-mapping(port(medium), text-style);
	establish-font(medium, font, drawable);
	new-cache := logior(new-cache, $medium-font-cached)
      end;
      when (zero?(logand(old-cache, $medium-region-cached)))
	establish-clipping-region(medium);
	new-cache := logior(new-cache, $medium-region-cached)
      end;
      medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
    end
  end;
  drawable
end method update-drawing-state;

define method establish-brush
    (medium :: <vanilla-medium>, brush :: type-union(<standard-brush>, <ink>), drawable) => ()
  let (pixel, fill-style, operation, image)
    = convert-ink-to-drawable-components(medium, drawable, brush);
  let clip-mask = compute-clip-mask(medium);
  //--- Update drawable state
  //--- This should set the tile-x/y to (0,0) if the brush is a stipple, or it
  //--- should align it to the left/top of the figure if the brush is a pattern
end method establish-brush;

define method establish-pen 
    (medium :: <vanilla-medium>, pen :: <standard-pen>, drawable) => ()
  //--- Update drawable state
end method establish-pen;

define method establish-font
    (medium :: <vanilla-medium>, font, drawable) => ()
  //--- Update drawable state
end method establish-font;


// Compute the clip mask. This suggested implementation keeps a cached
// version around for efficiency
define method compute-clip-mask (medium :: <vanilla-medium>) => (mask)
  medium.%clip-mask 
  | (medium.%clip-mask
       := begin
	    let sheet = medium-sheet(medium);
	    let sregion = sheet-device-region(sheet);
	    let mregion = medium-clipping-region(medium);
	    let valid? = #t;
	    if (sregion == $nowhere | mregion == $nowhere)
	      #"none"
	    else
	      let (sleft, stop, sright, sbottom) = box-edges(sregion);
	      unless (mregion == $everywhere)
		let (mleft, mtop, mright, mbottom) = box-edges(mregion);
		let (mleft, mtop, mright, mbottom)
		  = transform-box(sheet-device-transform(sheet), 
				  mleft, mtop, mright, mbottom);
		let (v, left, top, right, bottom)
		  = ltrb-intersects-ltrb?(sleft, stop, sright, sbottom,
					  mleft, mtop, mright, mbottom);
		sleft := left;
		stop  := top;
		sright  := right;
		sbottom := bottom;
		valid? := v
	      end;
	      if (valid?)
		vector(sleft, stop, sright, sbottom)
	      else
		#"none"
	      end
	    end
	  end)
end method compute-clip-mask;


define generic convert-ink-to-drawable-components
    (medium :: <vanilla-medium>, drawable, brush)
 => (pixel, fill-style, operation, image :: false-or(<image>));

// Given a color, returns a native pixel value
define function color->native-color (color :: <color>, mirror) => (native-color)
  let cache = mirror.%ink-cache;
  gethash(cache, color)
  | begin
      let (r, g, b) = color-rgb(color);
      let value = convert-color-to-native(mirror, r, g, b);
      gethash(cache, color) := value;
      value
    end
end function color->native-color;

// Given a native color, returns a color
define function native-color->color (native-color, mirror) => (color :: <color>)
  let cache = mirror.%ink-cache;
  gethash(cache, native-color)
  | begin
      let (r, g, b) = native-color-rgb(native-color);
      let value = make-rgb-color(r, g, b);
      gethash(cache, native-color) := value;
      value
    end
end function native-color->color;

define method convert-ink-to-drawable-components 
    (medium :: <vanilla-medium>, drawable, brush :: <foreground>)
  convert-ink-to-drawable-components(medium, drawable, medium-foreground(medium))
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components 
    (medium :: <vanilla-medium>, drawable, brush :: <background>)
  convert-ink-to-drawable-components(medium, drawable, medium-background(medium))
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components 
    (medium :: <vanilla-medium>, drawable, brush :: <color>)
  values(color->native-color(brush, drawable), #"solid", $boole-1, #f)
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components
    (medium :: <vanilla-medium>, drawable, brush :: <contrasting-color>)
  convert-ink-to-drawable-components(medium, contrasting-color->color(brush))
end method convert-ink-to-drawable-components;

//--- You might want to handle general <image> objects, too
define method convert-ink-to-drawable-components
    (medium :: <vanilla-medium>, drawable, brush :: <stencil>)
  let cache = mirror.%ink-cache;
  let pattern
    = gethash(cache, brush)
      | begin
	  let (array, colors) = decode-pattern(brush);
	  let width  = image-width(pattern);
	  let height = image-height(pattern);
	  let ncolors :: <integer> = size(colors);
	  let pixels :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
	  let image = make-native-pixarray(drawable, width, height);
	  without-bounds-checks
	    for (n :: <integer> from 0 below ncolors)
	      let pixel = convert-ink-to-drawable-components(medium, drawable, colors[n]);
	      design-pixels[n] := pixel
	    end;
	    for (y :: <integer> from 0 below height)
	      for (x :: <integer> from 0 below width)
		image[y,x] := pixels[array[y,x]]
	      end
	    end
	  end;
	  let value = make-native-image(drawable, image);
	  gethash(cache, brush) := value;
	  value
	end;
  values(convert-color(rep, #"white"), #"solid", $boole-1, pattern)
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components
    (medium :: <vanilla-medium>, drawable, brush :: <pixmap>)
  //--- You might be able to draw directly with a pixmap...
  values(convert-color(rep, #"white"), #"solid", $boole-1, brush)
end method convert-ink-to-drawable-components;

define method convert-ink-to-drawable-components
    (medium :: <vanilla-medium>, drawable, brush :: <standard-brush>)
  let (pixel, fill-style, operation, pattern)
    = case
	brush-tile(brush) =>
	  convert-ink-to-drawable-components(medium, drawable, brush-tile(brush));
	brush-stipple(brush) =>
	  convert-ink-to-drawable-components(medium, drawable, brush-stipple(brush));
	otherwise =>
	  convert-ink-to-drawable-components(medium, drawable, brush-foreground(brush));
      end;
  ignore(operation);
  values(pixel, fill-style, brush-mode(brush), pattern)
end method convert-ink-to-drawable-components;


/// Figure graphics

define method draw-point
    (medium :: <vanilla-medium>, x, y) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  with-device-coordinates (transform, x, y)
    //--- Draw the point
  end;
  #f
end method draw-point;

define method draw-points
    (medium :: <vanilla-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  do-coordinates
    (method (x, y)
       with-device-coordinates (transform, x, y)
	 //--- Draw the point
       end
     end,
     coord-seq);
  #f
end method draw-points;

define method draw-line
    (medium :: <vanilla-medium>, x1, y1, x2, y2) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  with-device-coordinates (transform, x1, y1, x2, y2)
    //--- Draw the line
  end;
  #f
end method draw-line;

define method draw-lines
    (medium :: <vanilla-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  do-endpoint-coordinates
    (method (x1, y1, x2, y2)
       with-device-coordinates (transform, x1, y1, x2, y2)
	 //--- Draw the line
       end
     end,
     coord-seq);
  #f
end method draw-lines;

define method draw-rectangle
    (medium :: <vanilla-medium>, left, top, right, bottom,
     #key filled? = #t) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  if (~rectilinear-transform?(transform))
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    let drawable = update-drawing-state(medium);
    with-device-coordinates (transform, left, top, right, bottom)
      //--- Draw the rectangle
    end
  end;
  #f
end method draw-rectangle;

define method draw-rectangles
    (medium :: <vanilla-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  do-endpoint-coordinates
    (method (left, top, right, bottom)
       with-device-coordinates (transform, left, top, right, bottom)
	 //--- Draw the rectangle
       end
     end,
     coord-seq);
  #f
end method draw-rectangles;

define method draw-rounded-rectangle
    (medium :: <vanilla-medium>, left, top, right, bottom,
     #key filled? = #t, radius) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  with-device-coordinates (transform, left, top, right, bottom)
    //--- Draw the rounded-corner rectangle
  end;
  #f
end method draw-rounded-rectangle;

define method draw-polygon
    (medium :: <vanilla-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  let length = size(coord-seq);
  with-stack-object (points :: <simple-object-vector>,
		     size: if (closed? & ~filled?) length + 2 else length end)
    replace-subsequence!(points, coord-seq);
    for (i :: <integer> = 0 then i + 2, until: i >= length)
      let x = points[i];
      let y = points[i + 1];
      with-device-coordinates (transform, x, y)
	points[i]     := x;
        points[i + 1] := y
      end
    end;
    when (closed? & ~filled?)
      points[length - 2] := points[0];
      points[length - 1] := points[1]
    end;
  //--- Draw the polygon
  end;
  #f
end method draw-polygon;

define method draw-ellipse
    (medium :: <vanilla-medium>, center-x, center-y, radius-1-dx,
     radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable = update-drawing-state(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      let (angle-2, x-radius, y-radius, angle-1)
        = singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
      if (x-radius = abs(y-radius)	// a circle - rotations are irrelevant
	  | zero?(angle-1))		// axis-aligned ellipse
	x-radius := abs(x-radius);
	y-radius := abs(y-radius);
	if (start-angle | end-angle)
	  if (filled?)
	    #f		//--- draw a filled partial ellipse
	  else
	    #f		//--- draw an unfilled partial ellipse
	  end
	else
	  if (filled?)
	    #f		//--- draw a filled ellipse
	  else
	    #f		//--- draw an unfilled ellipse
	  end
	end
      else
	#f		//--- draw a non-axis-aligned ellipse
      end
    end
  end;
  #f
end method draw-ellipse;

define method draw-image
    (medium :: <vanilla-medium>, image :: <image>, x, y) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let width  = image-width(image);
    let height = image-height(image);
    let drawable = medium-drawable(medium);
    when (drawable)
      let (pixel, fill-style, operation, pattern)
	= convert-ink-to-drawable-components(medium, drawable, image);
      //--- Draw the image
    end
  end;
  #f
end method draw-image;


/// Pixel drawing

define method set-pixel
    (medium :: <vanilla-medium>, color :: <rgb-color>, x, y) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable  = medium-drawable(medium);
  let (r, g, b) = color-rgb(color);
  let pixel     = convert-color-to-native(mirror, r, g, b);
  with-device-coordinates (transform, x, y)
    //--- Draw the pixel without perturbing the drawing state
  end;
  #f
end method set-pixel;

define method set-pixels
    (medium :: <vanilla-medium>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let drawable  = medium-drawable(medium);
  let (r, g, b) = color-rgb(color);
  let pixel     = convert-color-to-native(mirror, r, g, b);
  do-coordinates
    (method (x, y)
       with-device-coordinates (transform, x, y)
	 //--- Draw the pixels without perturbing the drawing state
       end
     end,
     coord-seq);
  #f
end method set-pixels;


/// Path graphics

define method start-path (medium :: <vanilla-medium>) => (record)
  //--- Start the path
  #f
end method start-path;

define method end-path (medium :: <vanilla-medium>) => (record)
  //--- End the path
  #f
end method end-path;

define method abort-path (medium :: <vanilla-medium>) => (record)
  //--- Abort the path
  #f
end method abort-path;

define method close-path (medium :: <vanilla-medium>) => (record)
  //--- Close the path
  #f
end method close-path;

define method stroke-path (medium :: <vanilla-medium>, #key filled?) => (record)
  let drawable = update-drawing-state(medium);
  //--- Stroke the path
  #f
end method stroke-path;

define method fill-path (medium :: <vanilla-medium>) => (record)
  let drawable = update-drawing-state(medium);
  //--- Fill the path
  #f
end method fill-path;

define method clip-from-path
    (medium :: <vanilla-medium>, #key function = $boole-and) => (record)
  //--- Clip from the path
  #f
end method clip-from-path;

define method save-clipping-region (medium :: <vanilla-medium>) => (record)
  //--- Push the clip region
  #f
end method save-clipping-region;

define method restore-clipping-region (medium :: <vanilla-medium>) => (record)
  //--- Pop the clip region
  #f
end method restore-clipping-region;

define method move-to (medium :: <vanilla-medium>, x, y) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  with-device-coordinates (transform, x, y)
    //--- Move-to
  end;
  #f
end method move-to;

define method line-to (medium :: <vanilla-medium>, x, y) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  with-device-coordinates (transform, x, y)
    //--- Line-to
  end;
  #f
end method line-to;

define method arc-to (medium :: <vanilla-medium>, center-x, center-y,
			 radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
			 #key start-angle, end-angle) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      //--- Arc-to
    end
  end;
  #f
end method arc-to;

define method curve-to (medium :: <vanilla-medium>, x1, y1, x2, y2, x3, y3) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  with-device-coordinates (transform, x1, y1, x2, y2, x3, y3)
    //--- Curve-to
  end;
  #f
end method curve-to;


/// 'draw-pixmap'

define method draw-pixmap
    (medium :: <vanilla-medium>, pixmap :: <pixmap>, x, y,
     #key function = $boole-1) => (record)
  // Coordinates will get transformed in 'copy-area'
  copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
	    medium, x, y, function: function);
  #f
end method draw-pixmap;


define method clear-box
    (medium :: <vanilla-medium>, left, top, right, bottom) => ()
  //--- Do it if you can do better than 'draw-rectangle'
end method clear-box;


/// Text drawing

define method draw-text
    (medium :: <vanilla-medium>, string :: <string>, x, y,
     #key start: _start = 0, end: _end = size(string),
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let text-style = medium-merged-text-style(medium);
  let font = text-style-mapping(port(medium), text-style);
  let drawable = update-drawing-state, font: font);
  with-device-coordinates (transform, x, y)
    when (towards-x)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    let (x-adjust, y-adjust)
      = compute-text-adjustment(medium, string, text-style, align-x, align-y,
				start: _start, end: _end);
    inc!(x, floor(x-adjust));
    inc!(y, floor(y-adjust));
    when (towards-x)
      inc!(towards-x, floor(x-adjust));
      inc!(towards-y, floor(y-adjust))
    end;
    //--- Draw the string
  end;
  #f
end method draw-text;

define method draw-text
    (medium :: <vanilla-medium>, character :: <character>, x, y,
     #key align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let transform = sheet-device-transform(medium-sheet(medium));
  let text-style = medium-merged-text-style(medium);
  let font = text-style-mapping(port(medium), text-style);
  let drawable = update-drawing-state(medium, font: font);
  with-device-coordinates (transform, x, y)
    when (towards-x)
      convert-to-device-coordinates!(transform, towards-x, towards-y)
    end;
    let (x-adjust, y-adjust)
      = compute-text-adjustment(medium, character, text-style, align-x, align-y);
    inc!(x, floor(x-adjust));
    inc!(y, floor(y-adjust));
    when (towards-x)
      inc!(towards-x, floor(x-adjust));
      inc!(towards-y, floor(y-adjust))
    end;
    //--- Draw the character
  end;
  #f
end method draw-text;

//--- Provide this method if you can do faster than the default
define method glyph-for-character
    (_port :: <vanilla-port>, char :: <character>, text-style :: <text-style>, #key font)
 => (index :: <integer>, font,
     escapement-x :: <real>, escapement-y :: <real>,
     origin-x :: <real>, origin-y :: <real>, bb-x :: <real>, bb-y :: <real>);
  let (index, char-set) = index-and-character-set(char);
  let font = font | text-style-mapping(_port, text-style, character-set: char-set);
  //--- Do it
end method glyph-for-character;

//--- Provide this method if you can do faster than the default
define method text-size
    (_port :: <vanilla-port>, char :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, total-height :: <real>,
     last-x :: <real>, last-y :: <real>, baseline :: <real>)
  ignore(_start, _end, do-newlines?);
  //--- Do it
end method text-size;

//--- Provide this method if you can do faster than the default,
//--- for instance, if there's a function to measure a whole string
define method text-size
    (_port :: <vanilla-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start = 0, end: _end = size(string), do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, total-height :: <real>,
     last-x :: <real>, last-y :: <real>, baseline :: <real>)
  //--- Do it
end method text-size;


/// Font metrics -- from text styles

define method font-metrics
    (text-style :: <text-style>, _port :: <vanilla-port>, #key character-set)
 => (font, width :: <real>, height :: <real>, ascent :: <real>, descent :: <real>)
  let font = text-style-mapping(text-style, _port);
  //--- Do this
end method font-metrics;

define method font-width
    (text-style :: <text-style>, _port :: <vanilla-port>, #key character-set)
 => (width :: <real>)
  let font = text-style-mapping(text-style, _port);
  //--- Do this
end method font-width;

define method font-height
    (text-style :: <text-style>, _port :: <vanilla-port>, #key character-set)
 => (height :: <real>)
  let font = text-style-mapping(text-style, _port);
  //--- Do this
end method font-height;

define method font-ascent
    (text-style :: <text-style>, _port :: <vanilla-port>, #key character-set)
 => (ascent :: <real>)
  let font = text-style-mapping(text-style, _port);
  //--- Do this
end method font-ascent;

define method font-descent
    (text-style :: <text-style>, _port :: <vanilla-port>, #key character-set)
 => (descent :: <real>)
  let font = text-style-mapping(text-style, _port);
  //--- Do this
end method font-descent;

define method fixed-width-font?
    (text-style :: <text-style>, _port :: <vanilla-port>, #key character-set)
 => (true? :: <boolean>)
  let font = text-style-mapping(text-style, _port);
  //--- Do this
end method fixed-width-font?;
