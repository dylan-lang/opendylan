Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Drawables

define sealed class <capi-drawable> (<object>)
  sealed slot %ink-cache :: <object-table> = make(<table>);
end class <capi-drawable>;

define method clear-ink-cache (drawable :: <capi-drawable>)
  //--- deallocate stuff first...
  remove-all-keys!(drawable.%ink-cache)
end method clear-ink-cache;


/// Mediums

define sealed class <capi-medium> (<basic-medium>)
  sealed slot %clip-mask = #f;
end class <capi-medium>;

define method make-medium
    (_port :: <capi-port>, sheet :: <sheet>)
 => (medium :: <capi-medium>)
  make(<capi-medium>,
       port: _port, sheet: sheet)
end method make-medium;

define method destroy-medium (medium :: <capi-medium>) => ()
  //--- deallocate all window system resources
  next-method()
end method destroy-medium;

define method deallocate-medium
    (_port :: <capi-port>, medium :: <capi-medium>) => ()
  next-method();
  medium-drawable(medium) := #f
end method deallocate-medium;

define method do-attach-medium
    (sheet :: <sheet>, medium :: <capi-medium>) => ()
  let mirror = sheet-mirror(sheet);
  clear-ink-cache(mirror);
  medium-drawable(medium) := mirror
end method do-attach-medium;

define method do-detach-medium
    (sheet :: <sheet>, medium :: <capi-medium>) => ()
  //--- deallocate any resources
  medium-drawable(medium) := #f
end method do-detach-medium;


define method medium-foreground-setter
    (fg :: <ink>, medium :: <capi-medium>) => (foreground)
  next-method();
  clear-ink-cache(medium-drawable(medium));
  fg
end method medium-foreground-setter;

define method medium-background-setter
    (bg :: <ink>, medium :: <capi-medium>) => (background)
  next-method();
  clear-ink-cache(medium-drawable(medium));
  unless (instance?(medium, <pixmap-medium>))
    repaint-sheet(medium-sheet(medium), $everywhere)
  end;
  bg
end method medium-background-setter;

define method medium-clipping-region-setter
    (region :: <region>, medium :: <capi-medium>) => (region)
  next-method();
  // Don't flush the cache if the region isn't really changing.
  // This situation comes up all the time during repainting, when we set
  // the clipping region for every output record, but we almost always
  // just set it to $everywhere.
  unless (region == medium-clipping-region(medium))
    decache-clip-mask(medium)
  end;
  region
end method medium-clipping-region-setter;

define method invalidate-cached-region (medium :: <capi-medium>) => ()
  decache-clip-mask(medium);
  next-method()
end method invalidate-cached-region;

define method invalidate-cached-transform (medium :: <capi-medium>) => ()
  decache-clip-mask(medium);
  next-method()
end method invalidate-cached-transform;

// No need to do anything, since we continually reuse a single gcontext
define method invalidate-cached-drawing-state
    (medium :: <capi-medium>, new-state :: <integer>) => ()
  ignore(new-state);
  #f 
end method invalidate-cached-drawing-state;

define method decache-clip-mask (medium :: <capi-medium>) => ()
  medium.%clip-mask := #f;
  // Decache the brush, too, because that's where the clip mask lives
  medium-drawing-state-cache(medium)
    := logand(medium-drawing-state-cache(medium), lognot($medium-brush-cached))
end method decache-clip-mask;


/// "Resources"

define function color->capi-color (rep, color :: <color>) => (capi-color)
  let (r, g, b) = color-rgb(color);
  convert-color(rep, make-rgb(r, g, b))
end function color->capi-color;

// Given a color, returns a CAPI pixel value
define function drawable-color->capi-color (drawable, color) => (capi-color)
  let cache = drawable.%ink-cache;
  gethash(cache, color)
  | begin
      let value = color->capi-color(ensure-representation(drawable), color);
      gethash(cache, color) := value;
      value
    end
end function drawable-color->capi-color;

define function capi-color->color (rep, capi-color) => (color :: <color>)
  ignore(rep);
  make-rgb-color(color-red(capi-color),
                 color-green(capi-color),
                 color-blue(capi-color))
end function capi-color->color;

// Given a CAPI color, returns a color
define function drawable-capi-color->color (drawable, capi-color) => (color)
  let cache = drawable.%ink-cache;
  gethash(cache, capi-color)
  | begin
      let value = capi-color->color(ensure-representation(drawable), capi-color);
      gethash(cache, capi-color) := value;
      value
    end
end function drawable-capi-color->color;


define method port-default-foreground
    (_port :: <capi-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>))
  #f	// use the default
end method port-default-foreground;

define method port-default-background
    (_port :: <capi-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>))
  #f	// use the default
end method port-default-background;

define method port-default-background
    (_port :: <capi-port>, sheet :: <drawing-pane>)
 => (background :: false-or(<ink>))
  $white
end method port-default-background;

// Viewports try to take their background from their child
define method port-default-background
    (_port :: <capi-port>, sheet :: <viewport>)
 => (background :: false-or(<ink>))
  let child = sheet-child(sheet);
  if (child)
    port-default-background(_port, child)
  else
    next-method()
  end
end method port-default-background;

define method port-default-background
    (_port :: <capi-port>, sheet :: <homegrown-control-mixin>)
 => (background :: false-or(<ink>))
  $white
end method port-default-background;

define method port-default-background
    (_port :: <capi-port>, sheet :: <homegrown-control-layout-mixin>)
 => (background :: false-or(<ink>))
  $white
end method port-default-background;

define method port-default-text-style
    (_port :: <capi-port>, sheet :: <sheet>)
 => (text-style :: false-or(<text-style>))
  #f	// use the default
end method port-default-text-style;


/// Pen and brush support

define method update-drawing-state
    (medium :: <capi-medium>, #key font, brush, pen) => (drawable)
  let drawable = medium-drawable(medium);
  let rep = ensure-representation(drawable);
  when (rep & ~instance?(rep, <list>)
	& representation-visible?(rep))
    let old-cache :: <integer> = medium-drawing-state-cache(medium);
    let new-cache :: <integer> = 0;
    let gs = graphics-port-graphics-state(drawable);
    when (old-cache ~= $medium-fully-cached)
      when (zero?(logand(old-cache, $medium-brush-cached)))
	let brush = brush | medium-brush(medium);
	establish-brush(medium, brush, drawable, gs);
	new-cache := logior(new-cache, $medium-brush-cached)
      end;
      when (zero?(logand(old-cache, $medium-pen-cached)))
	let pen = pen | medium-pen(medium);
	establish-pen(medium, pen, drawable, gs);
	new-cache := logior(new-cache, $medium-pen-cached)
      end;
      when (font & zero?(logand(old-cache, $medium-font-cached)))
	establish-font(medium, font, drawable, gs);
	new-cache := logior(new-cache, $medium-font-cached)
      end;
      new-cache := logior(new-cache, $medium-region-cached);
      medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
    end;
    drawable
  end
end method update-drawing-state;

//--- This should set the tile-x/y to (0,0) if the brush is a stipple, or it
//--- should align it to the left/top of the figure if the brush is a pattern
define method establish-brush (medium :: <capi-medium>, brush, drawable, gs) => ()
  let (pixel, fill-style, operation, pattern)
    = convert-ink-to-capi-components(medium, drawable, brush);
  graphics-state-foreground(gs) := pixel;
  graphics-state-fill-style(gs) := fill-style;
  graphics-state-operation(gs)  := operation;
  graphics-state-pattern(gs)    := pattern | #();	// i.e., Lisp NIL
  graphics-state-mask(gs) := compute-clip-mask(medium)
end method establish-brush;

define method establish-pen (medium :: <capi-medium>, pen, drawable, gs) => ()
  let width 
    = begin
	let width = pen-width(pen);
	when (pen-units(pen) == #"point")
	  width := width * display-pixels-per-point(display(medium))
	end;
	if (width < 2) 0 else truncate(width) end
      end;
  let dashes = pen-dashes(pen);
  let (dashed?, dash)
    = select (dashes by instance?)
	singleton(#f) => values(#f, #f);
	singleton(#t) => values(#t, #[4,4]);
	<vector>      => values(#t, dashes);
	<list>        => values(#t, as(<simple-object-vector>, dashes));
      end;
  let cap = select (pen-cap-shape(pen))
	      #"butt" => #"butt";
	      #"square" => #"projecting";
	      #"round" => #"round";
	      #"no-end-point" => #"not-last";
	    end;
  let joint = select (pen-joint-shape(pen))
		#"miter" => #"miter";
		#"none" => #"miter";
		#"bevel" => #"bevel";
		#"round" => #"round";
	      end;
  graphics-state-thickness(gs) := width;
  graphics-state-dashed(gs)    := dashed? | #();	// i.e., Lisp NIL
  when (dashed?)
    graphics-state-dash(gs)    := dash | #();	// i.e., Lisp NIL
  end;
  graphics-state-line-end-style(gs)   := cap;
  graphics-state-line-joint-style(gs) := joint
end method establish-pen;

define method establish-font (medium :: <capi-medium>, font, drawable, gs) => ()
  graphics-state-font(gs) := font
end method establish-brush;


define method compute-clip-mask (medium :: <capi-medium>) => (mask)
  #"none"	//---*** Hack until this works a little better!
  | medium.%clip-mask
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


define generic convert-ink-to-capi-components
    (medium :: <capi-medium>, drawable, brush)
     => (pixel, fill-style, operation, pattern);

define method convert-ink-to-capi-components 
    (medium :: <capi-medium>, drawable, brush :: <foreground>)
  convert-ink-to-capi-components(medium, drawable, medium-foreground(medium))
end method convert-ink-to-capi-components;

define method convert-ink-to-capi-components 
    (medium :: <capi-medium>, drawable, brush :: <background>)
  convert-ink-to-capi-components(medium, drawable, medium-background(medium))
end method convert-ink-to-capi-components;

define method convert-ink-to-capi-components 
    (medium :: <capi-medium>, drawable, brush :: <color>)
  values(drawable-color->capi-color(drawable, brush), #"solid", $boole-1, #f)
end method convert-ink-to-capi-components;

define method convert-ink-to-capi-components
    (medium :: <capi-medium>, drawable, brush :: <contrasting-color>)
  convert-ink-to-capi-components(medium, contrasting-color->color(brush))
end method convert-ink-to-capi-components;

define method as-generic-image (representation, pixarray) => (image)
  case
    gp-generic-image-p(pixarray) => pixarray;
    otherwise => pixarray-to-generic-image(representation, pixarray);
  end
end method as-generic-image;

define method convert-ink-to-capi-components
    (medium :: <capi-medium>, drawable, brush :: <stencil>)
  let cache = drawable.%ink-cache;
  let rep = ensure-representation(drawable);
  let pattern
    = gethash(cache, brush)
      | begin
	  let (array, colors) = decode-pattern(brush);
	  let width  = image-width(brush);
	  let height = image-height(brush);
	  let ncolors :: <integer> = size(colors);
	  let pixels :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
	  let pixarray = make-pixarray(representation(port(medium).%screen),
				       width, height);
	  for (n :: <integer> from 0 below ncolors)
	    let pixel = convert-ink-to-capi-components(medium, drawable, colors[n]);
	    pixels[n] := pixel
	  end;
	  for (y :: <integer> from 0 below height)
	    for (x :: <integer> from 0 below width)
	      pixarray[y,x] := pixels[array[y,x]]
	    end
	  end;
	  let image = pixarray-to-generic-image(rep, pixarray);
	  gethash(cache, brush) := image;
	  image
	end;
  //--- #"white" and white: are different things in the emulator...
  values(convert-color(rep, white:), #"solid", $boole-1, pattern)
end method convert-ink-to-capi-components;

define method convert-ink-to-capi-components
    (medium :: <capi-medium>, drawable, brush :: <pixmap>)
  let cache = drawable.%ink-cache;
  let rep = ensure-representation(drawable);
  let pattern
    = gethash(cache, brush)
      | begin
	  let pixmap = brush.%pixmap;		// it's a CAPI pixmap, right?
	  let image = as-generic-image(rep, pixmap);
	  gethash(cache, brush) := image;
	  image
	end;
  //--- #"white" and white: are different things in the emulator...
  values(convert-color(rep, white:), #"solid", $boole-1, pattern)
end method convert-ink-to-capi-components;

define method convert-ink-to-capi-components
    (medium :: <capi-medium>, drawable, brush :: <standard-brush>)
  let (pixel, fill-style, operation, pattern)
    = case
	brush-tile(brush) =>
	  convert-ink-to-capi-components(medium, drawable, brush-tile(brush));
	brush-stipple(brush) =>
	  convert-ink-to-capi-components(medium, drawable, brush-stipple(brush));
	otherwise =>
	  convert-ink-to-capi-components(medium, drawable, brush-foreground(brush));
      end;
  ignore(operation);
  values(pixel, fill-style, brush-mode(brush), pattern)
end method convert-ink-to-capi-components;


/// Figure graphics

define sealed method draw-point
    (medium :: <capi-medium>, x, y) => (record)
  let drawable = update-drawing-state(medium);
  when (drawable)
    let transform = medium-device-transform(medium);
    with-device-coordinates (transform, x, y)
      let width = pen-width(medium-pen(medium));
      if (width < 2)
	gp-draw-point(drawable, x, y)
      else
	let width = truncate/(width, 2);
	gp-draw-arc(drawable, x, y, width, width, 0, $2pi, filled: #t)
      end
    end
  end;
  #f
end method draw-point;

define sealed method draw-points
    (medium :: <capi-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let drawable = update-drawing-state(medium);
  when (drawable)
    let transform = medium-device-transform(medium);
    let width = pen-width(medium-pen(medium));
    if (width < 2)
      let coords :: <simple-object-vector> = make(<simple-vector>, size: size(coord-seq));
      let i :: <integer> = 0;
      do-coordinates
	(method (x, y)
	   with-device-coordinates (transform, x, y)
	     coords[i + 0] := x;
	     coords[i + 1] := y;
	     inc!(i, 2)
	   end
	 end,
	 coord-seq);
      gp-draw-points(drawable, coords)
    else
      do-coordinates
	(method (x, y)
	   with-device-coordinates (transform, x, y)
	     let width = truncate/(width, 2);
	     gp-draw-arc(drawable, x, y, width, width, 0, $2pi, filled: #t)
	   end
	 end,
	 coord-seq)
    end
  end;
  #f
end method draw-points;

define sealed method draw-line
    (medium :: <capi-medium>, x1, y1, x2, y2) => (record)
  let drawable = update-drawing-state(medium);
  when (drawable)
    let transform = medium-device-transform(medium);
    with-device-coordinates (transform, x1, y1, x2, y2)
      gp-draw-line(drawable, x1, y1, x2, y2)
    end
  end;
  #f
end method draw-line;

define sealed method draw-lines
    (medium :: <capi-medium>, coord-seq :: <coordinate-sequence>) => (record)
  let drawable = update-drawing-state(medium);
  when (drawable)
    let transform = medium-device-transform(medium);
    let coords :: <simple-object-vector> = make(<simple-vector>, size: size(coord-seq));
    let i = 0;
    do-endpoint-coordinates
      (method (x1, y1, x2, y2)
	 with-device-coordinates (transform, x1, y1, x2, y2)
	   coords[i + 0] := x1;
	   coords[i + 1] := y1;
	   coords[i + 2] := x2;
	   coords[i + 3] := y2;
	   inc!(i, 4)
	 end
       end,
       coord-seq);
    gp-draw-lines(drawable, coords)
  end;
  #f
end method draw-lines;

define sealed method draw-rectangle
    (medium :: <capi-medium>, x1, y1, x2, y2,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(medium, coords, filled?: filled?, closed?: #t)
    end
  else
    let drawable = update-drawing-state(medium);
    when (drawable)
      let brush = medium-brush(medium);
      with-device-coordinates (transform, x1, y1, x2, y2)
	// Draw the interior of the rectangle only when it's filled
	when (filled?)
	  inc!(x1);
	  inc!(y1)
	end;
	when (x1 > x2)
	  swap!(x1, x2)
	end;
	when (y1 > y2)
	  swap!(y1, y2)
	end;
	let width  = x2 - x1;
	let height = y2 - y1;
	when (~filled? | (width > 0 & height > 0))
	  case
	    filled? & image?(brush) =>
	      unless (width = 0 | height = 0)
		gp-draw-rectangle(drawable, x1, y1, width, height,
				  filled: #t,
				  pat-x: x1, pat-y: y1)
	      end;
	    filled? & brush?(brush) & (brush-tile(brush) | brush-stipple(brush)) =>
	      unless (width = 0 | height = 0)
		gp-draw-rectangle(drawable, x1, y1, width, height,
				  filled: #t,
				  pat-x: brush-ts-x(brush) | x1,
				  pat-y: brush-ts-y(brush) | y1)
	      end;
	    otherwise =>    
	      gp-draw-rectangle(drawable, x1, y1, width, height,
				filled: filled? | #())	// i.e., Lisp NIL
	  end
	end
      end
    end
  end;
  #f
end method draw-rectangle;

define sealed method draw-rectangles
    (medium :: <capi-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let transform = medium-device-transform(medium);
  if (~rectilinear-transform?(transform))
    draw-transformed-rectangles(medium, coord-seq, filled?: filled?)
  else
    let drawable = update-drawing-state(medium);
    when (drawable)
      let coords :: <simple-object-vector> = make(<simple-vector>, size: size(coord-seq));
      let i = 0;
      do-endpoint-coordinates
	(method (x1, y1, x2, y2)
	   with-device-coordinates (transform, x1, y1, x2, y2)
	     coords[i + 0] := x1;
	     coords[i + 1] := y1;
	     coords[i + 2] := x2;
	     coords[i + 3] := y2;
	     inc!(i, 4)
	   end
	 end,
	 coord-seq);
      gp-draw-rectangles(drawable, coords,
			 filled: filled? | #())	// i.e., Lisp NIL
    end
  end;
  #f
end method draw-rectangles;

define method draw-transformed-rectangles
    (medium :: <capi-medium>, coord-seq :: <coordinate-sequence>,
     #rest keys, #key filled? = #t) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  let len = size(coord-seq);
  assert(zero?(modulo(len, 4)),
	 "The coordinate sequence hsd ther wrong number of elements");
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

//--- Do this
define sealed method draw-rounded-rectangle
    (medium :: <capi-medium>, x1, y1, x2, y2,
     #key filled? = #t, radius) => (record)
  draw-rectangle(medium, x1, y1, x2, y2, filled?: filled?);
  #f
end method draw-rounded-rectangle;

define sealed method draw-polygon
    (medium :: <capi-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  let drawable = update-drawing-state(medium);
  when (drawable)
    let transform = medium-device-transform(medium);
    let length = size(coord-seq);
    let add-end-points? = closed? & ~filled?;
    with-stack-object (points :: <simple-object-vector>,
		       size: if (add-end-points?) length + 2 else length end)
      replace-subsequence!(points, coord-seq, end: size(coord-seq));
      for (i :: <integer> = 0 then i + 2,
	   until: i >= length)
	let x = points[i];
	let y = points[i + 1];
	with-device-coordinates (transform, x, y)
	  points[i + 0] := x;
	  points[i + 1] := y
	end
      end;
      if (add-end-points?)
	points[length] := points[0];
	points[length + 1] := points[1]
      end;
      //--- Yuck... GP only accepts lists.
      gp-draw-polygon(drawable, as(<list>, points),
		      filled: filled? | #())	// i.e., Lisp NIL
    end
  end;
  #f
end method draw-polygon;

define sealed method draw-ellipse
    (medium :: <capi-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  let drawable = update-drawing-state(medium);
  when (drawable)
    let transform = medium-device-transform(medium);
    with-device-coordinates (transform, center-x, center-y)
      with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
	if (start-angle & end-angle)
	  when (reflection-transform?(transform))
	    swap!(start-angle, end-angle)
	  end;
	  let (start-dx, start-dy)
	    = transform-distance(transform, cos(start-angle), sin(start-angle));
	  let (end-dx, end-dy)
	    = transform-distance(transform, cos(end-angle), sin(end-angle));
	  start-angle := atan2(start-dy, start-dx);
	  end-angle   := atan2(end-dy, end-dx);
	  start-angle := modulo(start-angle, $2pi);
	  end-angle   := modulo(end-angle, $2pi);
	  when (end-angle < start-angle)
	    end-angle := end-angle + $2pi
	  end
	else 
	  start-angle := start-angle | 0.0;
	  end-angle   := end-angle   | $2pi
	end;
	// Convert to X conventions
	end-angle := start-angle - end-angle;
	start-angle := $2pi - start-angle;
	let (angle-2, x-radius, y-radius, angle-1)
	  = singular-value-decomposition-2x2(radius-1-dx, radius-2-dx, radius-1-dy, radius-2-dy);
	if (#t					//---*** remove when tilted ellipses work
	    | x-radius = abs(y-radius)		// a circle - rotations are irrelevant
	    | zero?(angle-1))			// axis-aligned ellipse
	  x-radius := abs(x-radius);
	  y-radius := abs(y-radius);
	  fix-coordinates!(x-radius, y-radius);
	  gp-draw-arc(drawable,
		      center-x - x-radius, center-y - y-radius,
		      x-radius * 2, y-radius * 2,
		      start-angle, end-angle,
		      filled: filled? | #())	// i.e., Lisp NIL
	else
	  #f					//---*** do tilted ellipses here
	end
      end
    end
  end;
  #f
end method draw-ellipse;

define sealed method draw-image
    (medium :: <capi-medium>, image :: <image>, x, y) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    // Decode image brushes by hand for better performance
    let drawable = medium-drawable(medium);
    when (drawable)
      let rep = ensure-representation(drawable);
      when (rep & ~instance?(rep, <list>)
	    & representation-visible?(rep))
	let gs = graphics-port-graphics-state(drawable);
	let (pixel, fill-style, operation, pattern)
	  = convert-ink-to-capi-components(medium, drawable, image);
	let old-pixel = graphics-state-foreground(gs);
	let old-fill  = graphics-state-fill-style(gs);
	let old-op    = graphics-state-operation(gs);
	let old-pat   = graphics-state-pattern(gs);
	let old-mask  = graphics-state-mask(gs);
	graphics-state-foreground(gs) := pixel;
	graphics-state-fill-style(gs) := fill-style;
	graphics-state-operation(gs)  := operation;
	graphics-state-pattern(gs)    := pattern | #();	// i.e., Lisp NIL
	graphics-state-mask(gs)       := #"none";	//--- kludge
	let width  = image-width(image);
	let height = image-height(image);
	gp-draw-rectangle(drawable, x, y, width, height,
			  filled: #t,
			  pat-x: x, pat-y: y);
	graphics-state-foreground(gs) := old-pixel;
	graphics-state-fill-style(gs) := old-fill;
	graphics-state-operation(gs)  := old-op;
	graphics-state-pattern(gs)    := old-pat;
	graphics-state-mask(gs)       := old-mask
      end
    end
  end;
  #f
end method draw-image;


/// Pixel drawing

define sealed method set-pixel
    (medium :: <capi-medium>, color :: <rgb-color>, x, y) => (record)
  let transform = medium-device-transform(medium);
  let drawable  = medium-drawable(medium);
  let gs        = graphics-port-graphics-state(drawable);
  let old-pixel     = graphics-state-foreground(gs);
  let old-fill      = graphics-state-fill-style(gs);
  let old-operation = graphics-state-operation(gs);
  let old-pattern   = graphics-state-pattern(gs);
  graphics-state-foreground(gs) := drawable-color->capi-color(drawable, color);
  graphics-state-fill-style(gs) := #"solid";
  graphics-state-operation(gs)  := $boole-1;
  graphics-state-pattern(gs)    := #();		// i.e., Lisp NIL
  with-device-coordinates (transform, x, y)
    gp-set-pixel(drawable, x, y)
  end;
  graphics-state-foreground(gs) := old-pixel;
  graphics-state-fill-style(gs) := old-fill;
  graphics-state-operation(gs)  := old-operation;
  graphics-state-pattern(gs)    := old-pattern;
  #f
end method set-pixel;

define sealed method set-pixels
    (medium :: <capi-medium>, color :: <rgb-color>, coord-seq :: <coordinate-sequence>)
 => (record)
  let transform = medium-device-transform(medium);
  let drawable  = medium-drawable(medium);
  let gs        = graphics-port-graphics-state(drawable);
  let old-pixel     = graphics-state-foreground(gs);
  let old-fill      = graphics-state-fill-style(gs);
  let old-operation = graphics-state-operation(gs);
  let old-pattern   = graphics-state-pattern(gs);
  graphics-state-foreground(gs) := drawable-color->capi-color(drawable, color);
  graphics-state-fill-style(gs) := #"solid";
  graphics-state-operation(gs)  := $boole-1;
  graphics-state-pattern(gs)    := #();		// i.e., Lisp NIL
  let coords :: <simple-object-vector> = make(<simple-vector>, size: size(coord-seq));
  let i :: <integer> = 0;
  do-coordinates
    (method (x, y)
       with-device-coordinates (transform, x, y)
	 coords[i + 0] := x;
	 coords[i + 1] := y;
	 inc!(i, 2)
       end
     end,
     coord-seq);
  gp-draw-points(drawable, coords);
  graphics-state-foreground(gs) := old-pixel;
  graphics-state-fill-style(gs) := old-fill;
  graphics-state-operation(gs)  := old-operation;
  graphics-state-pattern(gs)    := old-pattern;
  #f
end method set-pixels;


/// Path graphics

define method start-path (medium :: <capi-medium>) => (record)
  //--- start the path
  #f
end method start-path;

define method end-path (medium :: <capi-medium>) => (record)
  //--- end the path
  #f
end method end-path;

define method abort-path (medium :: <capi-medium>) => (record)
  //--- abort the path
  #f
end method abort-path;

define method close-path (medium :: <capi-medium>) => (record)
  //--- close the path
  #f
end method close-path;

define method stroke-path (medium :: <capi-medium>, #key filled?) => (record)
  let drawable = update-drawing-state(medium);
  //--- stroke the path
  #f
end method stroke-path;

define method fill-path (medium :: <capi-medium>) => (record)
  let drawable = update-drawing-state(medium);
  //--- fill the path
  #f
end method fill-path;

define method clip-from-path
    (medium :: <capi-medium>, #key function = $boole-and) => (record)
  //--- clip from the path
  #f
end method clip-from-path;

define method save-clipping-region (medium :: <capi-medium>) => (record)
  //--- push the clip region
  #f
end method save-clipping-region;

define method restore-clipping-region (medium :: <capi-medium>) => (record)
  //--- pop the clip region
  #f
end method restore-clipping-region;

define method move-to (medium :: <capi-medium>, x, y) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    //--- move-to
  end;
  #f
end method move-to;

define method line-to (medium :: <capi-medium>, x, y) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    //--- line-to
  end;
  #f
end method line-to;

define method arc-to (medium :: <capi-medium>, center-x, center-y,
			 radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
			 #key start-angle, end-angle) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, center-x, center-y)
    with-device-distances (transform, radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
      //--- arc-to
    end
  end;
  #f
end method arc-to;

define method curve-to (medium :: <capi-medium>, x1, y1, x2, y2, x3, y3) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x1, y1, x2, y2, x3, y3)
    //--- curve-to
  end;
  #f
end method curve-to;


/// 'draw-pixmap'

define sealed method draw-pixmap
    (medium :: <capi-medium>, pixmap :: <pixmap>, x, y,
     #key function = $boole-1) => (record)
  // Coordinates will get transformed in 'copy-area'
  copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
	    medium, x, y, function: function);
  #f
end method draw-pixmap;


define sealed method clear-box
    (medium :: <capi-medium>, left, top, right, bottom) => ()
  dynamic-bind (medium-brush(medium) = medium-background(medium),
		medium-transform(medium) = $identity-transform)
    // The -1/+1 is to account for the fact that drawing a filled
    // rectangle draws the interior of the rectangle, but we want
    // to clear the entire region
    draw-rectangle(medium, left - 1, top - 1, right + 1, bottom + 1,
		   filled?: #t)
  end
end method clear-box;


/// Text drawing

define sealed method draw-text
    (medium :: <capi-medium>, character :: <character>, x, y,
     #key align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let text-style = medium-merged-text-style(medium);
  let font = text-style-mapping(port(medium), text-style);
  let drawable = update-drawing-state(medium, font: font);
  when (drawable)
    let transform = medium-device-transform(medium);
    with-device-coordinates (transform, x, y)
      when (towards-x)
	convert-to-device-coordinates!(transform, towards-x, towards-y)
      end;
      let (x-adjust, y-adjust)
	= compute-text-adjustment(medium, string, text-style, align-x, align-y);
      inc!(x, floor(x-adjust));
      inc!(y, floor(y-adjust));
      when (towards-x)
	inc!(towards-x, floor(x-adjust));
	inc!(towards-y, floor(y-adjust))
      end;
      gp-draw-char(drawable, character, x, y)
    end
  end;
  #f
end method draw-text;

define sealed method draw-text
    (medium :: <capi-medium>, string :: <string>, x, y,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
          align-x = #"left", align-y = #"baseline", do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let text-style = medium-merged-text-style(medium);
  let font = text-style-mapping(port(medium), text-style);
  let drawable = update-drawing-state(medium, font: font);
  when (drawable)
    let transform = medium-device-transform(medium);
    with-device-coordinates (transform, x, y)
      when (towards-x)
	convert-to-device-coordinates!(transform, towards-x, towards-y)
      end;
      let (x-adjust, y-adjust)
	= compute-text-adjustment(medium, string, text-style, align-x, align-y,
				  start: _start, end: _end, do-tabs?: do-tabs?);
      inc!(x, floor(x-adjust));
      inc!(y, floor(y-adjust));
      when (towards-x)
	inc!(towards-x, floor(x-adjust));
	inc!(towards-y, floor(y-adjust))
      end;
      if (do-tabs?)
        let tab-width  = text-size(medium, " ") * 8;
	let tab-origin = if (do-tabs? == #t) x else do-tabs? end;
        let x = 0;
        let s = _start;
        block (break)
          while (#t)
	    let e = find-character(string, '\t', start: s, end: _end);
	    gp-draw-string(drawable, string, tab-origin + x, y, start: s, end: e);
	    if (e = _end)
	      break()
	    else
	      let (x1, y1, x2, y2) = get-string-extent(drawable, string, font, s, e);
	      ignore(x1, y1, y2);
	      x := floor/(x + x2 + tab-width, tab-width) * tab-width;
	      s := min(e + 1, _end)
	    end
	  end
	end
      else
	gp-draw-string(drawable, string, x, y, start: _start, end: _end)
      end
    end
  end;
  #f
end method draw-text;


define method text-size
    (_port :: <capi-port>, char :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <integer>, total-height :: <integer>,
     last-x :: <integer>, last-y :: <integer>, baseline :: <integer>)
  ignore(_start, _end, do-newlines?);
  let drawable = _port.%window;
  let (font, width, height, ascent, descent)
    = font-metrics(text-style, _port);
  ignore(width, height);
  if (do-tabs? & char == '\t')
    let escapement-x = get-char-width(drawable, ' ', font);
    let escapement-y = 0;	
    let bb-x = escapement-x;
    let bb-y = ascent + descent;
    values(bb-x * 8, bb-y, escapement-x * 8, escapement-y, ascent)
  else
    let escapement-x = get-char-width(drawable, char, font);
    let escapement-y = 0;	
    let bb-x = escapement-x;
    let bb-y = ascent + descent;
    values(bb-x, bb-y, escapement-x, escapement-y, ascent)
  end
end method text-size;

define method text-size
    (_port :: <capi-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
          do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <integer>, total-height :: <integer>,
     last-x :: <integer>, last-y :: <integer>, baseline :: <integer>)
  let drawable = _port.%window;
  let (font, width, height, ascent, descent)
    = font-metrics(text-style, _port);
  ignore(height, descent);
  case
    do-tabs? & do-newlines? =>
      next-method();
    do-tabs? =>
      let tab-width = width * 8;
      let last-x = 0;
      let last-y = 0;
      let s = _start;
      block (return)
	while (#t)
	  let e = find-character(string, '\t', start: s, end: _end);
	  let (x1, y1, x2, y2) = get-string-extent(drawable, string, font, s, e);
	  ignore(x1);
	  if (e = _end)
	    last-x := last-x + x2
	  else
	    last-x := floor/(last-x + x2 + tab-width, tab-width) * tab-width;
	  end;
	  max!(last-y, y2 - y1);
	  s := min(e + 1, _end);
	  when (e = _end)
	    return(last-x, last-y, last-x, last-y, ascent)
	  end
	end
      end;
    do-newlines? =>
      let largest-x = 0;
      let largest-y = 0;
      let last-x = 0;
      let last-y = 0;
      let s = _start;
      block (return)
	while (#t)
	  let e = find-character(string, '\n', start: s, end: _end);
	  let (x1, y1, x2, y2) = get-string-extent(drawable, string, font, s, e);
	  ignore(x1);
	  max!(largest-x, x2);
	  last-x := x2;
	  inc!(largest-y, y2 - y1);
	  last-y := y2;
	  s := min(e + 1, _end);
	  when (e = _end)
	    return(largest-x, largest-y, last-x, last-y, ascent)
	  end
	end
      end;
    otherwise =>
      let (x1, y1, x2, y2) = get-string-extent(drawable, string, font, _start, _end);
      ignore(x1);
      values(x2, y2 - y1, x2, y2 - y1, ascent);
  end
end method text-size;


/// Font metrics -- from text styles

define method font-metrics
    (text-style :: <text-style>, _port :: <capi-port>, #key character-set)
 => (font, width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  let font = text-style-mapping(_port, text-style);
  let window = _port.%window;
  values(font,
	 get-font-width(window, font),
	 get-font-height(window, font),
	 get-font-ascent(window, font),
	 get-font-descent(window, font))
end method font-height;

define method font-width
    (text-style :: <text-style>, _port :: <capi-port>, #key character-set)
 => (width :: <integer>)
  let font = text-style-mapping(_port, text-style);
  let window = _port.%window;
  get-font-width(window, font)
end method font-width;

define method font-height
    (text-style :: <text-style>, _port :: <capi-port>, #key character-set)
 => (height :: <integer>)
  let font = text-style-mapping(_port, text-style);
  let window = _port.%window;
  get-font-height(window, font)
end method font-height;

define method font-ascent
    (text-style :: <text-style>, _port :: <capi-port>, #key character-set)
 => (ascent :: <integer>)
  let font = text-style-mapping(_port, text-style);
  let window = _port.%window;
  get-font-ascent(window, font)
end method font-ascent;

define method font-descent
    (text-style :: <text-style>, _port :: <capi-port>, #key character-set)
 => (descent :: <integer>)
  let font = text-style-mapping(_port, text-style);
  let window = _port.%window;
  get-font-descent(window, font)
end method font-descent;

define method fixed-width-font?
    (text-style :: <text-style>, _port :: <capi-port>, #key character-set)
 => (true? :: <boolean>)
  let font = text-style-mapping(_port, text-style);
  let window = _port.%window;
  lisp-true?(font-fixed-width-p(window, font))
end method fixed-width-font?;
