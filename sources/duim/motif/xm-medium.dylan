Module:    motif-duim
Synopsis:  Motif medium implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif mediums

define sealed class <motif-medium> (<basic-medium>)
  sealed slot %palette = #f;		//---*** MAYBE JUST USE THE FRAME'S PALETTE?
  sealed slot %foreground-pixel :: <integer> = 0;
  sealed slot %background-pixel :: <integer> = 0;
  // Cached clipping region
  sealed slot %clip-mask = #f;		// #f, #"none", or an X region
end class <motif-medium>;

define sealed domain make (singleton(<motif-medium>));
define sealed domain initialize (<motif-medium>);

// This is intentionally _not_ sealed
define method make-medium
    (_port :: <motif-port>, sheet :: <sheet>)
 => (medium :: <motif-medium>)
  make(<motif-medium>, port: _port, sheet: sheet)
end method make-medium;

define method clear-ink-cache (medium :: <motif-medium>)
  //--- Should we clear the ink cache from the palette?
end method clear-ink-cache;

define sealed method destroy-medium
    (medium :: <motif-medium>) => ()
  clear-ink-cache(medium);
  next-method();
  medium-drawable(medium) := #f
end method destroy-medium;

define sealed method do-attach-medium
    (sheet :: <sheet>, medium :: <motif-medium>) => ()
  let _port  = port(sheet);
  let mirror = sheet-mirror(sheet);
  assert(mirror,
	 "Unexpected failure: no mirror when attaching medium for %=",
	 sheet);
  clear-ink-cache(medium);
  //---*** SHOULD MAYBE BE 'xt-window(widget)'?
  medium-drawable(medium) := mirror;
  // Set up the palette and fg/bg pixels
  let widget  = mirror-widget(mirror);
  let palette = port-default-palette(_port);
  let fg = medium-foreground(medium);
  let bg = medium-background(medium);
  medium.%palette := palette;
  medium.%foreground-pixel
    :=  if (fg) allocate-color(fg, palette) else xt/XtGetValues(widget, #"foreground") end;
  medium.%background-pixel
    :=  if (fg) allocate-color(fg, palette) else xt/XtGetValues(widget, #"background") end;
end method do-attach-medium;

define sealed method do-detach-medium
    (sheet :: <sheet>, medium :: <motif-medium>) => ()
  //---*** So which is it, 'clear-ink-cache' or 'clear-colors'?
  clear-ink-cache(medium);
  clear-colors(medium.%palette);
  medium-drawable(medium) := #f
end method do-detach-medium;

define sealed method deallocate-medium
    (_port :: <motif-port>, medium :: <motif-medium>) => ()
  next-method();
  medium.%palette := #f;
  medium-drawable(medium) := #f
end method deallocate-medium;

define sealed method medium-foreground-setter
    (foreground :: <color>, medium :: <motif-medium>)
 => (foreground :: <color>)
  next-method();	// also sets 'medium-drawing-state-cache' to 0
  clear-ink-cache(medium);
  let pixel = allocate-color(foreground, medium.%palette);
  medium.%foreground-pixel := pixel;
  queue-repaint(medium-sheet(medium), $everywhere);
  foreground
end method medium-foreground-setter;

define sealed method medium-background-setter
    (background :: <color>, medium :: <motif-medium>)
 => (background :: <color>)
  next-method();	// also sets 'medium-drawing-state-cache' to 0
  clear-ink-cache(medium);
  let pixel = allocate-color(background, medium.%palette);
  medium.%background-pixel := pixel;
  queue-repaint(medium-sheet(medium), $everywhere);
  background
end method medium-background-setter;

define sealed method medium-clipping-region-setter
    (region :: <region>, medium :: <motif-medium>) => (region :: <region>)
  next-method();
  // Don't flush the cache if the region isn't really changing.
  // This situation comes up all the time during repainting, when we set
  // the clipping region for every output record, but we almost always
  // just set it to $everywhere.
  unless (region == medium-clipping-region(medium))
    medium.%clip-mask := #f
  end;
  region
end method medium-clipping-region-setter;

define sealed method invalidate-cached-region
    (medium :: <motif-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-region;

define sealed method invalidate-cached-transform
    (medium :: <motif-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-transform;

define sealed method invalidate-cached-drawing-state
    (medium :: <motif-medium>, cached-state :: <integer>) => ()
  ignore(cached-state);
  #f
end method invalidate-cached-drawing-state;


/// Display forcing

define sealed method force-display
    (medium :: <motif-medium>) => ()
  let _port = port(medium);
  x/XFlush(_port.%display)
end method force-display;

define sealed method synchronize-display
    (medium :: <motif-medium>) => ()
  let _port = port(medium);
  x/XSync(_port.%display)
end method synchronize-display;


/// Drawing state updating

define inline method get-gcontext
    (medium :: <motif-medium>)
 => (x-display :: x/<Display>, x-drawable :: x/<Drawable>, gcontext :: x/<GContext>)
  let palette  = medium.%palette;
  let display  = port(medium).%display;
  let drawable = XXX;		//---*** WHERE DO WE GET THE DRAWABLE?
  let gcontext = palette.%gcontext;
  values(display, drawable, gcontext)
end method get-gcontext;

// Note that the brush defaults to 'medium-brush(medium)',
// but the pen and font default to #f in order to avoid unnecessary work
define sealed method update-drawing-state
    (medium :: <motif-medium>, #key brush, pen, font)
 => (x-display :: x/<Display>, x-drawable :: x/<Drawable>, gcontext :: x/<GContext>)
  let (x-display :: x/<Display>, x-drawable :: x/<Drawable>, gcontext :: x/<GContext>)
    = get-gcontext(medium);
  let old-cache :: <integer> = medium-drawing-state-cache(medium);
  let new-cache :: <integer> = 0;
  when (old-cache ~= $medium-fully-cached)
    // Establish a brush, unless it's already cached
    when (zero?(logand(old-cache, $medium-brush-cached)))  
      let brush = brush | medium-brush(medium);
      establish-brush(x-display, medium, brush, gcontext);
      new-cache := logior(new-cache, $medium-brush-cached)
    end;
    // Establish a pen, unless it's already cached
    when (zero?(logand(old-cache, $medium-pen-cached)))
      let pen = pen | medium-pen(medium);
      establish-pen(x-display, medium, pen, gcontext);
      new-cache := logior(new-cache, $medium-pen-cached)
    end;
    // Establish a font only if requested, unless it's already cached
    when (font & zero?(logand(old-cache, $medium-font-cached)))
      establish-font(x-display, medium, font, gcontext);
      new-cache := logior(new-cache, $medium-font-cached)
    end;
    unless (medium.%clip-mask)
      let mask = compute-clip-mask(medium);
      medium.%clip-mask := mask;
      if (mask == #"none")
	x/XSetClipMask(x-display, gcontext, #f)
      else
	let region = xt/XCreateRegion();
	with-stack-structure (rect :: x/<XRectangle>)
	  without-bounds-checks
	    rect.x-value      := mask[0];
	    rect.y-value      := mask[1];
            rect.width-value  := mask[2] - mask[0];
            rect.height-value := mask[3] - mask[1]
	  end;
	  xt/XUnionRectWithRegion(rect, region, region);
	end;
	xt/XSetRegion(x-display, gcontext, region);
	xt/XDestroyRegion(region)
      end;
      new-cache := logior(new-cache, $medium-region-cached)
    end;
    medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
  end;
  values(x-display, x-drawable, gcontext)
end method update-drawing-state;


define constant $function-map :: <simple-object-vector>
    = make(<simple-vector>, size: 16);

begin
  $function-map[$boole-clr]   := x/$GXclear;
  $function-map[$boole-set]   := x/$GXset;
  $function-map[$boole-1]     := x/$GXcopy;
  $function-map[$boole-2]     := x/$GXnoop; 
  $function-map[$boole-c1]    := x/$GXcopy-inverted;
  $function-map[$boole-c2]    := x/$GXinvert;
  $function-map[$boole-and]   := x/$GXand;
  $function-map[$boole-ior]   := x/$GXior;
  $function-map[$boole-xor]   := x/$GXxor;
  $function-map[$boole-eqv]   := x/$GXequiv;
  $function-map[$boole-nand]  := x/$GXnand;
  $function-map[$boole-nor]   := x/$GXnor;
  $function-map[$boole-andc1] := x/$GXand-inverted;
  $function-map[$boole-andc2] := x/$GXand-reverse;
  $function-map[$boole-orc1]  := x/$GXor-inverted;
  $function-map[$boole-orc2]  := x/$GXor-reverse
end;

define sealed method establish-brush
    (x-display :: x/<Display>,
     medium :: <motif-medium>, brush :: <standard-brush>, gcontext :: x/<GContext>) => ()
  //---*** DO THE RIGHT THING
end method establish-brush;

define sealed method establish-brush
    (x-display :: x/<Display>,
     medium :: <motif-medium>, color :: <rgb-color>, gcontext :: x/<GContext>) => ()
  x/XSetFillStyle(x-display, gcontext, x/$FillSolid);
  x/XSetFunction(x-display, gcontext, $function-map[$boole-set]);
  x/XSetForeground(x-display, gcontext, allocate-color(color, medium.%palette))
end method establish-brush;

define sealed method establish-brush
    (x-display :: x/<Display>,
     medium :: <motif-medium>, color :: <contrasting-color>, gcontext :: x/<GContext>) => ()
  establish-brush(x-display, medium, contrasting-color->color(color), gcontext)
end method establish-brush;

define sealed method establish-brush
    (x-display :: x/<Display>,
     medium :: <motif-medium>, brush :: <foreground>, gcontext :: x/<GContext>) => ()
  x/XSetFillStyle(x-display, gcontext, x/$FillSolid);
  x/XSetFunction(x-display, gcontext, x/$GXcopy);
  x/XSetForeground(x-display, gcontext, medium.%foreground-pixel)
end method establish-brush;

define sealed method establish-brush
    (x-display :: x/<Display>,
     medium :: <motif-medium>, brush :: <background>, gcontext :: x/<GContext>) => ()
  x/XSetFillStyle(x-display, gcontext, x/$FillSolid);
  x/XSetFunction(x-display, gcontext, x/$GXcopy);
  x/XSetForeground(x-display, gcontext, medium.%background-pixel)
end method establish-brush;


define sealed method establish-pen 
    (x-display :: x/<Display>,
     medium :: <motif-medium>, pen :: <standard-pen>, gcontext :: x/<GContext>) => ()
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
	<list>        => values(#t, as(<vector>, dashes));
      end;
  let cap
    = select (pen-cap-shape(pen))
	#"butt"         => x/$CapButt;
	#"square"       => x/$CapProjecting;
	#"round"        => x/$CapRound;
	#"no-end-point" => x/$CapNotLast;
      end;
  let joint
    = select (pen-joint-shape(pen))
	#"miter" => x/$JoinMiter;
	#"none"  => x/$JoinMiter;
	#"bevel" => x/$JoinBevel;
	#"round" => x/$JoinRound;
      end;
  x/XSetLineAttributes(x-display, gcontext, 
		       width, if (dashed?) x/$LineOnOffDash else x/$LineSolid end,
		       cap-shape, joint-shape);
  when (dashed?)
    x/XSetDashes(x-display, gcontext, 0, dash)
  end
end method establish-pen;


define sealed method establish-font
    (x-display :: x/<Display>,
     medium :: <motif-medium>, font :: <motif-font>, gcontext :: x/<GContext>) => ()
  x/XSetFont(x-display, gcontext, font.%font-id)
end method establish-font;


/// Ink decoding

define generic decode-ink
    (medium :: <motif-medium>, gcontext :: x/<GContext>, brush)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>));

define sealed method decode-ink 
    (medium :: <motif-medium>, gcontext :: x/<GContext>, brush :: <foreground>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  decode-ink(medium, gcontext, medium-foreground(medium))
end method decode-ink;

define sealed method decode-ink 
    (medium :: <motif-medium>, gcontext :: x/<GContext>, brush :: <background>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  decode-ink(medium, gcontext, medium-background(medium))
end method decode-ink;

define sealed method decode-ink 
    (medium :: <motif-medium>, gcontext :: x/<GContext>, color :: <color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  values(allocate-color(color, medium.%palette), x/$FillSolid, $boole-1, #f)
end method decode-ink;

define sealed method decode-ink
    (medium :: <motif-medium>, gcontext :: x/<GContext>, color :: <contrasting-color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let color = contrasting-color->color(color);
  values(allocate-color(color, medium.%palette), x/$FillSolid, $boole-1, #f)
end method decode-ink;

define sealed method decode-ink
    (medium :: <motif-medium>, gcontext :: x/<GContext>, pattern :: <stencil>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let cache = medium.%ink-cache;
  let bitmap :: false-or(<image>)
    = gethash(cache, pattern)
      | begin
	  let (array, colors) = decode-pattern(pattern);
	  let width   :: <integer> = image-width(pattern);
	  let height  :: <integer> = image-height(pattern);
	  let ncolors :: <integer> = size(colors);
	  //---*** Should we create a DIB here or what?
	  let pixels   :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
	  let pixarray :: <array> = make(<array>, dimensions: list(width, height));
	  without-bounds-checks
	    for (n :: <integer> from 0 below ncolors)
	      let pixel = decode-ink(medium, gcontext, colors[n]);
	      pixels[n] := pixel
	    end;
	    for (y :: <integer> from 0 below height)
	      for (x :: <integer> from 0 below width)
		pixarray[y,x] := pixels[array[y,x]]
	      end
	    end
	  end;
	  //---*** Fill in the DIB
	  let bitmap = list(pixels, pixarray);
	  gethash(cache, pattern) := bitmap;
	  bitmap
	end;
  values($native-white, x/$FillSolid, $boole-1, bitmap)
end method decode-ink;

define sealed method decode-ink
    (medium :: <motif-medium>, gcontext :: x/<GContext>, pixmap :: <pixmap>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  //---*** Implement it!
  not-yet-implemented("decode-ink for <pixmap>")
end method decode-ink;

define sealed method decode-ink
    (medium :: <motif-medium>, gcontext :: x/<GContext>, brush :: <standard-brush>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let (color :: <native-color>, fill-style, operation :: <integer>, pattern)
    = decode-ink
        (medium, gcontext,
	 brush-tile(brush) | brush-stipple(brush) | brush-foreground(brush));
  // ignore(operation);
  values(color, fill-style, brush-mode(brush), pattern)
end method decode-ink;


/// Clipping region decoding

define sealed method compute-clip-mask
    (medium :: <motif-medium>) => (mask)
  let sheet   = medium-sheet(medium);
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
end method compute-clip-mask;
