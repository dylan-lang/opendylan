Module:       carbon-duim
Synopsis:     Macintosh medium implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MAC mediums

//---*** What should this be?
define constant <native-color> = <integer>;

define sealed class <carbon-medium> (<basic-medium>)
  sealed slot %ink-cache :: <object-table> = make(<table>);
  sealed slot %palette = #f;		//---*** MAYBE JUST USE THE FRAME'S PALETTE?
  sealed slot %foreground-color /* :: false-or(<GdkColor*>) */ = #f;
  sealed slot %background-color /* :: false-or(<GdkColor*>) */ = #f;
  // Cached clipping region
  sealed slot %clip-mask = #f;		// #f, #"none", or an X region
end class <carbon-medium>;

define sealed domain make (singleton(<carbon-medium>));
define sealed domain initialize (<carbon-medium>);

// This is intentionally _not_ sealed
define method make-medium
    (_port :: <carbon-port>, sheet :: <sheet>)
 => (medium :: <carbon-medium>)
  make(<carbon-medium>, port: _port, sheet: sheet)
end method make-medium;

define method clear-ink-cache (medium :: <carbon-medium>)
  //--- Should we clear the ink cache from the palette?
end method clear-ink-cache;

define sealed method destroy-medium
    (medium :: <carbon-medium>) => ()
  clear-ink-cache(medium);
  next-method();
  medium-drawable(medium) := #f
end method destroy-medium;

define sealed method do-attach-medium
    (sheet :: <sheet>, medium :: <carbon-medium>) => ()
  let _port  = port(sheet);
  let mirror = sheet-mirror(sheet);
  assert(mirror,
	 "Unexpected failure: no mirror when attaching medium for %=",
	 sheet);
  debug-message("Attaching medium to %= (medium-sheet %=)",
		sheet, medium-sheet(medium));
  /*---*** Not yet implemented!
  let window = mirror.mirror-window;
  let drawable = pointer-cast(<GdkDrawable*>, window.window-value);
  clear-ink-cache(medium);
  medium-drawable(medium) := drawable;
  */
  /*---*** More stuff that's not implemented yet!
  // Set up the palette and fg/bg pixels
  let control  = mirror-control(mirror);
  let palette = port-default-palette(_port);
  let fg = medium-foreground(medium);
  let bg = medium-background(medium);
  */
  /*---*** Colors not implemented yet!
  medium.%palette := palette;
  medium.%foreground-color
    := case
	 fg => allocate-color(fg, palette);
	   //---*** Fill this in!
	 otherwise => error("Can't get default foreground pixel!");
       end;
  medium.%background-color
    := case
	 fg => allocate-color(fg, palette);
	   //---*** Fill this in!
	 otherwise => error("Can't get default background pixel!");
       end;
  */
end method do-attach-medium;

define sealed method do-detach-medium
    (sheet :: <sheet>, medium :: <carbon-medium>) => ()
  //---*** So which is it, 'clear-ink-cache' or 'clear-colors'?
  clear-ink-cache(medium);
  // clear-colors(medium.%palette);
  medium-drawable(medium) := #f
end method do-detach-medium;

define sealed method deallocate-medium
    (_port :: <carbon-port>, medium :: <carbon-medium>) => ()
  next-method();
  medium.%palette := #f;
  medium-drawable(medium) := #f
end method deallocate-medium;

define sealed method medium-foreground-setter
    (foreground :: <color>, medium :: <carbon-medium>)
 => (foreground :: <color>)
  next-method();	// also sets 'medium-drawing-state-cache' to 0
  not-yet-implemented("medium-foreground-setter");
  /*---*** Colors not implemented yet!
  clear-ink-cache(medium);
  let color = allocate-color(foreground, medium.%palette);
  medium.%foreground-color := color;
  queue-repaint(medium-sheet(medium), $everywhere);
  foreground
  */
end method medium-foreground-setter;

define sealed method medium-background-setter
    (background :: <color>, medium :: <carbon-medium>)
 => (background :: <color>)
  next-method();	// also sets 'medium-drawing-state-cache' to 0
  not-yet-implemented("medium-foreground-setter");
  /*---*** Colors not implemented yet!
  clear-ink-cache(medium);
  let color = allocate-color(background, medium.%palette);
  medium.%background-color := color;
  queue-repaint(medium-sheet(medium), $everywhere);
  background
  */
end method medium-background-setter;

define sealed method medium-clipping-region-setter
    (region :: <region>, medium :: <carbon-medium>) => (region :: <region>)
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
    (medium :: <carbon-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-region;

define sealed method invalidate-cached-transform
    (medium :: <carbon-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-transform;

define sealed method invalidate-cached-drawing-state
    (medium :: <carbon-medium>, cached-state :: <integer>) => ()
  ignore(cached-state);
  #f
end method invalidate-cached-drawing-state;


/// Display forcing

define sealed method force-display
    (medium :: <carbon-medium>) => ()
  ignoring("force-display")
end method force-display;

define sealed method synchronize-display
    (medium :: <carbon-medium>) => ()
  ignoring("synchronize-display")
end method synchronize-display;


/// Drawing state updating

// Note that the brush defaults to 'medium-brush(medium)',
// but the pen and font default to #f in order to avoid unnecessary work
define sealed method update-drawing-state
    (medium :: <carbon-medium>, #key brush, pen, font) => ()
  let window = medium-drawable(medium);
  SetPort(window);
  ignoring("update-drawing-state");
  /*
  let old-cache :: <integer> = medium-drawing-state-cache(medium);
  let new-cache :: <integer> = 0;
  when (old-cache ~= $medium-fully-cached)
    // Establish a brush, unless it's already cached
    when (zero?(logand(old-cache, $medium-brush-cached)))  
      let brush = brush | medium-brush(medium);
      establish-brush(medium, brush);
      new-cache := logior(new-cache, $medium-brush-cached)
    end;
    // Establish a pen, unless it's already cached
    when (zero?(logand(old-cache, $medium-pen-cached)))
      let pen = pen | medium-pen(medium);
      establish-pen(medium, pen);
      new-cache := logior(new-cache, $medium-pen-cached)
    end;
    // Establish a font only if requested, unless it's already cached
    when (font & zero?(logand(old-cache, $medium-font-cached)))
      establish-font(medium, font);
      new-cache := logior(new-cache, $medium-font-cached)
    end;
    unless (medium.%clip-mask)
      //---*** Fill this in!
      ignoring("clip-mask in update-drawing-state");
      let mask = compute-clip-mask(medium);
      if (mask == #"none")
	//---*** Clear the mask!
      else
	let (x, y, width, height) = values(mask[0], mask[1], mask[2], mask[3]);
	//---*** Set the mask!
      end;
      new-cache := logior(new-cache, $medium-region-cached)
    end;
    medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
  end;
  */
end method update-drawing-state;


define sealed method establish-brush
    (medium :: <carbon-medium>, brush :: <standard-brush>)
 => ()
  ignoring("establish-brush for <standard-brush>")
end method establish-brush;

define sealed method establish-brush
    (medium :: <carbon-medium>, color :: <rgb-color>) => ()
  ignoring("establish-brush");
  /*---*** Colors not implemented yet!
  with-stack-structure (MacColor :: <RGBColor*>)
    let (red, green, blue) = color-rgb(color);
    MacColor.red-value   := red;
    MacColor.green-value := green;
    MacColor.blue-value  := blue;
    RGBForeColor(MacColor)
  end
  */
end method establish-brush;

define sealed method establish-brush
    (medium :: <carbon-medium>, color :: <contrasting-color>) => ()
  ignoring("establish-brush for <contrasting-color>")
end method establish-brush;

define sealed method establish-brush
    (medium :: <carbon-medium>, brush :: <foreground>) => ()
  ignoring("establish-brush");
end method establish-brush;

define sealed method establish-brush
    (medium :: <carbon-medium>, brush :: <background>) => ()
  ignoring("establish-brush");
end method establish-brush;


define sealed method establish-pen 
    (medium :: <carbon-medium>, pen :: <standard-pen>) => ()
  ignoring("establish-pen")
end method establish-pen;


define sealed method establish-font
    (medium :: <carbon-medium>, font :: <carbon-font>) => ()
  ignoring("establish-font");
end method establish-font;


/// Ink decoding

define generic decode-ink
    (medium :: <carbon-medium>, brush)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>));

define sealed method decode-ink 
    (medium :: <carbon-medium>, brush :: <foreground>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  decode-ink(medium, medium-foreground(medium))
end method decode-ink;

define sealed method decode-ink 
    (medium :: <carbon-medium>, brush :: <background>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  decode-ink(medium, medium-background(medium))
end method decode-ink;

define sealed method decode-ink 
    (medium :: <carbon-medium>, color :: <color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  not-yet-implemented("decode-ink for <color>")
end method decode-ink;

define sealed method decode-ink
    (medium :: <carbon-medium>, color :: <contrasting-color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  not-yet-implemented("decode-ink for <contrasting-color>")
end method decode-ink;

define sealed method decode-ink
    (medium :: <carbon-medium>, pattern :: <stencil>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  not-yet-implemented("decode-ink");
  /*---*** Not yet implemented!
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
	      let pixel = decode-ink(medium, colors[n]);
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
  values($native-white, $GDK-SOLID, $boole-1, bitmap)
  */
end method decode-ink;

define sealed method decode-ink
    (medium :: <carbon-medium>, pixmap :: <pixmap>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  not-yet-implemented("decode-ink for <pixmap>")
end method decode-ink;

define sealed method decode-ink
    (medium :: <carbon-medium>, brush :: <standard-brush>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let (color :: <native-color>, fill-style, operation :: <integer>, pattern)
    = decode-ink
        (medium, brush-tile(brush) | brush-stipple(brush) | brush-foreground(brush));
  // ignore(operation);
  values(color, fill-style, brush-mode(brush), pattern)
end method decode-ink;


/// Clipping region decoding

define sealed method compute-clip-mask
    (medium :: <carbon-medium>) => (mask)
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
