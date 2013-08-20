Module:       gtk-duim
Synopsis:     GTK medium implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK mediums

//---*** What should this be?
define constant <native-color> = <integer>;

define sealed class <gtk-medium> (<basic-medium>)
  sealed slot %ink-cache :: <object-table> = make(<table>);
  sealed slot %palette = #f;                //---*** MAYBE JUST USE THE FRAME'S PALETTE?
  sealed slot %foreground-color :: false-or(<GdkColor>) = #f;
  sealed slot %background-color :: false-or(<GdkColor>) = #f;
  // Cached clipping region
  sealed slot %clip-mask = #f;                // #f, #"none", or an X region
end class <gtk-medium>;

define sealed domain make (singleton(<gtk-medium>));
define sealed domain initialize (<gtk-medium>);

// This is intentionally _not_ sealed
define method make-medium
    (_port :: <gtk-port>, sheet :: <sheet>)
 => (medium :: <gtk-medium>)
  make(<gtk-medium>, port: _port, sheet: sheet)
end method make-medium;

define method clear-ink-cache (medium :: <gtk-medium>)
  //--- Should we clear the ink cache from the palette?
end method clear-ink-cache;

define sealed method destroy-medium
    (medium :: <gtk-medium>) => ()
  clear-ink-cache(medium);
  next-method();
  medium-drawable(medium) := #f
end method destroy-medium;

define sealed method do-attach-medium
    (sheet :: <sheet>, medium :: <gtk-medium>) => ()
  let _port  = port(sheet);
  let mirror = sheet-mirror(sheet);
  assert(mirror,
         "Unexpected failure: no mirror when attaching medium for %=",
         sheet);
  duim-debug-message("Attaching medium to %= (medium-sheet %=)",
                sheet, medium-sheet(medium));
  let widget = mirror.mirror-widget;
// FIXME
//  let drawable = widget.gtk-widget-get-window;
  clear-ink-cache(medium);
//  medium-drawable(medium) := drawable;
  // Set up the palette and fg/bg pixels
  let widget  = mirror-widget(mirror);
  let palette = port-default-palette(_port);
  let fg = medium-foreground(medium);
  let bg = medium-background(medium);
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
    (sheet :: <sheet>, medium :: <gtk-medium>) => ()
  //---*** So which is it, 'clear-ink-cache' or 'clear-colors'?
  clear-ink-cache(medium);
  // clear-colors(medium.%palette);
  medium-drawable(medium) := #f
end method do-detach-medium;

define sealed method deallocate-medium
    (_port :: <gtk-port>, medium :: <gtk-medium>) => ()
  next-method();
  medium.%palette := #f;
  medium-drawable(medium) := #f
end method deallocate-medium;

define sealed method medium-foreground-setter
    (foreground :: <color>, medium :: <gtk-medium>)
 => (foreground :: <color>)
  next-method();        // also sets 'medium-drawing-state-cache' to 0
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
    (background :: <color>, medium :: <gtk-medium>)
 => (background :: <color>)
  next-method();        // also sets 'medium-drawing-state-cache' to 0
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
    (region :: <region>, medium :: <gtk-medium>) => (region :: <region>)
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
    (medium :: <gtk-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-region;

define sealed method invalidate-cached-transform
    (medium :: <gtk-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-transform;

define sealed method invalidate-cached-drawing-state
    (medium :: <gtk-medium>, cached-state :: <integer>) => ()
  ignore(cached-state);
  #f
end method invalidate-cached-drawing-state;


/// Display forcing

define sealed method force-display
    (medium :: <gtk-medium>) => ()
  gdk-flush()
end method force-display;

define sealed method synchronize-display
    (medium :: <gtk-medium>) => ()
  gdk-flush()
end method synchronize-display;


/// Drawing state updating

define inline method get-gcontext
    (medium :: <gtk-medium>)
 => (gcontext :: <CairoContext>)
  let drawable = medium-drawable(medium);
  unless (drawable)
    let widget = medium.medium-sheet.sheet-mirror.mirror-widget;
    drawable := widget.gtk-widget-get-window;
    assert(~null-pointer?(drawable), "get-gcontext: window must not be null");
    medium-drawable(medium) := drawable;
  end;
  gdk-cairo-create(drawable)
end method get-gcontext;

// Note that the brush defaults to 'medium-brush(medium)',
// but the pen and font default to #f in order to avoid unnecessary work
define sealed method update-drawing-state
    (medium :: <gtk-medium>, #key brush, pen, font)
 => (gcontext :: <CairoContext>)
  let gcontext :: <CairoContext> = get-gcontext(medium);
  ignoring("update-drawing-state");
  let old-cache :: <integer> = medium-drawing-state-cache(medium);
  let new-cache :: <integer> = 0;
  when (old-cache ~= $medium-fully-cached)
    // Establish a brush, unless it's already cached
    when (zero?(logand(old-cache, $medium-brush-cached)))
      let brush = brush | medium-brush(medium);
      establish-brush(medium, brush, gcontext);
      new-cache := logior(new-cache, $medium-brush-cached)
    end;
    /*
    // Establish a pen, unless it's already cached
    when (zero?(logand(old-cache, $medium-pen-cached)))
      let pen = pen | medium-pen(medium);
      establish-pen(medium, pen, gcontext);
      new-cache := logior(new-cache, $medium-pen-cached)
    end;
    // Establish a font only if requested, unless it's already cached
    when (font & zero?(logand(old-cache, $medium-font-cached)))
      establish-font(medium, font, gcontext);
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
    */
    medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
  end;
  gcontext
end method update-drawing-state;


define constant $function-map :: <simple-object-vector>
    = make(<simple-vector>, size: 16);

/*
begin
  $function-map[$boole-clr]   := $GDK-CLEAR;
  $function-map[$boole-set]   := $GDK-SET;
  $function-map[$boole-1]     := $GDK-COPY;
  $function-map[$boole-2]     := $GDK-NOOP;
  $function-map[$boole-c1]    := $GDK-COPY-INVERT;
  $function-map[$boole-c2]    := $GDK-INVERT;
  $function-map[$boole-and]   := $GDK-AND;
  $function-map[$boole-ior]   := $GDK-OR;
  $function-map[$boole-xor]   := $GDK-XOR;
  $function-map[$boole-eqv]   := $GDK-EQUIV;
  $function-map[$boole-nand]  := $GDK-NAND;
  $function-map[$boole-nor]   := $GDK-OR-INVERT;  //---*** What should this be?
  $function-map[$boole-andc1] := $GDK-AND-INVERT;
  $function-map[$boole-andc2] := $GDK-AND-REVERSE;
  $function-map[$boole-orc1]  := $GDK-OR-INVERT;
  $function-map[$boole-orc2]  := $GDK-OR-REVERSE
end;
*/

define sealed method establish-brush
    (medium :: <gtk-medium>, brush :: <standard-brush>, gcontext :: <CairoContext>)
 => ()
  ignoring("establish-brush for <standard-brush>")
end method establish-brush;

define sealed method establish-brush
    (medium :: <gtk-medium>, color :: <rgb-color>, gcontext :: <CairoContext>) => ()
  let (red, green, blue, opacity) = color-rgb(color);
  cairo-set-source-rgba(gcontext,
                        as(<double-float>, red),
                        as(<double-float>, green),
                        as(<double-float>, blue),
                        as(<double-float>, opacity));
  /*---*** Colors not implemented yet!
  gdk-gc-set-fill(gcontext, $GDK-SOLID);
  gdk-gc-set-function(gcontext, $function-map[$boole-set]);
  gdk-gc-set-foreground(gcontext, allocate-color(color, medium.%palette))
  */
end method establish-brush;

define sealed method establish-brush
    (medium :: <gtk-medium>, color :: <contrasting-color>, gcontext :: <CairoContext>) => ()
  ignoring("establish-brush for <contrasting-color>")
end method establish-brush;

define sealed method establish-brush
    (medium :: <gtk-medium>, brush :: <foreground>, gcontext :: <CairoContext>) => ()
  let color = medium-foreground(medium);
  let (red, green, blue, opacity) = color-rgb(color);
  cairo-set-source-rgba(gcontext,
                        as(<double-float>, red),
                        as(<double-float>, green),
                        as(<double-float>, blue),
                        as(<double-float>, opacity));
  /*---*** Colors not implemented yet!
  gdk-gc-set-fill(gcontext, $GDK-SOLID);
  gdk-gc-set-function(gcontext, $GDK-COPY);
  gdk-gc-set-foreground(gcontext, medium.%foreground-color)
  */
end method establish-brush;

define sealed method establish-brush
    (medium :: <gtk-medium>, brush :: <background>, gcontext :: <CairoContext>) => ()
  let color = medium-background(medium);
  let (red, green, blue, opacity) = color-rgb(color);
  cairo-set-source-rgba(gcontext,
                        as(<double-float>, red),
                        as(<double-float>, green),
                        as(<double-float>, blue),
                        as(<double-float>, opacity));
  /*---*** Colors not implemented yet!
  gdk-gc-set-fill(gcontext, $GDK-SOLID);
  gdk-gc-set-function(gcontext, $GDK-COPY);
  gdk-gc-set-foreground(gcontext, medium.%background-color)
  */
end method establish-brush;


define sealed method establish-pen
    (medium :: <gtk-medium>, pen :: <standard-pen>, gcontext :: <CairoContext>) => ()
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
  let cap-shape
    = select (pen-cap-shape(pen))
        #"butt"         => $cairo-line-cap-butt;
        #"square"       => $cairo-line-cap-square;
        #"round"        => $cairo-line-cap-round;
        #"no-end-point" => $cairo-line-cap-butt;
      end;
  let joint-shape
    = select (pen-joint-shape(pen))
        #"miter" => $cairo-line-join-miter;
        #"none"  => $cairo-line-join-miter;
        #"bevel" => $cairo-line-join-bevel;
        #"round" => $cairo-line-join-round;
      end;
  cairo-set-line-width(gcontext, as(<double-float>, width));
  cairo-set-line-cap(gcontext, cap-shape);
  cairo-set-line-join(gcontext, joint-shape);
  if (dashed?)
    // This is probably wrong
    cairo-set-dash(gcontext, dash, size(dash), 0.0d0);
  else
    cairo-set-dash(gcontext, null-pointer(<C-double>), 0, 0.0d0);
  end if;
  when (dashed?)
    ignoring("pen dashes option");
    // gdk-gc-set-dashes(gcontext, 0, dash)
  end
end method establish-pen;


define sealed method establish-font
    (medium :: <gtk-medium>, font :: <gtk-font>, gcontext :: <CairoContext>) => ()
  ignoring("establish-font");
  // gdk-gc-set-font(gcontext, font.%font-id)
end method establish-font;


/// Ink decoding

define generic decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, brush)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>));

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, brush :: <foreground>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  decode-ink(medium, gcontext, medium-foreground(medium))
end method decode-ink;

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, brush :: <background>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  decode-ink(medium, gcontext, medium-background(medium))
end method decode-ink;

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, color :: <color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  values(allocate-color(color, medium.%palette), $GDK-SOLID, $boole-1, #f)
end method decode-ink;

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, color :: <contrasting-color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let color = contrasting-color->color(color);
  values(allocate-color(color, medium.%palette), $GDK-SOLID, $boole-1, #f)
end method decode-ink;

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, pattern :: <stencil>)
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
  values($native-white, $GDK-SOLID, $boole-1, bitmap)
  */
end method decode-ink;

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, pixmap :: <pixmap>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  not-yet-implemented("decode-ink for <pixmap>")
end method decode-ink;

define sealed method decode-ink
    (medium :: <gtk-medium>, gcontext :: <CairoContext>, brush :: <standard-brush>)
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
    (medium :: <gtk-medium>) => (mask)
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
