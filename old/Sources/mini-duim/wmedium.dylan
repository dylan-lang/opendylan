Module:    win32-duim
Synopsis:  Win32 medium implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Win32 mediums

define sealed class <win32-medium> (<basic-medium>)
  slot %ink-cache = make(<table>);
  slot %clip-mask = #f;
  slot %hFont = #f;
  // The last decoded <pen> and its Windows pen
  slot %cached-pen :: false-or(<pen>) = #f;
  slot %hPen :: false-or(<HPEN>) = #f;
  // The last decoded <brush> and its Windows brush
  slot %cached-brush :: false-or(type-union(<brush>, <color>)) = #f;
  slot %hBrush :: false-or(<HBRUSH>) = #f;
  slot %brush-color :: <native-color> = $native-black;
  slot %ROP2 = $R2-COPYPEN;
end class <win32-medium>;

define method clear-ink-cache (medium :: <win32-medium>)
  //---*** Deallocate stuff first...
  remove-all-keys!(medium.%ink-cache)
end method clear-ink-cache;

define method initialize (medium :: <win32-medium>, #key) => ()
  next-method();
  let cache = medium.%ink-cache;
  gethash(cache, $native-black) := $black;
  gethash(cache, $native-white) := $white;
end method initialize;

define method make-medium
    (_port :: <win32-port>, sheet :: <sheet>) => (medium :: <win32-medium>)
  make(<win32-medium>, port: _port, sheet: sheet)
end method make-medium;

define method do-destroy-medium (medium :: <win32-medium>) => ()
  clear-ink-cache(medium);
  release-DC(medium)
end method do-destroy-medium;

define method get-DC (medium :: <win32-medium>) => (hDC :: <HDC>)
  get-DC(medium-drawable(medium))
end method get-DC;

// Note that this should not be called by the drawing routines themselves;
// it will be called automatically when control returns to the event loop.
define method release-DC (medium :: <win32-medium>) => ()
  release-DC(medium-drawable(medium));
  drawing-state-cached?(medium) := #f	// calls 'invalidate-cached-drawing-state'
end method release-DC;

define method %window-handle (medium :: <win32-medium>) => (handle :: <HWND>)
  medium-drawable(medium).%window-handle
end method %window-handle;

define method do-attach-medium
    (sheet :: <sheet>, medium :: <win32-medium>) => ()
  let mirror = sheet-mirror(sheet);
  clear-ink-cache(medium);
  medium-drawable(medium) := mirror
end method do-attach-medium;

define method do-detach-medium
    (sheet :: <sheet>, medium :: <win32-medium>) => ()
  clear-ink-cache(medium);
  release-DC(medium)
end method do-detach-medium;

define method deallocate-medium
    (_port :: <win32-port>, medium :: <win32-medium>) => ()
  next-method();
  release-DC(medium)
end method deallocate-medium;

define method medium-foreground-setter
    (fg :: <ink>, medium :: <win32-medium>) => (foreground :: <ink>)
  next-method();	// also sets drawing-state-cached?(medium) := #f
  clear-ink-cache(medium);
  // Force repaint to be done later (#f => don't repaint the background)
  InvalidateRect(medium.%window-handle, $NULL-RECT, #f);
  fg
end method medium-foreground-setter;

define method medium-background-setter
    (bg :: <ink>, medium :: <win32-medium>) => (background :: <ink>)
  next-method();	// also sets drawing-state-cached?(medium) := #f
  clear-ink-cache(medium);
  //---*** Need to create an HBRUSH to be used by the WM_ERASEBKGND message.
  //---*** Also should remember the color to pass to SetBkColor for text.
  // Force repaint to be done later (#t => repaint the background)
  InvalidateRect(medium.%window-handle, $NULL-RECT, #t);
  bg
end method medium-background-setter;

define method medium-clipping-region-setter
    (region :: <region>, medium :: <win32-medium>) => (region :: <region>)
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

define method invalidate-cached-region (medium :: <win32-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-region;

define method invalidate-cached-transform (medium :: <win32-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-transform;

define method invalidate-cached-drawing-state
    (medium :: <win32-medium>, cached-state) => ()
  ignore(cached-state);
  if (medium.%hBrush)
    DeleteObject(medium.%hBrush);
    medium.%hBrush := #f;
    medium.%cached-brush := #f
  end;
  if (medium.%hPen)
    DeleteObject(medium.%hPen);
    medium.%hPen := #f;
    medium.%cached-pen := #f
  end
end method invalidate-cached-drawing-state;


define method beep (medium :: <win32-medium>) => ()
  MessageBeep($MB-OK)
end method beep;

define method force-display (medium :: <win32-medium>) => ()
  // Although this behavior is not documented anywhere, sometimes the effect
  // of drawing is not seen on the screen until 'ReleaseDC' is called.
  release-DC(medium)
end method force-display;

define method synchronize-display (medium :: <win32-medium>) => ()
  release-DC(medium)
end method synchronize-display;


/// Drawing state updating

define constant %brush_cached = #o01;
define constant %pen_cached   = #o02;
define constant %font_cached  = #o04;

// We could make this return #f if the update fails...
define method update-drawing-state
    (medium :: <win32-medium>, #key font, brush, pen) => (hDC :: false-or(<HDC>))
  let hDC :: <HDC> = get-DC(medium);
  let old-cache = drawing-state-cached?(medium) | 0;
  let new-cache = 0;
  if (zero?(logand(old-cache, %brush_cached)))  
    let brush = brush | medium-brush(medium);
    establish-brush(medium, brush, hDC);
    new-cache := logior(new-cache, %brush_cached)
  end;
  if (zero?(logand(old-cache, %pen_cached)))
    let pen = pen | medium-pen(medium);
    if (pen)
      establish-pen(medium, pen, hDC);
      new-cache := logior(new-cache, %pen_cached)
    end
  end;
  if (zero?(logand(old-cache, %font_cached)))
    if (font)
      establish-font(medium, font, hDC);
      new-cache := logior(new-cache, %font_cached)
    end
  end;
  drawing-state-cached?(medium) := logior(old-cache, new-cache);
  hDC
end method update-drawing-state;

define constant *rop2-map* :: <simple-object-vector>
  = make(<simple-object-vector>, size: 16);

begin
  *rop2-map*[$boole-clr]   := $R2-BLACK;
  *rop2-map*[$boole-set]   := $R2-WHITE;
  *rop2-map*[$boole-1]     := $R2-COPYPEN;
  *rop2-map*[$boole-2]     := $R2-NOP;
  *rop2-map*[$boole-c1]    := $R2-NOTCOPYPEN;
  *rop2-map*[$boole-c2]    := $R2-NOT;
  *rop2-map*[$boole-and]   := $R2-MASKPEN;
  *rop2-map*[$boole-ior]   := $R2-MERGEPEN;
  *rop2-map*[$boole-xor]   := $R2-XORPEN;
  *rop2-map*[$boole-eqv]   := $R2-NOTXORPEN;
  *rop2-map*[$boole-nand]  := $R2-NOTMASKPEN;
  *rop2-map*[$boole-nor]   := $R2-NOTMERGEPEN;
  *rop2-map*[$boole-andc1] := $R2-MASKNOTPEN;
  *rop2-map*[$boole-andc2] := $R2-MASKPENNOT;
  *rop2-map*[$boole-orc1]  := $R2-MERGENOTPEN;
  *rop2-map*[$boole-orc2]  := $R2-MERGEPENNOT
end;

define method establish-brush
    (medium :: <win32-medium>, brush :: <brush>, hDC :: <HDC>) => ()
  let hBrush :: <HBRUSH>
    = case
	brush == medium.%cached-brush =>
	  medium.%hBrush;
	otherwise =>
	  let (color :: <native-color>, fill-style, operation, image)
	    = convert-ink-to-DC-components(medium, hDC, brush);
	  //---*** Hack the image
	  //---***   This should set the tile-x/y to (0,0) if the brush is a stipple, or it
	  //---***   should align it to the left/top of the figure if the brush is a pattern
	  //---*** Make use of fill-style by using 'CreateHatchBrush' or
	  //---***   'CreatePatternBrush' instead of 'CreateSolidBrush'
	  medium.%ROP2 := *rop2-map*[operation];
	  medium.%brush-color := color;
	  check-result("CreateSolidBrush", CreateSolidBrush(color));
      end;
  if (medium.%hBrush & ~(medium.%hBrush == hBrush))
    DeleteObject(medium.%hBrush)
  end;
  medium.%cached-brush := brush;
  medium.%hBrush := hBrush;
  SelectObject(hDC, hBrush);
  SetROP2(hDC, medium.%ROP2)
end method establish-brush;

define method establish-brush
    (medium :: <win32-medium>, brush :: <color>, hDC :: <HDC>) => ()
  let hBrush :: <HBRUSH>
    = case
	brush == medium.%cached-brush =>
	  medium.%hBrush;
	brush == $black =>
	  medium.%brush-color := $native-black;
	  pointer-cast(<HBRUSH>, GetStockObject($BLACK-BRUSH));
	brush == $white =>
	  medium.%brush-color := $native-white;
	  pointer-cast(<HBRUSH>, GetStockObject($WHITE-BRUSH));
	otherwise =>
	  let color = color->native-color(brush, medium);
	  medium.%brush-color := color;
	  check-result("CreateSolidBrush", CreateSolidBrush(color));
	end;
  if (medium.%hBrush & ~(medium.%hBrush == hBrush))
    DeleteObject(medium.%hBrush)
  end;
  medium.%cached-brush := brush;
  medium.%hBrush := hBrush;
  medium.%ROP2 := $R2-COPYPEN;
  SelectObject(hDC, hBrush);
  SetROP2(hDC, medium.%ROP2)
end method establish-brush;

define method establish-brush
    (medium :: <win32-medium>, brush :: <foreground>, hDC :: <HDC>) => ()
  establish-brush(medium, medium-foreground(medium), hDC)
end method establish-brush;

define method establish-brush
    (medium :: <win32-medium>, brush :: <background>, hDC :: <HDC>) => ()
  establish-brush(medium, medium-background(medium), hDC)
end method establish-brush;

define method establish-pen 
    (medium :: <win32-medium>, pen :: <pen>, hDC :: <HDC>) => ();
  let hPen :: <HPEN>
    = case
	pen == medium.%cached-pen =>
	  medium.%hPen;
	otherwise =>
	  let width = truncate(pen-width(pen));
	  let style
	    = select (pen-dashes(pen))
		#f => $PS-SOLID;
		#t => $PS-DASH;
		pen-dashes($dotted-pen) => $PS-DOT;
		pen-dashes($dash-dot-pen) => $PS-DASHDOT;
		pen-dashes($dash-dot-dot-pen) => $PS-DASHDOTDOT;
		otherwise => $PS-DASHDOT;
	      end;
	  check-result("CreatePen", CreatePen(style, width, medium.%brush-color));
      end;
  if (medium.%hPen & ~(medium.%hPen == hPen))
    DeleteObject(medium.%hPen)
  end;
  medium.%cached-pen := pen;
  medium.%hPen := hPen;
  SelectObject(hDC, hPen)
end method establish-pen;


define method establish-font
    (medium :: <win32-medium>, font, hDC :: <HDC>) => ()
  let hFont = font.%font-handle;
  unless (hFont == medium.%hFont)
    if (SetTextColor(hDC, medium.%brush-color) = $CLR-INVALID)
      report-error("SetTextColor")
    end;
    SelectObject(hDC, hFont);
    medium.%hFont := hFont
  end
end method establish-font;


define generic convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>));

define method convert-ink-to-DC-components 
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <foreground>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  convert-ink-to-DC-components(medium, hDC, medium-foreground(medium))
end method convert-ink-to-DC-components;

define method convert-ink-to-DC-components 
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <background>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  convert-ink-to-DC-components(medium, hDC, medium-background(medium))
end method convert-ink-to-DC-components;

define method convert-ink-to-DC-components 
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <color>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  values(color->native-color(brush, medium), #"solid", $boole-1, #f)
end method convert-ink-to-DC-components;

define method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <contrasting-color>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  convert-ink-to-DC-components(medium, hDC, make-color-for-contrasting-color(brush))
end method convert-ink-to-DC-components;

define method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <stencil>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  let cache = medium.%ink-cache;
  let bitmap
    = gethash(cache, brush)
      | begin
	  let (array, colors) = decode-pattern(brush);
	  let width  = image-width(brush);
	  let height = image-height(brush);
	  let ncolors = size(colors);
	  //---*** Should we create a DIB here or what?
	  let pixels = make(<vector>, size: ncolors);			//---***
	  let pixarray = make(<array>, dimensions: list(width, height));//---***
	  for (n from 0 below ncolors)
	    let pixel = convert-ink-to-DC-components(medium, hDC, colors[n]);
	    pixels[n] := pixel
	  end;
	  for (y from 0 below height)
	    for (x from 0 below width)
	      pixarray[y,x] := pixels[array[y,x]]
	    end
	  end;
	  //---*** Fill in the DIB
	  let bitmap = list(pixels, pixarray);				//---***
	  gethash(cache, brush) := bitmap;
	  bitmap
	end;
  values($native-white, #"solid", $boole-1, bitmap)
end method convert-ink-to-DC-components;

define method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <pixmap>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  //---*** Implement it!
  not-yet-implemented("convert-ink-to-dc-components for <pixmap>")
end method convert-ink-to-DC-components;

define method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <brush>)
 => (color :: <native-color>, fill-style, operation,
     image :: false-or(<image>))
  let (color :: <native-color>, fill-style, operation, pattern)
    = convert-ink-to-DC-components
        (medium, hDC,
	 brush-tile(brush) | brush-stipple(brush) | brush-foreground(brush));
  // ignore(operation);
  values(color, fill-style, brush-mode(brush), pattern)
end method convert-ink-to-DC-components;


// Compute the clip mask. This suggested implementation keeps a cached
// version around for efficiency
define method compute-clip-mask (medium :: <win32-medium>) => (mask)
  medium.%clip-mask 
  | (medium.%clip-mask := do-compute-clip-mask(medium))
end method compute-clip-mask;

define method do-compute-clip-mask (medium :: <win32-medium>) => (mask)
  let sheet = medium-sheet(medium);
  let sregion = sheet-device-region(sheet);
  let mregion = medium-clipping-region(medium);
  let valid? = #t;
  if (sregion == $nowhere
      | mregion == $nowhere)
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
      fix-coordinates!(sleft, stop, sright, sbottom);
      vector(sleft, stop, sright, sbottom)
    else
      #"none"
    end
  end
end method do-compute-clip-mask;
