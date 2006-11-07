Module:    win32-duim
Synopsis:  Win32 medium implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 mediums

// Strictly speaking, this is 'limited(<integer>, min: 0, max: #x03FFFFFF)',
// but that actually makes things a bit slower!
define constant <native-color> = <integer>;

//---*** HughG asks:
// Is it valid to cache the Windows pen, brush and other DC objects?
// The Win32 SDK example calls GetCurrentObject to compare with the
// object it's about to select.  This may not be just to avoid having
// to put caching into the example: CancelDC can be called from one
// thread to affect another's DC, so maybe the current object is not
// guaranteed to stay the same until you explicitly change it.
// (This doesn't make much sense to me but then I don't understand
// CancelDC either, so I'm just generally suspicious.)
define sealed class <win32-medium> (<basic-medium>)
  sealed slot %ink-cache :: <object-table> = make(<table>);
  // Cached clipping region
  sealed slot %clip-mask = #f;  // #f, #"none", or #[left, top, right, bottom]
  sealed slot %hRect :: false-or(type-union(<LPRECT>, <HRGN>)) = #f;
  // Cached font
  sealed slot %hFont :: false-or(<HFONT>) = #f;
  // The last decoded <pen>, its cached Windows pen, and the Windows pen to
  // restore when we're finished
  sealed slot %cached-pen :: false-or(<standard-pen>) = #f;
  sealed slot %hPen       :: false-or(<HPEN>) = #f;
  sealed slot %old-hPen   :: false-or(<HPEN>) = #f;
  // The last decoded <brush>, its cached Windows brush and the Windows brush to
  // restore when we're finished
  sealed slot %cached-brush :: false-or(type-union(<standard-brush>, <color>)) = #f;
  sealed slot %hBrush       :: false-or(<HBRUSH>) = #f;
  sealed slot %old-hBrush   :: false-or(<HBRUSH>) = #f;
  sealed slot %brush-color  :: <native-color> = $native-black;
  sealed slot %ROP2         :: <integer> = $R2-COPYPEN;
end class <win32-medium>;

define sealed domain make (singleton(<win32-medium>));
define sealed domain initialize (<win32-medium>);

define variable $black-hbrush   :: false-or(<HBRUSH>) = #f;
define variable $white-hbrush   :: false-or(<HBRUSH>) = #f;
define variable $null-hbrush    :: false-or(<HBRUSH>) = #f;
define variable $black-hpen     :: false-or(<HPEN>)   = #f;
define variable $white-hpen     :: false-or(<HPEN>)   = #f;
define variable $null-hpen      :: false-or(<HPEN>)   = #f;
define variable $system-hfont   :: false-or(<HFONT>)  = #f;
define variable $gui-hfont      :: false-or(<HFONT>)  = #f;
define variable $fixed-hfont    :: false-or(<HFONT>)  = #f;
define variable $variable-hfont :: false-or(<HFONT>)  = #f;

define method initialize-stock-objects (_port :: <win32-port>) => ()
  $black-hbrush   := pointer-cast(<HBRUSH>, GetStockObject($BLACK-BRUSH));
  $white-hbrush   := pointer-cast(<HBRUSH>, GetStockObject($WHITE-BRUSH));
  $null-hbrush    := pointer-cast(<HBRUSH>, GetStockObject($NULL-BRUSH));
  $black-hpen     := pointer-cast(<HPEN>,   GetStockObject($BLACK-PEN));
  $white-hpen     := pointer-cast(<HPEN>,   GetStockObject($WHITE-PEN));
  $null-hpen      := pointer-cast(<HPEN>,   GetStockObject($NULL-PEN));
  $system-hfont   := pointer-cast(<HFONT>,  GetStockObject($SYSTEM-FONT));
  $gui-hfont      := pointer-cast(<HFONT>,  GetStockObject($DEFAULT-GUI-FONT));
  $fixed-hfont    := pointer-cast(<HFONT>,  GetStockObject($ANSI-FIXED-FONT));
  $variable-hfont := pointer-cast(<HFONT>,  GetStockObject($ANSI-VAR-FONT));
end method initialize-stock-objects;

define method clear-ink-cache (medium :: <win32-medium>)
  //--- This should really deallocate any cached pixmaps, etc.
  remove-all-keys!(medium.%ink-cache)
end method clear-ink-cache;

define sealed method initialize
    (medium :: <win32-medium>, #key) => ()
  next-method();
  let cache = medium.%ink-cache;
  gethash(cache, $native-black) := $black;
  gethash(cache, $native-white) := $white;
end method initialize;

// This is intentionally _not_ sealed
define method make-medium
    (_port :: <win32-port>, sheet :: <sheet>)
 => (medium :: <win32-medium>)
  make(<win32-medium>, port: _port, sheet: sheet)
end method make-medium;

define sealed method destroy-medium
    (medium :: <win32-medium>) => ()
  clear-ink-cache(medium);
  release-DC(medium);
  next-method();
  medium-drawable(medium) := #f
end method destroy-medium;

define sealed inline method get-DC
    (medium :: <win32-medium>) => (hDC :: <HDC>)
  get-DC(medium-drawable(medium))
end method get-DC;

// Note that this should not be called by the drawing routines themselves;
// it will be called automatically when control returns to the event loop.
define sealed method release-DC
    (medium :: <win32-medium>) => ()
  let drawable = medium-drawable(medium);
  drawable & release-DC(drawable);      // be careful during shutdown...
  // Force 'invalidate-cached-drawing-state'.  Note that 'release-DC' on
  // mirrors is careful not to do any work for CS_OWNDC windows.  That is
  // not the case here -- when 'release-DC' gets called on a medium, it
  // is done in order to release any allocated pens, brushes, etc.
  medium-drawing-state-cache(medium) := 0
end method release-DC;

define sealed method window-handle
    (medium :: <win32-medium>) => (handle :: <HWND>)
  window-handle(medium-drawable(medium))
end method window-handle;

define sealed method do-attach-medium
    (sheet :: <sheet>, medium :: <win32-medium>) => ()
  let mirror = sheet-mirror(sheet);
  assert(mirror,
         "Unexpected failure: no mirror when attaching medium for %=",
         sheet);
  clear-ink-cache(medium);
  medium-drawable(medium) := mirror
end method do-attach-medium;

define sealed method do-detach-medium
    (sheet :: <sheet>, medium :: <win32-medium>) => ()
  clear-ink-cache(medium);
  release-DC(medium);
  medium-drawable(medium) := #f
end method do-detach-medium;

define sealed method deallocate-medium
    (_port :: <win32-port>, medium :: <win32-medium>) => ()
  next-method();
  release-DC(medium);
  medium-drawable(medium) := #f
end method deallocate-medium;

define sealed method medium-foreground-setter
    (fg :: <ink>, medium :: <win32-medium>) => (foreground :: <ink>)
  next-method();        // also sets 'medium-drawing-state-cache' appropriately
  clear-ink-cache(medium);
  // Force repaint to be done later (#f => don't repaint the background)
  unless (instance?(medium, <pixmap-medium>))
    InvalidateRect(window-handle(medium), $NULL-RECT, #f)
  end;
  fg
end method medium-foreground-setter;

define sealed method medium-background-setter
    (bg :: <ink>, medium :: <win32-medium>) => (background :: <ink>)
  next-method();        // also sets 'medium-drawing-state-cache' appropriately
  clear-ink-cache(medium);
  //---*** Need to create an HBRUSH to be used by the WM_ERASEBKGND message.
  //---*** Also should remember the color to pass to SetBkColor for text.
  // Force repaint to be done later (#t => repaint the background)
  unless (instance?(medium, <pixmap-medium>))
    InvalidateRect(window-handle(medium), $NULL-RECT, #t)
  end;
  bg
end method medium-background-setter;

define sealed method medium-clipping-region-setter
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

define sealed method invalidate-cached-region
    (medium :: <win32-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-region;

define sealed method invalidate-cached-transform
    (medium :: <win32-medium>) => ()
  medium.%clip-mask := #f;
  next-method()
end method invalidate-cached-transform;

define sealed method invalidate-cached-drawing-state
    (medium :: <win32-medium>, new-state :: <integer>) => ()
  ignore(new-state);
  medium.%hFont := #f;
  when (medium.%hBrush
        & zero?(logand(new-state, $medium-brush-cached)))
    // Make sure the DC no longer refers to the cached brush before deleting it
    let hDC :: <HDC> = get-DC(medium);
    SelectObject(hDC, medium.%old-hBrush | $null-hBrush);
    DeleteObject(medium.%hBrush);
    medium.%old-hBrush   := #f;
    medium.%hBrush       := #f;
    medium.%cached-brush := #f
  end;
  when (medium.%hPen
        & (  zero?(logand(new-state, $medium-brush-cached))
           | zero?(logand(new-state, $medium-pen-cached))))
    // Make sure the DC no longer refers to the cached pen before deleting it
    let hDC :: <HDC> = get-DC(medium);
    SelectObject(hDC, medium.%old-hPen | $null-hPen);
    DeleteObject(medium.%hPen);
    medium.%old-hPen   := #f;
    medium.%hPen       := #f;
    medium.%cached-pen := #f
  end
end method invalidate-cached-drawing-state;


/// Display forcing

define sealed method force-display
    (medium :: <win32-medium>) => ()
  // Although this behavior is not documented anywhere, sometimes the effect
  // of drawing is not seen on the screen until 'ReleaseDC' is called.
  release-DC(medium)
end method force-display;

define sealed method synchronize-display
    (medium :: <win32-medium>) => ()
  release-DC(medium)
end method synchronize-display;


/// Drawing state updating

define constant $null-region = null-pointer(<HRGN>);

// Note that the brush defaults to 'medium-brush(medium)',
// but the pen and font default to #f in order to avoid unnecessary work
//---*** We need some control over opaque/transparent -- 'SetBkMode'
//---*** Shouldn't someone arrange to call 'SetMapMode', too?
define sealed method update-drawing-state
    (medium :: <win32-medium>, #key brush, pen, font)
 => (hDC :: <HDC>)
  let hDC :: <HDC> = get-DC(medium);
  let old-cache :: <integer> = medium-drawing-state-cache(medium);
  let new-cache :: <integer> = 0;
  when (old-cache ~= $medium-fully-cached)
    // Establish a brush, unless it's already cached
    when (zero?(logand(old-cache, $medium-brush-cached)))
      let brush = brush | medium-brush(medium);
      establish-brush(medium, brush, hDC);
      new-cache := logior(new-cache, $medium-brush-cached)
    end;
    // Establish a pen, unless it's already cached
    // Note that we have to establish a new pen if the brush changed,
    // because the pen encodes the brush color
    when (  zero?(logand(old-cache, $medium-brush-cached))
          | zero?(logand(old-cache, $medium-pen-cached)))
      let pen = pen | medium-pen(medium);
      establish-pen(medium, pen, hDC);
      new-cache := logior(new-cache, $medium-pen-cached)
    end;
    // Establish a font only if requested, unless it's already cached
    // Note that, even if the font didn't change, we still have to set
    // the text color if the brush changed
    when (font
          & (  zero?(logand(old-cache, $medium-brush-cached))
             | zero?(logand(old-cache, $medium-font-cached))))
      establish-font(medium, font, hDC);
      new-cache := logior(new-cache, $medium-font-cached)
    end;
    when (~medium.%clip-mask & medium.%hRect)
      let hrect = medium.%hRect;
      // If the clip mask is decached, flush the old hRect
      unless (hRect = $null-region)
        destroy(hRect)
      end;
      medium.%hRect := #f
    end;
    unless (medium.%hRect)
      let mask = compute-clip-mask(medium);
      if (mask == #"none")
        medium.%hRect := $null-region
      else
        let hRect :: <LPRECT> = make(<LPRECT>);
        without-bounds-checks
          hRect.left-value   := mask[0];
          hRect.top-value    := mask[1];
          hRect.right-value  := mask[2];
          hRect.bottom-value := mask[3]
        end;
        medium.%hRect := hRect
      end;
      //---*** SelectClipRgn isn't imported yet :-(
      // SelectClipRgn(hDC, medium.%hRect)
      new-cache := logior(new-cache, $medium-region-cached)
    end;
    medium-drawing-state-cache(medium) := logior(old-cache, new-cache)
  end;
  hDC
end method update-drawing-state;

define constant $rop2-map :: <simple-object-vector>
    = make(<simple-vector>, size: 16);

begin
  $rop2-map[$boole-clr]   := $R2-BLACK;
  $rop2-map[$boole-set]   := $R2-WHITE;
  $rop2-map[$boole-1]     := $R2-COPYPEN;
  $rop2-map[$boole-2]     := $R2-NOP;
  $rop2-map[$boole-c1]    := $R2-NOTCOPYPEN;
  $rop2-map[$boole-c2]    := $R2-NOT;
  $rop2-map[$boole-and]   := $R2-MASKPEN;
  $rop2-map[$boole-ior]   := $R2-MERGEPEN;
  $rop2-map[$boole-xor]   := $R2-XORPEN;
  $rop2-map[$boole-eqv]   := $R2-NOTXORPEN;
  $rop2-map[$boole-nand]  := $R2-NOTMASKPEN;
  $rop2-map[$boole-nor]   := $R2-NOTMERGEPEN;
  $rop2-map[$boole-andc1] := $R2-MASKNOTPEN;
  $rop2-map[$boole-andc2] := $R2-MASKPENNOT;
  $rop2-map[$boole-orc1]  := $R2-MERGENOTPEN;
  $rop2-map[$boole-orc2]  := $R2-MERGEPENNOT
end;

define sealed method establish-brush
    (medium :: <win32-medium>, brush :: <standard-brush>, hDC :: <HDC>) => ()
  let hBrush :: <HBRUSH>
    = case
        brush == medium.%cached-brush =>
          medium.%hBrush;
        otherwise =>
          let (color :: <native-color>, fill-style, operation :: <integer>, image)
            = convert-ink-to-DC-components(medium, hDC, brush);
          //---*** Hack the image
          //---***   This should set the tile-x/y to (0,0) if the brush is a stipple, or it
          //---***   should align it to the left/top of the figure if the brush is a pattern
          //---*** Make use of fill-style by using 'CreateHatchBrush' or
          //---***   'CreatePatternBrush' instead of 'CreateSolidBrush'
          medium.%brush-color := color;
          medium.%ROP2 := $rop2-map[operation];
          check-result("CreateSolidBrush", CreateSolidBrush(color));
      end;
  do-establish-brush(medium, brush, hBrush, hDC)
end method establish-brush;

define sealed method establish-brush
    (medium :: <win32-medium>, brush :: <rgb-color>, hDC :: <HDC>) => ()
  let hBrush :: <HBRUSH>
    = case
        brush == medium.%cached-brush =>
          medium.%hBrush;
        brush == $black =>
          medium.%brush-color := $native-black;
          medium.%ROP2 := $R2-COPYPEN;
          $black-hbrush;
        brush == $white =>
          medium.%brush-color := $native-white;
          medium.%ROP2 := $R2-COPYPEN;
          $white-hbrush;
        otherwise =>
          let color = color->native-color(brush, medium);
          medium.%brush-color := color;
          medium.%ROP2 := $R2-COPYPEN;
          check-result("CreateSolidBrush", CreateSolidBrush(color));
        end;
  do-establish-brush(medium, brush, hBrush, hDC)
end method establish-brush;

define sealed method establish-brush
    (medium :: <win32-medium>, brush :: <contrasting-color>, hDC :: <HDC>) => ()
   establish-brush(medium, contrasting-color->color(brush), hDC)
end method establish-brush;

define method do-establish-brush
    (medium :: <win32-medium>, brush, hBrush :: <HBRUSH>, hDC :: <HDC>) => ()
  when (medium.%hBrush ~= hBrush)
    medium.%cached-brush := brush;
    medium.%hBrush       := hBrush
  end;
  let old-hBrush = SelectObject(hDC, hBrush);
  check-result("SelectObject brush", old-hBrush);
  if (medium.%old-hBrush == #f)
    // The old brush was not selected by a call to 'establish-brush',
    // so we will need to put it back later
    medium.%old-hBrush := pointer-cast(<HBRUSH>, old-hBrush)
  else
    // The old brush was selected by a call to 'establish-brush',
    // so we can delete it now
    DeleteObject(medium.%old-hBrush)
  end;
  SetROP2(hDC, medium.%ROP2);
  //---*** Is this always the right thing?
  SetBkMode(hDC, $TRANSPARENT)
end method do-establish-brush;

define sealed method establish-brush
    (medium :: <win32-medium>, brush :: <foreground>, hDC :: <HDC>) => ()
  establish-brush(medium, medium-foreground(medium), hDC)
end method establish-brush;

define sealed method establish-brush
    (medium :: <win32-medium>, brush :: <background>, hDC :: <HDC>) => ()
  establish-brush(medium, medium-background(medium), hDC)
end method establish-brush;


define sealed method establish-pen 
    (medium :: <win32-medium>, pen :: <standard-pen>, hDC :: <HDC>) => ();
  let hPen :: <HPEN>
    = case
        pen == medium.%cached-pen =>
          medium.%hPen;
        otherwise =>
          let width = pen-width(pen);
          let style
            = select (pen-dashes(pen))
                #f                            => $PS-SOLID;
                #t                            => $PS-DASH;
                pen-dashes($dotted-pen)       => $PS-DOT;
                pen-dashes($dash-dot-pen)     => $PS-DASHDOT;
                pen-dashes($dash-dot-dot-pen) => $PS-DASHDOTDOT;
                otherwise                     => $PS-DASHDOT;
              end;
          // Note well!  Depends on the brush having been established first!
          check-result("CreatePen", CreatePen(style, width, medium.%brush-color));
      end;
  when (medium.%hPen ~= hPen)
    medium.%cached-pen := pen;
    medium.%hPen       := hPen
  end;
  let old-hPen = SelectObject(hDC, hPen);
  check-result("SelectObject pen", old-hPen);
  if (medium.%old-hPen == #f)
    // The old pen was not selected by a call to 'establish-pen',
    // so we will need to put it back later
    medium.%old-hPen := pointer-cast(<HPEN>, old-hPen)
  else
    // The old pen was selected by a call to 'establish-pen',
    // so we can delete it now
    DeleteObject(medium.%old-hPen)
  end
end method establish-pen;


define sealed method establish-font
    (medium :: <win32-medium>, font :: <win32-font>, hDC :: <HDC>) => ()
  let hFont = font.%font-handle;
  unless (hFont = medium.%hFont)
    when (SetTextColor(hDC, medium.%brush-color) = $CLR-INVALID)
      report-error("SetTextColor")
    end;
    check-result("SelectObject font", SelectObject(hDC, hFont));
    medium.%hFont := hFont
  end
end method establish-font;


define generic convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>));

define sealed method convert-ink-to-DC-components 
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <foreground>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  convert-ink-to-DC-components(medium, hDC, medium-foreground(medium))
end method convert-ink-to-DC-components;

define sealed method convert-ink-to-DC-components 
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <background>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  convert-ink-to-DC-components(medium, hDC, medium-background(medium))
end method convert-ink-to-DC-components;

define sealed method convert-ink-to-DC-components 
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <rgb-color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  values(color->native-color(brush, medium), #"solid", $boole-1, #f)
end method convert-ink-to-DC-components;

define sealed method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <contrasting-color>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  values(color->native-color(contrasting-color->color(brush), medium), #"solid", $boole-1, #f)
end method convert-ink-to-DC-components;

define sealed method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <stencil>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let cache = medium.%ink-cache;
  let bitmap :: false-or(<image>)
    = gethash(cache, brush)
      | begin
          let (array, colors) = decode-pattern(brush);
          let width   :: <integer> = image-width(brush);
          let height  :: <integer> = image-height(brush);
          let ncolors :: <integer> = size(colors);
          //---*** Should we create a DIB here or what?
          let pixels   :: <simple-object-vector> = make(<simple-vector>, size: ncolors);
          let pixarray :: <array> = make(<array>, dimensions: list(width, height));
          without-bounds-checks
            for (n :: <integer> from 0 below ncolors)
              let pixel = convert-ink-to-DC-components(medium, hDC, colors[n]);
              pixels[n] := pixel
            end;
            for (y :: <integer> from 0 below height)
              for (x :: <integer> from 0 below width)
                pixarray[y,x] := pixels[array[y,x]]
              end
            end
          end;
          //---*** Fill in the DIB from 'pixels' and 'pixarray'
          let bitmap = make(<win32-bitmap>,
                            width: width, height: height,
                            handle: #f);
          gethash(cache, brush) := bitmap;
          bitmap
        end;
  values($native-white, #"solid", $boole-1, bitmap)
end method convert-ink-to-DC-components;

define sealed method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <pixmap>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  //---*** Implement it!
  not-yet-implemented("convert-ink-to-dc-components for <pixmap>")
end method convert-ink-to-DC-components;

define sealed method convert-ink-to-DC-components
    (medium :: <win32-medium>, hDC :: <HDC>, brush :: <standard-brush>)
 => (color :: <native-color>, fill-style, operation :: <integer>,
     image :: false-or(<image>))
  let (color :: <native-color>, fill-style, operation :: <integer>, pattern)
    = convert-ink-to-DC-components
        (medium, hDC,
         brush-tile(brush) | brush-stipple(brush) | brush-foreground(brush));
  // ignore(operation);
  values(color, fill-style, brush-mode(brush), pattern)
end method convert-ink-to-DC-components;


// Compute and cache the clip rectangle
define sealed method compute-clip-mask
    (medium :: <win32-medium>) => (mask)
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
