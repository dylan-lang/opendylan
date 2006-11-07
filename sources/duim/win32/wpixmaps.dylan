Module:    win32-duim
Synopsis:  Win32 pixmap implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 pixmaps

define sealed class <win32-pixmap> (<mirror>, <pixmap>)
  sealed constant slot image-width  :: <integer>,
    required-init-keyword: width:;
  sealed constant slot image-height :: <integer>,
    required-init-keyword: height:;
  sealed slot %medium :: <win32-medium>,
    required-init-keyword: medium:;
  sealed slot %hDC     :: <HDC>     = $null-hDC;
  sealed slot %hbitmap :: <HBITMAP> = $null-bitmap;
end class <win32-pixmap>;

define sealed domain make (singleton(<win32-pixmap>));
define sealed domain initialize (<win32-pixmap>);

define sealed method do-make-pixmap
    (_port :: <win32-port>, medium :: <win32-medium>, 
     width :: <integer>, height :: <integer>)
 => (pixmap :: <win32-pixmap>)
  make(<win32-pixmap>, medium: medium, width: width, height: height)
end method do-make-pixmap;

define sealed method initialize
    (pixmap :: <win32-pixmap>, #key width, height, medium) => ()
  next-method();
  let hDC :: <HDC> = get-DC(medium);
  let bitmap-hDC :: <HDC>
    = check-result("CreateCompatibleDC", CreateCompatibleDC(hDC));
  let hbitmap :: <HBITMAP>
    = check-result("CreateCompatibleBitmap",
                   CreateCompatibleBitmap(hDC, width, height));
  SelectObject(bitmap-hDC, hbitmap);
  pixmap.%hDC     := bitmap-hDC;
  pixmap.%hbitmap := hbitmap
end method initialize;

define sealed method destroy-pixmap
    (pixmap :: <win32-pixmap>) => ()
  unless (pixmap.%hDC = $null-hDC)
    DeleteDC(pixmap.%hDC);
    pixmap.%hDC := $null-hDC
  end;
  unless (pixmap.%hbitmap = $null-bitmap)
    DeleteObject(pixmap.%hbitmap);
    pixmap.%hbitmap := $null-bitmap
  end;
  #f
end method destroy-pixmap;

define sealed method port 
    (pixmap :: <win32-pixmap>) => (port :: <win32-port>)
  port(pixmap.%medium)
end method port;

define sealed method pixmap-drawable
    (pixmap :: <win32-pixmap>) => (drawable)
  pixmap
end method pixmap-drawable;

define sealed inline method get-DC
    (pixmap :: <win32-pixmap>) => (hDC :: <HDC>)
  pixmap.%hDC
end method get-DC;

define sealed method draw-image
    (medium :: <win32-medium>, pixmap :: <win32-pixmap>, x, y) => (record)
  do-copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
               medium, x, y);
  #f
end method draw-image;

define sealed method draw-pixmap
    (medium :: <win32-medium>, pixmap :: <win32-pixmap>, x, y,
     #key function = $boole-1) => (record)
  do-copy-area(pixmap, 0, 0, image-width(pixmap), image-height(pixmap),
               medium, x, y, function: function);
  #f
end method draw-pixmap;


/// Win32 pixmap mediums

define sealed class <win32-pixmap-medium>
    (<win32-medium>, 
     <basic-pixmap-medium>)
end class <win32-pixmap-medium>;

define sealed domain make (singleton(<win32-pixmap-medium>));
define sealed domain initialize (<win32-pixmap-medium>);

define sealed method make-pixmap-medium
    (_port :: <win32-port>, sheet :: <sheet>, #key width, height)
 => (medium :: <win32-pixmap-medium>)
  with-sheet-medium (medium = sheet)
    let pixmap = do-make-pixmap(_port, medium, width, height);
    let medium = make(<win32-pixmap-medium>,
                      port: _port,
                      sheet: sheet,
                      pixmap: pixmap);
    medium-drawable(medium) := pixmap;
    medium
  end
end method make-pixmap-medium;


/// Win32 pixmap sheets (and mirrors)

define sealed method map-mirror
    (_port :: <win32-port>, sheet :: <pixmap-sheet>, mirror :: <win32-pixmap>) => ()
  #f
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <win32-port>, sheet :: <pixmap-sheet>, mirror :: <win32-pixmap>) => ()
  #f
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <win32-port>, sheet :: <pixmap-sheet>, mirror :: <win32-pixmap>,
     #key activate? = #t) => ()
  ignore(activate?);
  #f
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <win32-port>, sheet :: <pixmap-sheet>, mirror :: <win32-pixmap>) => ()
  #f
end method lower-mirror;


/// BitBlt

define constant $function-map :: <simple-object-vector>
    = make(<simple-vector>, size: 16);

begin
  $function-map[$boole-clr]   := $BLACKNESS;
  $function-map[$boole-set]   := $WHITENESS;
  $function-map[$boole-1]     := $SRCCOPY;
  $function-map[$boole-2]     := #xAA0029; 
  $function-map[$boole-c1]    := $NOTSRCCOPY;
  $function-map[$boole-c2]    := $DSTINVERT;
  $function-map[$boole-and]   := $SRCAND;
  $function-map[$boole-ior]   := $SRCPAINT;
  $function-map[$boole-xor]   := $SRCINVERT;
  $function-map[$boole-eqv]   := #x990066;
  $function-map[$boole-nand]  := #x7700E6;
  $function-map[$boole-nor]   := $NOTSRCERASE;
  $function-map[$boole-andc1] := #x220326;
  $function-map[$boole-andc2] := $SRCERASE;
  $function-map[$boole-orc1]  := $MERGEPAINT;
  $function-map[$boole-orc2]  := #xBB0226
end;

define sealed method do-copy-area
    (from-medium :: <win32-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <win32-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  if (from-medium == to-medium)
    let sheet = medium-sheet(from-medium);
    let transform = sheet-device-transform(sheet);
    let hDC :: <HDC> = get-DC(medium-drawable(from-medium));
    with-device-coordinates (transform, from-x, from-y, to-x, to-y)
      with-device-distances (transform, width, height)
        BitBlt(hDC, to-x, to-y, width, height,
               hDC, from-x, from-y,
               $function-map[function])
      end
    end
  else
    let from-sheet = medium-sheet(from-medium);
    let from-transform = sheet-device-transform(from-sheet);
    let to-sheet = medium-sheet(to-medium);
    let to-transform = sheet-device-transform(to-sheet);
    let from-hDC :: <HDC> = get-DC(medium-drawable(from-medium));
    let to-hDC   :: <HDC> = get-DC(medium-drawable(to-medium));
    with-device-coordinates (from-transform, from-x, from-y)
      with-device-coordinates (to-transform, to-x, to-y)
        with-device-distances (from-transform, width, height)
          BitBlt(to-hDC, to-x, to-y, width, height,
                 from-hDC, from-x, from-y,
                 $function-map[function])
        end
      end
    end
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <win32-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <win32-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-transform = sheet-device-transform(medium-sheet(from-medium));
  let from-hDC :: <HDC> = get-DC(medium-drawable(from-medium));
  let to-hDC   :: <HDC> = get-DC(medium-drawable(to-medium));
  with-device-coordinates (from-transform, from-x, from-y)
    with-device-distances (from-transform, width, height)
      BitBlt(to-hDC, to-x, to-y, width, height,
             from-hDC, from-x, from-y,
             $function-map[function])
    end
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <win32-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     pixmap :: <win32-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-transform = sheet-device-transform(medium-sheet(from-medium));
  let from-hDC :: <HDC> = get-DC(medium-drawable(from-medium));
  let to-hDC   :: <HDC> = get-DC(pixmap);
  with-device-coordinates (from-transform, from-x, from-y)
    with-device-distances (from-transform, width, height)
      BitBlt(to-hDC, to-x, to-y, width, height,
             from-hDC, from-x, from-y,
             $function-map[function])
    end
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <win32-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <win32-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let to-transform = sheet-device-transform(medium-sheet(to-medium));
  let from-hDC :: <HDC> = get-DC(medium-drawable(from-medium));
  let to-hDC   :: <HDC> = get-DC(medium-drawable(to-medium));
  with-device-coordinates (to-transform, to-x, to-y)
    BitBlt(to-hDC, to-x, to-y, width, height,
           from-hDC, from-x, from-y,
           $function-map[function])
  end
end method do-copy-area;

define sealed method do-copy-area
    (pixmap :: <win32-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <win32-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let to-transform = sheet-device-transform(medium-sheet(to-medium));
  let from-hDC :: <HDC> = get-DC(pixmap);
  let to-hDC   :: <HDC> = get-DC(medium-drawable(to-medium));
  with-device-coordinates (to-transform, to-x, to-y)
    BitBlt(to-hDC, to-x, to-y, width, height,
           from-hDC, from-x, from-y,
           $function-map[function])
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <win32-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <win32-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-hDC :: <HDC> = get-DC(medium-drawable(from-medium));
  let to-hDC   :: <HDC> = get-DC(medium-drawable(to-medium));
  BitBlt(to-hDC, to-x, to-y, width, height,
         from-hDC, from-x, from-y,
         $function-map[function])
end method do-copy-area;

define sealed method do-copy-area
    (from-pixmap :: <win32-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-pixmap :: <win32-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-hDC :: <HDC> = get-DC(from-pixmap);
  let to-hDC   :: <HDC> = get-DC(to-pixmap);
  BitBlt(to-hDC, to-x, to-y, width, height,
         from-hDC, from-x, from-y,
         $function-map[function])
end method do-copy-area;


/// Win32 images

define open abstract primary class <win32-image> (<image>)
  sealed constant slot image-width  :: <integer>,
    required-init-keyword: width:;
  sealed constant slot image-height :: <integer>,
    required-init-keyword: height:;
  //--- Type should be 'type-union(<byte-string>, <integer>, <machine-word>)'
  //--- except that 'make-gadget-control' expects to get a <byte-string>
  sealed constant slot image-resource-id :: <byte-string>,
    required-init-keyword: resource-id:;
end class <win32-image>;

//--- This probably belongs somewhere else, and could be done with <locator>s
define method concrete-resource-context
    (abstract-context == #"application",
     resource-type :: one-of(#"bitmap", #"icon"))
 => (concrete-context :: <HINSTANCE>)
  application-instance-handle()
end method concrete-resource-context;

define constant $null-<HINSTANCE> :: <HINSTANCE> = null-pointer(<HINSTANCE>);

define method concrete-resource-context
    (abstract-context == #"system",
     resource-type :: one-of(#"bitmap", #"icon"))
 => (concrete-context :: <HINSTANCE>)
  $null-<HINSTANCE>
end method concrete-resource-context;


/// Win32 images -- bitmaps

define sealed class <win32-bitmap> (<win32-image>)
  sealed constant slot image-handle :: false-or(<HBITMAP>),
    required-init-keyword: handle:;
  sealed slot %hBrush :: false-or(<HBRUSH>) = #f;
end class <win32-bitmap>;

define sealed domain make (singleton(<win32-bitmap>));
define sealed domain initialize (<win32-bitmap>);

define sideways method read-image-as
    (class :: subclass(<image>), resource-id :: <byte-string>,
     image-type == #"bitmap",
     #key width, height, error? = #f, resource-context = #"application")
 => (image :: false-or(<win32-bitmap>))
  let handle :: <HANDLE>
    = LoadImage(concrete-resource-context(resource-context, image-type),
                resource-id,
                $IMAGE-BITMAP, width | 0, height | 0, $LR-DEFAULTCOLOR);
  when (error?)
    check-result("LoadImage (bitmap)", handle)
  end;
  unless (null-pointer?(handle))
    let handle = pointer-cast(<HBITMAP>, handle);
    let (width, height)
      = if (width & height)
          values(width, height)
        else
          let (default-width, default-height) = win32-bitmap-size(handle);
          values(width | default-width, height | default-height)
        end;
    make(<win32-bitmap>,
         handle: handle,
         width: width, height: height,
         resource-id: resource-id)
  end
end method read-image-as;

// This handles ordinal bitmaps, OBM_BTNCORNERS, etc.
define sideways method read-image-as
    (class :: subclass(<image>), resource-id :: <integer>,
     image-type == #"bitmap",
     #key width, height, error? = #f, resource-context = #"application")
 => (image :: false-or(<win32-bitmap>))
  assert(resource-id > 32700 & resource-id < 32767,
         "The value #o%o is not a valid ordinal bitmap constant", resource-id);
  let resource-id-as-pointer = make(<LPTSTR>, address: resource-id);
  let handle :: <HBITMAP>
    = LoadBitmap(concrete-resource-context(resource-context, image-type),
                 resource-id-as-pointer);
  when (error?)
    check-result("LoadBitmap (bitmap)", handle)
  end;
  unless (null-pointer?(handle))
    let (width, height)
      = if (width & height)
          values(width, height)
        else
          let (default-width, default-height) = win32-bitmap-size(handle);
          values(width | default-width, height | default-height)
        end;
    make(<win32-bitmap>,
         handle: handle,
         width: width, height: height,
         //---*** This was 'resource-id: resource-id'
         //---*** The conversion to <byte-string> is just plain *wrong*,
         //---*** because trying to feed that string back to other Win32
         //---*** resource functions won't get the same object, if it works at all.
         resource-id: integer-to-string(resource-id))
  end
end method read-image-as;

define sealed method draw-image
    (medium :: <win32-medium>, bitmap :: <win32-bitmap>, x, y) => (record)
  let drawable     = medium-drawable(medium);
  let hDC :: <HDC> = get-DC(drawable);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let width  = image-width(bitmap);
    let height = image-height(bitmap);
    let hBrush
      = bitmap.%hBrush
        | begin
            //---*** Loses in Win-95 when the image is bigger than 8x8
            let hBrush = CreatePatternBrush(image-handle(bitmap));
            check-result("CreatePatternBrush", hBrush);
            bitmap.%hBrush := hBrush;
            hBrush
          end;
    let pen :: <HPEN> = $null-hpen;
    let old-hBrush = SelectObject(hDC, hBrush);
    let old-hPen   = SelectObject(hDC, pen);
    //--- '+ 1' because Windows doesn't draw the lower-right of rectangles
    Rectangle(hDC, x, y, x + width + 1, y + height + 1);
    SelectObject(hDC, old-hPen);
    SelectObject(hDC, old-hBrush)
  end;
  #f
end method draw-image;

define sealed method win32-bitmap-size
    (hBitmap :: <HBITMAP>) => (width :: <integer>, height :: <integer>)
  with-stack-structure (bitmap :: <LPBITMAP>)
    let result = GetObject(hBitmap, safe-size-of(<BITMAP>), bitmap);
    unless (result = 0)
      ensure-no-error("GetObject")
    end;
    values(bitmap.bmWidth-value, bitmap.bmHeight-value)
  end
end method win32-bitmap-size;


/// Win32 images -- icons

define sealed class <win32-icon> (<win32-image>)
  sealed constant slot image-handle :: false-or(<HICON>),
    required-init-keyword: handle:;
end class <win32-icon>;

define sealed domain make (singleton(<win32-icon>));
define sealed domain initialize (<win32-icon>);

define sideways method read-image-as
    (class :: subclass(<image>), resource-id :: <byte-string>,
     image-type == #"small-icon",
     #key width, height, error? = #f, resource-context = #"application")
 => (image :: false-or(<win32-icon>))
  read-image-as(class, resource-id, #"icon",
                width:  GetSystemMetrics($SM-CXSMICON),
                height: GetSystemMetrics($SM-CYSMICON),
                error?: error?,
                resource-context: resource-context)
end method read-image-as;

define sideways method read-image-as
    (class :: subclass(<image>), resource-id :: <byte-string>,
     image-type == #"large-icon",
     #key width, height, error? = #f, resource-context = #"application")
 => (image :: false-or(<win32-icon>))
  read-image-as(class, resource-id, #"icon",
                width:  GetSystemMetrics($SM-CXICON),
                height: GetSystemMetrics($SM-CYICON),
                error?: error?,
                resource-context: resource-context)
end method read-image-as;

define sideways method read-image-as
    (class :: subclass(<image>), resource-id :: <byte-string>,
     image-type == #"icon",
     #key width, height, error? = #f, resource-context = #"application")
 => (image :: false-or(<win32-icon>))
  let handle :: <HANDLE>
    = LoadImage(concrete-resource-context(resource-context, image-type),
                resource-id,
                $IMAGE-ICON, width | 0, height | 0, $LR-DEFAULTCOLOR);
  when (error?)
    check-result("LoadImage (icon)", handle)
  end;
  unless (null-pointer?(handle))
    let handle = pointer-cast(<HICON>, handle);
    let (width, height)
      = if (width & height)
          values(width, height)
        else
          let (default-width, default-height) = win32-icon-size(handle);
          values(width | default-width, height | default-height)
        end;
    make(<win32-icon>,
         handle: handle,
         width: width, height: height,
         resource-id: resource-id)
  end
end method read-image-as;

define sealed method draw-image
    (medium :: <win32-medium>, icon :: <win32-icon>, x, y) => (record)
  let drawable     = medium-drawable(medium);
  let hDC :: <HDC> = get-DC(drawable);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    let width  = image-width(icon);
    let height = image-height(icon);
    let hbrush = mirror-background-brush(medium-sheet(medium), drawable);
    DrawIconEx(hDC, x, y, image-handle(icon),
               width, height, 0, hbrush, $DI-COMPAT)
  end;
  #f
end method draw-image;

define sealed method win32-icon-size
    (hIcon :: <HICON>) => (width :: <integer>, height :: <integer>)
  with-stack-structure (icon-info :: <LPICONINFO>)
    let info   = GetIconInfo(hIcon, icon-info);
    let bitmap = icon-info.hbmColor-value;
    if (null-pointer?(bitmap))
      let mask = icon-info.hbmMask-value;
      win32-bitmap-size(mask)
    else
      win32-bitmap-size(bitmap)
    end
  end
end method win32-icon-size;
