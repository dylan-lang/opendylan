Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <capi-pixmap> (<pixmap>)
  sealed slot port :: false-or(<port>),
    required-init-keyword: port:,
    setter: %port-setter;
  sealed slot %pixmap = #f;     // the native CAPI pixmap
end class <capi-pixmap>;

define method do-make-pixmap
    (_port :: <capi-port>, medium :: <capi-medium>, width, height)
  make(<capi-pixmap>, port: _port, width: width, height: height)
end method do-make-pixmap;

define method initialize (pixmap :: <capi-pixmap>, #key width, height) => ()
  next-method();
  let gp-pixmap = make(<capi-pixmap-representation>);
  let _port = port(pixmap);
  representation(gp-pixmap)
    := realize-pixmap-for-port
         (representation(_port.%screen), gp-pixmap, width, height);
  pixmap.%pixmap := gp-pixmap
end method initialize;

define method destroy-pixmap (pixmap :: <capi-pixmap>) => ()
  clear-ink-cache(pixmap);
  gp-destroy-pixmap(pixmap.%pixmap)
end method destroy-pixmap;


define method pixmap-drawable (pixmap :: <capi-pixmap>)
  pixmap.%pixmap
end method pixmap-drawable;

define method clear-ink-cache (pixmap :: <capi-pixmap>)
  clear-ink-cache(pixmap.%pixmap)
end method clear-ink-cache;



//--- We shouldn't need this extra level of indirection, but for some
//--- reason we are getting the ink cache from the wrong place without
//--- it.  Hopefully this is just a GP/CAPI artifact...
define sealed class <capi-pixmap-representation> 
    (<capi-drawable>, <duim-pixmap>)
end class <capi-pixmap-representation>;


define method image-width (pixmap :: <capi-pixmap>)
  port-width(pixmap.%pixmap)
end method image-width;

define method image-height (pixmap :: <capi-pixmap>)
  port-height(pixmap.%pixmap)
end method image-height;

define method image-depth (pixmap :: <capi-pixmap>)
  //--- do it
end method image-depth;


define sealed class <capi-pixmap-medium>
    (<capi-medium>, <basic-pixmap-medium>)
end class <capi-pixmap-medium>;

define method make-pixmap-medium
    (_port :: <capi-port>, sheet :: <sheet>, #key width, height)
  with-sheet-medium (medium = sheet)
    let pixmap = do-make-pixmap(_port, medium, width, height);
    let pixmap-medium = make(<capi-pixmap-medium>,
			     port: _port,
			     sheet: sheet,
			     pixmap: pixmap);
    medium-drawable(pixmap-medium) := pixmap.%pixmap;
    pixmap-medium
  end
end method make-pixmap-medium;


/// COPY-AREA

define method do-copy-area
    (from-medium :: <capi-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <capi-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  if (from-medium == to-medium)
    let transform = sheet-device-transform(medium-sheet(from-medium));
    with-device-coordinates (transform, from-x, from-y, to-x, to-y)
      with-device-distances (transform, width, height)
	let rep = representation(medium-drawable(from-medium));
	gp-pixblt(rep, function, rep, 
		  to-x, to-y, width, height, 
		  from-x, from-y)
      end
    end
  else
    let from-transform = sheet-device-transform(medium-sheet(from-medium));
    let to-transform   = sheet-device-transform(medium-sheet(to-medium));
    with-device-coordinates (from-transform, from-x, from-y)
      with-device-coordinates (to-transform, to-x, to-y)
	with-device-distances (from-transform, width, height)
	  let from-rep = representation(medium-drawable(from-medium));
	  let to-rep   = representation(medium-drawable(to-medium));
	  gp-pixblt(to-rep, function, from-rep,
		    to-x, to-y, width, height, 
		    from-x, from-y)
	end
      end
    end
  end
end method do-copy-area;

define method do-copy-area
    (from-medium :: <capi-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <capi-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  let transform = sheet-device-transform(medium-sheet(from-medium));
  with-device-coordinates (transform, from-x, from-y)
    with-device-distances (transform, width, height)
      let pixmap-rep = port-pixmap-representation(medium-drawable(to-medium));
      let window-rep = representation(medium-drawable(from-medium));
      gp-pixblt(pixmap-rep, function, window-rep, to-x, to-y, width, height, from-x, from-y)
    end
  end
end method do-copy-area;

define method do-copy-area
    (from-medium :: <capi-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <capi-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  let transform = sheet-device-transform(medium-sheet(to-medium));
  with-device-coordinates (transform, to-x, to-y)
    let pixmap-rep = port-pixmap-representation(medium-drawable(from-medium));
    let window-rep = representation(medium-drawable(to-medium));
    gp-pixblt(window-rep, function, pixmap-rep, to-x, to-y, width, height, from-x, from-y)
  end
end method do-copy-area;

define method do-copy-area
    (from-medium :: <capi-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <capi-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  let from-rep = port-pixmap-representation(medium-drawable(from-medium));
  let to-rep   = port-pixmap-representation(medium-drawable(to-medium));
  gp-pixblt(to-rep, function, from-rep, to-x, to-y, width, height, from-x, from-y)
end method do-copy-area;

define method do-copy-area
    (from-medium :: <capi-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     pixmap :: <capi-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  let transform = sheet-device-transform(medium-sheet(from-medium));
  with-device-coordinates (transform, from-x, from-y)
    with-device-distances (transform, width, height)
      let pixmap-rep = port-pixmap-representation(pixmap.%pixmap);
      let window-rep = representation(medium-drawable(from-medium));
      gp-pixblt(pixmap-rep, function, window-rep, to-x, to-y, width, height, from-x, from-y)
    end
  end
end method do-copy-area;

define method do-copy-area
    (pixmap :: <capi-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <capi-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  let transform = sheet-device-transform(medium-sheet(to-medium));
  with-device-coordinates (transform, to-x, to-y)
    let pixmap-rep = port-pixmap-representation(pixmap.%pixmap);
    let window-rep = representation(medium-drawable(to-medium));
    gp-pixblt(window-rep, function, pixmap-rep, to-x, to-y, width, height, from-x, from-y)
  end
end method do-copy-area;

define method do-copy-area
    (from-pixmap :: <capi-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-pixmap :: <capi-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1)
  let from-rep = port-pixmap-representation(to-pixmap.%pixmap);
  let to-rep   = port-pixmap-representation(to-pixmap.%pixmap);
  gp-pixblt(to-rep, function, from-rep, to-x, to-y, width, height, from-x, from-y)
end method do-copy-area;


/// Support read-image

define class <generic-image-pixmap> (<pixmap>)
  sealed slot %image,
    required-init-keyword: image:;
end class <generic-image-pixmap>;

define method read-image-as
    (class == <pixmap>, string :: <string>, image-type,
     #key port = find-capi-port())
 => (image :: <capi-pixmap>)
  let gp-image = gp-load-generic-image(representation(port.%screen), string);
  make(<generic-image-pixmap>, image: gp-image)
end method read-image-as;

define method image-width
    (image :: <generic-image-pixmap>) => (width :: <integer>)
  gp-generic-image-width(image.%image)
end method image-width;

define method image-height
    (image :: <generic-image-pixmap>) => (height :: <integer>)
  gp-generic-image-height(image.%image)
end method image-height;

define method image-depth
    (image :: <generic-image-pixmap>) => (depth :: <integer>)
  gp-generic-image-depth(image.%image)
end method image-depth;

//---*** This doesn't seem to work, so we implement a 'draw-image'
//---*** method to get at least the simple case going
define method convert-ink-to-capi-components
    (medium :: <capi-medium>, drawable, brush :: <generic-image-pixmap>)
  let cache = drawable.%ink-cache;
  let rep = ensure-representation(drawable);
  let pattern
    = #f // gethash(cache, brush)
      | begin
          let image = brush.%image;
	  gethash(cache, brush) := image;
	  image
	end;
  //--- #"white" and white: are different things in the emulator...
  values(convert-color(rep, white:), #"solid", $boole-1, pattern)
end method convert-ink-to-capi-components;

define method draw-image
    (medium :: <capi-medium>, image :: <generic-image-pixmap>, x, y) => (record)
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, x, y)
    // We do image drawing by using the same gcontext as everyone else,
    // so make sure the cached brush gets invalidated before and after
    let drawable = update-drawing-state(medium);
    let width  = image-width(image);
    let height = image-height(image);
    when (drawable)
      gp-draw-generic-image(drawable, image.%image, x, y, width, height)
    end
  end;
  #f
end method draw-image;
