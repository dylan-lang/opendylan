Module:       carbon-duim
Synopsis:     Macintosh pixmap implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MAC pixmaps
/*---*** Not doing pixmaps yet!

define sealed class <carbon-pixmap> (<pixmap>)
  sealed slot %pixmap :: false-or(<GdkPixmap*>) = #f,
    init-keyword: pixmap:;
  sealed slot %medium :: <carbon-medium>,
    required-init-keyword: medium:;
end class <carbon-pixmap>;

define sealed domain make (singleton(<carbon-pixmap>));
define sealed domain initialize (<carbon-pixmap>);

define sealed method do-make-pixmap
    (_port :: <carbon-port>, medium :: <carbon-medium>, 
     width :: <integer>, height :: <integer>)
 => (pixmap :: <carbon-pixmap>)
  make(<carbon-pixmap>, medium: medium, width: width, height: height)
end method do-make-pixmap;

define sealed method initialize
    (pixmap :: <carbon-pixmap>, #key width, height, medium) => ()
  next-method();
  let x-display = port(pixmap).%display;
  let x-screen  = x/ScreenOfDisplay(sheet-direct-mirror(display(medium)));
  let drawable  = medium-drawable(medium);
  let depth     = x/DefaultDepthOfScreen(x-screen);
  let x-pixmap  = gdk-pixmap-new(window, width, height, depth);
  let pixmap    = make(<xmeduim-pixmap>, port: port, x-pixmap: x-pixmap);
  //---*** Do we have to call 'gdk-pixmap-ref'???
  pixmap.%pixmap := x-pixmap
end method initialize;

define sealed method destroy-pixmap
    (pixmap :: <carbon-pixmap>) => ()
  let x-pixmap  = pixmap.%pixmap;
  pixmap.%pixmap := #f;
  gdk-pixmap-unref(x-pixmap)
end method destroy-pixmap;

define sealed method port 
    (pixmap :: <carbon-pixmap>) => (port :: <carbon-port>)
  port(pixmap.%medium)
end method port;

define sealed method pixmap-drawable
    (pixmap :: <carbon-pixmap>) => (drawable)
  pixmap
end method pixmap-drawable;

define sealed method image-width 
    (pixmap :: <carbon-pixmap>) => (width :: <integer>)
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  let (success?, root, x, y, width, height, border, depth)
    = x/XGetGeometry(x-display, x-pixmap);
  ignore(success?, root, x, y, height, border, depth);
  width
end method image-width;

define sealed method image-height 
    (pixmap :: <carbon-pixmap>) => (height :: <integer>)
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  let (success?, root, x, y, width, height, border, depth)
    = x/XGetGeometry(x-display, x-pixmap);
  ignore(success?, root, x, y, width, border, depth);
  height
end method image-height;

define sealed method image-depth 
    (pixmap :: <carbon-pixmap>) => (depth :: <integer>)
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  let (success?, root, x, y, width, height, border, depth)
    = x/XGetGeometry(x-display, x-pixmap);
  ignore(success?, root, x, y, width, height, border);
  depth
end method image-depth;


/// MAC pixmap mediums

define sealed class <carbon-pixmap-medium>
    (<carbon-medium>, 
     <basic-pixmap-medium>)
end class <carbon-pixmap-medium>;

define sealed domain make (singleton(<carbon-pixmap-medium>));
define sealed domain initialize (<carbon-pixmap-medium>);

define sealed method make-pixmap-medium
    (_port :: <carbon-port>, sheet :: <sheet>, #key width, height)
 => (medium :: <carbon-pixmap-medium>)
  with-sheet-medium (medium = sheet)
    let pixmap = do-make-pixmap(_port, medium, width, height);
    let medium = make(<carbon-pixmap-medium>,
		      port: _port,
		      sheet: sheet,
		      pixmap: pixmap);
    medium-drawable(medium) := pixmap;
    medium
  end
end method make-pixmap-medium;


/// BitBlt

//---*** THESE ALL NEED TO GET A GC TO DO THE COPYING ON AND ESTABLISH THE COPYING GCONTEXT

define sealed method do-copy-area
    (from-medium :: <carbon-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <carbon-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  if (from-medium == to-medium)
    let sheet     = medium-sheet(from-medium);
    let transform = sheet-device-transform(sheet);
    let drawable  = medium-drawable(from-medium);
    with-device-coordinates (transform, from-x, from-y, to-x, to-y)
      with-device-distances (transform, width, height)
	gdk-window-copy-area(drawable, gcontext, to-x, to-y,
			     drawable, from-x, from-y, width, height)
      end
    end
  else
    let from-sheet     = medium-sheet(from-medium);
    let from-transform = sheet-device-transform(from-sheet);
    let from-drawable  = medium-drawable(from-medium);
    let to-sheet       = medium-sheet(to-medium);
    let to-transform   = sheet-device-transform(to-sheet);
    let to-drawable    = medium-drawable(to-medium);
    with-device-coordinates (from-transform, from-x, from-y)
      with-device-coordinates (to-transform, to-x, to-y)
	with-device-distances (from-transform, width, height)
	  gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
			       from-drawable, from-x, from-y, width, height)
	end
      end
    end
  end
  */
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <carbon-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <carbon-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  let from-transform = sheet-device-transform(medium-sheet(from-medium));
  let from-drawable  = medium-drawable(from-medium);
  let to-drawable    = medium-drawable(to-medium);
  with-device-coordinates (from-transform, from-x, from-y)
    with-device-distances (from-transform, width, height)
      gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
			   from-drawable, from-x, from-y, width, height)
    end
  end
  */
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <carbon-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     pixmap :: <carbon-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  let from-transform = sheet-device-transform(medium-sheet(from-medium));
  let from-drawable  = medium-drawable(from-medium);
  let to-drawable    = pixmap.%pixmap;
  with-device-coordinates (from-transform, from-x, from-y)
    with-device-distances (from-transform, width, height)
      gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
			   from-drawable, from-x, from-y, width, height)
    end
  end
  */
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <carbon-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <carbon-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  let to-transform  = sheet-device-transform(medium-sheet(to-medium));
  let from-drawable = medium-drawable(from-medium);
  let to-drawable   = medium-drawable(to-medium);
  with-device-coordinates (to-transform, to-x, to-y)
    gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
			 from-drawable, from-x, from-y, width, height)
  end
  */
end method do-copy-area;

define sealed method do-copy-area
    (pixmap :: <carbon-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <carbon-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  let to-transform  = sheet-device-transform(medium-sheet(to-medium));
  let from-drawable = pixmap.%pixmap;
  let to-drawable   = medium-drawable(to-medium);
  with-device-coordinates (to-transform, to-x, to-y)
    gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
			 from-drawable, from-x, from-y, width, height)
  end 
  */
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <carbon-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <carbon-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  let from-drawable = medium-drawable(from-medium);
  let to-drawable   = medium-drawable(to-medium);
  gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
		       from-drawable, from-x, from-y, width, height)
  */
end method do-copy-area;

define sealed method do-copy-area
    (from-pixmap :: <carbon-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-pixmap :: <carbon-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  ignoring("do-copy-area");
  /*
  let from-drawable = from-pixmap.%pixmap;
  let to-drawable   = to-pixmap.%pixmap;
  gdk-window-copy-area(to-drawable, gcontext, to-x, to-y,
		       from-drawable, from-x, from-y, width, height)
  */
end method do-copy-area;
*/
