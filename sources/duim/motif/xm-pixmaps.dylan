Module:    motif-duim
Synopsis:  Motif pixmap implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif pixmaps

define sealed class <motif-pixmap> (<pixmap>)
  sealed slot %pixmap :: false-or(x/<Pixmap>) = #f,
    init-keyword: pixmap:;
  sealed slot %medium :: <motif-medium>,
    required-init-keyword: medium:;
end class <motif-pixmap>;

define sealed domain make (singleton(<motif-pixmap>));
define sealed domain initialize (<motif-pixmap>);

define sealed method do-make-pixmap
    (_port :: <motif-port>, medium :: <motif-medium>, 
     width :: <integer>, height :: <integer>)
 => (pixmap :: <motif-pixmap>)
  make(<motif-pixmap>, medium: medium, width: width, height: height)
end method do-make-pixmap;

define sealed method initialize
    (pixmap :: <motif-pixmap>, #key width, height, medium) => ()
  next-method();
  let x-display = port(pixmap).%display;
  let x-screen  = x/ScreenOfDisplay(sheet-direct-mirror(display(medium)));
  let drawable  = medium-drawable(medium);
  let depth     = x/DefaultDepthOfScreen(x-screen);
  let x-pixmap  = x/XCreatePixmap(x-display, drawable, width, height, depth);
  let pixmap    = make(<xmeduim-pixmap>, port: port, x-pixmap: x-pixmap);
  pixmap.%pixmap := x-pixmap
end method initialize;

define sealed method destroy-pixmap
    (pixmap :: <motif-pixmap>) => ()
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  pixmap.%pixmap := #f;
  x/XFreePixmap(x-display, x-pixmap)
end method destroy-pixmap;

define sealed method port 
    (pixmap :: <motif-pixmap>) => (port :: <motif-port>)
  port(pixmap.%medium)
end method port;

define sealed method pixmap-drawable
    (pixmap :: <motif-pixmap>) => (drawable)
  pixmap
end method pixmap-drawable;

define sealed method image-width 
    (pixmap :: <motif-pixmap>) => (width :: <integer>)
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  let (success?, root, x, y, width, height, border, depth)
    = x/XGetGeometry(x-display, x-pixmap);
  ignore(success?, root, x, y, height, border, depth);
  width
end method image-width;

define sealed method image-height 
    (pixmap :: <motif-pixmap>) => (height :: <integer>)
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  let (success?, root, x, y, width, height, border, depth)
    = x/XGetGeometry(x-display, x-pixmap);
  ignore(success?, root, x, y, width, border, depth);
  height
end method image-height;

define sealed method image-depth 
    (pixmap :: <motif-pixmap>) => (depth :: <integer>)
  let x-display = port(pixmap).%display;
  let x-pixmap  = pixmap.%pixmap;
  let (success?, root, x, y, width, height, border, depth)
    = x/XGetGeometry(x-display, x-pixmap);
  ignore(success?, root, x, y, width, height, border);
  depth
end method image-depth;


/// Motif pixmap mediums

define sealed class <motif-pixmap-medium>
    (<motif-medium>, 
     <basic-pixmap-medium>)
end class <motif-pixmap-medium>;

define sealed domain make (singleton(<motif-pixmap-medium>));
define sealed domain initialize (<motif-pixmap-medium>);

define sealed method make-pixmap-medium
    (_port :: <motif-port>, sheet :: <sheet>, #key width, height)
 => (medium :: <motif-pixmap-medium>)
  with-sheet-medium (medium = sheet)
    let pixmap = do-make-pixmap(_port, medium, width, height);
    let medium = make(<motif-pixmap-medium>,
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
    (from-medium :: <motif-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <motif-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  if (from-medium == to-medium)
    let sheet     = medium-sheet(from-medium);
    let transform = sheet-device-transform(sheet);
    let drawable  = medium-drawable(from-medium);
    with-device-coordinates (transform, from-x, from-y, to-x, to-y)
      with-device-distances (transform, width, height)
	x/XCopyArea(x-display, drawable, drawable,
		    gcontext,
		    from-x, from-y, width, height, to-x, to-y)
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
	  x/XCopyArea(x-display, from-drawable, to-drawable,
		      gcontext,
		      from-x, from-y, width, height, to-x, to-y)
	end
      end
    end
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <motif-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <motif-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-transform = sheet-device-transform(medium-sheet(from-medium));
  let from-drawable  = medium-drawable(from-medium);
  let to-drawable    = medium-drawable(to-medium);
  with-device-coordinates (from-transform, from-x, from-y)
    with-device-distances (from-transform, width, height)
      x/XCopyArea(x-display, from-drawable, to-drawable,
		  gcontext,
		  from-x, from-y, width, height, to-x, to-y)
    end
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <motif-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     pixmap :: <motif-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-transform = sheet-device-transform(medium-sheet(from-medium));
  let from-drawable  = medium-drawable(from-medium);
  let to-drawable    = pixmap.%pixmap;
  with-device-coordinates (from-transform, from-x, from-y)
    with-device-distances (from-transform, width, height)
      x/XCopyArea(x-display, from-drawable, to-drawable,
		  gcontext,
		  from-x, from-y, width, height, to-x, to-y)
    end
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <motif-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <motif-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let to-transform  = sheet-device-transform(medium-sheet(to-medium));
  let from-drawable = medium-drawable(from-medium);
  let to-drawable   = medium-drawable(to-medium);
  with-device-coordinates (to-transform, to-x, to-y)
    x/XCopyArea(x-display, from-drawable, to-drawable,
		gcontext,
		from-x, from-y, width, height, to-x, to-y)
  end
end method do-copy-area;

define sealed method do-copy-area
    (pixmap :: <motif-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <motif-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let to-transform  = sheet-device-transform(medium-sheet(to-medium));
  let from-drawable = pixmap.%pixmap;
  let to-drawable   = medium-drawable(to-medium);
  with-device-coordinates (to-transform, to-x, to-y)
    x/XCopyArea(x-display, from-drawable, to-drawable,
		gcontext,
		from-x, from-y, width, height, to-x, to-y)
  end
end method do-copy-area;

define sealed method do-copy-area
    (from-medium :: <motif-pixmap-medium>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-medium :: <motif-pixmap-medium>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-drawable = medium-drawable(from-medium);
  let to-drawable   = medium-drawable(to-medium);
  x/XCopyArea(x-display, from-drawable, to-drawable,
	      gcontext,
	      from-x, from-y, width, height, to-x, to-y)
end method do-copy-area;

define sealed method do-copy-area
    (from-pixmap :: <motif-pixmap>, from-x :: <integer>, from-y :: <integer>,
     width :: <integer>, height :: <integer>,
     to-pixmap :: <motif-pixmap>, to-x :: <integer>, to-y :: <integer>,
     #key function = $boole-1) => ()
  let from-drawable = from-pixmap.%pixmap;
  let to-drawable   = to-pixmap.%pixmap;
  x/XCopyArea(x-display, from-drawable, to-drawable,
	      gcontext,
	      from-x, from-y, width, height, to-x, to-y)
end method do-copy-area;
