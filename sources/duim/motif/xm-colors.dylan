Module:    motif-duim
Synopsis:  Motif color and palette implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Palettes

//--- How much more do we need to flesh out palettes?
define sealed class <motif-palette> (<basic-palette>)
  sealed slot port :: false-or(<port>),
    required-init-keyword: port:,
    setter: %port-setter;
  sealed slot %colormap,
    required-init-keyword: colormap:;
  sealed slot %default-drawable,
    init-keyword: drawable:;
  sealed slot %gcontext = #f;
  sealed constant slot %gc-cache      :: <object-table> = make(<table>);
  sealed constant slot %color-cache   :: <object-table> = make(<table>);
  sealed constant slot %pattern-cache :: <object-table> = make(<table>);
end class <motif-palette>;

define sealed method make-palette
    (_port :: <motif-port>, #key color?, dynamic?, colormap, #all-keys)
 => (palette :: <motif-palette>)
  make(<motif-palette>,
       port: _port, color?: color?, dynamic?: dynamic?,
       colormap: colormap)
end method make-palette;

define sealed method install-default-palette
    (_port :: <motif-port>) => ()
  let visual   = xt/GetShellVisual(port.%app-shell);
  let vclass   = xt/VisualClass(visual);
  let colormap = xt/XtGetValues(app-shell, #"colormap");
  let (color?, dynamic?)
    = select (vclass)
	#"static-gray"  => values(#f, #f);
	#"gray-scale"   => values(#f, #t);
	#"static-color" => values(#t, #f);
	#"true-color"   => values(#t, #f);
	#"pseudo-color" => values(#t, #t);
	#"direct-color" => values(#t, #t);
      end;
  port-default-palette(_port)
    := make-palette(port,
		    color?: color?, dynamic?: dynamic?,
		    colormap: colormap)
end method install-default-palette;

define sealed method palette-depth
    (palette :: <motif-palette>) => (depth :: <integer>)
  let _port    = port(palette);
  let drawable = palette.%drawable;
  if (_port)
    let attributes = x/XGetWindowAttributes(port.%display, drawable);
    let depth      = attributes.depth-value;
    //---*** FREE THE ATTRIBUTE'S FFI OBJECT
    depth
  else
    1
  end
end method palette-depth;


define sealed method allocate-color 
    (color :: <color>, palette :: <motif-palette>)
 => (pixel :: <integer>)
  let cache = palette.%color-cache;
  gethash(cache, color)
  | begin
      let pixel = do-allocate-color(palette, color);
      gethash(cache, color) := pixel;
      pixel
    end
end method allocate-color;
	
define sealed method deallocate-color 
    (color :: <color>, palette :: <motif-palette>) => ()
  let cache = palette.%color-cache;
  let pixel = gethash(cache, color);
  when (pixel)
    let x-display = port(palette).%display;
    let pixels    = list(pixel);
    let planes    = 0;
    x-free-colors(x-display, palette.%colormap, pixels, planes);
    remhash(cache, color)
  end
end method deallocate-color;

define sealed method clear-colors
    (palette :: <motif-palette>) => ()
  let cache     = palette.%color-cache;
  let x-display = port(palette).%display;
  let pixels    = list(pixel);
  let planes    = 0;
  local method collect-pixel (color, pixel)
	  ignore(color);
	  pixels := add-new!(pixels, pixel, test: \=)
	end method;
  do(collect-pixel, cache);
  x-free-colors(x-display, medium.%colormap, pixels, planes);
  remove-all-keys!(cache)
end method clear-colors;


define sealed method do-allocate-color
    (palette :: <motif-palette>, color :: <rgb-color>)
 => (pixel :: <integer>)
  let x-display = port(palette).%display;
  let colormap  = palette.%colormap;
  let (r, g, b) = color-rgb(color);
  allocate-x-color(r, g, b, x-display, colormap)
end method do-allocate-color;

define sealed method do-allocate-color
    (palette :: <motif-palette>, color :: <contrasting-color>)
 => (pixel :: <integer>)
  let x-display = port(palette).%display;
  let colormap  = palette.%colormap;
  let (r, g, b) = color-rgb(contrasting-color->color(color));
  allocate-x-color(r, g, b, x-display, colormap)
end method do-allocate-color;

define sealed method do-allocate-color
    (palette :: <motif-palette>, color :: <dynamic-color>)
 => (pixel :: <integer>)
  //--- Check that the palette is dynamic and color
  let x-display = port(palette).%display;
  let colormap  = palette.%colormap;
  let (r, g, b) = color-rgb(color);
  let pixel     = allocate-x-read-write-color(r, g, b, x-display, colormap);
  dynamic-color-palettes(color) := add-new!(dynamic-color-palettes(color), color);
  pixel
end method do-allocate-color;


define sealed method find-color
    (name, palette :: <motif-palette>, #key error? = #t)
 => (color :: <rgb-color>)
  let _port     = port(palette);
  let x-display = port.%display;
  let colormap  = palette.%colormap;
  let cache     = palette.%color-cache;
  with-stack-structure (screen-color :: x/<XColor>)
    with-stack-structure (exact-color :: x/<XColor>)
      if (x/XAllocNamedColor(x-display, x-colormap, name, screen-color, exact-color))
	let color = make-rgb-color(16-bits->float(screen-color.x/red-value),
				   16-bits->float(screen-color.x/green-value),
				   16-bits->float(screen-color.x/blue-value));
	let pixel = screen-color.x/pixel-value;
	gethash(cache, color) := pixel;
	color
      else
	error? & error(make(<color-not-found>, color: name))
      end
    end
  end
end method find-color;


define inline function color->native-color
    (color :: <color>, palette :: <motif-palette>)
 => (native-color :: <integer>)
  allocate-color(color, palette)
end function color->native-color;

define inline function native-color->color
    (native-color :: <integer>, palette :: <motif-palette>)
 => (color :: <rgb-color>)
  query-pixel-for-color(native-color, palette)
end function native-color->color;

define method query-pixel-for-color
    (pixel :: <integer>, palette :: <motif-palette>)
 => (color :: <rgb-color>)
  let _port    = palette.%port;
  let colormap = palette.%colormap;
  let (r, g, b) = x/XQueryColor(_port.%display, colormap, pixel);
  make-rgb-color(16-bits->float(r), 16-bits->float(g), 16-bits->float(b))
end method query-pixel-for-color;


/// Color utilities

define constant $16-bits :: <integer> = ash(1, 16) - 1;

define inline function float->16-bits
    (x :: <real) => (16bits :: <integer>)
  round(as(<single-float>, x) * $16-bits)
end function float->16-bits;

define inline function 16-bits->float
    (16bits :: <integer>) => (x :: <real)
  as(<single-float>, 16-bits) / $16-bits
end function 16-bits->float;

define sealed method allocate-x-color
    (red :: <real>, green :: <real>, blue :: <real>,
     x-display :: x/<Display>, x-colormap :: x/<Colormap>)
 => (pixel :: <integer>)
  with-stack-structure (x-color :: x/<XColor>)
    x-color.x/red-value   := float->16-bits(red);
    x-color.x/green-value := float->16-bits(green);
    x-color.x/blue-value  := float->16-bits(blue);
    //--- Handle colormap resource exhaustion
    x/XAllocColor(x-display, x-colormap, x-color);
    x-color.x/pixel-value
  end
end method allocate-x-color;

define sealed method allocate-x-read-write-color
    (red :: <integer>, green :: <integer>, blue :: <integer>,
     x-display :: x/<Display>, x-colormap :: x/<Colormap>)
 => (pixel :: <integer>)
  //--- Handle colormap resource exhaustion
  let (plane-masks, pixels)
    = x/XAllocColorCells(x-display, x-colormap, #f, 0, 1)
  ignore(plane-masks);
  let pixel = pixels[0];
  set-x-read-write-color(pixel, red, green, blue, x-display, x-colormap);
  pixel
end method allocate-x-read-write-color;

define sealed method set-x-read-write-color
    (red :: <integer>, green :: <integer>, blue :: <integer>,
     x-display :: x/<Display>, x-colormap :: x/<Colormap>) => ()
  with-stack-structure (x-color :: x/<XColor>)
    x-color.x/red-value   := float->16-bits(red);
    x-color.x/green-value := float->16-bits(green);
    x-color.x/blue-value  := float->16-bits(blue);
    x-color.x/pixel-value := pixel;
    x-color.x/flags-value := 7;
    x/XStoreColor(x-display, x-colormap, x-color)
  end
end method allocate-x-read-write-color;
