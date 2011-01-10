Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Palettes

define protocol <<palette-protocol>> ()
  function make-palette
    (port, #key, #all-keys) => (palette :: <palette>);
  function add-colors
    (palette :: <palette>, #rest colors) => (palette :: <palette>);
  function do-add-colors
    (palette :: <palette>, #rest colors) => ();
  function remove-colors
    (palette :: <palette>, #rest colors) => (palette :: <palette>);
  function do-remove-colors
    (palette :: <palette>, #rest colors) => ();
  function find-color
    (name, palette, #key error?) => (color :: <color>);
  function allocate-color
    (color :: <color>, palette :: <palette>) => (pixel);
  function deallocate-color
    (color :: <color>, palette :: <palette>) => ();
  function update-palette-entry
    (palette :: <palette>, pixel, color) => ();
  function update-palette-entries
    (palette :: <palette>, updates) => ();
  getter color-palette?
    (palette :: <palette>) => (true? :: <boolean>);
  getter dynamic-palette?
    (palette :: <palette>) => (true? :: <boolean>);
end protocol <<palette-protocol>>;


define open abstract primary class <basic-palette> (<palette>)
  sealed slot color-palette? :: <boolean> = #f,
    init-keyword: color?:;
  sealed slot dynamic-palette? :: <boolean> = #f,
    init-keyword: dynamic?:;
  sealed slot palette-color-cache         :: <object-table> = make(<table>);
  sealed slot palette-dynamic-color-cache :: <object-table> = make(<table>);
  sealed slot palette-layered-color-cache :: <object-table> = make(<table>);
  // A vector of pairs of (cell, color)...
  sealed slot palette-delayed-recolors :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <basic-palette>;

define sealed inline method make
    (class == <palette>, #rest initargs, #key port, #all-keys)
 => (palette :: <palette>)
  dynamic-extent(initargs);
  apply(make-palette, port, initargs)
end method make;


define variable *palettes* :: <stretchy-object-vector> = make(<stretchy-vector>);

define method initialize (palette :: <palette>, #key)
  next-method();
  add!(*palettes*, palette)
end method initialize;


define sealed class <palette-full> (<error>)
  sealed constant slot %palette, required-init-keyword: palette:;
end class <palette-full>;

define method condition-to-string
    (condition :: <palette-full>) => (string :: <string>)
  format-to-string("The palette %= is full", condition.%palette)
end method condition-to-string;


define method add-colors
    (palette :: <palette>, #rest colors) => (palette :: <palette>)
  dynamic-extent(colors);
  let colors-done = #();
  for (color in colors)
    block ()
      push!(colors-done, color);
      allocate-color(color, palette)
    exception (condition :: <palette-full>)
      for (color in colors-done)
        deallocate-color(color, palette)
      end;
      error(condition)
    end
  end;
  apply(do-add-colors, palette, colors);
  palette
end method add-colors;

define method do-add-colors (palette :: <palette>, #rest colors) => ()
  dynamic-extent(colors);
  #f
end method do-add-colors;

define method remove-colors
    (palette :: <palette>, #rest colors) => (palette :: <palette>)
  dynamic-extent(colors);
  apply(do-remove-colors, palette, colors);
  for (color in colors)
    deallocate-color(color, palette)
  end;
  palette
end method remove-colors;

define method do-remove-colors (palette :: <palette>, #rest colors) => ()
  dynamic-extent(colors);
  #f
end method do-remove-colors;


/// Dynamic Colors

define protocol <<dynamic-color-protocol>> ()
  getter dynamic-color-color
    (dynamic-color) => (color :: <color>);
  setter dynamic-color-color-setter
    (color :: <color>, dynamic-color) => (color :: <color>);
end protocol <<dynamic-color-protocol>>;


define sealed class <dynamic-color> (<color>)
  sealed slot dynamic-color-color :: <color>,
    required-init-keyword: color:,
    setter: %color-setter;
  sealed slot dynamic-color-palettes :: <list> = #(),
    setter: %palettes-setter;
end class <dynamic-color>;

define method dynamic-color-color-setter
    (color :: <color>, dynamic-color :: <dynamic-color>) => (color :: <color>)
  dynamic-color.%color := color;
  recolor-dynamic-color(dynamic-color, color);
  color
end method dynamic-color-color-setter;

define method dynamic-color-palettes-setter
    (palettes :: <list>, dynamic-color :: <dynamic-color>) => (palettes :: <list>)
  dynamic-color.%palettes := palettes
end method dynamic-color-palettes-setter;

// make(<dynamic-color>) works, too
define inline function make-dynamic-color
    (color :: <color>) => (color :: <dynamic-color>)
  make(<dynamic-color>, color: color)
end function make-dynamic-color;

define method color-rgb
    (color :: <dynamic-color>)
 => (red :: <real>, green :: <real>, blue :: <real>, opacity :: <real>)
  color-rgb(dynamic-color-color(color))
end method color-rgb;

define method color-ihs
    (color :: <dynamic-color>)
 => (intensity :: <real>, hue :: <real>, saturation :: <real>, opacity :: <real>)
  color-ihs(dynamic-color-color(color))
end method color-ihs;

define thread variable *doing-delayed-recolors* = #f;

define method recolor-dynamic-color
    (dynamic-color :: <dynamic-color>, color :: <color>) => ()
  if (*doing-delayed-recolors*)
    for (palette in dynamic-color-palettes(dynamic-color))
      let cell = gethash(palette-dynamic-color-cache(palette), dynamic-color);
      let recolors = palette-delayed-recolors(palette);
      add!(recolors, pair(cell, color))
    end
  else
    for (palette in dynamic-color-palettes(dynamic-color))
      let cell = gethash(palette-dynamic-color-cache(palette), dynamic-color);
      update-palette-entry(palette, cell, color)
    end
  end
end method recolor-dynamic-color;

// Note that the actual color recoloring occurs on exiting the outermost
// call to 'with-delayed-recoloring'
define macro with-delayed-recoloring
  { with-delayed-recoloring ?:body end }
    => { begin
	   let _doing-delayed-recolors = *doing-delayed-recolors*;
	   dynamic-bind (*doing-delayed-recolors* = #t)
	     block ()
	       ?body
	     cleanup
	       unless (_doing-delayed-recolors)
		 for (_palette in *palettes*)
		   let _recolors = palette-delayed-recolors(_palette);
		   update-palette-entries(_palette, _recolors);
		   _recolors.size := 0
		 end
	       end
	     end
	   end
	 end }
end macro with-delayed-recoloring;


/// Layered Colors

define protocol <<layered-color-protocol>> ()
end protocol <<layered-color-protocol>>;


define sealed class <layered-color-set-table> (<table>)
end class <layered-color-set-table>;

define sealed method table-protocol
    (table :: <layered-color-set-table>)
 => (test :: <function>, hash :: <function>);
  values(\=, sequence-hash)
end method table-protocol;


define sealed class <layered-color-set> (<object>)
  sealed slot layered-color-set-layers = #(),
    init-keyword: layers:;
  sealed slot layered-color-set-cache = make(<layered-color-set-table>);
  sealed slot layered-color-set-dynamic-array,
    init-keyword: dynamic-array:;
end class <layered-color-set>;

define open generic layered-color (layered-color-set, #rest layers);

define inline function make-layered-color-set
    (#rest layers) => (color-set :: <layered-color-set>)
  make(<layered-color-set>, 
       layers: copy-sequence(layers),
       dynamic-array: make(<array>, dimensions: layers))
end function make-layered-color-set;


define sealed class <layered-color> (<ink>)
  sealed slot layered-color-set,
    init-keyword: set:;
  sealed slot layered-color-layers = #[],
    init-keyword: layers:;
  sealed slot %dynamic-colors = #[];
end class <layered-color>;

define inline function make-layered-color
    (set, layers) => (layered-color :: <layered-color>)
  make(<layered-color>, set: set, layers: layers)
end function make-layered-color;

define method do-layered-colors
    (function :: <function>, set :: <layered-color-set>, #key layers = #()) => ()
  local method do-layers (layers, set-layers, dims) => ()
	  if (empty?(set-layers))
	    function(dimensions)
	  else
	    let layer = head(layers);
	    let rest-layers = tail(layers);
	    let set-layer :: <integer> = head(set-layers);
	    let rest-set-layers = tail(set-layers);
	    let rest-dims = tail(dims);
	    if (layer)
	      head(dims) := layer;
	      do-layers(rest-layers, rest-set-layers, rest-dims)
	    else
	      for (i :: <integer> from 0 below set-layer)
		head(dims) := i;
		do-layers(rest-layers, rest-set-layers, rest-dims)
	      end
	    end
	  end
	end method;
  let set-layers = layered-color-set-layers(set);
  let dimensions = make(<list>, size: size(set-layers));
  do-layers(as(<list>, layers), set-layers, dimensions)
end method do-layered-colors;

define sealed method initialize
    (set :: <layered-color-set>, #key dynamic-array)
  next-method();
  do-layered-colors
    (method (dimensions)
       apply(aref-setter, make-dynamic-color($black), dynamic-array, dimensions)
     end,
     set)
end method initialize;

define method layered-color
    (set :: <layered-color-set>, #rest layers) => (color :: <layered-color>)
  let cache = layered-color-set-cache(set);
  gethash(cache, layers)
  | begin
      let layers = copy-sequence(layers);
      gethash(cache, layers) := make-layered-color(set, layers)
    end
end method layered-color;

define method layered-color-color-setter
    (color :: <color>, layered-color :: <layered-color>) => (color :: <color>)
  with-delayed-recoloring
    for (dynamic-color in layered-color-dynamic-colors(layered-color))
      dynamic-color-color(dynamic-color) := color
    end
  end;
  color
end method layered-color-color-setter;

// 'layered-color-dynamic-colors' should not be exported to the user.  It
// is important that these dynamics are not drawn with.  Instead, the
// fully specified layered is used.
define method layered-color-dynamic-colors
    (layered-color :: <layered-color>) => (dynamic-colors :: <sequence>)
  if (~empty?(layered-color.%dynamic-colors))
    layered-color.%dynamic-colors
  else
    layered-color.%dynamic-colors
      := begin
	   let dynamic-array
	     = layered-color-set-dynamic-array(layered-color-set(layered-color));
	   let dynamics :: <stretchy-object-vector> = make(<stretchy-vector>);
	   do-layered-colors
	     (method (dimensions)
		add!(dynamics, apply(aref, dynamic-array, dimensions))
	      end,
	      layered-color-set(layered-color),
              layers: layered-color-layers(layered-color));
	   dynamics
	 end
  end
end method layered-color-dynamic-colors;


/// Color constants

define sealed class <color-not-found> (<error>)
  sealed constant slot %color, required-init-keyword: color:;
end class <color-not-found>;

define method condition-to-string
    (condition :: <color-not-found>) => (string :: <string>)
  format-to-string("The color named %= was not found", condition.%color)
end method condition-to-string;


// Silly canned color table for people who don't implement real palettes
define variable $default-named-color-table :: <object-table> = make(<table>);

// Simplest possible palette returns canned, silly X Windows colors
define method find-color
    (name, palette :: <basic-palette>, #key error? = #t) => (color :: <color>)
  let color = element($default-named-color-table, name, default: #f);
  if (~color & error?)
    error(make(<color-not-found>, color: name))
  else
    color
  end
end method find-color;


define macro named-color-definer
  { define named-color ?:name = (?red:expression, ?green:expression, ?blue:expression) }
    => { $default-named-color-table[?#"name"]
           := make-rgb-color(?red / 255.0, ?green / 255.0, ?blue / 255.0) }
end macro named-color-definer;

// Default values for named colors -- the primaries
define named-color red     = (255,   0,   0);
define named-color green   = (  0, 255,   0);
define named-color blue    = (  0,   0, 255);
define named-color cyan    = (  0, 255, 255);
define named-color magenta = (255,   0, 255);
define named-color yellow  = (255, 255,   0);
define named-color white   = (255, 255, 255);
define named-color black   = (  0,   0,   0);

// Default values for named colors -- the silly X colors
define named-color snow = (255, 250, 250);
define named-color ghost-white = (248, 248, 255);
define named-color white-smoke = (245, 245, 245);
define named-color gainsboro = (220, 220, 220);
define named-color floral-white = (255, 250, 240);
define named-color old-lace = (253, 245, 230);
define named-color linen = (250, 240, 230);
define named-color antique-white = (250, 235, 215);
define named-color papaya-whip = (255, 239, 213);
define named-color blanched-almond = (255, 235, 205);
define named-color bisque = (255, 228, 196);
define named-color peach-puff = (255, 218, 185);
define named-color navajo-white = (255, 222, 173);
define named-color moccasin = (255, 228, 181);
define named-color cornsilk = (255, 248, 220);
define named-color ivory = (255, 255, 240);
define named-color lemon-chiffon = (255, 250, 205);
define named-color seashell = (255, 245, 238);
define named-color honeydew = (240, 255, 240);
define named-color mint-cream = (245, 255, 250);
define named-color azure = (240, 255, 255);
define named-color alice-blue = (240, 248, 255);
define named-color lavender = (230, 230, 250);
define named-color lavender-blush = (255, 240, 245);
define named-color misty-rose = (255, 228, 225);
define named-color dark-slate-gray = (47, 79, 79);
define named-color dim-gray = (105, 105, 105);
define named-color slate-gray = (112, 128, 144);
define named-color light-slate-gray = (119, 136, 153);
define named-color gray = (192, 192, 192);
define named-color light-gray = (211, 211, 211);
define named-color midnight-blue = (25, 25, 112);
define named-color navy-blue = (0, 0, 128);
define named-color cornflower-blue = (100, 149, 237);
define named-color dark-slate-blue = (72, 61, 139);
define named-color slate-blue = (106, 90, 205);
define named-color medium-slate-blue = (123, 104, 238);
define named-color light-slate-blue = (132, 112, 255);
define named-color medium-blue = (0, 0, 205);
define named-color royal-blue = (65, 105, 225);
define named-color dodger-blue = (30, 144, 255);
define named-color deep-sky-blue = (0, 191, 255);
define named-color sky-blue = (135, 206, 235);
define named-color light-sky-blue = (135, 206, 250);
define named-color steel-blue = (70, 130, 180);
define named-color light-steel-blue = (176, 196, 222);
define named-color light-blue = (173, 216, 230);
define named-color powder-blue = (176, 224, 230);
define named-color pale-turquoise = (175, 238, 238);
define named-color dark-turquoise = (0, 206, 209);
define named-color medium-turquoise = (72, 209, 204);
define named-color turquoise = (64, 224, 208);
define named-color light-cyan = (224, 255, 255);
define named-color cadet-blue = (95, 158, 160);
define named-color medium-aquamarine = (102, 205, 170);
define named-color aquamarine = (127, 255, 212);
define named-color dark-green = (0, 100, 0);
define named-color dark-olive-green = (85, 107, 47);
define named-color dark-sea-green = (143, 188, 143);
define named-color sea-green = (46, 139, 87);
define named-color medium-sea-green = (60, 179, 113);
define named-color light-sea-green = (32, 178, 170);
define named-color pale-green = (152, 251, 152);
define named-color spring-green = (0, 255, 127);
define named-color lawn-green = (124, 252, 0);
define named-color chartreuse = (127, 255, 0);
define named-color medium-spring-green = (0, 250, 154);
define named-color green-yellow = (173, 255, 47);
define named-color lime-green = (50, 205, 50);
define named-color yellow-green = (154, 205, 50);
define named-color forest-green = (34, 139, 34);
define named-color olive-drab = (107, 142, 35);
define named-color dark-khaki = (189, 183, 107);
define named-color khaki = (240, 230, 140);
define named-color pale-goldenrod = (238, 232, 170);
define named-color light-goldenrod-yellow = (250, 250, 210);
define named-color light-yellow = (255, 255, 224);
define named-color gold = (255, 215, 0);
define named-color light-goldenrod = (238, 221, 130);
define named-color goldenrod = (218, 165, 32);
define named-color dark-goldenrod = (184, 134, 11);
define named-color rosy-brown = (188, 143, 143);
define named-color indian-red = (205, 92, 92);
define named-color saddle-brown = (139, 69, 19);
define named-color sienna = (160, 82, 45);
define named-color peru = (205, 133, 63);
define named-color burlywood = (222, 184, 135);
define named-color beige = (245, 245, 220);
define named-color wheat = (245, 222, 179);
define named-color sandy-brown = (244, 164, 96);
define named-color tan = (210, 180, 140);
define named-color chocolate = (210, 105, 30);
define named-color firebrick = (178, 34, 34);
define named-color brown = (165, 42, 42);
define named-color dark-salmon = (233, 150, 122);
define named-color salmon = (250, 128, 114);
define named-color light-salmon = (255, 160, 122);
define named-color orange = (255, 165, 0);
define named-color dark-orange = (255, 140, 0);
define named-color coral = (255, 127, 80);
define named-color light-coral = (240, 128, 128);
define named-color tomato = (255, 99, 71);
define named-color orange-red = (255, 69, 0);
define named-color hot-pink = (255, 105, 180);
define named-color deep-pink = (255, 20, 147);
define named-color pink = (255, 192, 203);
define named-color light-pink = (255, 182, 193);
define named-color pale-violet-red = (219, 112, 147);
define named-color maroon = (176, 48, 96);
define named-color medium-violet-red = (199, 21, 133);
define named-color violet-red = (208, 32, 144);
define named-color violet = (238, 130, 238);
define named-color plum = (221, 160, 221);
define named-color orchid = (218, 112, 214);
define named-color medium-orchid = (186, 85, 211);
define named-color dark-orchid = (153, 50, 204);
define named-color dark-violet = (148, 0, 211);
define named-color blue-violet = (138, 43, 226);
define named-color purple = (160, 32, 240);
define named-color medium-purple = (147, 112, 219);
define named-color thistle = (216, 191, 216);
