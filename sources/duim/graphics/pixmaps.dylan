Module:       duim-graphics-internals
Synopsis:     DUIM graphics
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Pixmaps

define protocol-class pixmap (<image>) end;

define protocol <<pixmap-protocol>> (<<image-protocol>>)
  function draw-pixmap
    (drawable :: <drawable>, pixmap :: <pixmap>, x, y, #key function) => (record);
  function copy-area
    (drawable :: type-union(<drawable>, <pixmap>), from-x, from-y, width, height, to-x, to-y,
     #key function = $boole-1) => ();
  function do-copy-area
    (from-drawable :: type-union(<abstract-medium>, <pixmap>), from-x, from-y, width, height,
     to-drawable :: type-union(<abstract-medium>, <pixmap>), to-x, to-y,
     #key function = $boole-1) => ();
  function make-pixmap
    (medium :: <abstract-medium>, width, height) => (pixmap :: <pixmap>);
  function do-make-pixmap
    (port :: <abstract-port>, medium :: <abstract-medium>, width, height)
 => (pixmap :: <pixmap>);
  function destroy-pixmap
     (pixmap :: <pixmap>) => ();
  // Output to pixmaps...
  getter pixmap-drawable
    (pixmap :: <pixmap>) => (drawable);
  setter pixmap-drawable-setter
    (drawable, pixmap :: <pixmap>) => (drawable);
  function do-with-output-to-pixmap
    (drawable :: <drawable>, continuation :: <function>, #key width, height, clear?)
 => (pixmap :: <pixmap>);
  function do-with-double-buffering
    (drawable :: <drawable>, continuation :: <function>, #key x, y, width, height, pixmap)
 => (#rest values);
end protocol <<pixmap-protocol>>;


define method make-pixmap
    (medium :: <medium>, width, height) => (pixmap :: <pixmap>)
  do-make-pixmap(port(medium), medium, width, height)
end method make-pixmap;

define method destroy-pixmap (pixmap :: <pixmap>) => ()
  #f
end method destroy-pixmap;


/// Pixmap mediums

define open abstract class <pixmap-medium> (<medium>) end;

define open abstract class <basic-pixmap-medium> (<basic-medium>, <pixmap-medium>)
  sealed constant slot pixmap-medium-pixmap,
    required-init-keyword: pixmap:;
end class <basic-pixmap-medium>;

define open generic make-pixmap-medium
    (port :: <abstract-port>, sheet :: <abstract-sheet>, #key width, height)
 => (medium :: <pixmap-medium>);

define sealed inline method make
    (class == <pixmap-medium>, #key port, sheet, width, height)
 => (medium :: <pixmap-medium>)
  make-pixmap-medium(port, sheet, width: width, height: height)
end method make;


/// COPY-AREA

define method copy-area
    (sheet :: <basic-sheet>, from-x, from-y, width, height, to-x, to-y,
     #key function = $boole-1) => ()
  with-sheet-medium (medium = sheet)
    do-copy-area(medium, from-x, from-y, width, height,
		 medium, to-x, to-y, function: function)
  end
end method copy-area;

define method copy-area
    (sheet :: <permanent-medium-mixin>, from-x, from-y, width, height, to-x, to-y,
     #key function = $boole-1) => ()
  let medium = sheet-medium(sheet);
  do-copy-area(medium, from-x, from-y, width, height,
	       medium, to-x, to-y, function: function)
end method copy-area;

define sealed inline method copy-area
    (medium :: <basic-medium>,
     from-x, from-y, width, height, to-x, to-y,
     #key function = $boole-1) => ()
  do-copy-area(medium, from-x, from-y, width, height,
	       medium, to-x, to-y, function: function)
end method copy-area;


define sealed method copy-from-pixmap
    (pixmap :: <pixmap>, pixmap-x, pixmap-y, width, height,
     medium :: <medium>, medium-x, medium-y,
     #key function = $boole-1) => ()
  do-copy-area(pixmap, pixmap-x, pixmap-y, width, height,
	       medium, medium-x, medium-y, function: function)
end method copy-from-pixmap;

define sealed method copy-to-pixmap
    (medium :: <medium>, medium-x, medium-y, width, height,
     pixmap :: false-or(<pixmap>), pixmap-x, pixmap-y,
     #key function = $boole-1) => (pixmap :: <pixmap>)
  unless (pixmap)
    pixmap := make-pixmap(medium, width, height)
  end;
  do-copy-area(medium, medium-x, medium-y, width, height,
	       pixmap, pixmap-x, pixmap-y, function: function);
  pixmap
end method copy-to-pixmap;


/// Pixmap sheets

define sealed class <pixmap-sheet>
    (<permanent-medium-mixin>,
     <mirrored-sheet-mixin>,
     <basic-sheet>)
  keyword accepts-focus?: = #f;
end class <pixmap-sheet>;

define sealed domain make (singleton(<pixmap-sheet>));
define sealed domain initialize (<pixmap-sheet>);

define method initialize
    (sheet :: <pixmap-sheet>, #key port: _port, medium, width, height)
  // The medium must be a pixmap medium...
  check-type(medium, <basic-pixmap-medium>);
  next-method();
  sheet-transform(sheet) := $identity-transform;
  sheet-region(sheet) := make-bounding-box(0, 0, width, height);
  sheet.%port := _port;
  sheet-direct-mirror(sheet) := medium-drawable(medium);
  sheet-medium(sheet) := medium
end method initialize;

define method update-mirror-region
    (_port :: <port>, sheet :: <pixmap-sheet>, mirror) => ()
  #f
end method update-mirror-region;

define method update-mirror-transform
    (_port :: <port>, sheet :: <pixmap-sheet>, mirror) => ()
  #f
end method update-mirror-transform;


/// Interface to pixmaps

// Options can be WIDTH: and HEIGHT:
// Note that this returns the pixmap, not the values of the body
define macro with-output-to-pixmap
  { with-output-to-pixmap (?medium:name = ?sheet:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-output-to-pixmap-body = method (?medium) ?body end;
	   do-with-output-to-pixmap(?sheet, with-output-to-pixmap-body, ?options)
	 end }
  { with-output-to-pixmap (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-output-to-pixmap-body = method (?medium) ?body end;
	   do-with-output-to-pixmap(?medium, with-output-to-pixmap-body, ?options)
	 end }
end macro with-output-to-pixmap;

define method do-with-output-to-pixmap
    (medium :: <medium>, continuation :: <function>, #key width, height, clear? = #t)
 => (pixmap :: <pixmap>)
  let sheet = medium-sheet(medium);
  let _port = port(sheet);
  let pixmap-medium
    = make-pixmap-medium(_port, sheet,
                         width: width, height: height);
  let pixmap-sheet
    = make(<pixmap-sheet>,
	   port: _port, medium: pixmap-medium,
           width: width, height: height);
  medium-foreground(pixmap-medium) := medium-foreground(medium);
  medium-background(pixmap-medium) := medium-background(medium);
  medium-default-text-style(pixmap-medium) := medium-default-text-style(medium);
  medium-text-style(pixmap-medium) := medium-text-style(medium);
  sheet-mapped?(pixmap-sheet) := #t;
  when (clear?)
    clear-box(pixmap-medium, 0, 0, width, height)
  end;
  continuation(pixmap-medium);
  pixmap-medium-pixmap(pixmap-medium)
end method do-with-output-to-pixmap;

define method do-with-output-to-pixmap
    (sheet :: <sheet>, continuation :: <function>, #key width, height, clear? = #t)
 => (pixmap :: <pixmap>)
  with-sheet-medium (medium = sheet)
    do-with-output-to-pixmap(medium, continuation,
                             width: width, height: height, clear?: clear?)
  end
end method do-with-output-to-pixmap;


/// Double buffering

define macro with-double-buffering
  { with-double-buffering (?medium:name = ?sheet:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-double-buffering-body = method (?medium) ?body end;
	   do-with-double-buffering(?sheet, with-double-buffering-body, ?options)
	 end }
  { with-double-buffering (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-double-buffering-body = method (?medium) ?body end;
	   do-with-double-buffering(?medium, with-double-buffering-body, ?options)
	 end }
end macro with-double-buffering;

define method do-with-double-buffering
    (medium :: <medium>, continuation :: <function>,
     #key x = 0, y = 0, width, height, pixmap) => (#rest values)
  let sheet = medium-sheet(medium);
  unless (width & height)
    let (_w, _h) = box-size(sheet-device-region(sheet));
    width := _w;
    height := _h
  end;
  let the-pixmap
    = pixmap | medium-pixmap(medium) | make-pixmap(medium, width, height);
  block ()
    dynamic-bind (medium-drawable(medium) = pixmap-drawable(the-pixmap))
      // Clear the drawing state cache, since we may well need to establish
      // drawing state on the new drawable
      medium-drawing-state-cache(medium) := 0;
      clear-box(medium, 0, 0, width, height);
      continuation(medium)
    end
  cleanup
    medium-drawing-state-cache(medium) := 0;
    copy-from-pixmap(the-pixmap, 0, 0, width, height, medium, x, y);
    // If we allocated a pixmap, get rid of it now
    unless (pixmap | medium-pixmap(medium))
      destroy-pixmap(the-pixmap)
    end
  end
end method do-with-double-buffering;

define method do-with-double-buffering
    (sheet :: <sheet>, continuation :: <function>,
     #key x = 0, y = 0, width, height, pixmap) => (#rest values)
  with-sheet-medium (medium = sheet)
    do-with-double-buffering(medium, continuation,
			     x: x, y: y, width: width, height: height, pixmap: pixmap)
  end
end method do-with-double-buffering;


/// DRAW-PIXMAP

define method draw-pixmap
    (sheet :: <sheet>, pixmap :: <pixmap>, x, y,
     #rest keys, #key function = $boole-1) => (record)
  dynamic-extent(keys);
  ignore(function);
  with-sheet-medium (medium = sheet)
    apply(draw-pixmap, medium, pixmap, x, y, keys)
  end
end method draw-pixmap;

define method draw-pixmap
    (sheet :: <permanent-medium-mixin>, pixmap :: <pixmap>, x, y,
     #rest keys, #key function = $boole-1) => (record)
  dynamic-extent(keys);
  ignore(function);
  apply(draw-pixmap, sheet-medium(sheet), pixmap, x, y, keys)
end method draw-pixmap;

define function draw-pixmap*
    (medium :: <drawable>, pixmap :: <pixmap>, point,
     #rest keys, #key function = $boole-1) => (record)
  dynamic-extent(keys);
  ignore(function);
  apply(draw-pixmap, medium, pixmap,
	point-x(point), point-y(point), keys)
end function draw-pixmap*;


// Make 'draw-image' do the right thing on pixmaps
define method draw-image
    (medium :: <drawable>, pixmap :: <pixmap>, x, y) => (record)
  draw-pixmap(medium, pixmap, x, y)
end method draw-image;
