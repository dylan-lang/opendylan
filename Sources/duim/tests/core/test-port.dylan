Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Test Port

define class <test-port> (<basic-port>)
end class <test-port>;

define sideways method class-for-make-port
    (type == #"test", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  ignore(initargs);
  values(<test-port>, #f)
end method class-for-make-port;

define method port-type (port :: <test-port>) => (type :: <symbol>)
  #"test"
end method port-type;

define method port-name (port :: <test-port>) => (name :: false-or(<string>))
  #f
end method port-name;

define variable *test-port* = #f;

define method find-test-port ()
  *test-port* | (*test-port* := find-port(server-path: vector(#"test")))
end method find-test-port;

define method find-test-pointer ()
  port-pointer(find-test-port())
end method find-test-pointer;

define method find-test-frame-manager ()
  find-frame-manager(port: find-test-port())
end method find-test-frame-manager;

define class <test-palette> (<basic-palette>)
end class <test-palette>;

define method make-palette
    (port :: <test-port>, #key color?, dynamic?) => (palette :: <palette>)
  make(<test-palette>, port: port, color?: color?, dynamic?: dynamic?)
end method make-palette;

define method do-pointer-position
    (port :: <test-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let (px, py) = pointer-position(pointer);
  values(px, py)
end method do-pointer-position;

define method do-set-pointer-position
    (port :: <test-port>, pointer :: <pointer>, sheet :: <sheet>,
     x :: <integer>, y :: <integer>) => ()
end method do-set-pointer-position;

define method do-set-pointer-cursor
    (port :: <test-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  cursor
end method do-set-pointer-cursor;


define method do-set-sheet-cursor
    (port :: <test-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  cursor
end method do-set-sheet-cursor;


/// Test Display

define method initialize-display 
    (port :: <test-port>, _display :: <display>) => ()
  let region = make-bounding-box(0, 0, 1000, 800);
  sheet-region(_display) := region;
  sheet-direct-mirror(_display) := make(<test-mirror>,
					sheet: _display,
					region: region);
end method initialize-display;


/// Test Medium

define class <test-medium> (<basic-medium>)
  constant slot graphic-operations = make(<stretchy-vector>);
end class <test-medium>;

define method make-medium
    (_port :: <test-port>, sheet :: <sheet>) => (medium :: <test-medium>)
  make(<test-medium>,
       port: _port, sheet: sheet)
end method make-medium;

define method destroy-medium (medium :: <test-medium>) => ()
  //--- deallocate all window system resources
  next-method()
end method destroy-medium;

define method clear-graphic-operations (medium :: <test-medium>)
  graphic-operations(medium).size := 0;
end method clear-graphic-operations;


/// Pixmaps

define class <test-pixmap> (<pixmap>)
end class <test-pixmap>;

define method do-make-pixmap 
    (port :: <test-port>, medium :: <test-medium>, width, height)
 => (pixmap :: <test-pixmap>)
  make(<test-pixmap>, width: width, height: height)
end method do-make-pixmap;

define method glyph-for-character
    (_port :: <test-port>, char :: <character>, text-style :: <text-style>,
     #key font)
 => (index :: <integer>, font,
     escapement-x :: <real>, escapement-y :: <real>,
     origin-x :: <real>, origin-y :: <real>, bb-x :: <real>, bb-y :: <real>)
  ignore(font);
  let index = as(<integer>, char);
  values(index, text-style,
	 8, 0, 0, 10, 8, 12)
end method glyph-for-character;

define method font-width
    (text-style :: <text-style>, _port :: <test-port>, #key character-set)
 => (width :: <integer>)
  ignore(character-set);
  10
end method font-width;

define method font-height
    (text-style :: <text-style>, _port :: <test-port>, #key character-set)
 => (height :: <integer>)
  ignore(character-set);
  14
end method font-height;

define method font-ascent
    (text-style :: <text-style>, _port :: <test-port>, #key character-set)
 => (ascent :: <integer>)
  ignore(character-set);
 12
end method font-ascent;

define method font-descent
    (text-style :: <text-style>, _port :: <test-port>, #key character-set)
 => (descent :: <integer>)
  ignore(character-set);
  2
end method font-descent;

define method text-size
    (port :: <test-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start = 0, end: _end = size(string), do-newlines?, do-tabs?)
 => (largest-x :: <real>, largest-y :: <real>, 
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  ignore(text-style, do-newlines?, do-tabs?);
  let length = _end - _start;
  values(length * 8, 12, length * 8, 0, 10)
end method text-size;

define method text-size
    (port :: <test-port>, string :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start = 0, end: _end = size(string), do-newlines?, do-tabs?)
 => (largest-x :: <real>, largest-y :: <real>, 
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  ignore(text-style, _start, _end, do-newlines?);
  values(8, 12, 8, 0, 10)
end method text-size;


define method beep (medium :: <test-port>) => ()
  format-out("\nBeep!")
end method beep;


/// Test Mirrors

define class <test-mirror> (<mirror>)
  //---*** slot mirror-sheet, init-keyword: sheet:;
  slot %region, init-keyword: region:;
end class <test-mirror>;

define method do-make-mirror 
    (_port :: <test-port>, sheet :: <mirrored-sheet-mixin>)
 => (mirror :: <test-mirror>)
  // We're not using a real window system to back this up, so
  // just jiggle the coordinates so that it looks like we are
  let (left, top, right, bottom) = box-edges(sheet);
  let parent    = if (sheet-direct-mirror(sheet)) sheet else sheet-device-parent(sheet) end;
  let transform = sheet-delta-transform(sheet, parent);
  let (left, top, right, bottom)
    = transform-box(transform, left, top, right, bottom);
  let mirror
    = make(<test-mirror>,
	   sheet: sheet,
	   region: make-bounding-box(left, top, right, bottom));
  mirror
end method do-make-mirror;

define method destroy-mirror
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>) => ()
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define method map-mirror 
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>) => ()
  #t
end method map-mirror;

define method unmap-mirror
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>) => ()
  #f
end method unmap-mirror;

define method raise-mirror
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>,
     #key activate? = #t) => ()
  ignore(activate?);
  #t
end method raise-mirror;

define method lower-mirror
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>) => ()
  #t
end method lower-mirror;

define method mirror-visible?
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>)
 => (visible? :: <boolean>)
  #t
end method mirror-visible?;

define method mirror-edges
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

define method set-mirror-edges
    (_port :: <test-port>, sheet :: <sheet>, mirror :: <mirror>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom)
end method set-mirror-edges;


/// Test Frame Manager

define class <test-frame-manager> (<basic-frame-manager>)
end class <test-frame-manager>;

define method frame-wrapper
    (framem :: <test-frame-manager>, frame :: <simple-frame>, 
     layout :: false-or(<sheet>))
 => (sheet :: false-or(<column-layout>))
  let menu-bar   = frame-menu-bar(frame);
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  let children = make(<stretchy-vector>);
  when (menu-bar)   add!(children, menu-bar)   end;
  when (tool-bar)   add!(children, tool-bar)   end;
  when (layout)     add!(children, layout)     end;
  when (status-bar) add!(children, status-bar) end;
  unless (empty?(children))
    make-test-pane(<column-layout>, children: children)
  end
end method frame-wrapper;

define method make-frame-manager
    (_port :: <test-port>,
     #key palette, class = <test-frame-manager>, #all-keys)
 => (framem :: <test-frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


/// Test panes

define class <test-pane-mixin>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <mirrored-sheet-mixin>)
end class <test-pane-mixin>;

// The test port uses phony mirrors, so we need to repaint them ourself
define method port-handles-repaint?
    (_port :: <test-port>, sheet :: <test-pane-mixin>) => (true? :: <boolean>)
  #f
end method port-handles-repaint?;

// The test port uses phony mirrors, so we need to repaint them ourself
define method port-handles-repaint?
    (_port :: <test-port>, sheet :: <drawing-pane>) => (true? :: <boolean>)
  #f
end method port-handles-repaint?;


define class <test-gadget-mixin>
    (<sheet-with-medium-mixin>,		// here to support kludge 'handle-repaint' methods
     <test-pane-mixin>)
  constant slot handled-events :: <stretchy-vector>  = make(<stretchy-vector>);
end class <test-gadget-mixin>;

define method record-event
    (sheet :: <test-gadget-mixin>, event :: <event>) => ()
  add!(handled-events(sheet), event)
end method record-event;


define class <test-top-level-sheet>
    (<permanent-medium-mixin>,
     <test-pane-mixin>,
     <top-level-sheet>)
  slot top-level-sheet-region = #f;
end class <test-top-level-sheet>;

define method mirror-edges 
    (port :: <test-port>, sheet :: <test-top-level-sheet>, mirror :: <mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let region = top-level-sheet-region(sheet);
  if (region)
    box-edges(region);
  else
    next-method()
  end
end method mirror-edges;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-top-level-sheet>, #f)
end method class-for-make-pane;

define method record-event
    (sheet :: <test-top-level-sheet>, event :: <event>) => ()
  let frame = sheet-frame(sheet);
  add!(handled-events(frame), event)
end method record-event;


define class <test-viewport>
    (<test-pane-mixin>,
     <viewport>,
     <single-child-composite-pane>)
  constant slot horizontal-scroll-bar = #f, 
    init-keyword: horizontal-scroll-bar:;
  constant slot vertical-scroll-bar   = #f, 
    init-keyword: vertical-scroll-bar:;
end class <test-viewport>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-viewport>, #f)
end method class-for-make-pane;


define class <test-scroll-bar>
  (<test-gadget-mixin>,
   <scroll-bar>,
   <leaf-pane>)
end class <test-scroll-bar>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <scroll-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-scroll-bar>, #f)
end method class-for-make-pane;

define method do-compose-space
    (pane :: <test-scroll-bar>, #key width, height)
 => (space-req :: <space-requirement>)
  select (gadget-orientation(pane))
    #"horizontal" =>
      make(<space-requirement>,
	   width: width | 50, min-width: 50, max-width: $fill,
	   height: 10);
    #"vertical" =>
      make(<space-requirement>,
	   width: 10,
	   height: height | 50, min-height: 50, max-height: $fill);
  end
end method do-compose-space;


define class <test-slider-pane>
  (<test-gadget-mixin>,
   <slider>,
   <leaf-pane>)
end class <test-slider-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <slider>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-slider-pane>, #f)
end method class-for-make-pane;


define class <basic-test-button>
    (<test-gadget-mixin>,
     <leaf-pane>)
end class <basic-test-button>;

define method do-compose-space
    (pane :: <basic-test-button>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  make(<space-requirement>,
       width: 40,
       height: 15)
end method do-compose-space;

define method allocate-space 
    (pane :: <basic-test-button>, width :: <integer>, height :: <integer>) => ()
end method allocate-space;


define class <test-push-button-pane>
    (<push-button>,
     <basic-test-button>)
end class <test-push-button-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-push-button-pane>, #f)
end method class-for-make-pane;

define method do-compose-space
    (pane :: <test-push-button-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  //---*** Shouldn't be calling medium functions until we have a port
  if (#f) // (gadget-label(pane) & port(pane))
    let (lw, lh) = gadget-label-size(pane);
    let x-margin = 2;
    let y-margin = 2;
    make(<space-requirement>,
         width:  x-margin + lw + x-margin,
         height: y-margin + lh + y-margin)
  else
    make(<space-requirement>,
         width:  40, height: 15)
  end
end method do-compose-space;

define method handle-event 
    (pane :: <test-push-button-pane>, event :: <button-release-event>) => ()
  execute-activate-callback(pane, gadget-client(pane), gadget-id(pane))
end method handle-event;

define method handle-repaint
    (pane :: <test-push-button-pane>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(sheet-device-region(pane));
  draw-rectangle(medium, left, top, right, bottom, filled?: #f);
  //--- This isn't drawing at the right place
  when (gadget-label(pane))
    let x-margin = 2;
    let y-margin = 2;
    let x = right - left;
    let y = bottom + y-margin;
    draw-gadget-label(pane, medium, x, y,
		      align-x: #"center", align-y: #"baseline");
  end;
  draw-rectangle(medium, left, top, right, bottom, filled?: #f)
end method handle-repaint;


define class <test-radio-button-pane>
    (<radio-button>, 
     <basic-test-button>)
end class <test-radio-button-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-radio-button-pane>, #f)
end method class-for-make-pane;

define method handle-event 
    (pane :: <test-radio-button-pane>, event :: <button-release-event>) => ()
  gadget-value(pane, do-callback?: #t) := ~gadget-value(pane)
end method handle-event;

//--- Use drawing code from CLIM's db-button.lisp
define method handle-repaint
    (pane :: <test-radio-button-pane>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(sheet-device-region(pane));
  //--- square, selected/deselected
  draw-rectangle(medium, left, top, right, bottom, 
		 filled?: gadget-value(pane))
end method handle-repaint;


define class <test-check-button-pane>
    (<check-button>,
     <basic-test-button>)
end class <test-check-button-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-check-button-pane>, #f)
end method class-for-make-pane;

define method handle-event 
    (pane :: <test-check-button-pane>, event :: <button-release-event>) => ()
  gadget-value(pane, do-callback?: #t) := ~gadget-value(pane)
end method handle-event;

//--- Use drawing code from CLIM's db-button.lisp
define method handle-repaint
    (pane :: <test-check-button-pane>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(sheet-device-region(pane));
  //--- diamond, selected/deselected
  draw-rectangle(medium, left, top, right, bottom,
		 filled?: gadget-value(pane))
end method handle-repaint;


define class <test-list-box> 
    (<test-gadget-mixin>,
     <list-box>,
     <leaf-pane>)
end class <test-list-box>;

define method do-compose-space 
    (pane :: <test-list-box>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 30,
       height: height | 50,
       min-width: 30,
       min-height: 50,
       max-width: $fill,
       max-height: $fill)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-list-box>, #f)
end method class-for-make-pane;

define method handle-repaint
    (pane :: <test-list-box>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(sheet-device-region(pane));
  //--- square, selected/deselected
  draw-rectangle(medium, left, top, right, bottom, filled?: #f)
end method handle-repaint;


define class <test-option-box> 
    (<test-gadget-mixin>,
     <option-box>,
     <leaf-pane>)
end class <test-option-box>;

define method do-compose-space 
    (pane :: <test-option-box>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 30,
       height: height | 50,
       min-width: 30,
       min-height: 50,
       max-width: $fill,
       max-height: $fill)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-option-box>, #f)
end method class-for-make-pane;


define class <test-combo-box> 
    (<test-gadget-mixin>,
     <combo-box>,
     <leaf-pane>)
end class <test-combo-box>;

define method do-compose-space 
    (pane :: <test-combo-box>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 30,
       height: height | 50,
       min-width: 30,
       min-height: 50,
       max-width: $fill,
       max-height: $fill)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <combo-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-combo-box>, #f)
end method class-for-make-pane;


define class <test-spin-box> 
    (<test-gadget-mixin>,
     <spin-box>,
     <leaf-pane>)
end class <test-spin-box>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <spin-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-spin-box>, #f)
end method class-for-make-pane;


define class <test-menu-bar-pane>
    (<test-gadget-mixin>,
     <multiple-child-composite-pane>,
     <menu-bar>)
end class <test-menu-bar-pane>;

define method do-compose-space
    (menu :: <test-menu-bar-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let children = sheet-children(menu);
  make(<space-requirement>,
       width: width | 20 + 40 * size(children),
       height: 15)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-menu-bar-pane>, #f)
end method class-for-make-pane;

define class <test-menu-pane>
    (<test-gadget-mixin>,
     <multiple-child-composite-pane>,
     <menu>)
end class <test-menu-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-menu-pane>, #f)
end method class-for-make-pane;

//--- Relaying-out a menu doesn't make any sense.
define method relayout-parent
    (sheet :: <test-menu-pane>, #key width, height)
 => (did-layout? :: <boolean>)
  ignore(width, height);
  #f
end method relayout-parent;


define class <test-push-menu-button-pane>
    (<push-menu-button>,
     <basic-test-button>)
end class <test-push-menu-button-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-push-menu-button-pane>, #f)
end method class-for-make-pane;


define class <test-radio-menu-button-pane>
    (<radio-menu-button>,
     <basic-test-button>)
end class <test-radio-menu-button-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-radio-menu-button-pane>, #f)
end method class-for-make-pane;


define class <test-check-menu-button-pane>
    (<check-menu-button>,
     <basic-test-button>)
end class <test-check-menu-button-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-check-menu-button-pane>, #f)
end method class-for-make-pane;


define class <test-text-field-pane>
    (<test-gadget-mixin>,
     <text-field>,
     <leaf-pane>)
end class <test-text-field-pane>;

define method do-compose-space 
    (pane :: <test-text-field-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 100,
       height: height | 16,
       min-width: 40,
       min-height: 12,
       max-width: $fill,
       max-height: $fill)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-text-field-pane>, #f)
end method class-for-make-pane;


define class <test-password-field-pane>
    (<test-gadget-mixin>,
     <password-field>,
     <leaf-pane>)
end class <test-password-field-pane>;

define method do-compose-space 
    (pane :: <test-password-field-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 100,
       height: height | 16,
       min-width: 40,
       min-height: 12,
       max-width: $fill,
       max-height: $fill)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-password-field-pane>, #f)
end method class-for-make-pane;


define class <test-text-editor-pane>
    (<test-gadget-mixin>,
     <text-editor>,
     <leaf-pane>)
end class <test-text-editor-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-text-editor-pane>, #f)
end method class-for-make-pane;


define class <test-splitter-pane>
    (<test-gadget-mixin>,
     <splitter>,
     <leaf-pane>)
end class <test-splitter-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <splitter>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-splitter-pane>, #f)
end method class-for-make-pane;


define class <test-progress-bar> 
    (<test-gadget-mixin>,
     <progress-bar>,
     <leaf-pane>)
end class <test-tree-control>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <progress-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-progress-bar>, #f)
end method class-for-make-pane;


define class <test-status-bar> 
    (<test-gadget-mixin>,
     <status-bar>,
     <leaf-pane>)
end class <test-tree-control>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <status-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-status-bar>, #f)
end method class-for-make-pane;


define class <test-list-control> 
    (<test-gadget-mixin>,
     <list-control>,
     <leaf-pane>)
end class <test-list-control>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <list-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-list-control>, #f)
end method class-for-make-pane;


define class <test-table-control> 
    (<test-gadget-mixin>,
     <table-control>,
     <leaf-pane>)
end class <test-table-control>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <table-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-table-control>, #f)
end method class-for-make-pane;


define class <test-tree-control> 
    (<test-gadget-mixin>,
     <tree-control>,
     <leaf-pane>)
end class <test-tree-control>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <tree-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-tree-control>, #f)
end method class-for-make-pane;


define class <test-tab-control> 
    (<test-gadget-mixin>,
     <tab-control>,
     <leaf-pane>)
end class <test-tab-control>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <tab-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-tab-control>, #f)
end method class-for-make-pane;




/// Make functions

define method make-test-pane
    (class :: <class>, #rest initargs)
  with-frame-manager (find-test-frame-manager())
    apply(make, class, initargs)
  end
end method make-test-pane;

define class <test-frame> (<simple-frame>)
  constant slot handled-events :: <stretchy-vector> = make(<stretchy-vector>);
end class <test-frame>;

define method record-event
    (frame :: <test-frame>, event :: <event>) => ()
  add!(handled-events(frame), event)
end method record-event;

define method make-test-frame
    (class :: subclass(<frame>),
     #rest args,
     #key frame-manager: framem, #all-keys)
 => (frame :: <frame>)
  let framem = framem | find-test-frame-manager();
  with-frame-manager (framem)
    let frame
      = apply(make, class, 
	      title: "Test Frame",
	      args);
    frame-mapped?(frame) := #t;
    frame
  end
end method make-test-frame;


/// Debugging support

define table $gadget-class-names :: <object-table>
  = { <push-button>          => "push button",
      <radio-button>         => "radio button",
      <check-button>         => "check button",

      <button-box>           => "button box",
      <push-box>             => "push box",
      <radio-box>            => "radio box",
      <check-box>            => "check box",
      <push-box-pane>        => "push box pane",
      <radio-box-pane>       => "radio box pane",
      <check-box-pane>       => "check box pane",

      <list-box>             => "list box",
      <option-box>           => "option box",
      <combo-box>            => "combo box",
      <spin-box>             => "spin box",
      <slider>               => "slider",
      <separator>            => "separator",
      <text-field>           => "text field",
      <password-field>       => "password field",
      <text-editor>          => "text editor",

      <scroll-bar>           => "scroll-bar",
      <viewport>             => "viewport",
      <scroller>             => "scroller",

      <splitter>             => "splitter",
      <progress-bar>         => "progress control",
      <list-control>         => "list control",
      <status-bar>           => "status bar",
      <table-control>        => "table control",
      <tool-bar>             => "tool bar",
      <tree-control>         => "tree control",

      <menu-bar>             => "menu bar",
      <menu>                 => "menu",
      <menu-box>             => "menu box",
      <push-menu-box>        => "push menu component",
      <radio-menu-box>       => "radio menu component",
      <check-menu-box>       => "check menu component",
      <push-menu-box-pane>   => "push menu component pane",
      <radio-menu-box-pane>  => "radio menu component pane",
      <check-menu-box-pane>  => "check menu component pane",
      <push-menu-button>     => "push menu button",
      <radio-menu-button>    => "radio menu button",
      <check-menu-button>    => "check menu button",

      <row-layout>           => "row layout",
      <row-layout-pane>      => "row layout pane",
      <column-layout>        => "column layout",
      <column-layout-pane>   => "column layout pane",
      <stack-layout>         => "stack layout",
      <stack-layout-pane>    => "stack layout pane",
      <table-layout>         => "table layout",
      <table-layout-pane>    => "table layout pane",
      <grid-layout>          => "grid layout",
      <grid-layout-pane>     => "grid layout pane",
      <border>               => "border",
      <border-pane>          => "border pane",
      <group-box>            => "group box",
      <spacing>              => "spacing",
      <spacing-pane>         => "spacing pane",
      <single-child-wrapping-pane>   => "single child wrapping layout pane",
      <multiple-child-wrapping-pane> => "multiple child wrapping layout pane",
      <pinboard-layout>      => "pinboard layout",
      <pinboard-layout-pane> => "pinboard layout pane",

      <test-list-box>        => "test list box",

      <top-level-sheet>      => "top level sheet",
      <test-top-level-sheet> => "test top level sheet",

      <simple-frame>         => "simple frame",
      <frame>                => "frame"

      /*---*** Removed
      <spin-box-pane>        => "spin box pane",
      <list-control-pane>    => "list control pane",
      <table-control-pane>   => "table control pane",
      <tree-control-pane>    => "tree control pane"
      */
      };


// gadget class name
define method gadget-class-name 
    (class :: <class>) => (name :: <string>)
  element($gadget-class-names, class, default: #f)
    | format-to-string("unknown gadget %=", class)
end method gadget-class-name;

define method gadget-class-name 
    (class) => (name :: <string>)
  gadget-class-name(object-class(class))
end method gadget-class-name;
