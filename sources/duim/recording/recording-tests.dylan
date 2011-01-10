Module:       DUIM-Recording-Internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Recording test harness

//---*** This is all stolen intact from duim/examples/harness.dylan

define variable *example-frames* = make(<stretchy-vector>);

define method find-example-class (class :: <class>)
  block (return)
    for (example in *example-frames*)
      if (example[0] = class)
        return(example)
      end
    end
  end
end method find-example-class;

define method install-example 
    (frame-class :: <class>, title :: <string>)
  let example = find-example-class(frame-class);
  if (example)
    example[1] := title;
  else
    add!(*example-frames*, vector(frame-class, title))
  end;
  frame-class
end method install-example;

define method start-example-frame 
    (frame :: <frame>, class :: <class>, 
     #rest args,
     #key frame-manager: framem)
 => (example-frame :: <frame>)
  let example-frame
    = with-busy-cursor (frame)
        let example = find-example-class(class);
        let frame-class = example[0];
        let title = example[1];
        let example-frame
          = if (example)
	      apply(make, frame-class, owner: frame, title: title, args)
	    else
	      apply(make, class, owner: frame, title: "Example", args)
	    end;
        frame-mapped?(example-frame) := #t;
        example-frame
      end;
  start-frame(example-frame);
  example-frame
end method start-example-frame;

define method sorted-example-frames ()
  sort(*example-frames*,
       test: method (example1, example2)
               example1[1] < example2[1]
             end)
end method sorted-example-frames;

define frame <examples-harness> (<simple-frame>)
  pane update (frame)
    make(<push-button>,
	      label: "Update",
	      activate-callback: update-examples-harness);
  pane examples (frame)
    make(<list-control>,
         items: sorted-example-frames(),
         label-key: second,
         value-key: first,
         activate-callback: method (sheet :: <sheet>)
                              let frame = sheet-frame(sheet);
                              let example = gadget-value(sheet);
                              start-example-frame(frame, example)
                            end);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.update;
      frame.examples;
    end;
  layout (frame) frame.main-layout;
end frame <examples-harness>;

define method update-examples-harness 
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  gadget-items(frame.examples) := sorted-example-frames()
end method update-examples-harness;

define method start-examples 
    () => (status-code :: false-or(<integer>))
  let frame = make(<examples-harness>, title: "Recording Examples");
  start-frame(frame);
  #f
end method start-examples;


/// Simple drawing tests

//---*** This is all based on duim/tests/gui/graphics.dylan

define constant $default-graphic-width   = 60;
define constant $default-graphic-height  = 60;
define constant $default-graphic-spacing = 30;
define constant $default-graphic-border  = 10;

define class <graphics-pane> (<recording-pane>)
  constant slot graphics-to-test :: <list>,
    init-keyword: graphics:;
  constant slot graphic-width  :: <integer> = $default-graphic-width;
  constant slot graphic-height :: <integer> = $default-graphic-height;
end class <graphics-pane>;

define method do-compose-space
    (pane :: <graphics-pane>, #key width, height)
 => (space-requirement :: <space-requirement>)
  ignore(width, height);
  let width   = graphic-width(pane);
  let height  = graphic-height(pane);
  let spacing = $default-graphic-spacing;
  let border  = $default-graphic-border;
  let no-of-items = size(graphics-to-test(pane));
  let _port = port(pane);
  let text-height = font-height(get-default-text-style(_port, pane), _port);
  make(<space-requirement>,
       width:  (2 * (border + width)) + spacing,
       height: text-height + ((no-of-items - 1) * (height + spacing))
                 + height + 4 + 3 * border)
end method do-compose-space;

define method handle-event
    (sheet :: <graphics-pane>, 
     event :: <pointer-motion-event>) => ()
  let old = sheet-highlighted-record(sheet);
  let new = child-containing-position(sheet-output-history(sheet), event-x(event), event-y(event));
  when (old & old ~== new)
    highlight-output-record(old, sheet, #"unhighlight")
  end;
  when (new)
    highlight-output-record(new, sheet, #"highlight")
  end;
  sheet-highlighted-record(sheet) := new
end method handle-event;


define constant $test-graphics-color      = #(#"Blue", #"Black", #"Red");
define constant $test-graphics-thickness  = #(0, 1, 2, 3);

define pattern $pixmap-to-test
  (list($red, $green, $blue))
    2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2;
    1, 2, 0, 0, 0, 0, 0, 0, 0, 2, 1;
    1, 0, 2, 0, 0, 1, 0, 0, 2, 0, 1;
    1, 0, 0, 2, 0, 1, 0, 2, 0, 0, 1;
    1, 0, 0, 0, 2, 1, 2, 0, 0, 0, 1;
    1, 0, 1, 1, 1, 2, 1, 1, 1, 0, 1;
    1, 0, 0, 0, 2, 1, 2, 0, 0, 0, 1;
    1, 0, 0, 2, 0, 1, 0, 2, 0, 0, 1;
    1, 0, 2, 0, 0, 1, 0, 0, 2, 0, 1;
    1, 2, 0, 0, 0, 0, 0, 0, 0, 2, 1;
    2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2;
end pattern $pixmap-to-test;

define method draw-test-graphics (sheet :: <graphics-pane>) => ()
  clear-output-history(sheet);
  let frame  = sheet-frame(sheet);
  let medium = sheet-medium(sheet);
  let color = frame.%color;
  let brush = select (color by \==)
		#"Blue"  => $blue;
		#"Red"   => $red;
		#"Black" => $black;
	      end;
  let thickness = frame.%thickness;
  let pen       = make(<pen>, width: thickness);
  let width     = graphic-width(sheet);
  let height    = graphic-height(sheet);
  let spacing   = $default-graphic-spacing;
  let border    = $default-graphic-border;
  let x1 = border;
  let y1 = border;
  let x2 = x1 + width;
  let y2 = y1 + height;
  let x3 = x2 + spacing;
  let x4 = x3 + width;
  with-drawing-options (sheet, brush: brush, pen: pen)
    let text-style = medium-merged-text-style(medium);
    let text-height = font-height(text-style, port(sheet));
    draw-text(sheet, "Unfilled", x1, y1, align-y: #"top");
    draw-text(sheet, "Filled",   x3, y1, align-y: #"top");
    draw-line(sheet, x1, y1 + text-height + 2, x4, y1 + text-height + 2);
    y1 := y1 + text-height + 4 + border;
    y2 := y2 + text-height + 4 + border;
    for (graphic in sheet.graphics-to-test)
      draw-graphic(graphic, sheet, #f, x1, x2, y1, y2);
      draw-graphic(graphic, sheet, #t, x3, x4, y1, y2);
      y1 := y1 + height + spacing;
      y2 := y2 + height + spacing;
    end
  end
end method draw-test-graphics;

define method draw-graphic
    (graphic-type == #"rectangle", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  draw-rectangle(sheet, x1, y1, x2, y2, filled?: filled?)
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"arrow", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => ()
  let xinc = round/(x2 - x1, 5);
  let yinc = round/(y2 - y1, 5);
  for (i :: <integer> from x1 to x2 by xinc, j :: <integer> from x2 to x1 by -xinc)
    draw-arrow(sheet, i, y1, j, y2, from-head?: filled?, to-head?: filled?);
  end;
  for (i :: <integer> from y1 to y2 by yinc, j :: <integer> from y2 to y1 by -yinc)
    draw-arrow(sheet, x1, i, x2, j, from-head?: filled?, to-head?: filled?);
  end;
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"polygon", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  let xcen = x1 + (round/(x2 - x1, 2));
  let ycen = y1 + (round/(y2 - y1, 2));
  let x3 = xcen + (round/(x2 - xcen, 2));
  draw-polygon(sheet, list(xcen, y2, x1, ycen, xcen, y1, x3, ycen),
               closed?: filled?, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"regular-polygon", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => ()
  let xcen = floor/(x1 + x2, 2);
  let line-size = floor/(x2 - x1, 3);
  let x3 = xcen - floor/(line-size, 2);
  let x4 = xcen + floor/(line-size, 2);
  draw-regular-polygon(sheet, x3, y1, x4, y1, 8, closed?: filled?, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"triangle", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  let xcen = x1 + (round/(x2 - x1, 2));
  draw-triangle(sheet, xcen, y1, x1, y2, x2, y2, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"circle", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  let xcen = x1 + (round/(x2 - x1, 2));
  let ycen = y1 + (round/(y2 - y1, 2));
  draw-circle(sheet, xcen, ycen, (ycen - y1), filled?: filled?)
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"ellipse", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   let xrad = round/(xcen - x1, 2);
   let yrad = ycen - y1;
   draw-ellipse(sheet, xcen, ycen, xrad, 0, 0, yrad, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"oval", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  let xcen = x1 + (round/(x2 - x1, 2));
  let ycen = y1 + (round/(y2 - y1, 2));
  let yrad = round/(ycen - y1, 2);
  with-new-output-record (sheet)
    draw-oval(sheet, xcen, ycen, xcen - x1, yrad, filled?: filled?)
  end;
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"bezier-curve", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
  let xcen = x1 + (round/(x2 - x1, 2));
  let ycen = y1 + (round/(y2 - y1, 2));
  draw-bezier-curve(sheet, list(x1, ycen, xcen, y1, x2, y2, x2, ycen), filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"image", sheet :: <graphics-pane>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => ()
  let xcen = x1 + (round/(x2 - x1, 2));
  let ycen = y1 + (round/(y2 - y1, 2));
  draw-image(sheet, $pixmap-to-test, xcen, ycen);
end method draw-graphic;

define command-table *graphics-test-file-comtab* (*global-command-table*)
  menu-item "E&xit" = exit-frame,
    documentation: "Exit";
end command-table *graphics-test-file-comtab*;

define command-table *graphics-test-comtab* (*global-command-table*)
  menu-item "File" = *graphics-test-file-comtab*;
end command-table *graphics-test-comtab*;

define frame <graphics-test-frame> (<simple-frame>)
  slot %color = $test-graphics-color[0],
    init-keyword: color:;
  slot %thickness = $test-graphics-thickness[0],
    init-keyword: thickness:;
  pane color-radio-box (frame)
    make(<radio-box>,
	 items: $test-graphics-color,
	 value: frame.%color,
         label-key: curry(as, <string>),
 	 value-changed-callback:
	   method (gadget)
	     let frame = sheet-frame(gadget);
	     frame.%color := gadget-value(gadget);
	     repaint-panes(frame)
	   end method);
  pane thickness-list-box (frame)
    make(<spin-box>,
         items: $test-graphics-thickness,
         value: frame.%thickness,
         value-changed-callback:
	   method (gadget)
	     let frame = sheet-frame(gadget);
	     frame.%thickness := gadget-value(gadget);
	     repaint-panes(frame)
	   end method);
  pane drawable-left (frame)
    make(<graphics-pane>,
         graphics: list(#"rectangle", #"circle", #"polygon", #"regular-polygon"));
  pane drawable-right (frame)
    make(<graphics-pane>,
         graphics: list(#"triangle", #"ellipse", #"oval", #"bezier-curve", #"image"));
  pane main-layout (frame)
    vertically (spacing: 5)
      make(<separator>);
      make(<table-layout>,
	   x-spacing: 10, y-spacing: 2,
           x-alignment: #[#"right", #"left"],
           contents: vector(vector(make(<label>, label: "Color:"),
				   frame.color-radio-box),
			    vector(make(<label>, label: "Thickness:"),
				   frame.thickness-list-box)));
      make(<separator>);
      horizontally (spacing: 10)
        with-border (type: #"sunken")
	  frame.drawable-left
	end;
        with-border (type: #"sunken")
	  frame.drawable-right
	end
      end
    end;
  layout (frame) frame.main-layout;
  command-table (frame) *graphics-test-comtab*;
end frame <graphics-test-frame>;

// Redraw methods for new color and thickness, etc
define method repaint-panes
    (frame :: <graphics-test-frame>) => ()
  draw-test-graphics(frame.drawable-left);
  draw-test-graphics(frame.drawable-right);
end method repaint-panes;

install-example(<graphics-test-frame>, "Graphics");


/// Simple drawing tests with scrolling

define frame <scrolling-graphics-test-frame> (<simple-frame>)
  slot %color = $test-graphics-color[0],
    init-keyword: color:;
  slot %thickness = $test-graphics-thickness[0],
    init-keyword: thickness:;
  pane color-radio-box (frame)
    make(<radio-box>,
	 items: $test-graphics-color,
	 value: frame.%color,
         label-key: curry(as, <string>),
 	 value-changed-callback:
	   method (gadget)
	     let frame = sheet-frame(gadget);
	     frame.%color := gadget-value(gadget);
	     repaint-panes(frame)
	   end method);
  pane thickness-list-box (frame)
    make(<spin-box>,
         items: $test-graphics-thickness,
         value: frame.%thickness,
         value-changed-callback:
	   method (gadget)
	     let frame = sheet-frame(gadget);
	     frame.%thickness := gadget-value(gadget);
	     repaint-panes(frame)
	   end method);
  pane drawable-left (frame)
    make(<graphics-pane>,
         graphics: list(#"rectangle", #"circle", #"polygon", #"regular-polygon"),
         height: 200);
  pane drawable-right (frame)
    make(<graphics-pane>,
         graphics: list(#"triangle", #"ellipse", #"oval", #"bezier-curve", #"image"),
         height: 200);
  pane main-layout (frame)
    vertically (spacing: 5)
      make(<separator>);
      make(<table-layout>,
	   x-spacing: 10, y-spacing: 2,
           x-alignment: #[#"right", #"left"],
           contents: vector(vector(make(<label>, label: "Color:"),
				   frame.color-radio-box),
			    vector(make(<label>, label: "Thickness:"),
				   frame.thickness-list-box)));
      make(<separator>);
      horizontally (spacing: 10)
        with-border (type: #"sunken")
	  scrolling () frame.drawable-left end
	end;
        with-border (type: #"sunken")
	  scrolling () frame.drawable-right end
	end
      end
    end;
  layout (frame) frame.main-layout;
  command-table (frame) *graphics-test-comtab*;
end frame <scrolling-graphics-test-frame>;

// Redraw methods for new color and thickness, etc
define method repaint-panes
    (frame :: <scrolling-graphics-test-frame>) => ()
  draw-test-graphics(frame.drawable-left);
  draw-test-graphics(frame.drawable-right);
end method repaint-panes;

install-example(<scrolling-graphics-test-frame>, "Scrolling graphics");


/// Simple scribble application

//---*** This is all based on duim/examples/scribble/scribble.dylan

define class <scribble-pane> (<recording-pane>)
  slot scribble-segment = #f;
  constant slot scribble-popup-menu-callback = #f,
    init-keyword: popup-menu-callback:;
end class <scribble-pane>;

define method handle-button-event
    (sheet :: <scribble-pane>, 
     event :: <button-press-event>, 
     button == $left-button) => ()
  sheet.scribble-segment := make(<stretchy-vector>);
  add-scribble-segment(sheet, event.event-x, event.event-y);
  block (break)
    tracking-pointer (event, sheet)
      <pointer-drag-event> =>
        add-scribble-segment(sheet, event.event-x, event.event-y);
      <button-release-event> =>
        begin
          when (sheet.scribble-segment)
            with-output-recording-options (sheet, record?: #t)
              draw-polygon(sheet, sheet.scribble-segment, closed?: #f, filled?: #f)
            end
          end;
          sheet.scribble-segment := #f;
          break()
        end;
    end
  end
end method handle-button-event;

define method handle-button-event
    (sheet :: <scribble-pane>, 
     event :: <button-release-event>, 
     button == $right-button) => ()
  let popup-menu-callback = scribble-popup-menu-callback(sheet);
  when (popup-menu-callback)
    popup-menu-callback(sheet, event.event-x, event.event-y)
  end
end method handle-button-event;

define method add-scribble-segment
    (sheet :: <scribble-pane>, x, y) => ()
  let segment = sheet.scribble-segment;
  // The app can generate drag and release events before it has ever
  // seen a press event, so be careful
  when (segment)
    add!(segment, x);
    add!(segment, y);
    with-output-recording-options (sheet, record?: #f)
      draw-polygon(sheet, segment, closed?: #f, filled?: #f)
    end
  end
end method add-scribble-segment;

define frame <scribble-frame> (<simple-frame>)
  pane surface (frame)
    make(<scribble-pane>, 
	 popup-menu-callback: method (sheet, x, y)
				let frame = sheet-frame(sheet);
				popup-scribble-menu(frame, x, y)
			      end,
	 width:  300, max-width:  $fill,
	 height: 200, max-height: $fill);
  pane file-menu (frame)
    make(<menu>,
	 label: "File",
	 children: vector(make(<menu-button>,
			       label: "&Clear",
			       selection-mode: #"none",
			       activate-callback: method (button)
						    clear-output-history(frame.surface)
						  end),
			  make(<menu-button>,
			       label: "E&xit",
			       selection-mode: #"none",
			       activate-callback: method (button)
						    exit-frame(sheet-frame(button))
						  end)));
  pane scribble-popup-menu (frame)
    make(<menu>,
	 owner: frame.surface,
	 children: vector(make(<menu-button>,
			       label: "&Clear",
			       selection-mode: #"none",
			       activate-callback: method (button)
						    ignore(button);
						    clear-output-history(frame.surface)
						  end)));
  layout (frame) frame.surface;
  menu-bar (frame)
    make(<menu-bar>, children: vector(frame.file-menu));
end frame <scribble-frame>;

define method popup-scribble-menu
    (frame :: <scribble-frame>, x :: <integer>, y :: <integer>) => ()
  let menu = frame.scribble-popup-menu;
  display-menu(menu, x: x, y: y)
end method popup-scribble-menu;

install-example(<scribble-frame>, "Scribble");


/// 'with-room-for-graphics' test

define class <wrfg-pane> (<recording-pane>)
end class <wrfg-pane>;

define method handle-button-event
    (sheet :: <wrfg-pane>, 
     event :: <button-press-event>, 
     button == $left-button) => ()
  let medium = sheet-medium(sheet);
  let x = event-x(event);
  let y = event-y(event);
  with-drawing-options (medium, pen: $dotted-pen)
    draw-line(sheet, x, y, x + 20, y + 20)
  end;
  with-drawing-options (medium, pen: $solid-pen)
    with-room-for-graphics (sheet, x: x, y: y)
      draw-line(sheet, 0, 0, 20, 20)
    end
  end;
end method handle-button-event;

define frame <wrfg-frame> (<simple-frame>)
  pane surface (frame)
    make(<wrfg-pane>,
	 width:  300, max-width:  $fill,
	 height: 200, max-height: $fill);
  pane file-menu (frame)
    make(<menu>,
	 label: "File",
	 children: vector(make(<menu-button>,
			       label: "&Clear",
			       selection-mode: #"none",
			       activate-callback: method (button)
						    clear-output-history(frame.surface)
						  end),
			  make(<menu-button>,
			       label: "E&xit",
			       selection-mode: #"none",
			       activate-callback: method (button)
						    exit-frame(sheet-frame(button))
						  end)));
  layout (frame) frame.surface;
  menu-bar (frame)
    make(<menu-bar>, children: vector(frame.file-menu));
end frame <wrfg-frame>;

install-example(<wrfg-frame>, "Room...");


/// Initialization

start-examples();

// end
