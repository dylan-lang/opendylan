Module:       duim-gui-test-suite
Author:       Andy Armstrong, Shri Amit
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The colors and the thickness options in the radio and
// spin box in the top control area of the display.
//
define constant $test-graphics-color      = #(#"Blue", #"Black", #"Red");
define constant $test-graphics-thickness  = #(0, 1, 2, 3);

// The definition for the pixmap which is the #"image" object
//
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

define constant $default-graphic-width   = 60;
define constant $default-graphic-height  = 60;
define constant $default-graphic-spacing = 30;
define constant $default-graphic-border  = 10;

// The class of drawing-pane which is the drawable for the
// graphic objects
//
define class <graphics-drawing-pane> (<drawing-pane>)
  constant slot graphics-to-test :: <list>,
    init-keyword: graphics:;
  constant slot graphic-width :: <integer> = $default-graphic-width;
  constant slot graphic-height :: <integer> = $default-graphic-height;
end class <graphics-drawing-pane>;

define method do-compose-space
    (pane :: <graphics-drawing-pane>, #key width, height)
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

define method draw-test-graphics
    (pane :: <graphics-drawing-pane>, medium :: <medium>, region :: <region>) => ()
  let frame = sheet-frame(pane);
  local method draw-it ()
	  let color = frame.%color;
	  let brush = select (color by \==)
			#"Blue"  => $blue;
			#"Red"   => $red;
			#"Black" => $black;
		      end;
	  let thickness = frame.%thickness;
	  let pen = make(<pen>, width: thickness);
	  let width  = graphic-width(pane);
	  let height = graphic-height(pane);
	  let spacing = $default-graphic-spacing;
	  let border = $default-graphic-border;
	  let x1 = border;
	  let y1 = border;
	  let x2 = x1 + width;
	  let y2 = y1 + height;
	  let x3 = x2 + spacing;
	  let x4 = x3 + width;
	  with-drawing-options (medium, brush: brush, pen: pen)
	    let text-style = medium-merged-text-style(medium);
	    let _port = port(pane);
	    let text-height = font-height(text-style, _port);
	    draw-text(medium, "Unfilled", x1, y1, align-y: #"top");
	    draw-text(medium, "Filled",   x3, y1, align-y: #"top");
	    draw-line(medium, x1, y1 + text-height + 2, x4, y1 + text-height + 2);
	    y1 := y1 + text-height + 4 + border;
	    y2 := y2 + text-height + 4 + border;
	    for (graphic in pane.graphics-to-test)
	      draw-graphic(graphic, medium, #f, x1, x2, y1, y2);
	      draw-graphic(graphic, medium, #t, x3, x4, y1, y2);
	      y1 := y1 + height + spacing;
	      y2 := y2 + height + spacing;
	    end;
	  end
        end method;
  if (frame.%double-buffering?)
    with-double-buffering (medium)
      clear-box*(medium, $everywhere);
      draw-it()
    end
  else
    clear-box*(medium, $everywhere);
    draw-it()
  end
end method draw-test-graphics;

// All the draw-graphic methods for the various
// graphic objects defined in $graphics-to-test
//
define method draw-graphic
    (graphic-type == #"rectangle", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   draw-rectangle(medium, x1, y1, x2, y2, filled?: filled?)
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"arrow", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => ()
   let xinc = round/(x2 - x1, 5);
   let yinc = round/(y2 - y1, 5);
   for (i :: <integer> from x1 to x2 by xinc, j :: <integer> from x2 to x1 by -xinc)
     draw-arrow(medium, i, y1, j, y2, from-head?: filled?, to-head?: filled?);
   end for;
   for (i :: <integer> from y1 to y2 by yinc, j :: <integer> from y2 to y1 by -yinc)
     draw-arrow(medium, x1, i, x2, j, from-head?: filled?, to-head?: filled?);
   end for;
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"polygon", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   let x3 = xcen + (round/(x2 - xcen, 2));
   draw-polygon(medium, list(xcen, y2, x1, ycen, xcen, y1, x3, ycen),
                closed?: filled?, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"regular-polygon", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => ()
   let xcen = floor/(x1 + x2, 2);
   let line-size = floor/(x2 - x1, 3);
   let x3 = xcen - floor/(line-size, 2);
   let x4 = xcen + floor/(line-size, 2);
   draw-regular-polygon(medium, x3, y1, x4, y1, 8, closed?: filled?, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"triangle", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   let xcen = x1 + (round/(x2 - x1, 2));
   draw-triangle(medium, xcen, y1, x1, y2, x2, y2, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"circle", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   draw-circle(medium, xcen, ycen, (ycen - y1), filled?: filled?)
end method draw-graphic;

define method draw-graphic
  (graphic-type == #"ellipse", medium :: <medium>, filled? :: <boolean>, 
   x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   let xrad = round/(xcen - x1, 2);
   let yrad = ycen - y1;
   draw-ellipse(medium, xcen, ycen, xrad, 0, 0, yrad, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"oval", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   let yrad = round/(ycen - y1, 2);
   draw-oval(medium, xcen, ycen, xcen - x1, yrad, filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"bezier-curve", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => () 
   let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   draw-bezier-curve(medium, list(x1, ycen, xcen, y1, x2, y2, x2, ycen), filled?: filled?);
end method draw-graphic;

define method draw-graphic
    (graphic-type == #"image", medium :: <medium>, filled? :: <boolean>, 
     x1 :: <integer>, x2 :: <integer>, y1 :: <integer>, y2 :: <integer>) => ()
   let xcen = x1 + (round/(x2 - x1, 2));
   let ycen = y1 + (round/(y2 - y1, 2));
   draw-image(medium, $pixmap-to-test, xcen, ycen);
end method draw-graphic;


/// The main frame definiton and the layout of the various gadgets

define command-table *graphics-test-file-comtab* (*global-command-table*)
  menu-item "&Direct BitBlt" = frame-direct-bitblt,
    documentation: "Do a direct BitBlt";
  menu-item "&Pixmap BitBlt" = frame-pixmap-bitblt,
    documentation: "Do a BitBlt via a pixmap";
  separator;
  menu-item "&Close" = exit-frame,
    documentation: "Close the window";
end command-table *graphics-test-file-comtab*;

define command-table *graphics-test-comtab* (*global-command-table*)
  menu-item "File" = *graphics-test-file-comtab*;
end command-table *graphics-test-comtab*;

define frame <simple-graphics-test-frame> (<simple-frame>)
  slot %color = $test-graphics-color[0],
    init-keyword: color:;
  slot %thickness = $test-graphics-thickness[0],
    init-keyword: thickness:;
  slot %double-buffering? = #f;
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
  pane double-buffering-button (frame)
    make(<check-button>,
         value: #f,
         value-changed-callback:
	   method (gadget)
	     let frame = sheet-frame(gadget);
	     frame.%double-buffering? := gadget-value(gadget);
	     repaint-panes(frame)
	   end method);
  pane drawable-left (frame)
    make(<graphics-drawing-pane>,
         graphics: list(#"rectangle", #"circle", #"arrow", #"polygon", #"regular-polygon"),
	 display-function: draw-test-graphics);
  pane drawable-right (frame)
    make(<graphics-drawing-pane>,
         graphics: list(#"triangle", #"ellipse", #"oval", #"bezier-curve", #"image"),
	 display-function: draw-test-graphics);
  pane main-layout (frame)
    vertically (spacing: 5)
      make(<separator>);
      make(<table-layout>,
	   x-spacing: 10, y-spacing: 2,
           x-alignment: #[#"right", #"left"],
           contents: vector(vector(make(<label>, label: "Color:"),
				   frame.color-radio-box),
			    vector(make(<label>, label: "Thickness:"),
				   frame.thickness-list-box),
			    vector(make(<label>, label: "Buffered:"),
				   frame.double-buffering-button)));
      make(<separator>);
      horizontally (spacing: 10)
        with-border (type: #"sunken")
	  frame.drawable-left
	end;
        with-border (type: #"sunken")
	  frame.drawable-right
	end;
      end;
    end;
  layout (frame) frame.main-layout;
  command-table (frame) *graphics-test-comtab*;
end frame <simple-graphics-test-frame>;

// Redraw methods for new color and thickness, etc
define method repaint-panes
    (frame :: <simple-graphics-test-frame>) => ()
  let pane = frame.drawable-left;
  repaint-sheet(pane, $everywhere);
  let pane = frame.drawable-right;
  repaint-sheet(pane, $everywhere)
end method repaint-panes;

define method frame-direct-bitblt
    (frame :: <simple-graphics-test-frame>) => ()
  let pane   = frame.drawable-left;
  let medium = sheet-medium(pane);
  do-copy-area(medium, 0, 0, 100, 100,
	       medium, 50, 200)
end method frame-direct-bitblt;

define method frame-pixmap-bitblt
    (frame :: <simple-graphics-test-frame>) => ()
  let pane   = frame.drawable-right;
  let medium = sheet-medium(pane);
  let pixmap = make-pixmap(medium, 100, 100);
  do-copy-area(medium, 0, 0, 100, 100,
	       pixmap, 0, 0);
  do-copy-area(pixmap, 0, 0, 100, 100,
	       medium, 50, 200)
end method frame-pixmap-bitblt;


/// Drawing accuracy

define class <graphics-grid-pane> (<drawing-pane>)
  slot graphic-width :: <integer> = 40;
  slot graphic-height :: <integer> = 40;
end class <graphics-grid-pane>;

define method handle-repaint 
    (pane :: <graphics-grid-pane>, medium :: <medium>, region :: <region>) => ()
  let (width, height) = sheet-size(pane);
  block (return)
    for (i :: <integer> from 0 below width by 5)
      for (j :: <integer> from 0 below height by 5)
        draw-point(medium, i, j)
      end
    end
  end;
  let (left, top) = values(10, 10);
  let width = pane.graphic-width;
  let height = pane.graphic-height;
  let second-top = top + height + 10;
  with-drawing-options (medium, brush: $red)
    draw-rectangle(medium, left, top,        left + width, top        + height, filled?: #t);
    draw-rectangle(medium, left, second-top, left + width, second-top + height, filled?: #f);
  end
end method handle-repaint;

define frame <drawing-accuracy-frame> (<simple-frame>)
  pane width-pane (frame)
    make(<text-field>,
         value-type: <integer>,
         value: 40,
         width: 50, fixed-width?: #t,
         value-changed-callback: method (gadget)
                                   note-graphic-width-changed(frame, gadget-value(gadget))
                                 end);
  pane copy-button (frame)
    make(<push-button>,
         label: "Copy rectangles",
         activate-callback: method (gadget)
                              copy-rectangles(frame)
                            end);
  pane clear-button (frame)
    make(<push-button>,
         label: "Clear rectangles",
         activate-callback: method (gadget)
                              clear-rectangles(frame)
                            end);
  pane animate-button (frame)
    make(<push-button>,
         label: "Animate",
         activate-callback: method (gadget)
                              animate-rectangles(frame)
                            end);
  pane grid-pane (frame)
    make(<graphics-grid-pane>, width: 300, height: 300);
  layout (frame)
    frame.grid-pane;
  tool-bar (frame)
    make(<tool-bar>,
         child: horizontally (spacing: 0)
                  frame.width-pane;
                  frame.copy-button;
                  frame.clear-button;
                  frame.animate-button;
                end);
end frame <drawing-accuracy-frame>;

define method note-graphic-width-changed
    (frame :: <drawing-accuracy-frame>, width :: <integer>) => ()
  let pane = frame.grid-pane;
  pane.graphic-width  := width;
  pane.graphic-height := width;
  clear-box*(pane, $everywhere);
  repaint-sheet(pane, $everywhere)
end method note-graphic-width-changed;

define method copy-rectangles
    (frame :: <drawing-accuracy-frame>) => ()
  let pane = frame.grid-pane;
  let (left, top) = values(10, 10);
  let (new-left, new-top) = values(200, 10);
  let new-border = 4;
  let width = pane.graphic-width;
  let height = pane.graphic-height;
  let second-top = top + height + 10;
  let new-second-top = new-top + height + 10;
  with-drawing-options (pane, brush: $green)
    draw-rectangle(pane, new-left - new-border, new-top - new-border, 
                   new-left + width + new-border,
                   new-second-top + height + new-border);
  end;
  copy-area(pane, left, top, width, height, new-left, new-top);
  copy-area(pane, left, second-top, width, height, new-left, new-second-top);
end method copy-rectangles;

define method clear-rectangles
    (frame :: <drawing-accuracy-frame>) => ()
  let pane = frame.grid-pane;
  let (left, top) = values(10, 10);
  let width = pane.graphic-width;
  let height = pane.graphic-height;
  let second-top = top + height + 10;
  clear-box(pane, left, top, left + width, left + height);
  clear-box(pane, left, second-top, left + width, second-top + height);
end method clear-rectangles;

define method animate-rectangles
    (frame :: <drawing-accuracy-frame>) => ()
  let pane = frame.grid-pane;
  let (left, top) = values(10, 10);
  let width = pane.graphic-width;
  let height = pane.graphic-height;
  let second-top = top + height + 10;
  for (x from left below left + 100)
    copy-area(pane, x, top, width, height, x + 1, top);
    copy-area(pane, x, second-top, width, height, x + 1, second-top);
    sleep(0.1);
  end;
end method animate-rectangles;


/// Graphics frame

define variable $graphics-test-frame-tests
  = vector(vector("Simple graphics",            <simple-graphics-test-frame>),
	   vector("Drawing accuracy",           <drawing-accuracy-frame>));

define frame <graphics-test-frame> (<simple-frame>)
  pane examples (frame)
    make(<list-control>,
         scroll-bars: #"none",
	 documentation: "Double-click on a test name to run it",
	 items: $graphics-test-frame-tests,
	 lines: size($graphics-test-frame-tests),
	 label-key: first,
	 activate-callback: method (gadget :: <gadget>)
                              let frame = sheet-frame(gadget);
                              let value = gadget-value(gadget);
                              let title = first(value);
                              let class = second(value);
                              let test-frame = make(class, title: title, owner: frame);
                              start-frame(test-frame)
			    end);
  pane main-layout (frame)
    frame.examples;
  layout (frame) frame.main-layout;
end frame <graphics-test-frame>;

install-test(<graphics-test-frame>, "Graphics");

