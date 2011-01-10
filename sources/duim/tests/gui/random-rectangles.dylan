Module:       duim-gui-test-suite
Author:       Andy Armstrong, Shri Amit
Synopsis:     A random-rectangle generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $colors = vector($red, $green, $blue, $black);

define constant <coordinates> = limited(<vector>, of: <integer>);

// The rectangle to drawing pane class
define class <rectangle-drawing-pane> (<drawing-pane>)
  slot %coordinates :: false-or(<coordinates>) = #f,
    init-keyword: coordinates:;
end class <rectangle-drawing-pane>;

define method invalidate-coordinates
    (pane :: <rectangle-drawing-pane>) => ()
  pane.%coordinates := #f
end method invalidate-coordinates;

// When the rectangle resizes then force the coordinates to be recomputed
// so that they fill the new space.
define method do-allocate-space
    (pane :: <rectangle-drawing-pane>, width :: <integer>, height :: <integer>) => ()
  next-method();
  invalidate-coordinates(pane)
end method do-allocate-space;

// Returns max random coordinates based on the width and height
// of the drawing pane
define method compute-random-coordinates 
    (nrects :: <integer>, width :: <integer> , height :: <integer>) 
 => (coordinates :: <coordinates>)
  let ncoords = nrects * 4;
  let coordinates = make(<coordinates>, size: ncoords, fill: 0);
  for (i :: <integer> from 0 below ncoords by 4)
    coordinates[i + 0] := random(width);
    coordinates[i + 1] := random(height);
    coordinates[i + 2] := random(width);
    coordinates[i + 3] := random(height);
  end for;
  coordinates
end method compute-random-coordinates;

// The main display function to draw the rectangles with various
// options
define method draw-test-rectangles
    (pane :: <rectangle-drawing-pane>, medium :: <medium>, region :: <region>) => ()
  let frame           = sheet-frame(pane);
  let color           = frame.%color;
  let pen             = make(<pen>, width: frame.%thickness);
  let filled?         = frame.%filled?;
  let buffer?         = frame.%buffer?;
  let (width, height) = sheet-size(pane);
  let nrects          = frame.%number;
  let coordinates :: <coordinates>
    = pane.%coordinates
        | begin
	    pane.%coordinates
	      := compute-random-coordinates(nrects, width, height);
	  end;
  let ncoords :: <integer> = coordinates.size;

  local method draw-rectangles () => ()
          if (color == #"random")
            let no-of-colors = size($colors);
            for (i :: <integer> from 0 below ncoords by 4)
              let color = $colors[random(no-of-colors)];
              with-drawing-options (medium, brush: color, pen: pen)
                draw-rectangle(medium,
                               coordinates[i + 0],
                               coordinates[i + 1],
                               coordinates[i + 2],
                               coordinates[i + 3],
                               filled?: filled?)
              end with-drawing-options
            end for
          else
            with-drawing-options (medium, brush: color, pen: pen)
              for (i :: <integer> from 0 below ncoords by 4)
                draw-rectangle(medium,
                               coordinates[i + 0],
                               coordinates[i + 1],
                               coordinates[i + 2],
                               coordinates[i + 3],
                               filled?: filled?)
              end for
            end with-drawing-options
          end
        end method draw-rectangles;

  let (seconds, microseconds)
    = timing ()
        if (buffer?)
          with-double-buffering (medium)
            draw-rectangles()
          end
        else
          draw-rectangles()
        end
      end timing;
  let time :: <float> = seconds + microseconds / 1000000.0;
  let rate :: <integer> = floor/(nrects, time);
  frame-status-message(frame) := format-to-string("%d rectangles/second", rate)
end method draw-test-rectangles;

// The frame definition containing pane layouts and the
// slots for the various attributes of the rectangles
define frame <random-rectangles-test-frame> (<simple-frame>)
  slot %number      = 10,   init-keyword: number:;
  slot %color       = $red, init-keyword: color:;
  slot %thickness   = 1,    init-keyword: thickness:;
  slot %filled?     = #f,   init-keyword: filled?:;
  slot %buffer?     = #f,   init-keyword: buffer?:;

  pane color-radio-box (frame)
    make(<radio-box>,
	 items: vector(pair("Red",    $red),
                       pair("Blue",   $blue),
                       pair("Black",  $black),
		       pair("Random", #"random")),
         value: frame.%color,
         label-key: head,
	 value-key: tail,
 	 value-changed-callback: redraw-color-change);

  pane filled-radio-box (frame)
    make(<radio-box>,
	 items: vector(pair("Yes", #t),
                       pair("No",  #f)),
         value: frame.%filled?,
         label-key: head,
	 value-key: tail,
 	 value-changed-callback: redraw-filled-change);

  pane buffer-radio-box (frame)
    make(<radio-box>,
	 items: vector(pair("Yes", #t),
                       pair("No",  #f)),
         value: frame.%buffer?,
         label-key: head,
	 value-key: tail,
 	 value-changed-callback: redraw-buffer-change);

  pane thickness-list-box (frame)
    make(<spin-box>,
         items: #(0, 1, 2, 3),
         value: frame.%thickness,
         value-changed-callback: redraw-thickness-change);

  pane number-text-field (frame)
    make(<text-field>,
         label: "Number",
         value: frame.%number,
	 value-type: <integer>,
         value-changed-callback: redraw-number-change);

  pane rectangle-drawable (frame)
    make(<rectangle-drawing-pane>,
         coordinates: #f,
	 display-function: draw-test-rectangles);

  pane main-layout (frame)
    vertically (spacing: 5)
      make(<table-layout>,
	     x-spacing: 10, y-spacing: 2,
	     x-alignment: #(#"right", #"left"), y-alignment: #"center",
	   columns: 2,
	   children: vector(make(<label>, label: "Color:"),
			    frame.color-radio-box,
			    make(<label>, label: "Filled?:"),
			    frame.filled-radio-box,
			    make(<label>, label: "Double buffering?:"),
			    frame.buffer-radio-box,
			    make(<label>, label: "Thickness:"),
			    frame.thickness-list-box,
			    make(<label>, label: "Number:"),
			    frame.number-text-field));
      with-border (type: #"sunken")
        frame.rectangle-drawable
      end;
  end;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>)
end frame <random-rectangles-test-frame>;

// Callback repaint methods for color, thickness, filling
// and number of rectangle changes
define method redraw-color-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  frame.%color := gadget-value(gadget);
  redraw-rectangles(frame)
end method redraw-color-change;

define method redraw-thickness-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  frame.%thickness := gadget-value(gadget);
  redraw-rectangles(frame)
end method redraw-thickness-change;

define method redraw-filled-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  frame.%filled? := gadget-value(gadget);
  redraw-rectangles(frame)
end method redraw-filled-change;

define method redraw-buffer-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  frame.%buffer? := gadget-value(gadget);
  redraw-rectangles(frame)
end method redraw-buffer-change;

define method redraw-number-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let pane = rectangle-drawable(frame);
  frame.%number := gadget-value(gadget);
  invalidate-coordinates(pane);
  redraw-rectangles(frame)
end method redraw-number-change;

define method redraw-rectangles
    (frame :: <random-rectangles-test-frame>) => ()
  with-busy-cursor (frame)
    let pane = frame.rectangle-drawable;
    unless (frame.%buffer?)
      clear-box*(pane, $everywhere)
    end;
    repaint-sheet(pane, $everywhere)
  end
end method redraw-rectangles;

install-test(<random-rectangles-test-frame>, "Random rectangles");

