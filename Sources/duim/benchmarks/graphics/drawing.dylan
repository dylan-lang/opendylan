Module:    duim-graphics-benchmarks
Author:    Andy Armstrong, Shri Amit
Synopsis:  Benchmarking of basic drawing operations
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $colors = vector($red, $green, $blue, $black);

define constant <coordinates> = limited(<simple-object-vector>, of: <integer>);
define constant <colors> = limited(<simple-object-vector>, of: <color>);

define constant $drawing-operations = make(<stretchy-vector>);

define class <graphics-state> (<object>)
  slot %color = $red,
    init-keyword: color:;
  slot %thickness :: <integer> = 1,
    init-keyword: thickness:;
  slot %filled? :: <boolean> = #f,
    init-keyword: filled?:;
end class <graphics-state>;

define class <graphics-info> (<object>)
  constant slot %state :: <graphics-state>,
    required-init-keyword: state:;
  constant slot %count :: <integer>,
    required-init-keyword: count:;
  slot %colors :: false-or(<colors>) = #f,
    init-keyword: colors:;
end class <graphics-info>;

define method initialize (info :: <graphics-info>, #key) => ()
  next-method();
  let count = info.%count;
  info.%colors := compute-random-colors(count)
end method initialize;

define function install-drawing-benchmark
    (class :: subclass(<graphics-info>), title :: <string>) => ()
  add!($drawing-operations, pair(title, class))
end function install-drawing-benchmark;

define function drawing-operation-name
    (class :: subclass(<graphics-info>)) => (name :: <string>)
  block (return)
    for (pair :: <pair> in $drawing-operations)
      if (tail(pair) == class)
        return(head(pair))
      end
    end
  end
end function drawing-operation-name;

define class <graphics-drawing-pane> (<drawing-pane>)
  slot %info :: false-or(<graphics-info>) = #f;
end class <graphics-drawing-pane>;

define method invalidate-graphics-info
    (pane :: <graphics-drawing-pane>) => ()
  let old-info = pane.%info;
  pane.%info := #f
//  pane.%info := old-info & make(object-class(old-info), count: old-info.%count)
end method invalidate-graphics-info;

// When the rectangle resizes then force the coordinates to be recomputed
// so that they fill the new space.
define method do-allocate-space
    (pane :: <graphics-drawing-pane>, width :: <integer>, height :: <integer>)
 => ()
  next-method();
  invalidate-graphics-info(pane)
end method do-allocate-space;

// Returns max random coordinates based on the width and height
// of the drawing pane
define method compute-random-coordinates 
    (nrects :: <integer>, width :: <integer> , height :: <integer>, #key points = 2) 
 => (coordinates :: <coordinates>)
  let size = points * 2;
  let ncoords = nrects * size;
  let coordinates :: <coordinates> = make(<coordinates>, size: ncoords);
  for (i :: <integer> from 0 below ncoords by size)
    for (j :: <integer> from 0 below size by 2)
      coordinates[i + j + 0] := random(width);
      coordinates[i + j + 1] := random(height);
    end
  end for;
  coordinates
end method compute-random-coordinates;

// Returns max random coordinates based on the width and height
// of the drawing pane
define method compute-random-colors
    (count :: <integer>) => (colors :: <colors>)
  let colors :: <colors> = make(<colors>, size: count);
  let different-colors = size($colors);
  for (i :: <integer> from 0 below count)
    colors[i] := $colors[random(different-colors)]
  end;
  colors
end method compute-random-colors;

// The main display function to draw the rectangles with various
// options
define method draw-benchmark-graphics
    (pane :: <graphics-drawing-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  let frame = sheet-frame(pane);
  let (width, height) = sheet-size(pane);
  let graphics-class = frame.%graphics-class;
  let info :: <graphics-info>
    = pane.%info
        | begin
            pane.%info := make(graphics-class,
                               count: frame.%count,
                               state: frame.%state,
                               width: width, height: height)
          end;
  let count = info.%count;
  let coordinates :: <coordinates> = info.%coordinates;

  profiling (cpu-time-seconds, cpu-time-microseconds, allocation)
    draw-graphics(pane, medium, info)
  results
    let time :: <float> = cpu-time-seconds + cpu-time-microseconds / 1000000.0;
    let rate :: <integer> = floor/(count, time);
    frame-status-message(frame) 
      := format-to-string("%d %s/second, allocated %s", 
			  rate,
                          as-lowercase(drawing-operation-name(graphics-class)),
			  case
			    allocation < 10 * 1024 =>
			      format-to-string("%d bytes", allocation);
			    allocation < 10 * 1024 * 1024 =>
			      format-to-string("%= Kbytes", floor/(allocation, 1024));
			    otherwise  =>
			      format-to-string("%= Mbytes", floor/(allocation, 1024 * 1024));
			  end)
  end
end method draw-benchmark-graphics;


/// drawing-benchmark-definer

define macro drawing-benchmark-definer
  { define drawing-benchmark ?class:name (?title:expression, ?points:expression)
      ?body:body
    end }
    =>
    {
      define class "<" ## ?class ## "-info>" (<graphics-info>)
	slot %coordinates :: false-or(<coordinates>) = #f,
	  init-keyword: coordinates:;
      end class "<" ## ?class ## "-info>";

      define method initialize
	  (info ::  "<" ## ?class ## "-info>", #next next-method, #key width, height) => ()
	next-method();
	info.%coordinates
	  := compute-random-coordinates(info.%count, width, height, points: ?points)
      end method initialize;

      define method draw-graphics
	  (pane :: <graphics-drawing-pane>, ?=medium :: <medium>, info ::  "<" ## ?class ## "-info>") => ()
	let ?=coordinates :: <coordinates> = info.%coordinates;
	let colors :: <colors> = info.%colors;
	let ncoords = size(?=coordinates);
	let points = ?points * 2;
	let state = info.%state;
	let color = state.%color;
	let thickness = state.%thickness;
	let ?=filled?   = state.%filled?;
	let pen = make(<pen>, width: thickness);
	if (color == #"random")
	  let no-of-colors = size($colors);
	  for (?=i :: <integer> from 0 below ncoords by points,
	       color :: <color> in colors)
	    with-drawing-options (?=medium, brush: color, pen: pen)
	      ?body
	    end with-drawing-options
	  end for
	else
	  with-drawing-options (?=medium, brush: color, pen: pen)
	    for (?=i :: <integer> from 0 below ncoords by points)
	      ?body
	    end for
	  end with-drawing-options
	end
      end method draw-graphics;

      install-drawing-benchmark( "<" ## ?class ## "-info>", ?title)
    }
end macro drawing-benchmark-definer;

define drawing-benchmark ellipse ("Ellipse", 2)
  draw-ellipse(medium, 
               coordinates[i + 0],
               coordinates[i + 1],
               coordinates[i + 2], 0,
               0, coordinates[i + 3],
               filled?: filled?)
end drawing-benchmark ellipse;

define drawing-benchmark line ("Line", 2)
  draw-line(medium, 
            coordinates[i + 0],
            coordinates[i + 1],
            coordinates[i + 2],
            coordinates[i + 3])
end drawing-benchmark line;

define drawing-benchmark point ("Point", 1)
  draw-point(medium, 
             coordinates[i + 0],
             coordinates[i + 1])
end drawing-benchmark point;

define drawing-benchmark rectangle ("Rectangle", 2)
  draw-rectangle(medium, 
		 coordinates[i + 0],
		 coordinates[i + 1],
		 coordinates[i + 2],
		 coordinates[i + 3],
		 filled?: filled?)
end drawing-benchmark rectangle;

define drawing-benchmark triangle ("Triangle", 3)
  draw-triangle(medium, 
                coordinates[i + 0],
                coordinates[i + 1],
                coordinates[i + 2],
                coordinates[i + 3],
                coordinates[i + 4],
                coordinates[i + 5],
                filled?: filled?)
end drawing-benchmark triangle;


/// Frame

define constant $default-drawing-operation = <rectangle-info>;

define frame <basic-drawing-benchmark-frame> (<simple-frame>)
  constant slot %state :: <graphics-state> = make(<graphics-state>);
  slot %count :: <integer> = 10,
    init-keyword: number:;
  slot %graphics-class :: subclass(<graphics-info>) = $default-drawing-operation;

  pane drawing-operation-pane (frame)
    make(<option-box>,
         items: $drawing-operations,
         value: $default-drawing-operation,
         label-key: head,
         value-key: tail,
         value-changed-callback: switch-drawing-operation);

  pane color-radio-box (frame)
    make(<radio-box>,
	 items: vector(pair("Red",    $red),
                       pair("Blue",   $blue),
                       pair("Black",  $black),
		       pair("Random", #"random")),
         value: frame.%state.%color,
         label-key: head,
	 value-key: tail,
 	 value-changed-callback: redraw-color-change);

  pane filled-radio-box (frame)
    make(<radio-box>,
	 items: vector(pair("Yes", #t),
                       pair("No",  #f)),
         value: frame.%state.%filled?,
         label-key: head,
	 value-key: tail,
 	 value-changed-callback: redraw-filled-change);

  pane thickness-list-box (frame)
    make(<spin-box>,
         items: #(0, 1, 2, 3),
         value: frame.%state.%thickness,
         value-changed-callback: redraw-thickness-change);

  pane number-text-field (frame)
    make(<text-field>,
         label: "Number",
         value: frame.%count,
	 value-type: <integer>,
         value-changed-callback: redraw-number-change);

  pane graphics-pane (frame)
    make(<graphics-drawing-pane>,
         coordinates: #f,
         width: 500, height: 300,
	 display-function: draw-benchmark-graphics);

  pane main-layout (frame)
    vertically (spacing: 8)
      make(<table-layout>,
	     x-spacing: 10, y-spacing: 2,
	     x-alignment: #(#"right", #"left"), y-alignment: #"center",
	   columns: 2,
	   children: vector(make(<label>, label: "Shape:"),
			    frame.drawing-operation-pane,
                            make(<label>, label: "Color:"),
			    frame.color-radio-box,
			    make(<label>, label: "Filled?:"),
			    frame.filled-radio-box,
			    make(<label>, label: "Thickness:"),
			    frame.thickness-list-box,
			    make(<label>, label: "Number:"),
			    frame.number-text-field));
      with-border (type: #"sunken")
        frame.graphics-pane
      end;
  end;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>)
end frame <basic-drawing-benchmark-frame>;

define method switch-drawing-operation
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let pane = graphics-pane(frame);
  frame.%graphics-class := gadget-value(gadget);
  invalidate-graphics-info(pane);
  redraw-graphics-pane(frame)
end method switch-drawing-operation;

// Callback repaint methods for color, thickness, filling
// and number of rectangle changes
define method redraw-color-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let pane = graphics-pane(frame);
  frame.%state.%color := gadget-value(gadget);
  redraw-graphics-pane(frame)
end method redraw-color-change;

define method redraw-thickness-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let pane = graphics-pane(frame);
  frame.%state.%thickness := gadget-value(gadget);
  redraw-graphics-pane(frame)
end method redraw-thickness-change;

define method redraw-filled-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let pane = graphics-pane(frame);
  frame.%state.%filled? := gadget-value(gadget);
  redraw-graphics-pane(frame)
end method redraw-filled-change;

define method redraw-number-change (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let pane = graphics-pane(frame);
  frame.%count := gadget-value(gadget);
  invalidate-graphics-info(pane);
  redraw-graphics-pane(frame)
end method redraw-number-change;

define method redraw-graphics-pane
    (frame :: <basic-drawing-benchmark-frame>) => ()
  let pane = frame.graphics-pane;
  clear-box*(pane, $everywhere);
  repaint-sheet(pane, $everywhere)
end method redraw-graphics-pane;

install-benchmark(<basic-drawing-benchmark-frame>, "Basic Drawing");
