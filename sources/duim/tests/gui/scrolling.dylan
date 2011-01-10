Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM test code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Ruler pane

define sealed class <ruler-pane>
    (<oriented-gadget-mixin>,
     <basic-gadget>,
     <drawing-pane>)
  keyword orientation: = #"vertical";
end class <ruler-pane>;

define method handle-repaint 
    (pane :: <ruler-pane>, medium :: <medium>, region :: <region>) => ()
  select (gadget-orientation(pane))
    #"vertical" =>
      let (left, top, right, bottom) = box-edges(pane);
      let (width, height) = values(right - left, bottom - top);
      let gradations = 10;
      let right-x = width - 50;
      let half-x = floor/(right-x, 2);
      let quarter-x = floor/(right-x, 4);
      for (y from top to bottom by gradations)
        case
	  modulo(y, 100) = 0 =>
            draw-line(medium, 0, y, right-x, y);
            draw-text(medium, format-to-string("%d", y), 
                      right-x, y, 
                      align-x: #"left", 
                      align-y: case
                                 y == top                => #"top";
                                 y > bottom - gradations => #"bottom";
                                 otherwise               => #"center"
                               end);
          modulo(y, 50) = 0 =>
            draw-line(medium, 0, y, half-x, y);
          otherwise =>
            draw-line(medium, 0, y, quarter-x, y);
	end
      end;
    #"horizontal" =>
      let (left, top, right, bottom) = box-edges(pane);
      let (width, height) = values(right - left, bottom - top);
      let gradations = 10;
      let bottom-y = height - 50;
      let half-y = floor/(bottom-y, 2);
      let quarter-y = floor/(bottom-y, 4);
      for (x from left to right by gradations)
        case
	  modulo(x, 100) = 0 =>
            draw-line(medium, x, 0, x, bottom-y);
            draw-text(medium, format-to-string("%d", x),
                      x, bottom-y, 
                      align-x: case
                                 x == left              => #"left";
                                 x > right - gradations => #"right";
                                 otherwise              => #"center";
                               end,
                      align-y: #"top");
          modulo(x, 50) = 0 =>
            draw-line(medium, x, 0, x, half-y);
          otherwise =>
            draw-line(medium, x, 0, x, quarter-y);
	end
      end;
  end
end method handle-repaint;

define method do-compose-space
    (pane :: <ruler-pane>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let vertical? = gadget-orientation(pane) = #"vertical";
  let width  = if (vertical?) 100 else 401 end;
  let height = if (vertical?) 401 else 100 end;
  let max-width  = if (vertical?) 100 else $fill end;
  let max-height = if (vertical?) $fill else 100 end;
  make(<space-requirement>,
       width:  width,  min-width:  100, max-width:  max-width,
       height: height, min-height: 100, max-height: max-height)
end method do-compose-space;


/// grid display pane

define sealed class <checkboard-pane> (<drawing-pane>)
end class <checkboard-pane>;

define method handle-repaint 
    (pane :: <checkboard-pane>, medium :: <medium>, region :: <region>) => ()
  let first-filled? = #f;
  let (width, height) = sheet-size(pane);
  for (x from 0 below width by 50)
    let filled? = first-filled?;
    for (y from 0 below height by 50)
      draw-rectangle(medium, x, y, x + 50, y + 50, filled?: filled?);
      filled? := ~filled?;
    end;
    first-filled? := ~first-filled?
  end
end method handle-repaint;

define method do-compose-space
    (pane :: <checkboard-pane>, #key width, height)
 => (space-requirement :: <space-requirement>)
  make(<space-requirement>,
       width:  800, min-width:  1, max-width:  $fill,
       height: 800, min-height: 1, max-height: $fill)
end method do-compose-space;


/// scroll testing

define method test-scrolling 
    (frame :: <frame>) => ()
  contain(scrolling ()
	    make(<ruler-pane>, height: 800)
	  end,
          title: "Simple Scrolling Test",
          height: 400)
end method test-scrolling;

define method test-two-pane-scrolling 
    (frame :: <frame>) => ()
  let scroll-bar = make(<scroll-bar>, orientation: #"vertical");
  let viewport1
    = make(<viewport>, 
	   child: make(<ruler-pane>),
	   min-width: 100,
	   min-height: 100,
	   height: 300,
	   vertical-scroll-bar: scroll-bar);
  let viewport2
    = make(<viewport>, 
	   child: make(<ruler-pane>),
	   min-width: 100,
	   min-height: 100,
	   height: 300,
	   vertical-scroll-bar: scroll-bar);
  contain(with-border (type: #"sunken")
	    horizontally ()
	      viewport1; 
	      viewport2;
	      scroll-bar
            end
	  end,
          title: "Two Pane Scrolling Test")
end method test-two-pane-scrolling;

define method test-three-pane-scrolling
    (frame :: <frame>) => ()
  let horizontal-bar = make(<scroll-bar>, orientation: #"horizontal");
  let vertical-bar = make(<scroll-bar>, orientation: #"vertical");
  let horizontal-ruler
    = make(<viewport>, 
	   child: make(<ruler-pane>, orientation: #"horizontal"),
	   horizontal-scroll-bar: horizontal-bar);
  let vertical-ruler
    = make(<viewport>, 
	   child: make(<ruler-pane>, orientation: #"vertical"),
	   vertical-scroll-bar:   vertical-bar);
  let viewport
    = make(<viewport>,
	   child: make(<checkboard-pane>),
	   min-width: 300,
	   min-height: 300,
	   horizontal-scroll-bar: horizontal-bar,
	   vertical-scroll-bar:   vertical-bar);
  let table-contents
    = vector(vector(#f, horizontal-ruler, #f),
             vector(vertical-ruler, viewport, vertical-bar),
             vector(#f, horizontal-bar, #f));
  contain(with-border (type: #"sunken", width: 300, height: 300)
	    make(<table-layout>, contents: table-contents)
          end,
	  title: "Three Pane Scrolling Test")
end method test-three-pane-scrolling;

define method test-gadget-scrolling
    (frame :: <frame>, #key orientation = #"vertical") => ()
  contain(scrolling (width: 100, height: 100)
	    make(<radio-box>,
		 items: range(from: 1, to: 20),
		 label-key: method (i) format-to-string("%d", i) end,
		 orientation: orientation)
	  end,
          title: "Gadget Scrolling Test")
end method test-gadget-scrolling;


/// Advanced scrolling

define method make-spreadsheet 
    (frame :: <frame>) => (table :: <table-layout>)
  with-frame-manager (frame-manager(frame))
    let children = make(<vector>, size: 100);
    for (x from 0 below 10)
      for (y from 0 below 10)
        let label = format-to-string("Position %d, %d", x, y);
        children[x + y * 10] := make(<push-button>, label: label)
      end
    end;
    make(<table-layout>,
	 columns: 10,
	 x-spacing: 4,
	 y-spacing: 4,
	 children: children)
  end
end method make-spreadsheet;

define method test-advanced-scrolling 
    (frame :: <frame>) => ()
  contain(scrolling (width: 300, height: 200)
            make-spreadsheet(frame)
	  end,
          title: "Advanced Scrolling Test")
end method test-advanced-scrolling;


/// Hand-rolled scrolling

define sealed class <hand-rolled-scrolling-pane> 
    (<scrolling-sheet-mixin>, 
     <drawing-pane>)
  slot top-line-number :: <integer> = 0;
  constant slot last-line-number :: <integer> = 300;
end class <hand-rolled-scrolling-pane>;

define method do-allocate-space
    (pane :: <hand-rolled-scrolling-pane>, width :: <integer>, height :: <integer>) => ()
  next-method();
  update-scroll-bars(pane)
end method do-allocate-space;

define function visible-lines
    (pane :: <hand-rolled-scrolling-pane>) => (lines :: <integer>)
  let _port = port(pane);
  let (width, height) = sheet-size(pane);
  ignore(width);
  let text-style = get-default-text-style(_port, pane);
  let text-height = font-height(text-style, _port);
  let top-line = top-line-number(pane);
  floor/(height, text-height) + top-line;
end function visible-lines;

define method line-scroll-amount
    (pane :: <hand-rolled-scrolling-pane>, orientation == #"vertical")
 => (amount :: <integer>)
  1
end method line-scroll-amount;

define method page-scroll-amount
    (pane :: <hand-rolled-scrolling-pane>, orientation == #"vertical")
 => (amount :: <integer>)
  visible-lines(pane) - 1
end method page-scroll-amount;

define method sheet-scroll-range
    (pane :: <hand-rolled-scrolling-pane>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, 1, last-line-number(pane))
end method sheet-scroll-range;

define method sheet-visible-range
    (pane :: <hand-rolled-scrolling-pane>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let top-line = top-line-number(pane);
  values(0, top-line, 1, top-line + visible-lines(pane))
end method sheet-visible-range;

define method set-sheet-visible-range
    (pane :: <hand-rolled-scrolling-pane>,
     left :: <real>, top :: <real>, right :: <real>, bottom :: <real>) => ()
  ignore(left, right, bottom);
  top-line-number(pane) := round(top);
  clear-box*(pane, $everywhere);
  repaint-sheet(pane, $everywhere)
end method set-sheet-visible-range;

define method handle-repaint
    (pane :: <hand-rolled-scrolling-pane>, medium :: <medium>, region :: <region>) => ()
  let _port = port(pane);
  let text-style = get-default-text-style(_port, pane);
  let text-height = font-height(text-style, _port);
  let top-line = top-line-number(pane);
  let bottom-line = visible-lines(pane);
  for (i from top-line to bottom-line,
       count from 0)
    draw-text(medium, format-to-string("%d", i), 0, count * text-height, align-y: #"top")
  end;
end method handle-repaint;

define frame <hand-rolled-scrolling-test-frame> (<simple-frame>)
  pane scrolling-pane (frame)
    make(<hand-rolled-scrolling-pane>);
  layout (frame)
    scrolling (scroll-bars: #"vertical")
      frame.scrolling-pane
    end;
end frame <hand-rolled-scrolling-test-frame>;

define method test-hand-rolled-scrolling 
    (frame :: <frame>) => ()
  let test-frame
    = make(<hand-rolled-scrolling-test-frame>,
           owner: frame,
           title: "Hand Rolled Scrolling Test");
  start-frame(test-frame)
end method test-hand-rolled-scrolling;


/// Scrolling frame

define variable $scrolling-test-frame-tests
  = vector(vector("Simple",      test-scrolling),
	   vector("Two-Pane",    test-two-pane-scrolling),
	   vector("Three-Pane",  test-three-pane-scrolling),
	   vector("Gadget",      test-gadget-scrolling),
	   vector("Advanced",    test-advanced-scrolling),
	   vector("Hand Rolled", test-hand-rolled-scrolling));

define frame <scrolling-tests-frame> (<simple-frame>)
  pane tests (frame)
    make(<list-control>,
	 items: $scrolling-test-frame-tests,
	 lines: size($scrolling-test-frame-tests),
	 label-key: first,
	 value-key: second,
	 activate-callback: method (sheet :: <sheet>)
			      gadget-value(sheet)(sheet-frame(sheet))
			    end);
  pane main-layout (frame)
    frame.tests;
  layout (frame) frame.main-layout;
end frame <scrolling-tests-frame>;

install-test(<scrolling-tests-frame>, "Scrolling");
