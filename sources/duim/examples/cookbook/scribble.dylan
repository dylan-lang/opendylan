Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Scribble example, three ways


/// Scribble, storing the drawn points manually

define class <scribble-pane> (<drawing-pane>)
  slot scribble-segment = #f;
  slot scribble-segments = make(<stretchy-vector>);
end class <scribble-pane>;

define method draw-segment
    (medium :: <medium>, segment :: <sequence>) => ()
  draw-polygon(medium, segment, closed?: #f, filled?: #f)
end method draw-segment;

define method handle-repaint
    (sheet :: <scribble-pane>, medium :: <medium>, region :: <region>)
 => ()
  ignore(region);
  for (segment in sheet.scribble-segments)
    draw-segment(medium, segment)
  end
end method handle-repaint;

define method handle-event
    (sheet :: <scribble-pane>, event :: <button-press-event>) => ()
  sheet.scribble-segment := make(<stretchy-vector>);
  add-scribble-segment(sheet, event.event-x, event.event-y)
end method handle-event;

define method handle-event
    (sheet :: <scribble-pane>, event :: <pointer-drag-event>) => ()
  add-scribble-segment(sheet, event.event-x, event.event-y)
end method handle-event;

define method handle-event
    (sheet :: <scribble-pane>, event :: <button-release-event>) => ()
  if (add-scribble-segment(sheet, event.event-x, event.event-y))
    add!(sheet.scribble-segments, sheet.scribble-segment)
  end;
  sheet.scribble-segment := #f
end method handle-event;

define method add-scribble-segment
    (sheet :: <scribble-pane>, x, y) => (did-it? :: <boolean>)
  let segment = sheet.scribble-segment;
  // The app can generate drag and release events before it has ever
  // seen a press event, so be careful
  if (segment)
    add!(segment, x);
    add!(segment, y);
    draw-segment(sheet-medium(sheet), segment);
    #t
  end
end method add-scribble-segment;

define method clear-surface (sheet :: <scribble-pane>) => ()
  sheet.scribble-segments.size := 0;
  clear-box*(sheet, sheet-region(sheet))
end method clear-surface;


/// A wrapping frame
define frame <scribble-frame> (<simple-frame>)
  pane surface (frame)
    make-pane(<scribble-pane>, width: 300, height: 200);
  pane clear-button (frame)
    make-pane(<menu-button>,
	      label: "&Clear",
	      selection-mode: #"none",
	      activate-callback: method (button)
				   ignore(button);
				   clear-surface(frame.surface)
				 end);
  pane window-menu (frame)
    make-pane(<menu>,
	      label: "&Scribble",
	      children: vector(frame.clear-button));
  layout (frame) frame.surface;
  menu-bar (frame)
    make-pane(<menu-bar>, children: vector(frame.window-menu));
end frame <scribble-frame>;

install-example(<scribble-frame>, "Scribble");

define method scribble (#rest args)
  apply(start-example-frame, <scribble-frame>, args)
end method scribble;


/*
/// Scribble, using some sort of window system backing store
//--- Well, if we had a way to make a window with a backing store...

define class <scribble-pane> (<drawing-pane>)
end class <scribble-pane>;

define method handle-event
    (sheet :: <scribble-pane>, event :: <button-press-event>) => ()
  draw-point(sheet, event-x(event), event-y(event))
end method handle-event;

define method handle-event
    (sheet :: <scribble-pane>, event :: <pointer-drag-event>) => ()
  draw-point(sheet, event-x(event), event-y(event))
end method handle-event;

define frame <scribble-frame> (<simple-frame>)
  pane surface (frame) make-pane(<scribble-pane>, backing-store?: #t);
  layout (frame) frame.surface;
end frame <gadget-enabling-frame>;

*/


/*
/// Scribble, using 'contain'

define class <scribble-pane> (<drawing-pane>)
end class <scribble-pane>;

define method handle-event
    (sheet :: <scribble-pane>, event :: <button-press-event>) => ()
  draw-point(sheet, event-x(event), event-y(event))
end method handle-event;

define method handle-event
    (sheet :: <scribble-pane>, event :: <pointer-drag-event>) => ()
  draw-point(sheet, event-x(event), event-y(event))
end method handle-event;

contain(make-pane(<scribble-pane>));
*/


/*
/// Scribble, using output recording

define class <scribble-pane> (<recording-pane>)
end class <scribble-pane>;

define method handle-event
    (sheet :: <scribble-pane>, event :: <button-press-event>) => ()
  draw-point(sheet, event-x(event), event-y(event))
end method handle-event;

define method handle-event
    (sheet :: <scribble-pane>, event :: <pointer-drag-event>) => ()
  draw-point(sheet, event-x(event), event-y(event))
end method handle-event;

define frame <scribble-frame> (<simple-frame>)
  pane surface (frame) make-pane(<scribble-pane>);
  layout (frame) frame.surface;
end frame <gadget-enabling-frame>;

*/

