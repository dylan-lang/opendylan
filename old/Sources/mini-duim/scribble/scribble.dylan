Module:    scribble
Author:    Scott McKay
Synopsis:  Simple scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <scribble-pane> (<drawing-pane>)
  slot scribble-segment = #f;
  slot scribble-segments = make(<stretchy-vector>);
  slot scribble-popup-menu-callback = #f,
    init-keyword: popup-menu-callback:;
end class <scribble-pane>;

define method draw-segment
    (medium :: <medium>, segment :: <sequence>) => ()
  draw-polygon*(medium, segment, closed?: #f, filled?: #f)
end method draw-segment;

// Draw the scribble as several unconnected polylines
define method handle-repaint
    (sheet :: <scribble-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  for (segment in sheet.scribble-segments)
    draw-segment(medium, segment)
  end
end method handle-repaint;

define method handle-button-event
    (sheet :: <scribble-pane>, event :: <button-press-event>, button == $left-button)
 => ()
  sheet.scribble-segment := make(<stretchy-vector>);
  add-scribble-segment(sheet, event.event-x, event.event-y)
end method handle-button-event;

define method handle-button-event
    (sheet :: <scribble-pane>, event :: <pointer-drag-event>, button == $left-button)
 => ()
  add-scribble-segment(sheet, event.event-x, event.event-y)
end method handle-button-event;

define method handle-button-event
    (sheet :: <scribble-pane>, event :: <button-release-event>, button == $left-button)
 => ()
  if (add-scribble-segment(sheet, event.event-x, event.event-y))
    add!(sheet.scribble-segments, sheet.scribble-segment)
  end;
  sheet.scribble-segment := #f;
end method handle-button-event;

define method handle-button-event
    (sheet :: <scribble-pane>, event :: <button-release-event>, button == $right-button)
 => ()
  let popup-menu-callback = scribble-popup-menu-callback(sheet);
  if (popup-menu-callback)
    popup-menu-callback(sheet, event.event-x, event.event-y)
  end
end method handle-button-event;

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
  clear-box(sheet, sheet-region(sheet))
end method clear-surface;


/// A wrapping frame
define frame <scribble-frame> (<simple-frame>)
  pane surface (frame)
    make-pane(<scribble-pane>, 
	      popup-menu-callback: method (sheet, x, y)
				     let frame = sheet-frame(sheet);
				     popup-scribble-menu(frame, x, y)
				   end,
	      width: 300, height: 200);
  pane clear-button (frame)
    make-pane(<menu-button>,
	      label: "&Clear",
	      selection-mode: #"none",
	      activate-callback: method (button)
				   ignore(button);
				   clear-surface(frame.surface)
				 end);
  pane exit-button (frame)
    make-pane(<menu-button>,
              label: "Exit",
	      selection-mode: #"none",
	      activate-callback: method (button)
				   frame-exit(sheet-frame(button))
				 end);
  pane scribble-menu (frame)
    make-pane(<menu>,
	      label: "&Scribble",
	      owner: frame.surface,
	      children: vector(frame.clear-button));
  pane file-menu (frame)
    make-pane(<menu>,
	      label: "File",
	      children: vector(frame.exit-button));
  layout (frame) frame.surface;
  menu-bar (frame)
    make-pane(<menu-bar>, children: vector(frame.file-menu));
end frame <scribble-frame>;

define method popup-scribble-menu
    (frame :: <scribble-frame>, x :: <integer>, y :: <integer>)
 => ()
  let menu = scribble-menu(frame);
  set-sheet-position(menu, x, y);
  sheet-mapped?(menu) := #t
end method popup-scribble-menu;

define method scribble 
    (#key container, container-region)
 => (frame :: <scribble-frame>)
  let frame
    = make(<scribble-frame>,
	   container: container,
	   container-region: container-region,
	   title: "Scribble");
  //--- Do we need any other initializations?
  frame
end method scribble;


/// A container for scribble

// Call 'attach-scribble' with an <HWND> and the four integers specifying
// the edges of the region, and Scribble will attach itself there.
define method attach-scribble 
    (container, left, top, right, bottom)
 => (frame :: <frame>)
  let frame
    = make(<scribble-frame>,
	   title: "Scribble",
	   container: container,
	   container-region: make-bounding-box(left, top, right, bottom));
  start-frame(frame, activate?: #f);
  frame
end method attach-scribble;

define method contained-scribble () => (frame :: <frame>)
  let container = make(<frame>, title: "Scribble container");
  start-frame(container, activate?: #f);
  let frame = attach-scribble(container, 20, 20, 200, 200);
  values(container, frame)
end method contained-scribble;

define method contained-button () => (frame :: <frame>)
  let container = make(<frame>, title: "Button frame container");
  start-frame(container, activate?: #f);
  let frame
    = make(<frame>,
	   layout: make(<push-button>, label: "Hello"),
	   container: container,
	   container-region: make-bounding-box(100, 100, 200, 200));
  start-frame(frame, activate?: #f);
  values(container, frame)
end method contained-button;
