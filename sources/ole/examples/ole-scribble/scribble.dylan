Module:    scribble
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple OLE scribble application

define class <scribble-pane> (<drawing-pane>)
  slot scribble-segment = #f;
  constant slot scribble-segments = make(<stretchy-vector>);
  constant slot scribble-popup-menu-callback = #f,
    init-keyword: popup-menu-callback:;
end class <scribble-pane>;

define method draw-segment
    (medium :: <medium>, segment :: <sequence>) => ()
  draw-polygon(medium, segment, closed?: #f, filled?: #f)
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
    (sheet :: <scribble-pane>, 
     event :: <button-press-event>, 
     button == $left-button) => ()
  sheet.scribble-segment := make(<stretchy-vector>);
  add-scribble-segment(sheet, event.event-x, event.event-y)
end method handle-button-event;

define method handle-button-event
    (sheet :: <scribble-pane>, 
     event :: <pointer-drag-event>, 
     button == $left-button) => ()
  add-scribble-segment(sheet, event.event-x, event.event-y)
end method handle-button-event;

define method handle-button-event
    (sheet :: <scribble-pane>, 
     event :: <button-release-event>, 
     button == $left-button) => ()
  when (add-scribble-segment(sheet, event.event-x, event.event-y))
    add!(sheet.scribble-segments, sheet.scribble-segment);
    note-embedded-data-changed(sheet-frame(sheet));
  end;
  sheet.scribble-segment := #f
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
    (sheet :: <scribble-pane>, x, y) => (did-it? :: <boolean>)
  let segment = sheet.scribble-segment;
  // The app can generate drag and release events before it has ever
  // seen a press event, so be careful
  when (segment)
    add!(segment, x);
    add!(segment, y);
    draw-segment(sheet-medium(sheet), segment);
    #t
  end
end method add-scribble-segment;

define method clear-surface (sheet :: <scribble-pane>) => ()
  let already-clear? = empty?(sheet.scribble-segments);
  sheet.scribble-segments.size := 0;
  clear-box*(sheet, sheet-region(sheet));
  unless (already-clear?)
    note-embedded-data-changed(sheet-frame(sheet))
  end
end method clear-surface;


/// A wrapping frame, which gets realized as an OLE server
define frame <scribble-frame> (<embeddable-frame>)
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
						    clear-surface(frame.surface)
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
						    clear-surface(frame.surface)
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
