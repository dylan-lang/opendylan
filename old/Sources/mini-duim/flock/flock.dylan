module:    flock
Synopsis:  Flying tiles
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <graphics-area> (<drawing-pane>)
  slot %pixmap = #f
end class <graphics-area>;

define method graphics-area-pixmap (sheet :: <graphics-area>)
  /*
  sheet.%pixmap 
    | (sheet.%pixmap := 
         with-sheet-medium (medium = sheet)
           make-pixmap(medium, 200, 200)
         end)
  */
  #f
end method graphics-area-pixmap;

define variable *number-of-repaints* = 20;

define method handle-repaint 
    (sheet :: <graphics-area>, medium :: <medium>, region :: <region>)
  ignore(region);
  let region = sheet-region(sheet);
  for (i from 0 below *number-of-repaints*)
    begin /* with-double-buffering (medium, pixmap: graphics-area-pixmap(sheet)) */
      clear-box(medium, region);
      test-repaint(medium, region)
    end;
  end;
end method handle-repaint;

/*
define method test-repaint (medium :: <medium>, region :: <region>)
  clear-box(medium, region);
  for (i from 0 below 150)
    let offset = truncate(rnd(40.0, 60.0));
    clear-box*(medium, i, i, i + offset, i + offset);
    draw-rectangle*(medium, i, i, i + offset, i + offset, filled?: #f)
  end
end method test-repaint;
*/

define method test-repaint (medium :: <medium>, region :: <region>)
  move-squares();
  for (s in *squares*)
    let r = s.r;
    let size = 0.5;
    let scale = 50;
    let offset = 50;
    let x1 = r.x * scale + offset;
    let y1 = r.y * scale + offset;
    let x2 = (r.x + size) * scale + offset;
    let y2 = (r.y + size) * scale + offset;
    draw-rectangle* (medium, x1, y1, x2, y2, filled?: #f);
  end;
end method test-repaint;

define method handle-event
    (sheet :: <graphics-area>, event :: <double-click-event>) => ()
  ignore(event);
  if (event.event-button == $right-button)
    enter-debugger("Enter debugger on double-click in sheet %=", sheet)
  end
end method handle-event;


/// Flock frame

define frame <flock-frame> (<simple-frame>)
  pane graphics-area (frame)
    make(<graphics-area>, width: 300, height: 300);
  layout (frame) frame.graphics-area;
end frame <flock-frame>;

define method initialize (frame :: <flock-frame>, #key) => ()
  next-method();
  initialize-grid();
end method initialize;

define method flock ()
  make(<flock-frame>, title: "Flying Tiles", mapped?: #t);
end method flock;
