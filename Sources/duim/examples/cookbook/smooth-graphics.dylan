Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Smooth graphics

define class <graphics-area> (<simple-pane>)
  slot %pixmap = #f
end class <graphics-area>;

define method graphics-area-pixmap (sheet :: <graphics-area>)
  sheet.%pixmap 
    | (sheet.%pixmap := 
         with-sheet-medium (medium = sheet)
           make-pixmap(medium, 200, 200)
         end)
end method graphics-area-pixmap;

define method handle-repaint 
    (sheet :: <graphics-area>, medium :: <medium>, region :: <region>)
 => ()
  with-double-buffering (medium, pixmap: graphics-area-pixmap(sheet))
    test-repaint(medium, region)
  end
end method handle-repaint;

define method test-repaint (medium :: <medium>, region :: <region>)
  clear-box*(medium, region);
  for (i from 0 below 150)
    clear-box(medium, i, i, i + 50, i + 50);
    draw-rectangle(medium, i,i, i + 50, i + 50)
  end
end method test-repaint;

define frame <smooth-graphics-frame> (<simple-frame>)
  pane graphics-area (frame)
    make(<graphics-area>, width: 300, height: 300);
  pane main-layout (frame)
    vertically ()
      frame.graphics-area
    end;
  layout (frame) frame.main-layout;
end frame <smooth-graphics-frame>;

install-example(<smooth-graphics-frame>, "Smooth Graphics");

