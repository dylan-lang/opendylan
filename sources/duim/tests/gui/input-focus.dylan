Module:    duim-gui-test-suite
author:    Andy Armstrong
Synopsis:  An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test input sheet
define class <test-input-sheet> (<drawing-pane>)
  keyword height: = 50;
  keyword fixed-height?: = #t;
end class <test-input-sheet>;

define method handle-repaint
    (sheet :: <test-input-sheet>, medium :: <medium>, region :: <region>) => ()
  let color
    = if (sheet == frame-input-focus(sheet-frame(sheet)))
        $red
      else
        $blue
      end;
  with-drawing-options (medium, brush: color)
    let (left, top, right, bottom) = box-edges(region);
    draw-rectangle(medium, left, top, right, bottom)
  end
end method handle-repaint;

define method handle-event
    (sheet :: <test-input-sheet>, event :: <input-focus-in-event>) => ()
  repaint-sheet(sheet, $everywhere)
end method handle-event;

define method handle-event
    (sheet :: <test-input-sheet>, event :: <input-focus-out-event>) => ()
  repaint-sheet(sheet, $everywhere)
end method handle-event;

define method sheet-handles-keyboard?
    (sheet :: <test-input-sheet>) => (handles-keyboard? :: <boolean>)
  #f
end method sheet-handles-keyboard?;


/// Input focus frame
define frame <input-focus-frame> (<simple-frame>)
  pane text-pane (frame)
    make(<text-field>);
  pane button (frame)
    make(<push-button>, label: "Press me!");
  pane test-sheet (frame)
    make(<test-input-sheet>);
  layout (frame)
    vertically (spacing: 2)
      horizontally (spacing: 2)
        make(<label>, label: "Text:");
        frame.text-pane
      end;
      frame.test-sheet;
      frame.button;
    end;
  status-bar (frame) 
    make(<status-bar>);
end frame <input-focus-frame>;

define method handle-event
    (frame :: <input-focus-frame>, event :: <frame-input-focus-changed-event>)
 => ()
  frame-status-message(frame)
    := format-to-string("Focus now in %=", event.event-new-focus)
end method handle-event;

install-test(<input-focus-frame>, "Input focus");
