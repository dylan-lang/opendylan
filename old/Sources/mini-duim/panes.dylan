Module:    mini-duim
Synopsis:  Mini-DUIM top level sheets
Author:    Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Top level sheets

define open abstract class <top-level-sheet>
    (<mirrored-sheet-mixin>, <basic-sheet>)
  slot %frame = #f,
    init-keyword: frame:;
  // The container is a sheet, frame, or some sort of window handle...
  slot sheet-container = #f,
    init-keyword: container:;
  slot sheet-container-region = #f,
    init-keyword: container-region:;
end class <top-level-sheet>;

define method sheet-frame (sheet :: <sheet>) => (frame :: false-or(<frame>))
  let top-sheet = top-level-sheet(sheet);
  top-sheet & top-sheet.%frame
end method sheet-frame;

define method frame-manager 
    (sheet :: <sheet>) => (framem :: false-or(<frame-manager>))
  let frame = sheet-frame(sheet);
  frame & frame-manager(frame)
end method frame-manager;

define method port
    (sheet :: <sheet>) => (_port :: false-or(<port>))
  let frame = sheet-frame(sheet);
  frame & port(frame)
end method port;

define method top-level-sheet 
    (sheet :: <sheet>) => (sheet :: false-or(<sheet>))
  let parent = sheet-parent(sheet);
  parent & top-level-sheet(parent)
end method top-level-sheet;

define method top-level-sheet 
    (sheet :: <top-level-sheet>) => (sheet :: <top-level-sheet>)
  sheet
end method top-level-sheet;

define method find-parent-of-class 
    (sheet :: <sheet>, class :: <class>)
 => (sheet :: false-or(<sheet>))
  block (return)
    for (s = sheet then sheet-parent(s))
      if (~s | instance?(s, class)) return(s) end
    end;
    #f
  end
end method find-parent-of-class;

define method handle-event
    (sheet :: <top-level-sheet>, event :: <window-configuration-event>) => ()
  let frame = sheet-frame(sheet);
  let layout = frame-layout(frame);
  if (layout)
    //--- Note that Windows returns the 'client area' not the external area
    let (left, top, right, bottom) = box-edges(event-region(event));
    set-sheet-edges(layout, left, top, right, bottom)
  end
end method handle-event;


/// Simple pane
define open class <simple-pane>
    (<mirrored-sheet-mixin>, <basic-sheet>)
  slot %display-function = #f,
    init-keyword: display-function:;
end class <simple-pane>;

define method handle-repaint
    (pane :: <simple-pane>, medium :: <medium>, region :: <region>) => ()
  let function = pane.%display-function;
  if (function)
    function(pane, medium, region)
  end
end method handle-repaint;


define open class <drawing-pane>
    (<sheet-with-medium-mixin>, <simple-pane>)
end class <drawing-pane>;
