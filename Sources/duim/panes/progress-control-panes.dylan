Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of progress note panes

define sealed class <progress-bar-pane> 
    (<simple-pane>, <progress-bar>)
end class <progress-bar-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <progress-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<progress-bar-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<progress-bar-pane>));
define sealed domain initialize (<progress-bar-pane>);

define method do-compose-space 
    (pane :: <progress-bar-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(height);
  let min-width = 100;
  let height = 12;
  make(<space-requirement>,
       min-width: min-width, width: max(width | 0, min-width), max-width: $fill,
       min-height: height, height: height, max-height: height)
end method do-compose-space;

define constant $progress-note-brush = make-gray-color(0.7);

define method handle-repaint
    (pane :: <progress-bar-pane>, medium :: <medium>, region :: <region>) => ()
  let value = gadget-value(pane);
  let (left, top, right, bottom) = box-edges(pane);
  let range = gadget-value-range(pane);
  let start-value = range[0];
  let total-range = range[size(range) - 1] - start-value;
  let width = right - left;
  let filled-width
    = floor/((value - start-value) * width, total-range);
  with-drawing-options (pane, brush: $progress-note-brush)
    draw-rectangle(medium, left, top, left + filled-width, bottom)
  end;
  with-drawing-options (pane, brush: $background)
    draw-rectangle(medium, left + filled-width, top, right, bottom)
  end;
  draw-rectangle(medium, left, top, right - 1, bottom - 1, filled?: #f)
end method handle-repaint;

define method note-gadget-value-changed
    (pane :: <progress-bar-pane>) => ()
  when (sheet-mapped?(pane))
    repaint-sheet(pane, $everywhere);
    force-display(pane)
  end
end method note-gadget-value-changed;
