Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple implementation of carets

define sealed class <simple-caret> (<basic-caret>)
  sealed slot %visible? = #f;
end class <simple-caret>;

define sealed domain make (singleton(<simple-caret>));
define sealed domain initialize (<simple-caret>);

define sideways method make-caret
    (_port :: <basic-port>, sheet :: <sheet>, #key x, y, width, height)
 => (caret :: <simple-caret>)
  with-sheet-medium (medium = sheet)
    let (w, h) = text-size(medium, " ");
    make(<simple-caret>,
	 port: _port, sheet: sheet,
	 x: x | 0, y: y | 0,
	 width: width | ceiling(w), height: height | ceiling(h))
  end
end method make-caret;


define sealed method do-set-caret-position
    (caret :: <simple-caret>, x :: <integer>, y :: <integer>) => ()
  when (caret.%visible?)
    let (old-x, old-y)  = caret-position(caret);
    let (width, height) = caret-size(caret);
    draw-caret(caret, old-x, old-y, width, height, #f);
    draw-caret(caret, x, y, width, height, #t)
  end
end method do-set-caret-position;

define sealed method do-set-caret-size
    (caret :: <simple-caret>, width :: <integer>, height :: <integer>) => ()
  when (caret.%visible?)
    let (x, y)  = caret-position(caret);
    let (old-width, old-height) = caret-size(caret);
    draw-caret(caret, x, y, old-width, old-height, #f);
    draw-caret(caret, x, y, width, height, #t)
  end
end method do-set-caret-size;

define sealed method do-show-caret
    (caret :: <simple-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  unless (caret.%visible?)
    let (x, y)  = caret-position(caret);
    let (width, height) = caret-size(caret);
    draw-caret(caret, x, y, width, height, #t)
  end
end method do-show-caret;

define sealed method do-hide-caret
    (caret :: <simple-caret>, #key tooltip?) => ()
  ignore(tooltip?);
  when (caret.%visible?)
    let (x, y)  = caret-position(caret);
    let (width, height) = caret-size(caret);
    draw-caret(caret, x, y, width, height, #f)
  end
end method do-hide-caret;


define sealed method draw-caret
    (caret :: <simple-caret>, x, y, width, height, on?) => ()
  let sheet = caret-sheet(caret);
  let _port = port(sheet);
  let focus? = _port & (port-input-focus(_port) == sheet);
  with-sheet-medium (medium = sheet)
    with-drawing-options (medium, brush: $xor-brush)
      draw-rectangle(medium, x, y, x + width, y + height, filled?: focus?)
    end;
    force-display(medium)
  end;
  caret.%visible? := on?
end method draw-caret;
