Module:       duim-sheets-internals
Synopsis:     'print-object' methods for DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (_port :: <port>, stream :: <stream>) => ()
  printing-object (_port, stream, type?: #t, identity?: #t)
    format(stream, "%s", port-name(_port) | "")
  end
end method print-object;


define method print-object
    (event :: <window-event>, stream :: <stream>) => ()
  printing-object (event, stream, type?: #t, identity?: #t)
    let (left, top, right, bottom) = box-edges(event-region(event));
    format(stream, "on %= from (%d,%d) to (%d,%d)",
	   event-sheet(event), left, top, right, bottom)
  end
end method print-object;

define method print-object
    (event :: <pointer-boundary-event>, stream :: <stream>) => ()
  printing-object (event, stream, type?: #t, identity?: #t)
    format(stream, "on %= kind %=",
	   event-sheet(event), boundary-event-kind(event))
  end
end method print-object;

define method print-object
    (event :: <pointer-motion-event>, stream :: <stream>) => ()
  printing-object (event, stream, type?: #t, identity?: #t)
    format(stream, "on %= at (%d,%d)",
	   event-sheet(event), event-x(event), event-y(event))
  end
end method print-object;

define method print-object
    (event :: <pointer-button-event>, stream :: <stream>) => ()
  printing-object (event, stream, type?: #t, identity?: #t)
    format(stream, "on %= with %= %=",
	   event-sheet(event), event-button(event), event-modifier-state(event))
  end
end method print-object;

define method print-object
    (event :: <keyboard-event>, stream :: <stream>) => ()
  printing-object (event, stream, type?: #t, identity?: #t)
    format(stream, "on %= with %= (%=) %=",
	   event-sheet(event),
	   event-key-name(event), event-character(event), event-modifier-state(event))
  end
end method print-object;
