Module: emtiles
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *last-tiles-window* = #f;

define method go ()
  *last-tiles-window* := realize(<tiling-view>, extent: point(500, 400))
end method;

/*
  tv := go();
  for (i from 0 below 20)
    tv.idle
  end;
*/

// eof

