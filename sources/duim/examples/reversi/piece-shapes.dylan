Module:       reversi
Author:       Jonathon Lee
Synopsis:     Reversi game extensions (1)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// this file contains the first incremental change to the reversi example. 
// It adds the functionality that allows the user to change the shape of the 
// players pieces.

*changed?* := #t;

define method draw-piece 
    (sheet :: <reversi-square>, medium :: <medium>, shape == #"square") => ()
  when (square-piece(sheet))
    let (cx, cy, xr, yr, color) = square-geometry(sheet);
    with-drawing-options (medium, brush: color)
      draw-rectangle(medium,
                     cx - xr, cy - yr, cx + xr, cy + yr,
                     filled?: #t);
    end;
  end;
end method draw-piece;

define method draw-piece 
    (sheet :: <reversi-square>, medium :: <medium>, shape == #"triangle") => ()
  when (square-piece(sheet))
    let (cx, cy, xr, yr, color) = square-geometry(sheet);
    with-drawing-options (medium, brush: color)
      draw-triangle(medium,
                    cx - xr, cy + yr,
                    cx + xr, cy + yr,
                    cx,      cy - yr,
                    filled: #t);
    end;
  end;
end method draw-piece;


define method set-shape
    (frame :: <reversi-frame>, new-shape) => ()
  let game = reversi-frame-game(frame);
  *reversi-piece-shape* := new-shape;
  repaint-sheet(reversi-frame-table-pane(frame), $everywhere);
end method set-shape;
