Module: life
Author: Carl Gay
Synopsis: Event handling for the game of Life.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// --------------------------------------------------------
// On-screen pattern editing + events
// Only allow pattern editing while the display-thread is stopped.
// --------------------------------------------------------

// The <life-sheet> class supports double-buffering, and on-screen
// pattern editing.
define class <life-sheet> (<drawing-pane>)
  // These slots are used to implement the on-screen pattern editing.
  slot saved-row :: <integer> = -1;
  slot saved-col :: <integer> = -1;
  slot saved-val :: <integer> = $dead;
end class <life-sheet>;


// This is called when the window becomes (partially) unobscured.
// Copy the contents of the offscreen-buffer onto the screen.
define method handle-repaint
    (sheet :: <life-sheet>, medium :: <medium>, region :: <region>)
 => ()
  display-board(sheet-frame(sheet));
end method handle-repaint;


define method handler-helper		   // get it?
    (sheet :: <life-sheet>, x :: <integer>, y :: <integer>,
     dragging? :: <boolean>)
 => ()
  let frame = sheet-frame(sheet);
  ensure-board(frame);
  let board = frame.current-board;
  let rows = dimension(board, 0);
  let cols = dimension(board, 1);
  // Convert from sheet coordinates to an index into the board array.
  // This works for both rows and columns since the board is aligned
  // to the upper-left corner of the sheet.
  local method coord-to-index(n :: <integer>)
	  floor/(n, frame.cell-radius * 2 + 1);
	end;
  let row = coord-to-index(y);
  let col = coord-to-index(x);
  if (row >= 0 & row < rows & col >= 0 & col < cols)
    let val = if (dragging?)
		sheet.saved-val;
	      else
		if (board[row, col] == $alive) $dead else $alive end;
	      end if;
    if (~dragging?
	  | saved-row ~== row
	  | saved-col ~== col)
      board[row, col] := val;
      display-one-cell(frame, sheet, row, col);
      sheet.saved-row := row;
      sheet.saved-col := col;
      sheet.saved-val := val;
    end if;
  end if;
end method handler-helper;


define method handle-button-event
    (sheet :: <life-sheet>, event :: <button-press-event>, b == $left-button)
 => ()
  if (sheet-frame(sheet).command == #"stop")
    handler-helper(sheet, event.event-x, event.event-y, #f);
    clear-status-bar(sheet-frame(sheet));
  end if;
end method handle-button-event;


define method handle-button-event
    (sheet :: <life-sheet>, event :: <pointer-drag-event>, b == $left-button)
 => ()
  if (sheet-frame(sheet).command == #"stop")
    handler-helper(sheet, event.event-x, event.event-y, #t);
  end if;
end method handle-button-event;

