Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// On-screen pattern editing + event handling
//

// The <life-sheet> class supports double-buffering, and on-screen
// pattern editing.
define class <life-sheet> (<drawing-pane>)
  // These slots are used to implement the on-screen pattern editing.
  slot saved-row :: <integer> = -1;
  slot saved-col :: <integer> = -1;
  slot saved-val :: <integer> = set-modified(set-alive(0, #f), #f);
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
  let rows = board.board-rows;    //dimension(board, 0);
  let cols = board.board-columns; //dimension(board, 1);
  // Convert from sheet coordinates to an index into the board array.
  // This works for both rows and columns since the board is aligned
  // to the upper-left corner of the sheet.
  local method coord-to-index(n :: <integer>)
	  floor/(n, frame.cell-size);
	end;
  let row = coord-to-index(y);
  let col = coord-to-index(x);
  if (row >= 0 & row < rows & col >= 0 & col < cols)
    let old-val :: <integer> = get-cell(board, row, col);
    let was-alive? :: <boolean> = alive?(old-val);
    let val = if (dragging?)
		sheet.saved-val;
	      else
                if (was-alive?)
                  set-alive(0, #f)
                else
                  set-alive(0, #t)
                end if;
	      end if;
    if (~dragging?
	  | saved-row ~== row
	  | saved-col ~== col)
      set-cell(board, val, row, col);
      if (alive?(val) ~== alive?(old-val))
        frame.live-cell-count
          := frame.live-cell-count + if (was-alive?) -1 else 1 end;
      end if;
      display-live-cell-count(frame);
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
  // Only allow pattern editing while the display-thread is stopped.
  if (sheet-frame(sheet).command == #"stop")
    handler-helper(sheet, event.event-x, event.event-y, #f);
    clear-status-bar(sheet-frame(sheet));
  end if;
end method handle-button-event;


define method handle-button-event
    (sheet :: <life-sheet>, event :: <pointer-drag-event>, b == $left-button)
 => ()
  // Only allow pattern editing while the display-thread is stopped.
  if (sheet-frame(sheet).command == #"stop")
    handler-helper(sheet, event.event-x, event.event-y, #t);
  end if;
end method handle-button-event;
