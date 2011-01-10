Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// This is the top-level loop of the thread that computes the next
// generation of the life world.  It calls do-n-generations to get
// the new world and then sends an event to the main event loop
// (via call-in-frame) telling it to do redisplay.  This is necessary
// because only the thread that created a window is allowed to draw
// on that window.
//
// This thread also watches frame.command to see whether the user
// has clicked on Start, Stop, Step, etc.
// 

define method life-logic-loop (frame :: <life-frame>) => ()
  ensure-board(frame);
  while (frame.command ~== #"exit")
    block ()
      // Note that this lock isn't strictly necessary from a resource-sharing
      // point of view.  It is simply using the notification facility to get
      // the equivalent of wait-with-timeout so that we don't spin while waiting
      // for frame.command to change from #"stop" to something else.  The thread
      // scheduler can do a better job of it than we can.  Note that the call
      // to wait-for atomically releases the lock frame.lock and starts blocking,
      // waiting for another thread to release the notification.
      with-lock(frame.lock)
	if (frame.command == #"stop")
	  wait-for(frame.user-input-received);
	end if;
      end;
      if (frame.command ~== #"exit")
	clear-status-bar(frame);
      end;
      while(frame.command == #"run")
        with-sheet-medium (medium = display-sheet(frame))
          let new-board = #f;
          let cell-delta :: <integer> = 0;
          let alive? = #f;
          let any-changed? = #f;
          let (sec :: <integer>, usec :: <integer>)
            = timing ()
                // need multiple-value-setq...
                let (a, b, c, d) = do-n-generations(frame.step-size,
                                                    frame.current-board,
                                                    frame.buffer-board);
                new-board := a; cell-delta := b; alive? := c;
                any-changed? := d;
              end;
          update-time(frame, sec, usec);
          update-live-cell-count(frame, cell-delta);
                
	  // new-board contains the new current board.  swap if necessary.
	  if (new-board ~== frame.current-board)
	    frame.buffer-board := frame.current-board;
	    frame.current-board := new-board;
	  end if;
          call-in-frame(frame, display-changed-cells, frame, frame.current-board);
	  // Wait for the redisplay to complete before letting this thread
	  // continue, else it may be possible for this thread to swamp the
	  // main thread with events, making it hard for the user to interact
	  // with the application (e.g., click the Stop button).
          with-lock(frame.lock)
            wait-for(frame.display-done)
          end;
          frame.current-generation := frame.current-generation + frame.step-size;
          display-current-generation(frame);
          frame.generations-to-go := frame.generations-to-go - 1;
          if (frame.generations-to-go == 0)
            frame.command := #"stop";
          end if;
          if (frame.sleep-interval)
            sleep(frame.sleep-interval);
          end;
          if (~alive?)
            format-to-status-bar(frame, "They're dead, Jim.");  // humor
            frame.command := #"stop";
          elseif (~any-changed?)
            format-to-status-bar(frame, "Stasis reached.");
            frame.command := #"stop";
          end if;
          // Both X and Windows buffer drawing requests.  This forces them
          // to be drawn immediately.  (Once per generation seems enough.)
          force-display(port(medium));
        end with-sheet-medium;
      end while;
    exception (<abort>)
      // handle abort restarts by stoping the display loop
      frame.command := #"stop";
    end block;
  end while;
end method life-logic-loop;

define constant $colors :: <simple-object-vector>
    = vector($red, $blue, $green, $yellow, $cyan, $magenta, $black);

define variable *color-index* :: <integer> = 0;

define method current-color (frame :: <life-frame>) => (color :: <color>)
  frame.cell-color
  | begin
      *color-index* := modulo(*color-index* + 1, size($colors));
      element($colors, *color-index*)
    end
end method current-color;

// Display a single cell in the life world.
define method display-one-cell (frame :: <life-frame>, sheet :: <life-sheet>,
				row :: <integer>, col :: <integer>,
				#key value)
 => ()
  with-sheet-medium (medium = sheet)
    display-one-cell(frame, medium, row, col, value: value);
  end with-sheet-medium;
end method display-one-cell;

define method display-one-cell (frame :: <life-frame>, medium :: <medium>,
				row :: <integer>, col :: <integer>,
				#key value)
 => ()
  value := value | get-cell(frame.current-board, row, col);
  with-drawing-options (medium,
                        brush: if(alive?(value))
				 current-color(frame)
			       else
                                 $background
			       end)
    let cell-size = frame.cell-size;
    let x1 = col * cell-size;
    let y1 = row * cell-size;
    draw-rectangle(medium, x1, y1, x1 + cell-size - 1, y1 + cell-size - 1);
  end;
end method display-one-cell;

// Display the entire life grid.  It's faster to clear it first
// and then only draw the live cells than to draw dead cells too.
define method display-board (frame :: <life-frame>) => ()
  //ensure-board(frame);
  clear-display-sheet(frame);
  let sheet = display-sheet(frame);
  let board = frame.current-board;
  with-sheet-medium (medium = sheet)
    for (row :: <integer> from 0 below board-rows(board))
      for (col :: <integer> from 0 below board-columns(board))
        let cell = get-cell(board, row, col);
        if (alive?(cell))
          display-one-cell(frame, medium, row, col, value: cell)
        end if;
      end for;
    end for;
  end with-sheet-medium;
end method display-board;

// Display only those cells that have been modified.
// Assume the board is initialized.
// Try to be fast.
// This method is called in the main thread UI thread.
define method display-changed-cells (frame :: <life-frame>, board :: <board>) => ()
  let v :: <intvector> = board-contents(board);
  let cell-size = frame.cell-size;
  let cols = board-columns(board);
  with-sheet-medium (medium = display-sheet(frame))
    with-lock ($board-lock)
      for (row :: <integer> from 0 below board-rows(board))
        for (col :: <integer> from 0 below cols)
          let cell = v[row * cols + col];
          if (modified?(cell))
            with-drawing-options (medium, brush: if (alive?(cell))
                                                   current-color(frame)
                                                 else
                                                   $background
                                                 end)
              let x1 = col * cell-size;
              let y1 = row * cell-size;
              draw-rectangle(medium, x1, y1, x1 + cell-size - 1, y1 + cell-size - 1);
            end;
            v[row * cols + col] := set-modified(cell, #f);
          end if;
        end for;
      end for;
    end with-lock;
  end with-sheet-medium;
  with-lock(frame.lock)
    release(frame.display-done)
  end;
end method;


// This is called by various commands to make sure that the board
// array has been created (at least), if not initialized with a pattern.
define method ensure-board (frame :: <life-frame>, #key force-init?, pattern)
  let (left, top, right, bottom) = sheet-edges(display-sheet(frame));
  // Setup the board to be the right size to fit in the display sheet.
  let cols :: <integer> = floor/(right - left, frame.cell-size);
  let rows :: <integer> = floor/(bottom - top, frame.cell-size);
  if (board-rows(frame.current-board) ~== rows
      | board-columns(frame.current-board) ~== cols)
    frame.current-board := make(<board>, rows: rows, columns: cols);
    frame.buffer-board  := make(<board>, rows: rows, columns: cols);
    initialize-game(frame, pattern: pattern);
  elseif (force-init? | pattern)
    initialize-game(frame, pattern: pattern);
  end if;
end method ensure-board;
  
define inline method clear-display-sheet (frame :: <life-frame>) => ()
  clear-box*(frame.display-sheet, $everywhere);
end method clear-display-sheet;
