Module: life
Author: Carl Gay
Synopsis: Game display support code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This is the top-level loop of the thread that displays the board.
define method life-display-top-level (frame :: <life-frame>) => ()
  ensure-board(frame);
  while (frame.command ~== #"exit")
    block ()
      // Note that this lock isn't strictly necessary from a resource-sharing
      // point of view.  It is simply using the notification facility to get
      // the equivalent of wait-with-timeout so that we don't spin while waiting
      // for frame.command to change from #"stop" to something else.  The thread
      // scheduler can do a better job of it than we can.
      with-lock(frame.lock)
	if (frame.command == #"stop")
	  wait-for(frame.notification);
	end if;
      end;
      if (frame.command ~== #"exit")
	clear-status-bar(frame);
      end;
// BEGIN (1)
      let sheet = display-sheet(frame);
      let updates :: <list> = #();
      local method display-updates (frame, updates :: <list>)
              for (update in updates)
                let (row, col, val) = apply(values, update);
                display-one-cell(frame, sheet, row, col, value: val);
              end for;
             end method;
// END (1)
      while(frame.command == #"run")
	let sheet = display-sheet(frame);
	let (changed?, alive?)
	  = begin
	      local method cell-displayer (row, col, val)
		      call-in-frame(frame, display-one-cell, frame, sheet, row, col, value: val);
		    end;
// BEGIN (1)
	      local method cell-displayer (row, col, val)
		      updates := add(updates, vector(row, col, val));
		    end;
// END (1)
	      do-n-generations(frame.step-size,
			       frame.current-board, frame.buffer-board,
			       cell-displayer: cell-displayer);
//			       cell-displayer: #f); // (2)
	    end;
// BEGIN (1)
        // Have to copy and reset in this thread so the next step doesn't
        // disrupt the collection the repaint thread's iterating over!
        call-in-frame(frame, display-updates, frame, copy-sequence(updates)); // (1)
        updates := #();
// END (1)
//        call-in-frame(frame, display-board, frame); // (2)
	let curr = frame.current-board;
	frame.current-board := frame.buffer-board;
	frame.buffer-board := curr;
	frame.generations-to-go := frame.generations-to-go - 1;
	if (frame.generations-to-go == 0)
	  frame.command := #"stop";
	end if;
	sleep(frame.sleep-interval);
	if (~alive?)
	  format-to-status-bar(frame, "They're dead, Jim.");
	  frame.command := #"stop";
	elseif (~changed?)
	  format-to-status-bar(frame, "Stasis reached.");
	  frame.command := #"stop";
	end if;
	// Both X and Windows buffer drawing requests.  This forces them
	// to be drawn immediately.  (Once per generation seems enough.)
	force-display(port(sheet));
      end while;
    exception (<abort>)
      // handle abort restarts by stoping the display loop
      frame.command := #"stop";
    end block;
  end while;
end method life-display-top-level;

define constant $colors :: <simple-object-vector>
    = vector($red, $blue, $green, $yellow, $cyan,
             $magenta, $black, $white);

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
  value := value | frame.current-board[row, col];
  with-drawing-options (sheet,
                        brush: if(value == $alive)
				 current-color(frame)
			       else
                                 $background
			       end)
    let radius = frame.cell-radius;
    let cell-size = radius * 2 + 1;
    draw-circle(sheet, 
		col * cell-size + radius,
		row * cell-size + radius, radius, filled?: #t);
  end;
end method display-one-cell;

// Display the entire life grid.  It's faster to clear it first
// and then only draw the live cells than to draw dead cells too.
define method display-board (frame :: <life-frame>) => ()
  ensure-board(frame);
  clear-display-sheet(frame);
  let sheet = display-sheet(frame);
  let board = frame.current-board;
  for (row :: <integer> from 0 below board-rows(board))
    for (col :: <integer> from 0 below board-columns(board))
      if (board[row, col] == $alive)
	display-one-cell(frame, sheet, row, col, value: $alive)
      end if;
    end for;
  end for;
end method display-board;


// This is called by various commands to make sure that the board
// array has been created (at least), if not initialized with a pattern.
define method ensure-board (frame :: <life-frame>, #key force-init?, pattern)
  let (left, top, right, bottom) = sheet-edges(display-sheet(frame));
  // Setup the board to be the right size to fit in the display sheet.
  let cols :: <integer> = floor/(right - left, frame.cell-radius * 2 + 1);
  let rows :: <integer> = floor/(bottom - top, frame.cell-radius * 2 + 1);
  if (frame.current-board == #f
	| dimension(frame.current-board, 0) ~== rows
	| dimension(frame.current-board, 1) ~== cols)
    frame.current-board := make(<array>, dimensions: vector(rows, cols));
    frame.buffer-board := make(<array>, dimensions: vector(rows, cols));
    // As of 97-08-13, the fill: parameter to make(<array>) isn't recognized.
    // So do it "by hand".  -carlg
    for (row :: <integer> from 0 below rows)
      for (col :: <integer> from 0 below cols)
	frame.current-board[row,col] := $dead;
	frame.buffer-board[row,col] := $dead;
      end for;
    end for;
    initialize-game(frame, pattern: pattern);
  elseif (force-init? | pattern)
    initialize-game(frame, pattern: pattern);
  end if;
end method ensure-board;
  
define inline method clear-display-sheet (frame :: <life-frame>) => ()
  clear-box*(frame.display-sheet, $everywhere);
end method clear-display-sheet;
