Module: life
Author: Carl Gay
Synopsis: Game logic portion of the game of Life.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// -------------------------------------
// Game logic
// -------------------------------------

// Do-n-generations is called by the Life application to update the
// state of the life "world".
// Parameters:
//   n: the number of generations to run.
//   board: an array containing the current state of the world.
//   buffer-board: an array that contain the state of the world upon exit.
//   cell-displayer: a function that can be used to display a life cell.
define method do-n-generations (n :: <integer>, board :: <array>,
			        buffer-board :: <array>, #key cell-displayer)
  let rows :: <integer> = dimension(board, 0) - 1;
  let cols :: <integer> = dimension(board, 1) - 1;
  let any-changed? :: <boolean> = #f;
  let any-alive? :: <boolean> = #f;
  local
    method index(index, max) => (index :: <integer>)
      if (index < 0) max elseif (index > max) 0 else index end
    end,
    method get-neighbors(r :: <integer>, c :: <integer>) => (index :: <integer>)
      let top = index(r - 1, rows);
      let bot = index(r + 1, rows);
      let left = index(c - 1, cols);
      let right = index(c + 1, cols);
      board[top, left] + board[top, c] + board[top, right] + board[r, left] +
	board[r, right] + board[bot, left] + board[bot, c] + board[bot, right]
    end,
    method update(row :: <integer>, col :: <integer>, new-value) => ()
      buffer-board[row, col] := new-value;
      any-changed? := #t;
      if (cell-displayer)
        cell-displayer(row, col, new-value);
      end;
    end;
  while (n > 0)
    for (row :: <integer> from 0 to rows)
      for (col :: <integer> from 0 to cols)
	let neighbors = get-neighbors(row, col);
	let old-val = board[row, col];
	if (old-val == $alive)
	  if (neighbors < 2 | neighbors > 3)
	    update(row, col, $dead);
	  else
	    buffer-board[row,col] := old-val;
	    any-alive? := #t;
	  end;
	else
	  if (neighbors == 3)
	    update(row, col, $alive);
	    any-alive? := #t;
	  else
	    buffer-board[row, col] := old-val;
	  end if;
	end if;
      end for;
    end for;
    n := n - 1;
  end while;
  values(any-changed?, any-alive?);
end method do-n-generations;

