Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -------------------------------------
// Game logic
// -------------------------------------

// Do-n-generations is called by the Life application to update the
// state of the life "world".  This is about the most naive
// implementation possible.  It simply uses a big array to represent
// the life world and traverses the entire array each time it is
// called.  As it traverses the array it updates a second copy of the
// array with the new state of the world.  The two arrays are then
// swapped and the process starts over again.  

// Parameters:
//   n: the number of generations to run.
//   board: an array containing the current state of the world.
//   buffer-board: an array that contain the state of the world upon exit.
//   cell-displayer: a function that can be used to display a life cell.
define method do-n-generations
    (generations :: <integer>, board :: <board>, buffer-board :: <board>,
     #key cell-displayer)
 => (new-current :: <board>, cell-delta :: <integer>, any-alive? :: <boolean>,
     any-changed? :: <boolean>)
  let rows :: <integer> = board-rows(board);
  let cols :: <integer> = board-columns(board);
  let max-row :: <integer> = rows - 1;
  let max-col :: <integer> = cols - 1;
  let any-changed? :: <boolean> = #f;
  let cell-count-delta :: <integer> = 0;
  let any-alive?   :: <boolean> = #f;
  local
    method index (index :: <integer>, max :: <integer>) => (index :: <integer>)
      if (index < 0) max elseif (index > max) 0 else index end
    end,
    method get-neighbors (r :: <integer>, c :: <integer>, old-vec :: <intvector>)
     => (index :: <integer>)
      let top = index(r - 1, max-row);
      let bot = index(r + 1, max-row);
      let left  = index(c - 1, max-col);
      let right = index(c + 1, max-col);
      let bot*cols = bot * cols;
      let top*cols = top * cols;
      if (alive?(old-vec[top*cols + left]))      1 else 0 end
        + if (alive?(old-vec[top*cols + c]))     1 else 0 end
        + if (alive?(old-vec[top*cols + right])) 1 else 0 end
        + if (alive?(old-vec[r * cols + left]))  1 else 0 end
        + if (alive?(old-vec[r * cols + right])) 1 else 0 end
        + if (alive?(old-vec[bot*cols + left]))  1 else 0 end
        + if (alive?(old-vec[bot*cols + c]))     1 else 0 end
        + if (alive?(old-vec[bot*cols + right])) 1 else 0 end
    end,
    method update (row :: <integer>, col :: <integer>, new-value, new-vec :: <intvector>)
     => ()
      new-vec[row * cols + col] := new-value;
      any-changed? := #t;
      cell-count-delta := cell-count-delta + if (alive?(new-value)) 1 else -1 end;
      if (cell-displayer)
        cell-displayer(row, col, new-value);
      end;
    end;
  while (generations > 0)
    let old-vec :: <intvector> = board-contents(board);
    let new-vec :: <intvector> = board-contents(buffer-board);
    let dead+mod  = set-modified(set-alive(0, #f), #t);
    let alive+mod = set-modified(set-alive(0, #t), #t);
    with-lock ($board-lock)
      // For each cell in the array, implement the Life rules
      // and store the result in the new array.
      for (row :: <integer> from 0 below rows)
        for (col :: <integer> from 0 below cols)
          let neighbors :: <integer> = get-neighbors(row, col, old-vec);
          let old-val = old-vec[row * cols + col];
          if (alive?(old-val))
            if (neighbors < 2 | neighbors > 3)
              update(row, col, dead+mod, new-vec);
            else
              // Just copy the cell over.  No need to mark it modified.
              new-vec[row * cols + col] := old-val;
              any-alive? := #t;
            end if;
          else
            if (neighbors == 3)
              update(row, col, alive+mod, new-vec);
              any-alive? := #t;
            else
              new-vec[row * cols + col] := old-val;  // just copy over
            end if;
          end if;
        end for;
      end for;
    end with-lock;
    generations := generations - 1;
    let temp = board;		// swap(board, buffer-board)
    board := buffer-board;
    buffer-board := temp;
  end while;
  values(board, cell-count-delta, any-alive?, any-changed?);
end method do-n-generations;
