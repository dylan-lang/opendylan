Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ----------------------------------------------
/// Utilities
/// ----------------------------------------------

//
// Each cell in the life board is represented by an integer which encodes
// whether the cell is alive or dead and whether or not it has been modified
// since the last redraw.  Could eventually encode the color in the same
// integer to fix the bug that when the display is redisplayed (e.g., because
// the window is uncovered) the cells are redrawn with different colors.
//

define constant $alive?-mask    :: <integer> = #b01;

define constant $modified?-mask :: <integer> = #b10;

define inline-only function alive?
    (i :: <integer>) => (b :: <boolean>)
  logand(i, $alive?-mask) ~== 0
end;

define inline-only function modified?
    (i :: <integer>) => (b :: <boolean>)
  logand(i, $modified?-mask) ~== 0
end;

define inline-only function set-modified
    (i :: <integer>, b :: <boolean>) => (i :: <integer>)
  if (b)
    logior(i, $modified?-mask)
  else
    logand(i, lognot($modified?-mask))
  end
end;

define inline-only function set-alive
    (i :: <integer>, b :: <boolean>) => (i :: <integer>)
  if (b)
    logior(i, $alive?-mask)
  else
    logand(i, lognot($alive?-mask))
  end
end;

//
// Emulate a 2d array with a vector.  In Functional Developer 1.1 this
// can use limited(<array>, of: <integer>), but we'll leave it as is
// for now.
//

define constant <intvector> = limited(<vector>, of: <integer>);

define class <board> (<object>)
  slot board-contents :: <intvector>,
    init-keyword: contents:;
  constant slot board-rows :: <integer>,
    required-init-keyword: rows:;
  constant slot board-columns :: <integer>,
    required-init-keyword: columns:;
end class;

define method initialize (board :: <board>, #key)
  next-method();
  if (~slot-initialized?(board, board-contents))
    board.board-contents := make(<intvector>,
                                 size: board.board-rows * board.board-columns,
                                 fill: set-modified(set-alive(0, #f), #f));
  end if;
end method;

define method get-cell
    (board :: <board>, row :: <integer>, col :: <integer>) => (int :: <integer>)
  let index :: <integer> = row * board-columns(board) + col;
  board-contents(board)[index]
end;

define method set-cell
    (board :: <board>, new-value :: <integer>, row :: <integer>, col :: <integer>) => ()
  let index :: <integer> = row * board-columns(board) + col;
  board-contents(board)[index] := new-value;
end;

define constant $board-lock :: <simple-lock> = make(<lock>);

define inline-only function canonicalize-board-element
    (elem) => (elem :: <integer>)
  if (member?(elem, #[#f, '0', ' ', 0]))
    set-modified(set-alive(0, #f), #t)
  else
    set-modified(set-alive(0, #t), #t)
  end if
end function;

define macro with-board-size
  { with-board-size (?board:expression, ?rows:name, ?cols:name)
     ?:body
    end
  } => { let board = ?board;
         let ?rows :: <integer> = board-rows(board);
         let ?cols :: <integer> = board-columns(board);
         ?body
       }
end macro;
