Module: life
Author: Carl Gay
Synopsis: The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// ----------------------------------------------
/// Utilities
/// ----------------------------------------------

define constant $alive :: <integer> =  1;
define constant $dead :: <integer> = 0;

define inline-only function canonicalize-board-element (elem)
 => (elem :: <integer>)
  if (member?(elem, #[#f, '0', ' ', 0]))
    $dead
  else
    $alive
  end if;
end;

// Don't want to have to remember which dimension is which throughout
// the rest of the code.  Maybe I need a <board> class?

define macro board-rows
  { board-rows(?board:expression) } => { dimension(?board, 0) }
end macro;

define macro board-columns
  { board-columns(?board:expression) } => { dimension(?board, 1) }
end macro;

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


				       
