Module: life
Author: Carl Gay
Synopsis: Support for predefined patterns for the game of Life.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *patterns* = #();

define class <life-pattern> (<object>)
  slot name :: <symbol>,
    required-init-keyword: name:;
  slot pretty-name :: <byte-string> = "",
    init-keyword: pretty-name:;
  slot documentation :: <byte-string> = "",
    init-keyword: documentation:;
  slot init-function :: <function>,
    required-init-keyword: init-function:;
end class <life-pattern>;

define method initialize (p :: <life-pattern>, #key)
  next-method();
  if (p.pretty-name = "")
    p.pretty-name := as-lowercase(as(<byte-string>, p.name));
    element(p.pretty-name, 0) := as-uppercase(element(p.pretty-name, 0));
  end if;
  // Add this pattern to the global list of patterns.
  // +++ Eventually should rebuild the pattern menu when a new pattern
  //     is defined.
  *patterns* := remove(*patterns*, p,
		       test: method (p1 :: <life-pattern>, p2 :: <life-pattern>)
                               p1.name == p2.name
                             end);
  *patterns* := sort(add(*patterns*, p),
		     test: method (p1 :: <life-pattern>, p2 :: <life-pattern>)
			     p1.pretty-name < p2.pretty-name
			   end method);
end method;

define function initialize-board-from-sequence (board, pat :: <sequence>,
						#key row, col)
 => ()
  // pat is a sequence of rows (which are also sequences).
  if (size(pat) > 0 & instance?(element(pat, 0), <sequence>))
    with-board-size(board, rows, cols)
      if (row & row < 0) row := rows + row end;  // #rows from bottom
      if (col & col < 0) col := cols + col end;  // #cols from top
      let start-row :: <integer> = row | floor/(rows, 2) - floor/(size(pat), 2);
      let start-col :: <integer> = col | floor/(cols, 2) - floor/(size(element(pat, 0)), 2);
      for (brow :: <integer> from start-row,
	   prow in pat)
	for (bcol :: <integer> from start-col,
	     val in prow)
	  if (brow >= 0 & brow < rows
		& bcol >= 0 & bcol < cols)
	    board[brow, bcol] := canonicalize-board-element(val);
	  end if;
	end for;
      end for;
    end;
  end if;
end function initialize-board-from-sequence;

define macro life-pattern-definer
  { define life-pattern ?pattern-name:name ?options end }
    => { make(<life-pattern>, name: ?#"pattern-name", ?options ); }
  options:
    { } => { }
    { ?option; ... } => { ?option, ... }
  option:
    { rows: ?val:expression }
      => { init-function: method (board)
                            initialize-board-from-sequence(board, ?val)
			  end
	    }
    // Pass along any other key/val pairs to make(<life-pattern> ...)
    { ?x:* } => { ?x }
end macro;


/// ----------------------------------------------
/// Pattern definitions
/// ----------------------------------------------

// Random dots on the screen.
define life-pattern random
  pretty-name: "Random";
  documentation: "Fill the display with a random pattern";
  init-function: method (board)
		   with-board-size(board, rows, cols)
		     for (row :: <integer> from 0 below rows)
		       for (col :: <integer> from 0 below cols)
			 board[row, col] := if (random(100) > 60) 1 else 0 end;
		       end for;
		     end for;
		   end;
		 end method;
end life-pattern;

// I walk the line.
define life-pattern walker
  documentation: "A pattern that moves steadily in one direction";
  rows: #(".. ", ". .", ".  ");
end life-pattern;

// Walkers that do violence to one another
define life-pattern walker-texas-ranger
  pretty-name: "Walker, Texas Ranger";
  documentation: "Walkers that do violence to one another.";
  init-function: method (board)
		   let ul = #(" . ", "  .", "...");
		   let ur = #(" . ", ".  ", "...");
		   let ll = #("...", "  .", " . ");
		   let lr = #("...", ".  ", " . ");
		   initialize-board-from-sequence(board, ul, row: 0, col: 0);
		   initialize-board-from-sequence(board, ur, row: 5, col: -6);
		   initialize-board-from-sequence(board, ll, row: -7, col: 4);
		   initialize-board-from-sequence(board, lr, row: -3, col: -3);
		 end;
end life-pattern;

// Something that looks a bit like a frog hopping between two rocks.
define life-pattern frogger
  documentation: "A frog hopping between two rocks.";
  init-function: method (board)
		   let square = #("..", "..");
		   let center = #("..",
				  ". .",
				  " ...",
				  "  ...",
				  " ...",
				  ". .",
				  "..");
		   initialize-board-from-sequence(board, square, col: 2);
		   initialize-board-from-sequence(board, center, col: 11);
		   initialize-board-from-sequence(board, square, col: 22);
		 end method;
end life-pattern;


// A repeating pattern starts out as two abutting squares.
define life-pattern squaresville
  rows: #("...", "...", "...", "   ...", "   ...", "   ...");
end life-pattern;


// Like it sez.
define life-pattern undulator
  rows: #(" .. ..",
	  " .. ..",
	  "  . .",
	  ". . . .",
	  ". . . .",
	  "..   ..");
end life-pattern;


define life-pattern pulsar
  rows: #("   ..",
	  "  .  .",
	  " .    .",
	  ".      .",
	  ".      .",
	  " .    .",
	  "  .  .",
	  "   ..");
end life-pattern;


define life-pattern trapped
  rows: #("      .",
	  "     . .",
	  "    . . .",
	  "    .   .",
	  "  ..  .  ..",
	  " .    .    .",
	  ". . .. .. . .",
	  " .    .    .",
	  "  ..  .  ..",
	  "    .   .",
	  "    . . .",
	  "     . .",
	  "      .");
  pretty-name: "Trapped";
end life-pattern;


