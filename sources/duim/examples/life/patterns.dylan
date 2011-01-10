Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// Some predefined patterns.
//

// Holds all known patterns.  Each pattern must have a unique name.
define variable *patterns* = #();

define class <life-pattern> (<object>)
  constant slot name :: <symbol>,
    required-init-keyword: name:;
  slot pretty-name :: <byte-string> = "",
    init-keyword: pretty-name:;
  constant slot documentation :: <byte-string> = "",
    init-keyword: documentation:;
  // Each pattern's init-function is responsible for returning the number
  // of live cells it created, in order to maintain the live-cell-count
  // display in the UI.  This should really be done by querying the life
  // "world" for how many live cells it has after initialization, but we
  // don't yet have first class life "worlds".
  constant slot init-function :: <function>,
    required-init-keyword: init-function:;
end class <life-pattern>;

// Add patterns to *patterns* when they're created.
define method initialize (p :: <life-pattern>, #key)
  next-method();
  if (p.pretty-name = "")
    p.pretty-name := as-lowercase(as(<byte-string>, p.name));
    element(p.pretty-name, 0) := as-uppercase(element(p.pretty-name, 0));
  end if;
  // Remove the pattern if it was already there.
  *patterns* := remove(*patterns*, p,
		       test: method (p1 :: <life-pattern>, p2 :: <life-pattern>)
                               p1.name == p2.name
                             end);
  // Add this pattern to the global list of patterns.
  *patterns* := sort(add(*patterns*, p),
		     test: method (p1 :: <life-pattern>, p2 :: <life-pattern>)
			     p1.pretty-name < p2.pretty-name
			   end method);
end method;

// Stores the given pattern (pat) in board at the given row and column (col).
define function initialize-board-from-sequence (board, pat :: <sequence>,
						#key row, col)
 => (live-cell-count :: <integer>)
  // pat is a sequence of rows (which are also sequences).
  if (size(pat) > 0 & instance?(element(pat, 0), <sequence>))
    with-board-size(board, rows, cols)
      if (row & row < 0) row := rows + row end;  // #rows from bottom
      if (col & col < 0) col := cols + col end;  // #cols from top
      let start-row :: <integer> = row | floor/(rows, 2) - floor/(size(pat), 2);
      let start-col :: <integer>
        = col | floor/(cols, 2) - floor/(size(element(pat, 0)), 2);
      let live-cells :: <integer> = 0;
      for (brow :: <integer> from start-row,
	   prow in pat)
	for (bcol :: <integer> from start-col,
	     val in prow)
	  if (brow >= 0 & brow < rows
              & bcol >= 0 & bcol < cols)
            let val2 = canonicalize-board-element(val);
            set-cell(board, val2, brow, bcol);
            if (alive?(val2))
              live-cells := live-cells + 1;
            end if;
	  end if;
	end for;
      end for;
      live-cells
    end
  else
    0
  end if
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
                     let live-cells :: <integer> = 0;
		     for (row :: <integer> from 0 below rows)
		       for (col :: <integer> from 0 below cols)
                         let alive = if (random(100) > 60) #f else #t end;
                         set-cell(board, canonicalize-board-element(alive), row, col);
                         if (alive)
                           live-cells := live-cells + 1;
                         end if;
		       end for;
		     end for;
                     live-cells
		   end
		 end method;
end life-pattern;

// I walk the line.
define life-pattern walker
  documentation: "A pattern that moves steadily in one direction";
  rows: #(".. ",
	  ". .",
	  ".  ");
end life-pattern;

define life-pattern blinker-ship
  pretty-name: "Blinker Ship";
  documentation: "Leaves a trail of blinkers which are eaten by a pattern"
                 " with not quite a large enough appetite.";
  rows: #("           ....",
          "          .   .",
          "      .       .",
          " .......  .  . ",
          ".  ... ..      ",
          " .......  .  . ",
          "      .       .",
          "          .   .",
          "           ....");
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
		   initialize-board-from-sequence(board, ul, row: 0, col: 0)
		     + initialize-board-from-sequence(board, ur, row: 5, col: -6)
                     + initialize-board-from-sequence(board, ll, row: -7, col: 4)
                     + initialize-board-from-sequence(board, lr, row: -3, col: -3)
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
		   initialize-board-from-sequence(board, square, col: 2)
		     + initialize-board-from-sequence(board, center, col: 11)
		     + initialize-board-from-sequence(board, square, col: 22)
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

