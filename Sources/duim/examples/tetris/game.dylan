Module:       tetris
Synopsis:     DUIM implementation of the game Tetris
Author:       Richard Tucker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file defines the game of tetris in the abstract, without methods
// for displaying the state of the board, or processing user input.

// Everything that happens in a game of tetris is the result of an action,
// which can be:
//
// left, right, turn, drop -- player actions
// down                    -- make the pieces fall one square
// tick                    -- a unit of time has passed
// none                    -- dummy action

define constant <action> =
 one-of(#"left", #"right", #"down", #"drop", #"turn", #"tick", #"none");

// The <coord> class is a convenient way of dealing with
// integer coordinate pairs, with addition and rotation

define class <coord> (<object>)
  slot x-coord :: <integer>, required-init-keyword: x-coord:;
  slot y-coord :: <integer>, required-init-keyword: y-coord:;
end;

define constant <coord-vector> = limited(<vector>, of: <coord>);

define method xy (x :: <integer>, y :: <integer>) => (c :: <coord>);
  make(<coord>, x-coord: x, y-coord: y);
end;

define method \+ (a :: <coord>, b :: <coord>) => (c :: <coord>);
  xy(a.x-coord + b.x-coord, a.y-coord + b.y-coord);
end;

define method \- (a :: <coord>, b :: <coord>) => (c :: <coord>);
  xy(a.x-coord - b.x-coord, a.y-coord - b.y-coord);
end;

// In a call to the rotate method, r :: <coord> should be the coordinate
// that (1, 0) would rotate to.

define method rotate (c :: <coord>, r :: <coord>) => (c :: <coord>);
  xy(c.x-coord * r.x-coord - c.y-coord * r.y-coord,
     c.x-coord * r.y-coord + c.y-coord * r.x-coord);
end;
 
// An object of class <board> represents a game of tetris.
// width           of the board in squares,
// height          of the board in squares,
// wall            the current colour of each square,
// fixed           whether each square is part of the fixed wall,
// back-colour     the background colour (black by default),
// full-row-colour the colour to use in highlighting full rows,
// pieces          the set of currently falling pieces,
// full-rows       the set of (y-coordinates of) rows that are currently
//                 full, and hence awaiting removal,
// score           the current score,
// time            the number of tick events received so far,
// next-time       the time (in ticks) at which the next piece
//                 should appear,
// piece-frequency the current time (in ticks) between pieces,
// fall-frequency  the time (in ticks) it takes for each piece to
//                 fall by one square,
// speed           the time in seconds between tick events,
// finished?       true when the game is over
//
// (0,0) is the top left corner of the board

define class <board> (<object>)
  constant slot width :: <integer>, required-init-keyword: width:;
  constant slot height :: <integer>, required-init-keyword: height:;
  slot wall :: <array>;
  slot fixed :: <array>;
  constant slot back-colour = make-rgb-color(0, 0, 0);
  constant slot full-row-colour = make-rgb-color(1, 1, 1);
  slot pieces :: <deque>;
  slot full-rows :: <vector>;
  slot score :: <integer>;
  slot time :: <integer>;
  slot next-time :: <integer>;
  slot piece-frequency :: <integer>;
  slot fall-frequency :: <integer>;
  slot speed :: <real>;
  slot finished? :: <boolean>;
end;

define method initialize (board :: <board>, #key)
  next-method();
  restart-game(board);
end;

// Start a new game
// This means resetting the score, times and speeds, and
// clearing the board

define method restart-game (board :: <board>)
  board.score := 0;
  board.time := 0;
  board.next-time := 0;
  board.piece-frequency := 30;
  board.fall-frequency := 2;
  board.speed := .3;
  board.finished? := #f;
  board.pieces := make(<deque>);
  board.wall := make(<array>,
    dimensions: vector(board.width, board.height), fill: board.back-colour);
  board.fixed := make(<array>,
    dimensions: vector(board.width, board.height), fill: #f);
  update-pieces(board);
  board.full-rows := make(<vector>, size: board.height, fill: #f); 
end;

// Make the game harder as the score increases, by gradually
// increasing the frequency with which pieces appear. When
// it has reached a maximum, we reset it and speed the whole
// game up slightly

define method increment-score(board :: <board>)
  board.score := board.score + 1;
  board.piece-frequency := board.piece-frequency - 1;
  if (board.piece-frequency < 8)
    board.piece-frequency := 30;
    board.speed := board.speed * 0.6;
  end;
end;

// A square is vacant if (a) it is within the board and
// (b) it is not part of the wall. We test (a) first to
// avoid invalid array accesses.

define method vacant? (board :: <board>, c :: <coord>) => (r :: <boolean>);
  let (x, y) = values(c.x-coord, c.y-coord);
  if (x < 0 | y < 0 | x >= board.width | y >= board.height)
    #f;
  else
    ~board.fixed[x,y];
  end;
end;

// Check to see if all the squares in the specified row
// are part of the wall. If so, highlight the row and add
// it to the set of full rows.

define method check-row-full(board :: <board>, row :: <integer>)
  block (exit)
    if (board.full-rows[row])
      exit();
    end;
    for (i :: <integer> from 0 below board.width)
      if (~board.fixed[i, row])
	exit();
      end;
    end;
    board.full-rows[row] := #t;
    for (i :: <integer> from 0 below board.width)
      board.wall[i, row] := board.full-row-colour;
    end;
    increment-score(board);
  end;
end;

// Remove full rows from the board, copying the rows
// above them.

define method remove-full-rows(board :: <board>)

  // first check to see if there are any full rows

  if (any?(identity, board.full-rows))

    // if so "undraw" all the pieces from the board

    do(curry(unwop, board), board.pieces);

    // loop over the rows, from the bottom up.
    // to-row is a row in the board, and from-row is the row we
    // must copy over it. Initially, they are the same, and of
    // course we won't actually do any copying when from-row = to-row.
    // Each time round the loop, they are both moved up one row.

    let row = board.height - 1;
    for (to-row from row to 0 by -1, from-row from row by -1)

      // If from-row is full, decrement it until it isn't.
      // n.b. if row 0 (the top row) is full,
      // this may result in from-row becoming -1 or less.

      while (from-row >= 0 & board.full-rows[from-row])
	board.full-rows[from-row] := #f;
	from-row := from-row - 1
      end;

      // If from-row is < 0, we don't copy, just set to-row to empty.

      if (from-row < 0)
	for (i from 0 below board.width)
	  board.wall[i, to-row] := board.back-colour;
	  board.fixed[i, to-row] := #f
	end

	  // otherwise copy from-row onto to-row, if they are different

      elseif (from-row ~= to-row)
	for (i from 0 below board.width)
	  board.wall[i, to-row] := board.wall[i, from-row];
	  board.fixed[i, to-row] := board.fixed[i, from-row];
	end
      end
    end;

    // all done, so "draw" the pieces again

    do(curry(wop, board), board.pieces);
  end;
end;

// An instance of <piece> represent a falling piece. It has the
// following slots:
// colour       colour (for cosmetic purposes only)
// shape        as a vector of coordinates pairs
// centre       centre of gravity times 2
// location     current position and rotation on the board
//
// The centre of gravity is used when determining how a piece
// should rotate. It is stored times 2 to allow a piece to rotate
// correctly about the corner, edge, or centre of a square.
//
// All the standard tetris pieces have only four squares, and are
// connected, but the definitions here do not rely on these
// properties.

define class <piece> (<object>)
  constant slot colour :: <color>,
    required-init-keyword: colour:;
  constant slot shape :: <coord-vector>,
    required-init-keyword: shape:;
  slot centre :: <coord>;
  slot location :: <position>;
end;

define method initialize (me :: <piece>, #key)
  next-method();
  let n :: <integer> = me.shape.dimensions[0];

  // compute the centre of gravity times 2 by adding up all the
  // coordinates, multiplying by 2, then dividing by the number of
  // squares in the piece and rounding
 
  let c :: <coord> = reduce(\+, xy(0, 0), me.shape);
  me.centre := xy(round/(2 * c.x-coord, n), round/(2 * c.y-coord, n));
  me.location := make(<position>, offset: xy(5,2), rotation: xy(1, 0));
end;

// An instance of <position> represent the position of a piece
// on the board, as an (x,y) offset and a rotation vector. The
// offset gives the coordinates of nearest square to the
// centre-of-gravity of the piece.

define class <position> (<object>)
  slot offset :: <coord>, init-keyword: offset:;
  slot rotation :: <coord>, init-keyword: rotation:;
end;

// Duplicate a position

define method dup (p :: <position>) => (q :: <position>);
  let q = make(<position>);
  q.offset := xy(p.offset.x-coord, p.offset.y-coord);
  q.rotation := xy(p.rotation.x-coord, p.rotation.y-coord);
  q;
end;

// Calculate the set of board squares that a piece would occupy
// were it in a given position.
// The procedure for this is:
// (1) rotate the centre of gravity of the piece and calculate the
// neccessary offset to make it appear at the required board square.
// (2) rotate the coordinates of each square of the piece, and
// add the offset calculated in (1).
//
// In (1) we have to be careful: the centre-of-gravity is stored
// multiplied by two, and so must be rotated, then divided by 2,
// then rounded. The choice of rounding function is critical: we
// must ensure that the offset is rounded consistently up or down.
// Otherwise, pieces with order 2 rotational symmetry will
// wiggle back and forth when we rotate them.

define method cells (piece :: <piece>, pos :: <position>)
 => (r :: <coord-vector>);
  let centre = rotate(piece.centre, pos.rotation);
  let offset = pos.offset -
    xy(floor/(centre.x-coord, 2), floor/(centre.y-coord, 2));
  map-as(<coord-vector>,
	 method (c :: <coord>)
	   rotate(c, pos.rotation) + offset;
	 end,
	 piece.shape);
end;

// Would a piece fit on the board in a given position?

define method fits? (board :: <board>, piece :: <piece>, pos :: <position>)
  every?(method (c) vacant?(board, c) end, cells(piece, pos));
end;

// "Draw" a piece. This doesn't really draw it on screen, just
// sets the relevant board squares to the right colour.

define method wop (board :: <board>, piece :: <piece>)
  for (p in cells(piece, piece.location))
    board.wall[p.x-coord, p.y-coord] := piece.colour;
  end;
end;

// "Undraw" a piece. This doesn't really remove it from the
// screen, just sets the relevant board squares to the
// background colour.

define method unwop (board :: <board>, piece :: <piece>)
  for (p in cells(piece, piece.location))
    board.wall[p.x-coord, p.y-coord] := board.back-colour;
  end;
end;

// Cause a piece to stick in place, and become part of the
// fixed wall. In doing the, we may cause rows to
// become full, so we also notice the range of rows occupied
// by the piece, and check whether each is now full.
// We also have to remove it from the set of falling pieces.

define method stick (board :: <board>, piece :: <piece>)
  let miny = 0;
  let maxy = board.height - 1;
  for (p in cells(piece, piece.location))
    board.fixed[p.x-coord, p.y-coord] := #t;
    if (p.y-coord < miny)
      miny := p.y-coord;
    elseif (p.y-coord > maxy)
      maxy := p.y-coord;
    end;
  end;
  remove!(board.pieces, piece);
  for (row from miny to maxy)
    check-row-full(board, row);
  end;
end;

// Move a piece left, right, or down, or turn it, if possible.
// Returns true if the attempted move was successful.

define method move-if-possible 
    (board :: <board>, piece :: <piece>, action :: <action>)
 => (r :: <boolean>);

  // Take a copy of the piece's location
  
  let pos = dup(piece.location);

  // Update the copy of the location according to the action
  
  select (action)
    #"left"  => pos.offset.x-coord := pos.offset.x-coord - 1;
    #"right" => pos.offset.x-coord := pos.offset.x-coord + 1;
    #"down"  => pos.offset.y-coord := pos.offset.y-coord + 1;
    #"turn"  => pos.rotation := rotate(pos.rotation, xy(0, -1));
 end;

  // If it would fit, update the piece's location and draw it.

  if (fits?(board, piece, pos))
    unwop(board, piece);
    piece.location := pos;
    wop(board, piece);
    #t;
  end;
end;

// Process an action.

define method act (board :: <board>, action :: <action>)

  // Don't do anything if the game is over.

  if (~board.finished?)
    select (action)

      // Drop applies only to the first (i.e. lowest) falling piece
      // It should move down until it can't any more, and then stick.

      #"drop" =>
	let p = first(board.pieces);
	while (move-if-possible(board, p, #"down"))
	end;
	stick(board, p);

      // Tick causes full rows to be removed, and every
      // board.fall-frequency ticks, all the falling pieces
      // move down one square. A piece that can't move down gets
      // stuck.

      // n.b. We first try to move all the pieces down, _then_ stick
      // the ones that didn't. If instead we called stick(board, p)
      // within the first loop, we'd be in trouble, becuase stick
      // would modify the set (board.pieces) we are iterating over.

      #"tick" =>
	remove-full-rows(board);
	if (modulo(board.time, board.fall-frequency) = 0)
	  let stuck = make(<deque>);
	  for (p in board.pieces)
	    if (~move-if-possible(board, p, #"down"))
	      push(stuck, p);
	    end;
	  end;
	  for (p in stuck)
	    stick(board, p);
	  end;
	end;
	board.time := board.time + 1;
	
	// Other actions (left, right, turn) cause all pieces to
	// move, if they can.

	otherwise
	  for (p in board.pieces)
	    move-if-possible(board, p, action);
	  end;
    end;
    update-pieces(board);
  end;
end;

// Create a new falling piece, if it is time for one, or if
// there are no pieces currently falling. This method also
// calculates and sets the time at which the next piece should
// appear.

define method update-pieces(board :: <board>)
  if (empty?(board.pieces) | board.time >= board.next-time)
    let piece = new-piece();
    wop(board, piece);
    push-last(board.pieces, piece);
    board.next-time := board.time + board.piece-frequency;
    if (~ fits?(board, piece, piece.location))
      board.finished? := #t;
    end;
  end;
end;

// Create a new piece, by choosing one of the standard types at random.

define method new-piece()
  let piece-type = element($standard-pieces, random($standard-pieces.size));
  make(piece-type);
end;

define class <red-piece> (<piece>)
  keyword colour: = make-rgb-color(1, 0, 0);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,0), xy(1,0), xy(2,0), xy(3,0)));
end;

define class <blue-piece> (<piece>)
  keyword colour: = make-rgb-color(0, 0, 1);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,0), xy(1,0), xy(0,1), xy(1,1)));
end;

define class <orange-piece> (<piece>)
  keyword colour: = make-rgb-color(1, .7, 0);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,0), xy(1,0), xy(2,0), xy(1,1)));
end;

define class <cyan-piece> (<piece>)
  keyword colour: = make-rgb-color(0, .8, 1);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,0), xy(1,0), xy(1,1), xy(2,1)));
end;

define class <green-piece> (<piece>)
  keyword colour: = make-rgb-color(0, 1, .2);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,1), xy(1,1), xy(1,0), xy(2,0)));
end;

define class <grey-piece> (<piece>)
  keyword colour: = make-rgb-color(.7, .7, .7);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,0), xy(1,0), xy(2,0), xy(0,1)));
end;

define class <purple-piece> (<piece>)
  keyword colour: = make-rgb-color(1, .2, 1);
  keyword shape: = as(<coord-vector>,
		      vector(xy(0,0), xy(1,0), xy(2,0), xy(2,1)));
end;

define constant $standard-pieces 
  = vector(<red-piece>, <blue-piece>, <orange-piece>,
	   <purple-piece>, <green-piece>,
	   <grey-piece>, <cyan-piece>);

