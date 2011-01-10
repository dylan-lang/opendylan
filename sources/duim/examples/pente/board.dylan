Module:       pente
Author:       James Kirsch
Synopsis:     Pente game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Constants

define variable *pente-square-size* :: <integer> = 40;
define variable *rows* :: <integer> = 9;
define variable *columns* :: <integer> = 9;

define constant $directions 
  = #[#[-1, -1], #[0, -1], #[1, -1],
      #[-1, 0],            #[1, 0],
      #[-1, 1],  #[0, 1],  #[1, 1]];

define constant $capture-masks = 
  begin
    let masks = make(<array>, dimensions: #(2));
    masks[0] := 1221;
    masks[1] := 2112;
    masks;
  end;

define constant $five-in-a-row-masks =
  begin
    let masks = make(<array>, dimensions: #(2));
    masks[0] := 11111;
    masks[1] := 22222;
    masks;
  end;

// Current player = 1 or 2
define constant <turn> = limited(<integer>, min: 1, max: 2);

define class <pente-game> (<object>)
  slot pente-board :: false-or(<simple-object-vector>) = #f,
    init-keyword: pente-squares:;
  slot turn :: <turn> = 1,
    init-keyword: turn:;
  constant slot captures :: <array> = make(<array>, dimensions: #(2)),
    init-keyword: captures:;
  slot game-over? :: <boolean> = #f;
end class <pente-game>;

define method initialize (game :: <pente-game>, #key)
  next-method();
  game.captures[0] := 0;
  game.captures[1] := 0;
end method initialize;

define constant <square-occupied> = false-or(<turn>);

// Code related to drawing and mainting a single pente square

define class <pente-square-pane> (<simple-pane>)
  constant slot square-coordinates :: <vector>,
    init-keyword: coordinates:;  
  slot square-occupied :: <square-occupied> = #f;
  constant slot square-color :: <color> = $white,
    init-keyword: color:;
end class <pente-square-pane>;

define inline method square-value (square :: <pente-square-pane>)
 => (piece-value :: <integer>)
  square-occupied(square) | 0;
end method square-value;

// Sets a pente square to either blank, player 1, or player 2;

define method pente-square-setter 
    (value :: <square-occupied>, square :: <pente-square-pane>)
 => ()
  square.square-occupied := value;
  if (sheet-mapped?(square))
    repaint-sheet(square, $everywhere);
  end;
end method pente-square-setter;

// Paints a pente piece (on top of the already painted square)

define method draw-pente-piece
    (pente-square :: <pente-square-pane>, medium :: <medium>)
 => ()
  when (square-occupied(pente-square))
    let (left, top, right, bottom) = box-edges(pente-square);
    let half-width = floor/(right - left, 2);
    let half-height = floor/(bottom - top, 2);
    let x-radius = floor/(right - left, 3);
    let y-radius = floor/(bottom - top, 3);
    let color = select (square-occupied(pente-square))
		  1 => $red;
		  2 => $blue;
		  otherwise => $background;
		end;
    with-drawing-options(medium, brush: color)
      draw-ellipse(medium, left + half-width, top + half-height,
		   x-radius, 0, 0, y-radius, filled?: #t)
      //draw-circle(medium, left + half-width, top + half-height,
	//	  radius, filled: #t)
    end;
  end;
end method draw-pente-piece;

// Paints a single pente-square

define method handle-repaint
    (square :: <pente-square-pane>, medium :: <medium>, region :: <region>)
 => ()
  debug-message("repainting");
  let (left, top, right, bottom) = box-edges(square);
  with-drawing-options(medium, brush: default-background(square))
    draw-rectangle(medium, left, top, right, bottom, filled?: #t);
  end; 
  with-drawing-options(medium, brush: square.square-color)
    let middlex = floor/(right - left, 2);
    let middley = floor/(bottom - top, 2);
    draw-line(medium, middlex, top, middley, bottom);
    draw-line(medium, left, middley, right, middley);
  end;
  if (square.square-occupied)
    draw-pente-piece(square, medium);
  end if;
end method handle-repaint;


// If someone clicks on the pente board. . .

define method handle-event
    (pente-square :: <pente-square-pane>, event :: <button-release-event>)
 => ()
  let frame = sheet-frame(pente-square);
  if (frame)
    if (frame.game.game-over?)
      beep(pente-square);
    elseif (event-button(event) == $left-button
	      & ~(pente-square.square-occupied))
      add-pente-piece-to-board(pente-square, frame);
    end if;
  end if;
end method handle-event;

  
// Pente game class - keeps track of board, turn, number of captures

// Pente frame

define frame <pente-frame> (<simple-frame>)
  constant slot game :: false-or(<pente-game>) = make(<pente-game>),
    init-keyword: game:;

  pane pente-frame-table-pane (frame)
      make(<grid-layout>,
	   children: pente-frame-squares(frame),
	   cell-space-requirement: make(<space-requirement>,
					width: *pente-square-size*,
					height: *pente-square-size*),
	   rows: *rows*,
	   columns: *columns*,
	   spacing: 0);

  pane buttons (frame)
      make(<push-button>,
	   label: "New game",
	   activate-callback: new-game);

  pane status (frame)
    make(<status-bar>);

  pane main-layout (frame)
    vertically (spacing: 10)
      horizontally (spacing: 2, x-alignment: #"center")
        frame.buttons;
      end;
      pente-frame-table-pane(frame);
    end;

  pane exit-button (frame)
    make(<push-menu-button>,
	 label: "E&xit",
	 activate-callback: method (button)
			      exit-frame(sheet-frame(button));
			    end);
  pane game-menu (frame)
    make(<menu>,
	 label: "&Game",
	 children: vector(make(<menu-button>,
			       label: "New Game",
			       activate-callback: new-game),
			  make(<push-menu-box>,
			       children: vector(frame.exit-button))));
  menu-bar (frame)
    make(<menu-bar>,
	 children: vector(frame.game-menu));

  layout (frame)
    frame.main-layout;

  status-bar (frame)
    frame.status;

end frame <pente-frame>;

define method format-to-status-bar
    (frame :: <pente-frame>, format-string :: <string>, #rest message-args)
 => ()
  let status-bar = frame-status-bar(frame);
  let message = apply(format-to-string, format-string, message-args);
  gadget-label(status-bar) := message;
end method format-to-status-bar;

define inline method clear-status-bar (frame :: <pente-frame>)
 => ()
  format-to-status-bar(frame, "");
end method clear-status-bar;

define method pente-frame-squares (frame :: <pente-frame>)
 => (squares :: <simple-object-vector>)
  frame.game.pente-board
  | begin
      frame.game.pente-board := make-pente-board();
    end;
end method pente-frame-squares;

define method make-pente-board () => (squares :: <simple-object-vector>)
  let squares = make(<simple-object-vector>,
		     size: *rows* * *columns*);
  let half-row = truncate/(*rows*, 2);
  let half-column = truncate/(*columns*, 2);
  let index :: <integer> = 0;
  for (i from 0 below *rows*)
    for (j from 0 below *columns*)
      squares[index] := make(<pente-square-pane>, 
			     coordinates: vector(i, j),
			     color: if (i == half-row & j == half-column)
				      $red;
				    else
				      $white;
				    end if);
      index := index + 1;
    end for;
  end for;
  debug-message("board created");
  squares
end method make-pente-board;

// Adds a players pente piece to the board, then checks for captures or wins

define method add-pente-piece-to-board 
    (square :: <pente-square-pane>, frame :: <pente-frame>)
 => ()
  pente-square-setter(frame.game.turn, square);
  if (check-five-in-a-row?(square, frame)
      | check-capture?(square, frame))
    frame.game.game-over? := #t;
    format-to-status-bar(frame, "Game over!");
  else
    frame.game.turn := modulo(frame.game.turn, 2) + 1;
  end if;
end method add-pente-piece-to-board;

define method add-collections (vector1 :: <vector>, vector2 :: <vector>)
 => (result :: <vector>)
  map(\+, vector1, vector2);
end method add-collections;
						   
define method check-capture? 
    (pente-square :: <pente-square-pane>, pente-frame :: <pente-frame>)
 => (win? :: <boolean>)
  let turn = pente-frame.game.turn - 1;
  for (dir in $directions)
    if ($capture-masks[turn] == board-mask(dir, 4, pente-square, pente-frame))
      let next-square = find-next-square(pente-frame, pente-square, dir);
      pente-square-setter(#f, next-square);
      next-square := find-next-square(pente-frame, next-square, dir);
      pente-square-setter(#f, next-square);
      pente-frame.game.captures[turn] := pente-frame.game.captures[turn] + 1;
    end if; 
  end for;
  if (pente-frame.game.captures[turn] == 5)
    #t // win the game
  else
    #f
  end if;
end method check-capture?;

define method check-five-in-a-row?
    (pente-square :: <pente-square-pane>, pente-frame :: <pente-frame>)
 => (win? :: <boolean>)
  let turn = pente-frame.game.turn - 1;
  block (return)
    for (direction in $directions)
      let current-square = pente-square;
      let opposite-direction = opposite-direction(direction);
      for (unused from 1 to 3)	// repeat 3 times
	if (current-square)
	  if ($five-in-a-row-masks[turn] == 
		board-mask(direction, 5, current-square, pente-frame))
	    //debug-message("five in a row found");
	    return(#t);
	  end if;
	  current-square := 
	    find-next-square(pente-frame, current-square, opposite-direction);
	end if;
      end for;
    end for;
    #f;
  end block;
end method check-five-in-a-row?;

define method opposite-direction (direction :: <vector>)
 => (opposite-direction :: <vector>)
  vector(-1 * (first(direction)), -1 * (second(direction)))
end method opposite-direction;

define method in-bounds? (vector :: <vector>)
 => (in-bounds? :: <boolean>)
  if (first(vector) >= 0 & first(vector) < *rows* 
      & second(vector) >= 0 & second(vector) < *columns*)
    #t
  else
    #f
  end if;
end method in-bounds?;

define method find-next-square
    (frame :: <pente-frame>, square :: <pente-square-pane>, 
     next-coordinates :: <vector>)
 => (square :: false-or(<pente-square-pane>))
  let pos = add-collections(next-coordinates, square.square-coordinates);
  if (in-bounds?(pos))
    let row = pos[0];
    let col = pos[1];
    let index = row * *columns* + col;
    frame.game.pente-board[index];
  end if;
end method find-next-square;
  
// Return an integer that encodes the set of pieces within DISTANCE
// of SQUARE, in direction DIRECTION.
define method board-mask
    (direction :: <vector>, distance :: <integer>,
     square :: <pente-square-pane>, pente-frame :: <pente-frame>)
 => (mask :: <integer>)
  let pos = add-collections(direction, square.square-coordinates);
  if (in-bounds?(pos))
    if (distance == 1)
      square-value(square);
    else
      let row = pos[0];
      let col = pos[1];
      let index = row * *columns* + col;
      square-value(square) +
	10 * board-mask(direction, distance - 1,
			pente-frame.game.pente-board[index],
			pente-frame);
    end if;
  else 
    3     // never equals a winning mask value.
  end if;  
end method board-mask;

define method new-game (button)
  let frame = sheet-frame(button);
  frame.game.game-over? := #f;
  frame.game.turn := 1;
  frame.game.captures[0] := 0;
  frame.game.captures[1] := 0;
  for (i from 0 below *rows* * *columns*)
    pente-square-setter(#f, frame.game.pente-board[i]);
  end for;
  clear-status-bar(frame);
end method new-game;  

define method play-pente ()
  start-frame(make(<pente-frame>, title: "Pente"));
end method play-pente;

