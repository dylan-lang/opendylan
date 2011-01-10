Module:    reversi
Author:    Jonathon Lee
Synopsis:  Reversi game externsions 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// this file contains the first incremental change to the reversi example. 
// It adds the functionality that allows the user to change the shape of the 
// players pieces.

*changed?* := #t;

define method draw-piece 
    (sheet :: <reversi-square>, medium :: <medium>, shape :: <symbol>)
 => ()
  when (square-piece(sheet))
    let (left, top, right, bottom) = box-edges(sheet);
    let half-width  = floor/(right - left, 2);
    let half-height = floor/(bottom - top, 2);
    let x-radius = floor/(right - left, 3);
    let y-radius = floor/(bottom - top, 3);
    let color = select (square-piece(sheet))
		  #"black" => $black;
		  #"white" => $white;
		end;
    with-drawing-options (medium, brush: color)
      select (shape)
	#"square" =>
	  draw-rectangle(medium, 
			 half-width - x-radius, 
			 half-height - y-radius, 
			 half-width + x-radius, 
			 half-height + y-radius, 
			 filled?: #t);
	#"triangle" =>
	  draw-polygon(medium, 
		       list(half-width - x-radius,
			    half-height + y-radius,
			    half-width + x-radius,
			    half-height + y-radius,
			    half-width, 
			    half-height - y-radius),
		       filled: #t);
	otherwise => debug-message("Illegal Shape");
      end select;
    end;
  end;
end method draw-piece;

define method reversi-game-force-reset
    (game :: <reversi-game>) => ()
  let board = reversi-game-board(game);
  let squares = reversi-board-squares(board);
  for (square from 0 below size(squares))
    let temp = squares[square];
    if (temp)
      squares[square] := #f;
      note-game-updated(game);
      squares[square] := temp;
    end if;
  end for;
  note-game-updated(game);
end method reversi-game-force-reset;


define method set-shape
    (frame :: <reversi-frame>, new-shape) => ()
  let game = reversi-frame-game(frame);
  *reversi-piece-shape* := new-shape;
  reversi-game-force-reset(game);
end method set-shape;
