Module:       tic-tac-toe
Author:       Jonathon Lee
Synopsis:     The classic game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $application-name    = "Functional Developer Tic-Tac-Toe";
define constant $application-version = "version 1.0";

define constant $ttt-square-color    = $red;
define constant $ttt-edge-color      = $black;
define constant $tic-tac-toe-pen     = make(<pen>, width: 3, cap-shape: #"round");

define constant $ttt-square-size     = 40;
define constant $ttt-square-rows     = 3;
define constant $ttt-square-columns  = 3;
define constant $max-number-of-moves = $ttt-square-rows * $ttt-square-columns;

define constant <square-occupied> = false-or(limited(<integer>, min: 1, max: 2));

define class <ttt-square> (<simple-pane>)
  slot square-coordinates :: <vector>, init-keyword: square-coordinates:;  
  slot square-occupied :: <square-occupied> = #f;
end class <ttt-square>;

define method ttt-square-setter 
    (square :: <ttt-square>, player :: <square-occupied>) => ()
  square.square-occupied := player;
  if (sheet-mapped?(square))
    repaint-sheet(square, $everywhere);
  end;
end method ttt-square-setter;
 
define method draw-ttt-piece 
    (ttt-square :: <ttt-square>, medium :: <medium>) => ()
  when (square-occupied(ttt-square))
    let (left, top, right, bottom) = box-edges(ttt-square);
    let half-width  = floor/(right - left, 2);
    let half-height = floor/(bottom - top, 2);
    let x-radius = floor/(right - left, 3);
    let y-radius = floor/(bottom - top, 3);
    let x-center = left + half-width;
    let y-center = top  + half-height;
    let color = select (square-occupied(ttt-square))
		  1 => $white;
		  2 => $black;
		end;
    with-drawing-options (medium, brush: color, pen: $tic-tac-toe-pen)
      if (square-occupied(ttt-square) = 2)
	draw-ellipse(medium, x-center, y-center, 
		     x-radius, 0, 0, y-radius, filled?: #f)
      else
	draw-line(medium, 
                  x-center - x-radius, y-center - y-radius,
                  x-center + x-radius, y-center + y-radius);
	draw-line(medium, 
                  x-center - x-radius, y-center + y-radius,
                  x-center + x-radius, y-center - y-radius);
      end if;
      force-display(medium)
    end;
  end;
end method draw-ttt-piece;

define method handle-repaint
    (ttt-square :: <ttt-square>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(ttt-square);
  with-drawing-options (medium, brush: $ttt-square-color)
    draw-rectangle(medium, left, top, right, bottom, filled?: #t);
  end;
  with-drawing-options (medium, brush: $ttt-edge-color)
    draw-rectangle(medium, left, top, right, bottom, filled?: #f);
  end;
  if (ttt-square.square-occupied)
    draw-ttt-piece(ttt-square, medium);
  end if;
end method handle-repaint;

define method display-message
    (frame :: <frame>, message :: <string>, #key beep? = #f)
 => ()
  frame-status-message(frame) := message;
  when (beep?) beep(frame) end
end method display-message;

define method handle-event
    (ttt-square :: <ttt-square>, event :: <button-release-event>) => ()
  let frame = sheet-frame(ttt-square);
  if (frame)
    if (event-button(event) == $left-button &
	  ~(ttt-square.square-occupied))
      select (frame.game.turn)
        3                 => display-message(frame, "Game is over");
        *computer-player* => display-message(frame, "Its not your turn", beep?: #t);
        otherwise         => add-ttt-piece(ttt-square, frame);
      end select;
    end if;
  end if;
end method handle-event;

define frame <ttt-frame> (<simple-frame>)
  constant slot game :: <ttt-game> = make(<ttt-game>),
    init-keyword: game:;
  pane ttt-frame-table-pane (frame)
    make(<grid-layout>,
         contents: ttt-frame-squares(frame),
         cell-space-requirement: make(<space-requirement>,
                                      width: $ttt-square-size,
                                      height: $ttt-square-size),
         spacing: 0);
  pane buttons (frame) 
    make(<push-button>,
	 label: "New game",
	 activate-callback: method (button)
			      new-game(sheet-frame(button))
			    end);
  pane status (frame)
    make(<status-bar>);
  pane main-layout (frame)
    vertically (spacing: 10)
      horizontally (spacing: 2, x-alignment: #"center")
        frame.buttons;
      end;
      make(<drawing-pane>,
	   children: vector(ttt-frame-table-pane(frame)),
	   foreground: $ttt-square-color); 
    end;
  pane exit-button (frame)
    make(<push-menu-button>,
	 label: "E&xit",
	 activate-callback: method (button)
			      exit-frame(sheet-frame(button));
			    end);
  pane file-menu (frame)
    make(<menu>,
	 label: "&File",
	 children: 
	   vector(make(<menu-button>,
		       label: "New Game",
		       activate-callback: method (button)
					    new-game(sheet-frame(button))
					  end),
		  make(<push-menu-box>,     
		       children: vector(frame.exit-button))));
  pane options-menu (frame)
    make(<menu>,
	 label: "&Options",
	 children: 
	   vector(make(<radio-menu-box>,
		       items: #(#("Play &X",      #"x"),
			        #("Play &Y",      #"o"),
                                #("&Two Players", #"two-players")),
		       label-key: first,
		       value-key: second,
		       value-changed-callback:
			 method (b)
			   ttt-set-players(sheet-frame(b), gadget-value(b))
			 end)));
  pane help-menu (frame)
    make(<menu>,
	 label: "Help",
	 children: 
	   vector(make(<push-menu-button>,
		       label: format-to-string("About %s", $application-name),
		       activate-callback: 
			 method (button)
			   about-ttt(sheet-frame(button))
			 end)));
  menu-bar (frame)
    make(<menu-bar>,
	 children: vector(frame.file-menu, 		       
			  frame.options-menu,
			  frame.help-menu));
  layout (frame) frame.main-layout;
  status-bar (frame) frame.status;
  keyword title: = $application-name
end frame <ttt-frame>;
			      
// Return something useful for the contents: init arg of <grid-pane>.
define method ttt-frame-squares
    (frame :: <ttt-frame>) => ()
  let array = frame.game.ttt-board
                | begin
                    frame.game.ttt-board := make-ttt-board();
                  end;
  map-as(<vector>,
         method (row)
           map-as(<vector>,
                  method (col) array[row, col] end,
                  range(from: 0, below: dimension(array, 1)))
         end,
         range(from: 0, below: dimension(array, 0)))
end method ttt-frame-squares;

define method add-ttt-piece 
    (ttt-square :: <ttt-square>, frame :: <ttt-frame>) => ()
  let game = frame.game;
  if (game.turn = *human-player1* | game.turn = *human-player2*)
    ttt-square-setter(ttt-square, game.turn);
    game.moves := game.moves + 1;
    let over = over?(game, ttt-square);
    if (over)
      display-winner(frame, over, game.turn);
      //game.turn := 3;
    else
      game.turn := modulo(game.turn, 2) + 1;
      if (game.turn = *computer-player*) 
	computers-turn(frame);
      end if;
    end if;
  end if;
end method add-ttt-piece;

define method frame-play-computers-move
    (frame :: <ttt-frame>) => (row :: <integer>, col :: <integer>)
  let game = frame.game;
  game-play-computers-move(game);
end method frame-play-computers-move;

define method computers-turn
    (frame :: <ttt-frame>)
  let (i, j) = frame-play-computers-move(frame);
  let game = frame.game;
  let over = game-computers-turn(game, i, j);
  
  if (over)
    display-winner(frame, over, *computer-player*);
    // game.turn := 3;
  else 
    game.turn := modulo(game.turn, 2) + 1;
  end if;
end method computers-turn;  

define method new-game (frame :: <ttt-frame>)
  display-message(frame, "");
  let game = frame.game;
  game.turn := 1;
  game.moves := 0;
  for (i from 0 below $ttt-square-rows)
    for (j from 0 below $ttt-square-columns)
      ttt-square-setter(aref(game.ttt-board, i, j), #f);
    end for;
  end for;
  if (*computer-player* = game.turn)
    computers-turn(frame);
  end if;
end method new-game;  


define method choose-players 
    (frame :: <ttt-frame>, type :: one-of(#"x", #"o", #"two-players"))
 => ()
  select (type)
    #"x" =>
      *human-player1* := 1;
      *human-player2* := 1;
      *computer-player* := 2;
    #"o" =>
      *human-player1* := 2;
      *human-player2* := 2;
      *computer-player* := 1;
    #"two-players" =>
      *human-player1* := 1;
      *human-player2* := 2;
      *computer-player* := 5;
  end;
  new-game(frame)
end method choose-players;

define method ttt-set-players
    (frame :: <ttt-frame>, new-players) => ()
  choose-players(frame, new-players)
end method ttt-set-players;


define method display-winner 
    (frame :: <ttt-frame>, 
     outcome :: one-of(#"win", #"draw"), 
     winner :: <integer>) 
 => ()
  select(outcome)
    #"win" =>
      select (winner)
        1 => display-message(frame, "X wins!");
	2 => display-message(frame, "O wins!");
      end;
    #"draw" =>
      display-message(frame, "A draw.");
  end;
end method display-winner;

define method about-ttt (frame :: <ttt-frame>) => ()
  notify-user(format-to-string("%s %s", $application-name, $application-version),
              owner: frame)
end method about-ttt;

define method play-tic-tac-toe () => ()
  start-frame(make(<ttt-frame>));
end method play-tic-tac-toe;


