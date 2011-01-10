Module:    reversi
Author:    Andy Armstrong
Synopsis:  Reversi game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//----------------------------------------------------------------------------
// Reversi square
//
// This is a sheet that draws itself as a square with an optional
// coloured piece upon it.
//----------------------------------------------------------------------------

define variable *reversi-square-color* = $red;
define variable *reversi-square-size* = 30;
define variable *reversi-piece-shape* = #"circle";

define class <reversi-square> (<simple-pane>)
  slot square-number = #f, init-keyword: number:;
  slot %piece = #f, init-keyword: piece:;
end class <reversi-square>;

define method square-piece 
    (sheet :: <reversi-square>) => (piece :: <piece>)
  sheet.%piece
end method square-piece;

define method square-piece-setter 
    (piece :: <piece>, sheet :: <reversi-square>)
 => (piece :: <piece>)
  sheet.%piece := piece;
  if (sheet-mapped?(sheet))
    repaint-sheet(sheet, $everywhere)
  end;
  piece;
end method square-piece-setter;

define method do-compose-space 
    (sheet :: <reversi-square>, #key width, height)
 => (space-requirement :: <space-requirement>)
  make(<space-requirement>,
       width: *reversi-square-size*, 
       height: *reversi-square-size*);
end method do-compose-space;


define method handle-repaint
    (sheet :: <reversi-square>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(sheet);
  let frame = sheet-frame(sheet);
  with-drawing-options (medium, brush: default-background(sheet))
    draw-rectangle(medium, left, top, right, bottom, filled?: #t)
  end; 
  draw-piece(sheet, medium, *reversi-piece-shape*)
end method handle-repaint;

define method handle-event
    (square :: <reversi-square>, event :: <button-release-event>) => ()
  let frame = sheet-frame(square);
  if (frame)
    if (event-button(event) = $left-button)
      reversi-frame-press-square(frame, square-number(square));
      force-display(port(square))
    end;
  end;
end method handle-event;

define method draw-piece 
    (sheet :: <reversi-square>, medium :: <medium>, shape == #"circle")
 => ()
  when (square-piece(sheet))
    let (left, top, right, bottom) = box-edges(sheet);
    let half-width  = floor/(right - left, 2);
    let half-height = floor/(bottom - top, 2);
    let x-radius = floor/(right - left, 3);
    let y-radius = floor/(bottom - top, 3);
    let color
      = select (square-piece(sheet))
	  #"black" => $black;
	  #"white" => $white;
	end;
    with-drawing-options (medium, brush: color)
      draw-ellipse(medium, left + half-width, top + half-height, 
		   x-radius, 0, 0, y-radius, filled?: #t);
    end;
  end;
end method draw-piece;

//----------------------------------------------------------------------------
// Reversi frame
//
// This really is just a presentation of the abstract game defined by
// the class <reversi-game>.
//----------------------------------------------------------------------------

//--- We need to pick up something pretty as the frame background
//--- (in CLOS, we'd use :default-init-keywords (:background ...))
define frame <reversi-frame> (<simple-frame>)
  slot reversi-square-color = *reversi-square-color*,
    init-keyword: square-color:;
  slot %game :: false-or(<reversi-game>) = #f;
  slot %squares :: false-or(<sequence>) = #f;
  slot %saved-file-name :: false-or(<pathname>) = #f;
  pane buttons (frame)
    make(<push-button>,
	 label: "New game",
	 activate-callback: reversi-frame-new-game);
  pane size-box (frame)
    begin
      let game = reversi-frame-game(frame);
      make(<spin-box>,
	   items: #(6, 8, 10, 12, 14, 16, 18, 20, 22, 24),
	   value: reversi-game-size(game),
	   value-changed-callback: method (b)
				     reversi-frame-change-size(sheet-frame(b),
							       gadget-value(b))
				   end,
	   label-key: method (item) 
			format-to-string("%d", item)
		      end)
    end;
  pane reversi-frame-table-pane (frame)
    begin
      let game = reversi-frame-game(frame);
      make(<grid-layout>, 
	   children: reversi-frame-squares(frame),
	   cell-space-requirement: make(<space-requirement>,
					width: *reversi-square-size*, 
					height: *reversi-square-size*),
	   rows: reversi-game-size(game),
	   spacing: 2)
    end;
  pane status (frame)
    make(<status-bar>);
  pane main-layout (frame)
    vertically (spacing: 2)
      horizontally (spacing: 2, y-alignment: #"center")
        frame.buttons;
        /** Removed for the demo since spin boxes aren't implemented natively!
            make(<label>, label: "Size:");
            frame.size-box;
         */
      end;
      make(<drawing-pane>,
	   children: vector(reversi-frame-table-pane(frame)),
	   foreground: *reversi-square-color*);
    end;
  pane exit-button (frame)
    make(<push-menu-button>,
	 label: "E&xit",
	 activate-callback: method (button)
			      exit-application(0);
			    end);
  pane file-menu (frame)
    make(<menu>,
	 label: "&File",
	 children: vector(make(<menu-button>,
			       label: "New Game",
			       activate-callback: reversi-frame-new-game),
			  make(<menu-button>,
			       label: "Open...",
			       activate-callback:
				 method (button)
				   let file = choose-file(direction: #"input",
							  owner: frame);
				   if (file)
				     reversi-frame-load-game(frame,
							     file);
				   end;
				 end, 
			       enabled?: *changed2?*),
			  make(<menu-button>,
			       label: "Save",
			       activate-callback: 
				 method (button)
				   let file = frame.%saved-file-name 
				     | begin 
					 frame.%saved-file-name := 
					   choose-file(direction: #"output", 
						       owner: frame);
				       end;
				   if (file)
				     reversi-frame-save-game(frame, file);
				   end;
				 end,
			       enabled?: *changed2?*),
			  make(<menu-button>,
			       label: "Save As...",
			       activate-callback: 
				 method (button)
				   let file = 
				     choose-file(direction: #"output", 
						    owner: frame);
				   if (file)
				     frame.%saved-file-name := file;
				     reversi-frame-save-game(frame,
							     file);
				   end;
				 end,
			       enabled?: *changed2?*),
			  make(<push-menu-box>,
			       children: vector(frame.exit-button))));
  pane options-menu (frame)
    make(<menu>,
	 label: "&Options",
	 children: vector(make(<radio-menu-box>,
			       items: #(#("&Black vs Computer", #"black"),
					#("&White vs Computer", #"white"),
					#("&Computer vs Computer", 
					  #"computers"),
					#("&Two Players", #"two-players")),
			       label-key: first,
			       value-key: second,
			       value-changed-callback:
				 method (b)
				   reversi-frame-set-players(sheet-frame(b), 
							     gadget-value(b))
				 end), 
			  
			  make(<radio-menu-box>, 
			       items: #(#("&Circles", #"circle"),
					#("&Squares", #"square"),
					#("&Triangles", #"triangle")),
			       label-key: first,
			       value-key: second,
			       value-changed-callback:
				 method (b)
				   reversi-frame-set-shape(sheet-frame(b), 
							   gadget-value(b))
				 end, 
			       enabled?: *changed?*))); 
/*  make(<menu-button>,
	 label: "Black Algorithm...",
	 activate-callback:
	   rcurry(reversi-frame-choose-algorithm, 
		  #"black")),
	 make(<menu-button>,
	      label: "White Algorithm...",
	      activate-callback:
		rcurry(reversi-frame-choose-algorithm, 
		       #"white")),
	 make(<menu-button>,
	      label: "Board Size...",
	      activate-callback: 
		method (b)
		  reversi-frame-change-size(sheet-frame(b))
		end) */

  pane help-menu (frame)
    make(<menu>,
	 label: "Help",
	 children: vector(make(<push-menu-button>,
			       label: "About Functional Developer Reversi",
			       activate-callback: 
				 method (button)
				   about-reversi(sheet-frame(button))
				 end)));
  menu-bar (frame)
    make(<menu-bar>,
	 children: vector(frame.file-menu, 
			  frame.options-menu, 
			  frame.help-menu));
  layout (frame) frame.main-layout;
  status-bar (frame) frame.status;
end frame <reversi-frame>;

define method reversi-frame-game 
    (frame :: <reversi-frame>)
 => (game :: <reversi-game>)
  frame.%game
  | begin
      let game = reversi-frame-make-game(frame);
      frame.%game := game;
      reversi-frame-new-game(frame);
      game
    end
end method reversi-frame-game;

define method reversi-frame-game-setter
    (game :: <reversi-game>, frame :: <reversi-frame>)
 => (game :: <reversi-game>)
  frame.%game := game
end method reversi-frame-game-setter;

define method reversi-frame-squares
    (frame :: <reversi-frame>)
 => (squares :: <sequence>)
  frame.%squares
  | begin
      frame.%squares := make-reversi-squares(frame)
    end
end method reversi-frame-squares;

define method reversi-frame-squares-setter
    (squares :: <sequence>, frame :: <reversi-frame>)
 => (squares :: <sequence>)
  frame.%squares := squares
end method reversi-frame-squares-setter;

define method make-reversi-squares 
    (frame :: <reversi-frame>) => (squares :: <sequence>)
  let game = reversi-frame-game(frame);
  let board = reversi-game-board(game);
  let board-size = reversi-board-size(board);
  let square-color = reversi-square-color(frame);
  let no-of-squares = reversi-board-no-of-squares(board);
  let squares = make(<vector>, size: no-of-squares);
  for (i from 0 below no-of-squares)
    squares[i]
      := make(<reversi-square>,
	      number: i,
	      background: square-color)
  end;

  squares
end method make-reversi-squares;
                       
define method reversi-frame-press-square
    (frame :: <reversi-frame>, square :: <integer>) => ()
  let game = reversi-frame-game(frame);
  unless (reversi-game-play-square(game, square))
    with-sheet-medium (medium = frame-layout(frame))
      beep(medium)
    end
  end;
end method reversi-frame-press-square;

define method reversi-frame-display-message 
    (frame :: <reversi-frame>, format-string :: <string>, #rest message-args)
 => ()
  let status-bar = frame-status-bar(frame);
  let message = apply(format-to-string, format-string, message-args);
  //--- Should there be an easier way to output into the status bar?
  //--- Maybe it's own stream?
  gadget-label(status-bar) := message
end method reversi-frame-display-message;

define method reversi-frame-make-game
    (frame :: <reversi-frame>, #key board-size = $default-board-size)
 => (game :: <reversi-game>)
  make(<reversi-game>,
       update-callback: method (game)
                          update-reversi-frame(frame)
                        end,
       message-function: method (message, #rest message-args)
                           apply(reversi-frame-display-message, 
                                 frame, message, message-args)
                         end,
       board-size: board-size)
end method reversi-frame-make-game;

define method reversi-frame-change-size
    (frame :: <reversi-frame>, board-size) => ()
  notify-user("Not yet implemented", frame: frame);
  /*
  reversi-frame-size(frame) := board-size;
  let table-pane = reversi-frame-table-pane(frame);
  pane-contents(table-pane)
    := make(<array>, dimensions: vector(board-size, board-size));
  sheet-children(table-pane) := make-reversi-squares(frame);
  //--- Reallocate space for the table pane
  //--- How should we be handle this, really?
  let (width, height) = box-size(reversi-frame-table-pane(frame));
  set-sheet-size(reversi-frame-table-pane(frame), width, height);
  new-game(reversi-frame-game(frame));
  frame
  */
end method reversi-frame-change-size;
  
define method update-reversi-frame (frame :: <reversi-frame>) => ()
  let game = reversi-frame-game(frame);
  let board = reversi-game-board(game);
  for (piece in reversi-board-squares(board),
       square in reversi-frame-squares(frame))
    unless (square-piece(square) = piece)
      square-piece(square) := piece
    end;
  end;
  force-display(port(frame))
end method update-reversi-frame;

define method play-reversi 
    (#key port = default-port(), board-size = $default-board-size)
  let frame
    = make(<reversi-frame>,
	   size: board-size,
	   title: "Functional Developer Reversi");
  start-frame(frame);
  frame
end method play-reversi;


/// Callbacks

define method reversi-frame-save-game
    (frame :: <reversi-frame>, file :: <pathname>) => ()
  let game = reversi-frame-game(frame);
  reversi-game-save-game(game, file)
end method reversi-frame-save-game;

define method reversi-frame-save-game 
    (sheet :: <sheet>, file :: <pathname>) => ()
  let frame = sheet-frame(sheet);
  reversi-frame-save-game(frame, file)
end method reversi-frame-save-game;

define method reversi-frame-load-game
    (frame :: <reversi-frame>, file :: <pathname>) => ()
  let game = reversi-frame-game(frame);
  reversi-game-load-game(game, file);
end method reversi-frame-load-game;

define method reversi-frame-load-game 
    (sheet :: <sheet>, file :: <pathname>) => ()
  let frame = sheet-frame(sheet);
  reversi-frame-load-game(frame, file);
end method reversi-frame-load-game;

define method reversi-frame-new-game 
    (frame :: <reversi-frame>) => ()
  let game = reversi-frame-game(frame);
  new-game(game)
end method reversi-frame-new-game;

define method reversi-frame-new-game 
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  reversi-frame-new-game(frame)
end method reversi-frame-new-game;

define method reversi-frame-set-players
    (frame :: <reversi-frame>, new-players) => ()
  let game = reversi-frame-game(frame);
  choose-players(game, new-players)
end method reversi-frame-set-players;

define method reversi-frame-set-shape
    (frame :: <reversi-frame>, new-shape) => ()
  if (*changed?*)
    set-shape(frame, new-shape);
  end if;
end method reversi-frame-set-shape;

define method reversi-frame-choose-algorithm 
    (sheet :: <sheet>, player :: <player>) => ()
  let frame = sheet-frame(sheet);
  let game = reversi-frame-game(frame);
  let algorithm
    = choose-from-dialog(all-algorithms(),
                         frame: frame,
                         label-key: algorithm-title,
                         value-key: identity,
                         default-item: algorithm-for-player(game, player));
  if (algorithm)
    algorithm-for-player(game, player) := algorithm
  end
end method reversi-frame-choose-algorithm;

define method about-reversi (frame :: <reversi-frame>) => ()
  notify-user("Functional Developer Reversi version 1.0", owner: frame)
end method about-reversi;
