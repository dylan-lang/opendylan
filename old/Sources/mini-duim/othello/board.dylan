Module:    othello
Author:    Andy Armstrong
Synopsis:  Othello game
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Othello board pane

define class <othello-board-pane> (<drawing-pane>)
  slot %old-squares = make(<vector>, size: 64);
  slot othello-board-game,
    required-init-keyword: game:;
end class <othello-board-pane>;

define constant $othello-square-gap        = 2;
define constant $othello-square-size       = 30;
define constant $othello-square-half-size  = floor/($othello-square-size, 2);
define constant $othello-square-third-size = floor/($othello-square-size, 3);

//--- Make the first one a variable to allow changing it in the demo.
define variable *piece-background-brush* = make(<brush>, foreground: $red);
define constant $black-piece-brush       = make(<brush>, foreground: $black);
define constant $white-piece-brush       = make(<brush>, foreground: $white);

define method handle-repaint 
    (sheet :: <othello-board-pane>, medium :: <medium>, region :: <region>)
 => ()
  ignore(region);	//--- We should optimize by using the region...
  let game = othello-board-game(sheet);
  let board = othello-game-board(game);
  let squares = othello-board-squares(board);
  let old-squares = sheet.%old-squares;
  //---*** Hack to ensure that the medium doesn't have the wrong brush
  //---*** cached when the frame is iconized and then redisplayed.
  with-drawing-options (medium, brush: $white-piece-brush)
    draw-line*(medium, 0, 0, 0, 0)
  end;
  for (j from 0 below 8)
    for (i from 0 below 8)
      let square = i + j * 8;
      let piece = squares[square];
      let x = i * ($othello-square-size + $othello-square-gap);
      let y = j * ($othello-square-size + $othello-square-gap);
      old-squares[square] := piece;
      with-drawing-options (medium, brush: *piece-background-brush*)
	draw-rectangle*(medium, 
			x, y,
			x + $othello-square-size, y + $othello-square-size);
      end;
      if (piece)
	let brush 
	  = select (piece)
	      #"black" => $black-piece-brush;
	      #"white" => $white-piece-brush;
	    end;
	with-drawing-options (medium, brush: brush)
	  draw-ellipse*(medium,
			x + $othello-square-half-size,
			y + $othello-square-half-size,
			$othello-square-third-size, 0, 
			0, $othello-square-third-size)
        end
      end
    end
  end
end method handle-repaint;

define method handle-event
    (sheet :: <othello-board-pane>, event :: <button-release-event>) => ()
  let frame = sheet-frame(sheet);
  if (frame)
    if (event-button(event) = $left-button)
      let x = event.event-x;
      let y = event.event-y;
      let i = floor/(x, $othello-square-size + $othello-square-gap);
      let j = floor/(y, $othello-square-size + $othello-square-gap);
      let square = i + j * 8;
      othello-frame-press-square(frame, square);
      // force-output(port(square))
    end
  end
end method handle-event;

define method handle-event
    (sheet :: <othello-board-pane>, event :: <double-click-event>) => ()
  ignore(event);
  if (event.event-button == $right-button)
    enter-debugger("Enter debugger on double-click in sheet %=", sheet)
  end
end method handle-event;

define method update-othello-board 
    (sheet :: <othello-board-pane>) => ()
  let medium = sheet-medium(sheet);
  let game = othello-board-game(sheet);
  let board = othello-game-board(game);
  let squares = othello-board-squares(board);
  let old-squares = sheet.%old-squares;
  for (piece     in squares,
       old-piece in old-squares,
       square from 0)
    unless (old-piece == piece)
      let j = floor/(square, 8);
      let i = square - (j * 8);
      let x = i * ($othello-square-size + $othello-square-gap);
      let y = j * ($othello-square-size + $othello-square-gap);
      old-squares[square] := piece;
      let brush 
	= select (piece)
	    #"black" => $black-piece-brush;
	    #"white" => $white-piece-brush;
	    #f       => *piece-background-brush*;
	  end;
      with-drawing-options (medium, brush: brush)
	draw-ellipse*(medium,
		      x + $othello-square-half-size,
		      y + $othello-square-half-size,
		      $othello-square-third-size, 0, 
		      0, $othello-square-third-size)
      end
    end
  end;
  // force-output(port(board))
end method update-othello-board;


//----------------------------------------------------------------------------
// Othello board
//
// This really is just a presentation of the abstract game defined by
// the class <othello-game>.
//----------------------------------------------------------------------------

//--- We need to pick up something pretty as the frame background
//--- (in CLOS, we'd use :default-init-keywords (:background ...))
define frame <othello-frame> (<simple-frame>)
  slot %game :: false-or(<othello-game>) = #f;
  pane othello-frame-board-pane (frame)
    begin
      let board-size = $othello-square-gap * 7 + $othello-square-size * 8;
      make(<othello-board-pane>,
	   game: frame.othello-frame-game,
	   x: 0, y: 22,
	   width: board-size, height: board-size)
    end;
  pane status-pane (frame)
    make(<label>, label: "",
	 x: 0, y: 0, width: 200, height: 20);
  pane main-layout (frame)
    begin
      let status-pane = frame.status-pane;
      let board-pane = frame.othello-frame-board-pane;
      let region = sheet-region(board-pane);
      let (width, height)
	= transform-position(sheet-transform(board-pane),
			     box-right(board-pane), box-bottom(board-pane));
      make(<simple-pane>,
	   children: vector(status-pane, board-pane),
	   width:  width,
	   height: height)
    end;
  pane new-button (frame)
    make(<menu-button>,
         label: "&New Game",
         selection-mode: #"none",
         activate-callback: othello-frame-new-game);
  pane print-button (frame)
    make(<menu-button>,
         label: "Print",
	 enabled?: #f,
         selection-mode: #"none",
         activate-callback: method (sheet)
			      beep(sheet)
			    end);
  pane exit-button (frame)
    make(<menu-button>,
         label: "Exit",
         selection-mode: #"none",
         activate-callback: method (sheet)
			      frame-exit(sheet-frame(sheet))
			    end);
  pane print-menu-box (frame)
    make(<menu-box>,
	 children: vector(frame.print-button));
  pane game-menu (frame)
    make(<menu>,
	 label: "File",
	 children: vector(frame.new-button,
			  frame.print-menu-box,
			  frame.exit-button));
  menu-bar (frame)
    make(<menu-bar>,
	 children: vector(frame.game-menu));
  layout (frame) frame.main-layout;
end class <othello-frame>;

define method initialize (frame :: <othello-frame>, #key) => ()
  next-method();
  othello-frame-new-game(frame)
end method initialize;

define method othello-frame-game 
    (frame :: <othello-frame>)
 => (game :: <othello-game>)
  frame.%game
  | (frame.%game := othello-frame-make-game(frame))
end method othello-frame-game;

define method othello-frame-game-setter
    (game :: <othello-game>, frame :: <othello-frame>)
 => (game :: <othello-game>)
  frame.%game := game
end method othello-frame-game-setter;

define method repaint-board
    (frame :: <othello-frame>) => ()
  let pane = othello-frame-board-pane(frame);
  repaint-sheet(pane, sheet-region(pane))
end method repaint-board;

define method othello-frame-press-square 
    (frame :: <othello-frame>, square :: <integer>) => ()
  let game = othello-frame-game(frame);
  unless (othello-game-play-square(game, square))
    beep(othello-frame-board-pane(frame))
  end;
end method othello-frame-press-square;

define method othello-frame-display-message
    (frame :: <othello-frame>, format-string :: <string>, #rest message-args)
 => ()
  let sheet = status-pane(frame);
  let message = apply(format-to-string, format-string, message-args);
  gadget-label(sheet) := message
end method othello-frame-display-message;

define method othello-frame-make-game 
    (frame :: <othello-frame>, #key board-size = 8)
 => (game :: <othello-game>)
  make(<othello-game>,
       update-callback: method (game)
			  ignore(game);
                          let pane = othello-frame-board-pane(frame);
			  if (pane & sheet-mirror(pane))
			    update-othello-board(pane)
			  end
                        end,
       message-function: method (message, #rest message-args)
                           apply(othello-frame-display-message, 
                                 frame, message, message-args)
                         end,
       board-size: board-size)
end method othello-frame-make-game;

define method play-othello (#key activate? = #t) => (frame :: <othello-frame>)
  let frame
    = make(<othello-frame>,
	   title: "Functional Developer Othello",
	   mapped?: #t);
  start-frame(frame, activate?: activate?);
  frame
end method play-othello;


/// Callbacks

define method othello-frame-new-game (frame :: <othello-frame>) => ()
  let game = othello-frame-game(frame);
  new-game(game)
end method othello-frame-new-game;

define method othello-frame-new-game (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  othello-frame-new-game(frame)
end method othello-frame-new-game;


/// A container for othello

// Call 'attach-othello' with an <HWND> and the four integers specifying
// the edges of the region, and Othello will attach itself there.
define method attach-othello 
    (container, left, top, right, bottom)
 => (frame :: <frame>)
  let frame
    = make(<othello-frame>,
	   title: "Othello",
	   container: container,
	   container-region: make-bounding-box(left, top, right, bottom));
  start-frame(frame, activate?: #f);
  frame
end method attach-othello;

define method contained-othello () => (frame :: <frame>)
  let container
    = make(<frame>,
	   title: "Othello container",
	   width: 500, height: 500);
  start-frame(container, activate?: #f);
  let frame = attach-othello(container, 100, 100, 400, 400);
  values(container, frame)
end method contained-othello;
