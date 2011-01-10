Module:       tetris
Synopsis:     DUIM implementation of the game Tetris
Author:       Richard Tucker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file defines the user interface for the game of tetris.

// A game-pane is a rectangular pane displaying the game board.
// cell-size    the size of each board square
// game         the tetris game associated with this pane

define class <game-pane> (<drawing-pane>)
  constant slot cell-size :: <integer>,
    required-init-keyword: cell-size:;
  constant slot game :: <board>,
    required-init-keyword: game:;
  keyword resizable?: = #f;
end;

// The width and height of the game-pane can only be calculated
// once we know both the cell-size and the dimensions of the
// game board.

define method make-game-pane(#key cell-size :: <integer>, game :: <board>)
  make(<game-pane>,
       cell-size: cell-size,
       game: game,
       width: game.width * cell-size,
       height: game.height * cell-size)
end;

// Make a button with a specified label that, when pressed, sends a
// specified action to the game-pane.

define method make-action-button 
    (frame :: <game-frame>, action :: <action>,
     label :: type-union(<string>, <image>))
 => (button :: <push-button>)
  make(<push-button>, label: label, accepts-focus?: #f,
       activate-callback:
	 method (button)
	   act(frame.game-pane, action)
	 end);
end;

// The frame for a game of tetris contains:
// - File and Help menu
// - Action buttons for the player actions
// - A <game-pane> to display the board
// - A status bar to display the score
// and it has the following slots:
// game            the associated game of tetris
// cell-size       (as for <game-pane>)
// ready-for-tick  is the frame ready to receive the next
//                 tick action?
//
// ready-for-tick is set to false after each tick action
// is sent (see file tetris.dylan), and set to true after
// each action is processed (see the act method below).
// This prevents a tick action being sent before the
// previous one has been dealt with.

define frame <game-frame> (<simple-frame>)
  constant slot game :: <board>, required-init-keyword: game:;
  constant slot cell-size :: <integer>,
    init-keyword: cell-size:,
    init-value: 16;
  slot ready-for-tick :: <boolean>, init-value: #t;
  pane exit-button (frame)
    make(<push-menu-button>, label: "E&xit",
	 accelerator:
	   make(<keyboard-gesture>, keysym: #"f4", modifier-state: $alt-key),
	 activate-callback:
	   method (button)
	     exit-frame(sheet-frame(button))
	   end);
  pane controls (frame)
    horizontally (spacing: 2)
      make-action-button(frame, #"left", "L");
      make-action-button(frame, #"turn", "T");
      make-action-button(frame, #"right", "R");
      make-action-button(frame, #"drop", "Drop");
    end;
  pane game-pane (frame)
    make-game-pane(cell-size: frame.cell-size, game: frame.game);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.controls;
      frame.game-pane;
    end;
  pane status (frame)
    make(<status-bar>);
  menu-bar (frame)
    make(<menu-bar>, 
	 children:
	   vector(make(<menu>, label: "&File", 
		       children: 
			 vector(make(<menu-button>, label: "New Game",
				     activate-callback: (method (b) new-game(frame); end),
				     accelerator: make(<keyboard-gesture>,
						       keysym: #"n", modifier-state: $control-key)),
				make(<menu-box>,
				     children: vector(frame.exit-button))
				  )),
     make(<menu>, label: "&Help", 
	  children: 
	    vector(make(<menu-button>, label: "About Tetris",
			activate-callback: (method (b) about-display(frame); end),
			accelerator: make(<keyboard-gesture>,
					  keysym: #"f1", modifier-state: 0))
		     ))
		    ));
  layout (frame) frame.main-layout;
  status-bar (frame) frame.status;
  keyword title: = "Tetris";
  keyword resizable?: = #f;
end;

// We want user key-presses to control the pieces, so the game-pane
// gets the input focus.

define method initialize (me :: <game-frame>, #key)
  next-method();
  frame-input-focus(me) := me.game-pane;
end;

// Set the status bar message to report the score

define method update-score (me :: <game-frame>)
  me.status.gadget-label := format-to-string("Score: %d", me.game.score);
end;


// Redraw the game pane.
// If this routine were clever, it might work out which parts of the
// board intersected the <region> paramter, and only redraw them. Instead,
// it always redraws every square in the board.

define method handle-repaint (me :: <game-pane>, med :: <medium>, reg :: <region>)
 => ()
  let cs :: <integer> = me.cell-size;
  let cx :: <integer> = 0;
  let cy :: <integer> = 0;
  for (y :: <integer> from 0 below me.game.height)
    cx := 0;
    for (x :: <integer> from 0 below me.game.width)
      with-drawing-options (med, brush: me.game.wall[x,y])
	draw-rectangle(med, cx, cy, cx + cs + 1, cy + cs + 1, filled?: #t);
      end;
      cx := cx + cs;
    end;
    cy := cy + cs;
  end;
end;


// Translate key-press events into tetris actions.

define method event-action(event :: <key-press-event>) => (a :: <action>);
  select (event-key-name(event))
    #"left"        => #"left";
    #"right"       => #"right";
    #"up", #"down" => #"turn";
    #"space"       => #"drop";
    otherwise      => #"none";
  end;
end;

// Handle a key-press event, by translating it to a tetris action,
// then acting on it.

define method handle-event (me :: <game-pane>, event :: <key-press-event>) => ();
  act(me, event-action(event));
end;

// Process an action, by:
// - ignoring #"none" actions (these correspond to irrelevant key-presses)
// - passing other actions on to the game,
// - updating the score,
// - redrawing the baord,
// - setting the ready-for-tick flag

define method act (me :: <game-pane>, action :: <action>)
  if (action ~= #"none")
    act(me.game, action);
    update-score(me.sheet-frame);
    me.sheet-frame.ready-for-tick := #t;
    repaint-sheet(me, $everywhere);
  end;
end;

// Start a new game, by restarting the game, updating the score display,
// and redrawing the display.

define method new-game(frame :: <game-frame>)
  restart-game(frame.game);
  repaint-sheet(frame.game-pane, $everywhere);
  update-score(frame); 
end;

// Display a simple about message

define method about-display (frame :: <game-frame>)
  notify-user
    ("Tetris, version 1.0\n\n"
     "Use arrow keys to move and space to drop, or click on the buttons.", 
     owner: frame)
end;
