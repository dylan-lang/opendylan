Module: life
Author: Carl Gay
Synopsis: Life frame and menu item support code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define frame <life-frame> (<simple-frame>)
  // Holds current board state, except during do-one-gen.
  slot current-board,
    init-value: #f;
  // Holds temporary board state, used during do-one-generation as a buffer.
  slot buffer-board,
    init-value: #f;
  // Radius of cells drawn in the life board.
  // +++ Should be cell-size, and should allow other cell shapes.
  slot cell-radius :: <integer>,
    init-value: 8;
  // Seconds to wait between display updates.
  slot sleep-interval :: <real>,
    init-value: 0.01;
  // Color of cells drawn in the life board.  #f = rainbow.
  slot cell-color :: false-or(<color>),
    init-value: #f;
  // Thread that loops displaying the current state of the life world.
  // See display.dylan.
  slot display-thread :: false-or(<thread>),
    init-value: #f;
  // Number of generations between each redisplay.
  slot step-size :: <integer>,
    init-value: 1;
  // Number of generations left to run before the display-thread should stop.
  slot generations-to-go :: <integer>,
    init-value: 0;
  // Lock used to synchronize drawing to the display-sheet.
  // Two threads can't draw on the same sheet without expecting errors.
  constant slot lock :: <lock>,
    init-value: make(<lock>);
  // Notification used to alert the display-thread of user input.
  slot notification :: false-or(<notification>),
    init-value: #f;
  // Used for communication between the display thread and the DUIM thread.
  slot command :: one-of(#"run", #"stop", #"exit"),
    init-value: #"stop";

  pane display-sheet (frame)   // Sheet on which the life board will be drawn.
    make(<life-sheet>,
	 width: 500, max-width: $fill,
	 height: 500, max-height: $fill);

  // Windows doesn't have a way of labelling sliders natively
  // so we label them ourselves.
  pane speed-slider (frame)
    vertically ()
      make(<slider>,
	   value-range: range(from: 0, to: 9),
           min-label: "Slow",
           max-label: "Fast",
           tick-marks: 10,
	   value: 10,
	   value-changed-callback: speed-slider-callback,
	   value-changing-callback: speed-slider-callback);
      horizontally ()
        make(<label>, label: "Slow");
        make(<null-pane>,	// fill space in between the labels
	     width: $fill,
	     min-width: 10,
	     max-width: 500);
        make(<label>, label: "Fast");
      end;
    end;
  pane run-button (frame)
    make(<push-button>,
	 label: "Run",
	 documentation: "Continuously run the Life rules on the current pattern",
	 activate-callback: method (gadget)
			      ignore(gadget);
			      start-display-thread(frame);
                              with-lock(frame.lock)
			        frame.command := #"run";
			        // Effectively run forever.
			        frame.generations-to-go := -1;
                                release(frame.notification);
                              end;
			    end);
  pane stop-button (frame)
    make(<push-button>,
	 label: "Stop",
	 documentation: "Stop updating the current pattern",
	 activate-callback: method (g)
			      ignore(g);
			      frame.command := #"stop"
			    end);
  pane step-button (frame)
    make(<push-button>,
	 label: "Step",
	 documentation: "Run the Life rules one 'step' on the current pattern",
	 activate-callback: method (g)
			      ignore(g);
			      start-display-thread(frame);
                              with-lock(frame.lock)
			        frame.command := #"run";
			        frame.generations-to-go := 1;
                                release(frame.notification);
                              end;
			    end);
  pane clear-button (frame)
    make(<push-button>,
	 label: "Clear",
	 documentation: "Clear the display",
	 activate-callback: method (button)
			      ignore(button);
			      if (frame.current-board)
				clear-board(frame);
				clear-status-bar(frame);
				clear-display-sheet(frame);
			      end if;
			    end);

  pane exit-button (frame)
    begin
      local method my-callback (button)
	      // Tell the display thread to exit.
	      with-lock(frame.lock)
		frame.command := #"exit";
	        release(frame.notification);
	      end;
              // Wait for the display thread to terminate.
              if (frame.display-thread)
		join-thread(frame.display-thread);
	      end if;
	      exit-application(0);
            end method;
      let button = make(<menu-button>,
			label: "E&xit",
			documentation: "Exit the Life application",
			activate-callback: my-callback);
      // A <menu-box> separates its children from other items in the
      // menu.  In Java this would be done with Menu.addSeparator().
      make(<menu-box>, children: vector(button))
    end;

  pane options-button (frame)
    make(<menu-button>,
	 label: "Options",
	 documentation: "Choose display options",
	 activate-callback: method (button)
			      ignore(button);
			      display-options(frame);
			    end method);

  pane about-button (frame)
    make(<menu-button>,
	 label: "About Life...",
	 documentation: "Explain the meaning of Life",
	 activate-callback: method (button)
			      ignore(button);
			      notify-user("\"42\"", owner: frame);
			    end method);

  pane rules-button (frame)
    make(<menu-button>,
	 label: "Rules",
	 documentation: "Display the rules of the game of Life",
	 activate-callback: method (button)
			      ignore(button);
			      display-rules(frame);
			    end method);

  pane life-menu (frame)
    make(<menu>,
	 label: "&Life",
	 children: vector(frame.options-button,
			  frame.exit-button));

  pane help-menu (frame)
    make(<menu>,
         label: "&Help",
         children: vector(frame.rules-button,
                          frame.about-button));

  pane pattern-menu (frame)
    make(<menu>,
	 label: "Pattern",
	 children: map-as(<vector>,
			  method (p)
			    make(<menu-button>,
				 label: p.pretty-name,
				 documentation: p.documentation,
				 activate-callback:
				   method (b)
				     ignore(b);
				     ensure-board(frame, force-init?: #t,
						  pattern: p);
				   end)
			  end method,
			  *patterns*));

  pane color-menu (frame)
    make-color-menu(frame);

  pane status-bar (frame)
    make(<status-bar>);

  layout (frame)
    vertically (spacing: 3)
      horizontally (spacing: 3)
        frame.run-button;
        frame.step-button;
        frame.stop-button;
        frame.clear-button;
      end;
      horizontally (y-alignment: #"center")
        make(<label>, label: "Speed ");
        frame.speed-slider;
      end;
      frame.display-sheet;
    end;

  menu-bar (frame)
    make(<menu-bar>, children: vector(frame.life-menu,
                                      frame.color-menu,
                                      frame.pattern-menu,
                                      frame.help-menu));
  status-bar (frame)
    frame.status-bar;

end frame <life-frame>;


define method initialize (frame :: <life-frame>, #key)
  next-method();
  frame.notification := make(<notification>, lock: frame.lock);
end method initialize;


define method start-display-thread (frame :: <life-frame>)
  if (~frame.display-thread)
    frame.display-thread := make(<thread>,
				 function: method ()
					     life-display-top-level(frame)
					   end,
				 name: "Life display loop");
  end if;
end method start-display-thread;


define method speed-slider-callback (gadget)
  let frame :: <life-frame> = sheet-frame(gadget);
  frame.sleep-interval := element(#[2, 1.75, 1.5, 1.25, 1,
				    0.75, 0.5, 0.25, 0.1, 0],
				  gadget-value(gadget));
  format-to-status-bar(frame, "Speed changed to %s",
		       gadget-value(gadget) + 1);
end method speed-slider-callback;


define method make-color-menu (frame :: <life-frame>)
  let names = #("Rainbow", "Green", "White", "Blue", "Magenta", "Cyan",
		"Yellow", "Black");
  let colors = list(#f, $green, $white, $blue, $Magenta, $cyan,
		    $yellow, $black);
  make(<menu>,
       label: "Color",
       children:
	 vector(make(<radio-menu-box>,
		     items: map(pair, names, colors),
		     //+++ Change this to label-key: after new Kan release.
		     name-key: first,
		     value-key: identity,
		     value-changed-callback:
		       method (bbox)
			 let frame = sheet-frame(bbox);
			 frame.cell-color := tail(gadget-value(bbox));
			 format-to-status-bar(frame, "Color set to %s",
					      first(gadget-value(bbox)));
		       end)))
end method make-color-menu;


define constant *life-rules*  = "1. STASIS: If, for a given cell, the number of live neighbours is exactly two, the cell maintains its current status into the next generation.  If the cell is alive, it stays alive, if it is dead, it stays dead.\n2. GROWTH: If the number of live neighbours is exactly three, the cell will be alive in the next generation. This is regardless of the cell's current state.\n3. DEATH: If the number of live neighbours is 0, 1, or 4-8, the cell will be dead in the next generation.";

define method display-rules (frame :: <life-frame>) => ()
  with-frame-manager (frame-manager(frame))
    let rules = make(<text-editor>,
		     text: *life-rules*,
		     read-only?: #t,
                     scroll-bars: #"vertical",
		     columns: 80,
	             lines: 8);
    let dialog = make(<dialog-frame>, title: "Rules to the Game of Life",
		      layout: rules);
    start-frame(dialog);
  end;
end method display-rules;

define method display-options (frame :: <life-frame>) => ()
  with-frame-manager (frame-manager(frame))
    let cell-size = make(<text-field>,
                         value-type: <integer>,
                         value: frame.cell-radius * 2);
    let step-size = make(<text-field>,
                         value-type: <integer>,
                         value: frame.step-size);
    let dialog
      = make(<dialog-frame>, title: "Options",
	     layout: vertically (spacing: 5)
                       horizontally (spacing: 5, y-alignment: #"center")
	                 make(<label>, label: "Cell size ");
	                 cell-size;
	               end;
                       horizontally (spacing: 5, y-alignment: #"center")
	                 make(<label>, label: "Generations between redisplay ");
	                 step-size;
                       end;
                     end);
    if (start-frame(dialog))
      let new-cell-radius = ceiling/(gadget-value(cell-size), 2);
      if (new-cell-radius < 1)
	notify-user("Cell size too small.  Setting it to 2.", owner: frame);
	new-cell-radius := 1;
      end if;
      frame.cell-radius := new-cell-radius;
      frame.step-size := gadget-value(step-size);
      ensure-board(frame);
    end if;
  end;
end method display-options;

define method format-to-status-bar (frame :: <life-frame>,
				    format-string :: <string>,
				    #rest message-args)
 => ()
  local method do-format-to-status-bar ()
	  let status-bar = frame-status-bar(frame);
	  let message = apply(format-to-string, format-string, message-args);
	  gadget-label(status-bar) := message;
	end method;
  call-in-frame(frame, do-format-to-status-bar);
end method format-to-status-bar;

define inline method clear-status-bar (frame :: <life-frame>) => ()
  format-to-status-bar(frame, "");
end method clear-status-bar;

define method clear-board (frame :: <life-frame>) => ()
  let board = frame.current-board;
  if (board)
    with-board-size(board, rows, cols)
      for (row :: <integer> from 0 below rows)
	for (col :: <integer> from 0 below cols)
	  board[row, col] := $dead;
	end for;
      end for;
    end;
  end if;
end method clear-board;

define method initialize-game (frame :: <life-frame>, #key pattern, clear? = #t) => ()
  clear? & clear-board(frame);
  pattern & pattern.init-function(frame.current-board);
  clear-status-bar(frame);
  display-board(frame);
end method initialize-game;
