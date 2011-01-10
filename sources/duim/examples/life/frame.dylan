Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// This file defines the user interface for the game of Life
//


// Imelements the "Exit" command.
define method frame-exit (frame :: <life-frame>) => ()
  // Tell the logic thread to exit.
  frame.command := #"exit";
  // Awaken it from its stupor.
  with-lock(frame.lock)
    release(frame.user-input-received);
  end;
  exit-frame(frame);
end method frame-exit;

define command-table *file-command-table* (*global-command-table*)
    menu-item "Start" = com-start,
      documentation: "Continuously run the Life rules on the current pattern";
    menu-item "Stop" = com-stop,
      documentation: "Stop updating the current pattern";
    menu-item "Step" = com-step,
      documentation: "Run the Life rules one 'step' on the current pattern";
    menu-item "Clear All" = com-clear,
      documentation: "Clear the display";
    separator;
    menu-item "Exit" = frame-exit,
      documentation: "Exit the Life application";
end;

define method com-start
    (frame :: <life-frame>) => ()
  start-logic-thread(frame);
  with-lock(frame.lock)
    frame.command := #"run";
    // Effectively run forever.
    frame.generations-to-go := -1;
    release(frame.user-input-received);
    release(frame.display-done);
  end;
end method com-start;

define method com-stop
    (frame :: <life-frame>) => ()
  frame.command := #"stop";
end method com-stop;

define method com-step
    (frame :: <life-frame>) => ()
  start-logic-thread(frame);
  with-lock(frame.lock)
    frame.command := #"run";
    frame.generations-to-go := 1;
    release(frame.user-input-received);
    release(frame.display-done);
  end;
end method com-step;

define method com-clear
    (frame :: <life-frame>) => ()
  clear-all(frame);
end method com-clear;


// Returns a function that implements a "Color" menu item (e.g., "Red").
define function make-color-menu-function
    (name :: <string>, color :: false-or(<color>)) => (f :: <function>)
  method (frame :: <life-frame>)
    frame.cell-color := color;
    format-to-status-bar(frame, "Color set to %s", name);
  end
end function make-color-menu-function;

define command-table *color-command-table* (*global-command-table*)
    menu-item "Rainbow" = make-color-menu-function("Rainbow", #f),
      documentation: "Set live cell color to rainbow";
    menu-item "Red" = make-color-menu-function("Red", $red),
      documentation: "Set live cell color to red";
    menu-item "Green" = make-color-menu-function("Green", $green),
      documentation: "Set live cell color to green";
    menu-item "White" = make-color-menu-function("White", $white),
      documentation: "Set live cell color to white";
    menu-item "Blue" = make-color-menu-function("Blue", $blue),
      documentation: "Set live cell color to blue";
    menu-item "Magenta" = make-color-menu-function("Magenta", $magenta),
      documentation: "Set live cell color to magenta";
    menu-item "Cyan" = make-color-menu-function("Cyan", $cyan),
      documentation: "Set live cell color to cyan";
    menu-item "Yellow" = make-color-menu-function("Yellow", $yellow),
      documentation: "Set live cell color to yellow";
    menu-item "Black" = make-color-menu-function("Black", $black),
      documentation: "Set live cell color to black";
end;

// The "Load" command.  (Not yet implemented.)
define method frame-load-pattern (frame :: <life-frame>)
end method frame-load-pattern;

// The "Save" command.  (Not yet implemented.)
define method frame-save-pattern (frame :: <life-frame>)
end method frame-save-pattern;

// Cribbed from the duim-gui-test-suite.  Dynamically creates
// the "Pattern" menu from the patterns in *patterns*.
define method add-pattern-menu-items
    (frame :: <life-frame>) => (buttons :: false-or(<menu-box>))
  let menu
    = block (return)
	do-command-menu-gadgets(method (menu) return(menu) end,
				frame, *pattern-command-table*,
				tool-bar?: #f);
	#f
      end;
  when (menu)
    let buttons = make(<vector>, size: size(*patterns*));
    for (pattern in *patterns*,
         i :: <integer> from 0)
      buttons[i] := make(<push-menu-button>,
                         label: pattern.pretty-name,
                         documentation: pattern.documentation,
                         activate-callback: method (g)
                                              ignore(g);
                                              ensure-board(frame,
                                                           force-init?: #t,
                                                           pattern: pattern);
                                            end);
    end for;
    let menu-box = make(<push-menu-box>,
			children: as(<simple-vector>, buttons));
    add-child(menu, menu-box);
    sheet-mapped?(menu-box) := sheet-mapped?(menu);
    menu-box
  end
end method add-pattern-menu-items;

define command-table *pattern-command-table* (*global-command-table*)
    menu-item "Load..." = frame-load-pattern,
      documentation: "Load a Life pattern from file";
    menu-item "Save..." = frame-save-pattern,
      documentation: "Save the current pattern to file";
end;

define command-table *help-command-table* (*global-command-table*)
    menu-item "Rules..." = frame-display-rules,
      documentation: "Display the rules of life";
    menu-item "About Life..." = method (frame)
                                  notify-user("\"42\"", owner: frame);
                                end,  // humor
      documentation: "What is the meaning of life?";
end;

define command-table *main-command-table* (*global-command-table*)
    menu-item "File" = *file-command-table*;
    menu-item "Color" = *color-command-table*;
    menu-item "Pattern" = *pattern-command-table*;
    menu-item "Help" = *help-command-table*;
end;

// Number of seconds to sleep between Life generations.
define constant $sleep-intervals :: <simple-object-vector>
  = #[1, 0.75, 0.5, 0.25, 0.17, 0.12, 0.06, 0.03, 0.015, 0];

define constant $initial-speed :: <integer> = 9;  // index into $sleep-intervals

define function speed->sleep-interval
    (index :: <integer>) => (interval :: false-or(<real>))
  element($sleep-intervals, index)
end;

define method display-current-generation (frame :: <life-frame>)
  gadget-label(frame.gen-display-pane)
    := format-to-string("%d", frame.current-generation);
end method display-current-generation;

define method display-live-cell-count (frame :: <life-frame>)
  gadget-label(frame.live-cell-count-display-pane)
    := format-to-string("%d", frame.live-cell-count);
end method display-live-cell-count;

define method update-live-cell-count
    (frame :: <life-frame>, cell-delta :: <integer>) => ()
  frame.live-cell-count := frame.live-cell-count + cell-delta;
  display-live-cell-count(frame);
end method update-live-cell-count;

// Display the amount of time it takes to calculate a new generation.
// This doesn't include any of the time taken for redisplay, etc., so
// it's a good way to compare different do-n-generations algorithms.
define method display-time/generation (frame :: <life-frame>) => ()
  let gens :: <integer> = frame.current-generation;
  let it :: <integer>
    =  if (gens == 0)
         0
       else
         // I'm sure there's a more efficient way...
         truncate(((frame.elapsed-seconds * 1000.0
                    + frame.elapsed-useconds / 1000.0)
                   / gens) * 100)
       end if;
  let (msecs, rem) = floor/(it, 100);
  gadget-label(frame.time-pane) := format-to-string("%d.%d", msecs, rem);
end method display-time/generation;

// Called when the "Clear" button is clicked and when a game is initialized.
define method clear-all (frame :: <life-frame>) => ()
  clear-board(frame);
  clear-status-bar(frame);
  clear-display-sheet(frame);
  frame.elapsed-seconds := 0;
  frame.elapsed-useconds := 0;
  frame.current-generation := 0;
  frame.live-cell-count := 0;
  display-all(frame);
end method clear-all;

define method display-all (frame :: <life-frame>) => ()
  display-board(frame);
  display-time/generation(frame);
  display-current-generation(frame);
  display-live-cell-count(frame);
end method display-all;

define method update-time
    (frame :: <life-frame>, sec :: <integer>, usec :: <integer>) => ()
  frame.elapsed-seconds := frame.elapsed-seconds + sec;
  frame.elapsed-useconds := frame.elapsed-useconds + usec;
  if (frame.elapsed-useconds >= 1000000)
    frame.elapsed-useconds := frame.elapsed-useconds - 1000000;
    frame.elapsed-seconds := frame.elapsed-seconds + 1;
  end if;
  display-time/generation(frame);
end method update-time;

//--- Note that this is Win32-specific code!
define constant $life-icon  = read-image-as(<win32-icon>, "LLARGE", #"small-icon");
define constant $play-icon  = read-image-as(<win32-icon>, "PLAY",   #"small-icon");
define constant $step-icon  = read-image-as(<win32-icon>, "STEP",   #"small-icon");
define constant $stop-icon  = read-image-as(<win32-icon>, "STOP",   #"small-icon");
define constant $erase-icon = read-image-as(<win32-icon>, "ERASE",  #"small-icon");

define frame <life-frame> (<simple-frame>)
  // Holds current board state, except during do-n-generations.
  slot current-board :: <board> = make(<board>, rows: 0, columns: 0);
  // Holds temporary board state, used during do-n-generations as a buffer.
  slot buffer-board :: <board> = make(<board>, rows: 0, columns: 0);
  // Cell size plus 1 (for one pixel of whitespace between cells).
  // i.e., should not be smaller than 2.
  slot cell-size :: <integer> = 8;
  // Seconds to wait between display updates.
  slot sleep-interval :: false-or(<real>) = speed->sleep-interval($initial-speed);
  // Color of cells drawn in the life board.  #f = rainbow.
  slot cell-color :: false-or(<color>) = #f;
  // Thread that loops recalculating the current state of the life world.
  // See display.dylan.
  slot logic-thread :: false-or(<thread>) = #f;
  slot current-generation :: <integer> = 0;
  slot live-cell-count :: <integer> = 0;
  slot elapsed-seconds :: <integer> = 0;
  slot elapsed-useconds :: <integer> = 0;
  // Number of generations between each redisplay.
  constant slot step-size :: <integer> = 1;
  // Number of generations left to run before the logic-thread should stop.
  slot generations-to-go :: <integer> = 0;
  // Lock used to synchronize drawing to the display-sheet.
  // Two threads can't draw on the same sheet without expecting errors.
  constant slot lock :: <lock> = make(<lock>);
  // Notification used to alert the logic-thread of user input.
  slot user-input-received :: false-or(<notification>) = #f;
  // Notification used to signal the compute process that display is done
  slot display-done :: false-or(<notification>) = #f;
  // Used for communication between the display thread and the DUIM thread.
  slot command :: one-of(#"run", #"stop", #"exit") = #"stop";

  pane display-sheet (frame)   // Sheet on which the life board will be drawn.
    make(<life-sheet>,
	 width: 500, max-width: $fill,
	 height: 400, max-height: $fill);

  pane run-button (frame)
    make(<push-button>,
	 label: $play-icon,
	 documentation: "Start",
	 activate-callback: method (gadget)
                              com-start(sheet-frame(gadget))
			    end);
  pane stop-button (frame)
    make(<push-button>,
	 label: $stop-icon,
	 documentation: "Stop",
	 activate-callback: method (g)
                              com-stop(sheet-frame(g));
			    end);
  pane step-button (frame)
    make(<push-button>,
	 label: $step-icon,
	 documentation: "Step",
	 activate-callback: method (g)
                              com-step(sheet-frame(g));
			    end);
  pane clear-button (frame)
    make(<push-button>,
	 label: $erase-icon,
	 documentation: "Clear All",
	 activate-callback: method (button)
                              com-clear(sheet-frame(button))
			    end);
  pane cell-size-pane (frame)
    labelling("Cell size:")
      make(<spin-box>,
           items: range(from: 3, to: 20),
           value: frame.cell-size,
           value-changed-callback: cell-size-callback /*,
           width: 20,
           max-width: 20 */ )
    end labelling;

  pane speed-pane (frame)
    labelling("Speed:")
      make(<spin-box>,
           items: range(from: 1, to: 10),
           value: $initial-speed + 1,
           value-changed-callback: speed-callback /*,
           width: 20,
           max-width: 20 */ )
    end;

  pane gen-display-pane (frame)
    make(<label>, label: "0", width: 20, max-width: $fill);

  pane live-cell-count-display-pane (frame)
    make(<label>, label: "0", width: 3, max-width: $fill);

  pane time-pane (frame)
    make(<label>, label: "0", width: 30, max-width: $fill);

  pane status-bar (frame)
    make(<status-bar>);

  layout (frame)
    vertically (spacing: 3)
      horizontally (spacing: 0, y-alignment: #"center")
        frame.run-button;
        frame.step-button;
        frame.stop-button;
        frame.clear-button;
        make(<null-pane>, width: 10);  // a spacer
        frame.cell-size-pane;
        make(<null-pane>, width: 10);  // a spacer
        frame.speed-pane;
      end;
      with-border (type: #"groove")
        horizontally (spacing: 3)
          labelling ("Generation:") frame.gen-display-pane end;
          labelling ("Live Cells:") frame.live-cell-count-display-pane end;
          labelling ("msec/gen:") frame.time-pane end;
        end
      end;
      frame.display-sheet;
    end;

  command-table (frame)
    *main-command-table*;

  status-bar (frame)
    frame.status-bar;

  keyword icon: = $life-icon;

end frame <life-frame>;


define method initialize (frame :: <life-frame>, #key)
  next-method();
  frame.user-input-received := make(<notification>, lock: frame.lock);
  frame.display-done := make(<notification>, lock: frame.lock);
  // Disable some unimplemented menu items.
  command-enabled?(frame-load-pattern, frame) := #f;
  command-enabled?(frame-save-pattern, frame) := #f;
end method initialize;

// Start up the logic thread.  The new thread begins executing
// immediately after being created.
define method start-logic-thread (frame :: <life-frame>)
  if (~frame.logic-thread)
    frame.logic-thread := make(<thread>,
                               function: method ()
                                           life-logic-loop(frame)
					 end,
			       name: "Life logic loop");
  end if;
end method start-logic-thread;

// Have to wait until the frame is mapped to add anything to a menu.
define method handle-event
    (frame :: <life-frame>,  event :: <frame-mapped-event>) => ()
  add-pattern-menu-items(frame);
end method handle-event;

// Called when the user change the value in the "Speed" gadget.
define method speed-callback (gadget :: <spin-box>)
  let frame :: <life-frame> = sheet-frame(gadget);
  frame.sleep-interval := speed->sleep-interval(gadget-value(gadget) - 1);
  format-to-status-bar(frame, "Speed set to %s", gadget-value(gadget));
end method speed-callback;

// Called when the user change the value in the "Cell Size" gadget.
define method cell-size-callback (gadget :: <spin-box>)
  let frame :: <life-frame> = sheet-frame(gadget);
  frame.cell-size := gadget-value(gadget);
  display-all(frame);
  format-to-status-bar(frame, "Cell size set to %d", frame.cell-size);
end method cell-size-callback;

define constant *life-rules*
  = begin
      "1. STASIS: If, for a given cell, the number of live neighbours is exactly "
      "two, the cell maintains its current status into the next generation.  If "
      "the cell is alive, it stays alive, if it is dead, it stays dead.\n"
      "2. GROWTH: If the number of live neighbours is exactly three, the cell will "
      "be alive in the next generation. This is regardless of the cell's current "
      "state.\n"
      "3. DEATH: If the number of live neighbours is 0, 1, or 4-8, the "
      "cell will be dead in the next generation."
    end;

// Imlements the "Rules..." menu item.
define method frame-display-rules (frame :: <life-frame>) => ()
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
end method frame-display-rules;

define method format-to-status-bar (frame :: <life-frame>,
				    format-string :: <string>,
				    #rest message-args)
 => ()
  local method do-format-to-status-bar ()
	  let message = apply(format-to-string, format-string, message-args);
          frame-status-message(frame) := message;
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
      let dead = set-modified(set-alive(0, #f), #f);
      for (row :: <integer> from 0 below rows)
	for (col :: <integer> from 0 below cols)
	  set-cell(board, dead, row, col);
	end for;
      end for;
    end;
    frame.live-cell-count := 0;
    frame.current-generation := 0;
  end if;
end method clear-board;

define method initialize-game
    (frame :: <life-frame>, #key pattern) => ()
  clear-all(frame);
  if (pattern)
    let live-cells = pattern.init-function(frame.current-board);
    if (instance?(live-cells, <integer>))
      frame.live-cell-count := frame.live-cell-count + live-cells;
    end if;
  end if;
  display-all(frame);
end method initialize-game;

