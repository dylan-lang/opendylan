Module:       tetris
Synopsis:     DUIM implementation of the game Tetris
Author:       Richard Tucker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// this is the main body of the program

define function main () => ()

  // create a game of tetris

  let board = make(<board>, width: 10, height: 30);

  // and a frame to display it

  let frame = make(<game-frame>, game: board);

  // the tick-loop method sends "tick" actions to the frame. The interval
  // between ticks is board.speed, and a tick will only be sent if the frame
  // is ready for it. This simple semaphore prevents a backlog of ticks from
  // accumulating when the frame thread is busy (for example when the user is
  // looking at the menus).

  local method tick-loop() => ();
	  while (#t)
	    sleep(board.speed);
	    if (frame.ready-for-tick)
	      frame.ready-for-tick := #f;
	      call-in-frame(frame, act, frame.game-pane, #"tick");
	    end;
	  end;
	end;

  // start a new thread to send the tick actions

  make(<thread>, name: "ticker", function: tick-loop);
  start-frame(frame);
end function main;

begin
  main();
end;

