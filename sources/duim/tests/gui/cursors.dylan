Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM testing code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Cursors

define frame <cursor-test-frame> (<simple-frame>)
  pane drawing-pane (frame)
    make(<drawing-pane>,		// mirrored
	 cursor: #"i-beam");
  pane simple-pane (frame)
    make(<simple-pane>,			// unmirrored
	 cursor: #"cross",
	 display-function: ignore);
  pane button (frame)
    make(<push-button>,
	 label: "Get Busy...",
	 activate-callback: test-busy-cursor);
  pane background-button (frame)
    make(<push-button>,
	 label: "Background it...",
	 activate-callback: test-background-busy-cursor);
  layout (frame)
    vertically (y-spacing: 4)
      horizontally (min-width: 400)
        with-border (type: #"sunken")
          frame.drawing-pane
	end;
        with-border (type: #"sunken")
          frame.simple-pane
	end;
      end;
      horizontally (spacing: 4)
        frame.button;
        frame.background-button;
      end
    end;
  status-bar (frame)
    make(<status-bar>, progress-bar?: #t);
end frame <cursor-test-frame>; 

define method note-operation-progress 
    (frame :: <cursor-test-frame>, value :: <integer>, max :: <integer>) => ()
  note-progress(value, max,
		label: format-to-string("%d%% of the way through",
					floor/(value * 100, max)))
end method note-operation-progress;

define method test-busy-cursor (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  with-busy-cursor (frame)
    noting-progress (frame, "Test")
      for (i from 0 to 100)
	note-operation-progress(frame, i, 100);
	sleep(0.1)
      end
    end
  end
end method test-busy-cursor;

define method test-background-busy-cursor (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  make(<thread>,
       function: 
	 method ()
	   with-background-cursor (frame)
	     noting-progress (frame, "Test")
	       for (i from 0 to 100)
		 note-operation-progress(frame, i, 100);
		 sleep(0.1)
	       end
	     end
	   end
	 end)
end method test-background-busy-cursor;


/// Install the test

install-test(<cursor-test-frame>, "Cursors");

