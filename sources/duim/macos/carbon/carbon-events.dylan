Module:       carbon-duim
Synopsis:     Macintosh event processing implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Event handling

//---*** Should be in DUIM
define function process-pending-events
    (sheet :: <sheet>) => ()
  let top-sheet = top-level-sheet(sheet);
  block (return)
    if (sheet-event-queue(top-sheet))
      while (#t)
	let event = read-event-no-hang(top-sheet);
	unless (event) return() end;
	let client = event-client(event);
	unless (instance?(event, <pointer-motion-event>))
	  debug-message("  Handling event %= for %=", event, client)
	end;
	// Note that if a <frame-exited-event> comes in, the
	// event handler for it will exit this loop by calling the
	// frame.%exit-function thunk
	handle-event(event-handler(client), event)
      end
    end
  end
end function process-pending-events;


/// Install event handlers

define sealed method generate-trigger-event
    (port :: <carbon-port>, sheet :: <sheet>) => ()
  let mirror = sheet-mirror(sheet);
  when (mirror)
    let control = mirror-control(mirror);
    ignoring("generate-trigger-event")
  end
end method generate-trigger-event;

define sealed method process-next-event
    (_port :: <carbon-port>, #key timeout)
 => (timed-out? :: <boolean>)
  //--- We should do something with the timeout
  ignore(timeout);
  ignoring("process-next-event");
  #f
end method process-next-event;

define sealed method note-mirror-enabled/disabled
    (_port :: <carbon-port>, sheet :: <sheet>, enabled? :: <boolean>) => ()
  ignoring("note-mirror-enabled/disabled")
end method note-mirror-enabled/disabled;

define sealed method note-mirror-enabled/disabled
    (_port :: <carbon-port>, sheet :: <top-level-sheet>, enabled? :: <boolean>) => ()
  ignoring("note-mirror-enabled/disabled")
end method note-mirror-enabled/disabled;
