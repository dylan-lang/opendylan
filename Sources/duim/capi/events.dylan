Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Handle the events

define variable *trace-events* = #f;
define variable *trace-motion-events* = #f;

define method trace-sheet-event
    (mirror :: <capi-mirror>, format-string, #rest args)
  when (*trace-events*)
    let sheet = mirror-sheet(mirror);
    debug-message("Event for %=: ", sheet);
    apply(debug-message, format-string, args)
  end
end method trace-sheet-event;

define method distribute-capi-event (mirror :: <capi-mirror>, event :: <event>)
  let sheet = mirror-sheet(mirror);
  let _port = sheet & port(sheet);
  if (_port)
    distribute-event(_port, event)
  else
    warn("Ignored CAPI event for mirror %=, sheet %= with no port", 
         mirror, sheet)
  end;
end method distribute-capi-event;

define method distribute-event 
    (_port :: <capi-port>, event :: <event>) => ()
  next-method();
  let client = event-client(event);
  //---*** This is to get around the CAPI not handling events properly
  when (client)
    handle-all-events(client)
  end;
  force-display(_port)
end method distribute-capi-event;

define method handle-all-events (sheet :: type-union(<sheet>, <gadget>)) => ()
  let top-sheet = top-level-sheet(sheet);
  if (top-sheet)
    do-handle-all-events(sheet-event-queue(top-sheet))
  else
    warn("Unable to handle all events for client %= with no top level sheet", sheet)
  end
end method handle-all-events;

define method handle-all-events (frame :: <frame>) => ()
  do-handle-all-events(frame-event-queue(frame))
end method handle-all-events;

define method do-handle-all-events (queue :: <event-queue>) => ()
  until (event-queue-empty?(queue))
    let event = event-queue-pop(queue);
    handle-event(event-handler(event-client(event)), event)
  end
end method do-handle-all-events;


define method generate-trigger-event
    (_port :: <capi-port>, sheet :: <sheet>) => ()
  force-display(_port)
end method generate-trigger-event;


define method distribute-abled-event
    (mirror :: <capi-mirror>, state)
end method distribute-abled-event;

define method distribute-exposure-event
    (mirror :: <capi-mirror>, x, y, width, height)
  trace-sheet-event(mirror, "repaint %= by %= at %=,%=", width, height, x, y);
  let sheet = mirror-sheet(mirror);
  //---*** Nasty hack so that we don't repaint too early
  when (every?(port, sheet-children(sheet)))
    repaint-sheet(sheet, 
                  make-bounding-box(x, y, x + width, y + height))
  end
end method distribute-exposure-event;

define method distribute-button-event
    (mirror :: <capi-mirror>, type, x, y, modifier-state, button)
  let sheet = mirror-sheet(mirror);
  let _port = sheet & port(sheet);
  let pointer = _port & port-pointer(_port);
  let duim-button
    = select (button)
        1 => $left-button;
        2 => $middle-button;
        3 => $right-button;
      end;
  let class
    = select (type)
        press:        => <button-press-event>;
        double-click: => <double-click-event>;
        release:      => <button-release-event>;
      end;
  if (type == press:)
    pointer-button-state(pointer) := duim-button
  else
    pointer-button-state(pointer) := 0		// no chords in CAPI...
  end;
  trace-sheet-event(mirror, "%= Button %= at %=,%=", type, button, x, y);
  distribute-capi-event(mirror,
                        make(class,
                             sheet: sheet,
                             pointer: pointer,
                             button: duim-button,
			     modifier-state: modifier-state,
                             x: x, y: y));
  if (class == <double-click-event>)
    distribute-button-event(mirror, release:, x, y, modifier-state, button)
  end
end method distribute-button-event;

define method distribute-motion-event
    (mirror :: <capi-mirror>, type, x, y, modifier-state)
  let sheet = mirror-sheet(mirror);
  let _port = sheet & port(sheet);
  let pointer = _port & port-pointer(_port);
  let duim-button = pointer-button-state(pointer);
  if (*trace-motion-events*)
    trace-sheet-event(mirror, "motion at %=, %=", x, y)
  end;
  if (zero?(duim-button))
    distribute-capi-event(mirror,
			  make(<pointer-motion-event>,
			       sheet: sheet,
			       pointer: pointer,
			       modifier-state: modifier-state,
			       x: x, y: y))
  else
    distribute-capi-event(mirror,
			  make(<pointer-drag-event>,
			       sheet: sheet,
			       pointer: pointer,
			       button: duim-button,
			       modifier-state: modifier-state,
			       x: x, y: y))
  end
end method distribute-motion-event;

define method distribute-entry-exit-event
    (mirror :: <capi-mirror>, type, x, y, modifier-state, mode, kind)
  let sheet = mirror-sheet(mirror);
  let _port = sheet & port(sheet);
  if (*trace-motion-events*)
    trace-sheet-event(mirror, "%= at %=, %=", type, x, y)
  end;
  let event-class
    = select (type)
	#"enter" => <pointer-enter-event>;
        #"exit"  => <pointer-exit-event>;
      end;
  distribute-capi-event(mirror,
                        make(event-class,
                             sheet: sheet,
                             pointer: _port & port-pointer(_port),
			     modifier-state: modifier-state,
                             x: x, y: y))
end method distribute-entry-exit-event;

define method distribute-key-event
    (mirror :: <capi-mirror>, type, modifier-state, keysym, char)
  trace-sheet-event(mirror, "key %= of %=", type, keysym);
  let sheet = mirror-sheet(mirror);
  let _port = sheet & port(sheet);
  let event-class
    = select (type)
        #"release" => <key-release-event>;
        #"press"   => <key-press-event>;
      end;
  port-modifier-state(_port) := modifier-state;
  distribute-capi-event(mirror,
                        make(event-class,
                             sheet: sheet,
                             modifier-state: modifier-state,
                             key-name: keysym,
                             character: char))
end method distribute-key-event;


/// Port hack to make start-frame work without messing around with CLUE

define method process-next-event
    (_port :: <capi-port>, #key timeout)
 => (timed-out? :: <boolean>)
  //---*** Another indicator of CAPI's broken event processing...
  error("CAPI event processing not implemented!\n")
end method process-next-event;

//--- Hack to avoid the whole start-frame mess in the CAPI backend, because
//--- then we have to mess around with CLUE event queues.
//--- Note that this means that nobody is calling 'execute-frame-top-level',
//--- meaning no user methods for top-level functions will ever get run.  Tough.
//--- The read/handle-event stuff gets done directly in distribute-event in CAPI.
define method port-start-frame
    (_port :: <capi-port>, frame :: <simple-frame>)
 => (status-code :: false-or(<integer>))
  frame-mapped?(frame) := #t;
  #f
end method port-start-frame;
