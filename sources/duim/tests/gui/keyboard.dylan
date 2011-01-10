Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Keyboard handling

define class <keyboard-pane> (<drawing-pane>)
end class <keyboard-pane>;

define method handle-event
    (pane :: <keyboard-pane>, event :: <key-press-event>) => ()
  let frame = sheet-frame(pane);
  if (frame.pending-key-presses = 0)
    clear-events(frame)
  end;
  frame.pending-key-presses := frame.pending-key-presses + 1;
  let character = event-character(event);
  record-event(frame, 
	       format-to-string("Pressed key %= %s, %s",
				event-key-name(event),
				if (character)
				  format-to-string("(char %=)", character)
				else
				  format-to-string("(no char)", character)
				end,
				modifier-names(event-modifier-state(event))))
end method handle-event;

define method handle-event
    (pane :: <keyboard-pane>, event :: <key-release-event>) => ()
  let frame = sheet-frame(pane);
  frame.pending-key-presses := frame.pending-key-presses - 1;
  record-event(frame,
               format-to-string("Released key %=, %s",
				event-key-name(event),
				modifier-names(event-modifier-state(event))))
end method handle-event;

define method modifier-names
    (modifier :: <integer>) => (names :: <string>)
  let shift?   = ~zero?(logand(modifier, $shift-key));
  let control? = ~zero?(logand(modifier, $control-key));
  let alt?     = ~zero?(logand(modifier, $alt-key));
  if (shift? | control? | alt?)
    format-to-string("modifiers are: %s%s%s",
		     if (alt?) "alt " else "" end,
		     if (shift?) "shift " else "" end,
		     if (control?) "control " else "" end)
  else
    "no modifiers"
  end
end method modifier-names;

define method handle-event
    (pane :: <keyboard-pane>, event :: <button-press-event>) => ()
  let frame = sheet-frame(pane);
  frame.pending-key-presses := 0;
  clear-events(frame)
end method handle-event;

define frame <keyboard-handling-frame> (<simple-frame>)
  slot pending-key-presses :: <integer> = 0;
  pane keyboard-pane (frame)
    make(<keyboard-pane>, caret: #t);
  pane events-pane (frame)
    make(<list-control>, lines: 12);
  pane main-layout (frame)
    vertically (width: 400, height: 500)
      make(<label>, label: "Press some keys:");
      with-border (type: #"sunken")
        frame.keyboard-pane
      end;
      frame.events-pane
    end;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>);
  keyword alt-key-is-meta?: = #t;
end frame <keyboard-handling-frame>;

define method initialize
    (frame :: <keyboard-handling-frame>, #key) => ()
  next-method();
  frame-input-focus(frame) := frame.keyboard-pane
end method initialize;

define method clear-events
    (frame :: <keyboard-handling-frame>) => ()
  gadget-items(frame.events-pane) := #[]
end method clear-events;

define method record-event
    (frame :: <keyboard-handling-frame>, message :: <string>) => ()
  let events-pane = frame.events-pane;
  gadget-items(events-pane)
    := concatenate-as(<vector>, gadget-items(events-pane), vector(message))
end method record-event;

install-test(<keyboard-handling-frame>, "Keyboard");
