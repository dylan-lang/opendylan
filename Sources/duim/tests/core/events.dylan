Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Event testing

define sideways method make-test-instance
    (class :: subclass(<sheet-event>)) => (event :: <sheet-event>)
  make(class, sheet: $dummy-sheet)
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<gadget-event>)) => (event :: <gadget-event>)
  make(class, gadget: $dummy-gadget)
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<frame-event>)) => (event :: <frame-event>)
  make(class, frame: $dummy-frame)
end method make-test-instance;

define sideways method class-test-function
    (class :: subclass(<event>)) => (function :: <function>)
  test-event-class
end method class-test-function;

define open generic test-event-class
    (class :: subclass(<event>), #key, #all-keys) => ();

define method test-event-class
    (class :: subclass(<event>), #key name, instantiable?, #all-keys) => ()
  if (instantiable?)
  end
end method test-event-class;


/// event tests

define method handle-all-events
    (sheet :: <top-level-sheet>) => ()
  while (event-pending?(sheet))
    let event = read-event-no-hang(sheet);
    handle-event(event-handler(event-client(event)), event)
  end;
end method handle-all-events;

define method ensure-handled-events
    (sheet :: <sheet>) => (events :: <sequence>)
  handle-all-events(top-level-sheet(sheet));
  handled-events(sheet)
end method ensure-handled-events;

define method ensure-handled-events
    (frame :: <frame>) => (events :: <sequence>)
  handle-all-events(top-level-sheet(frame));
  handled-events(frame)
end method ensure-handled-events;

define method check-event (name, number, class, client)
  let events = ensure-handled-events(client);
  let event = (number < size(events)) & events[number];
  check-true(name,
             event
               & instance?(event, class)
               & event-client(event) = client)
end method check-event;

define method make-event-frame 
    (#rest args,
     #key gadget-class = <list-box>,
          gadget-class-2,
     #all-keys)
  let gadget1 = apply(make-test-pane, gadget-class, args);
  let gadget2 = gadget-class-2 & make-test-pane(gadget-class-2);
  let layout
    = make-test-pane(<column-layout>,
		     children: if (gadget2)
				 vector(gadget1, gadget2)
			       else
				 vector(gadget1)
			       end);
  let frame = make-test-frame(<test-frame>, layout: layout);
  values(frame, layout, gadget1, gadget2);
end method make-event-frame;

define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <timer-event>) => ()
  record-event(sheet, event)
end method handle-event;

define test timer-events-test ()
  let (frame, layout) = make-event-frame();
  ignore(layout);
  distribute-event(find-test-port(), make(<timer-event>, frame: frame));
  frame;
end test timer-events-test;


define method handle-event 
    (frame :: <test-frame>, event :: <frame-exit-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <frame-exited-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <application-exited-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <frame-created-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <frame-mapped-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <frame-unmapped-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <frame-destroyed-event>) => ()
  record-event(frame, event)
end method handle-event;

define method handle-event 
    (frame :: <test-frame>, event :: <port-terminated-event>) => ()
  record-event(frame, event)
end method handle-event;

define test frame-events-test ()
  let port = find-test-port();
  let (frame, layout) = make-event-frame();
  ignore(layout);
  let events = ensure-handled-events(frame);
  check-true("Initial frame creation event received",
             size(events) = 1 & instance?(events[0], <frame-created-event>));
  handled-events(frame).size := 0;
  let event = make(<frame-created-event>, frame: frame);
  distribute-event(port, event);
  let events = ensure-handled-events(frame);
  check-true("Frame created event handled",
             size(events) = 1 & events[0] = event);
  handled-events(frame).size := 0;
  let event = make(<frame-exit-event>, frame: frame);
  distribute-event(port, event);
  let events = ensure-handled-events(frame);
  check-true("Frame exit event handled",
             size(events) = 1 & events[0] = event);
  handled-events(frame).size := 0;
  let event = make(<frame-exited-event>, frame: frame, status-code: 0);
  distribute-event(port, event);
  let events = ensure-handled-events(frame);
  check-true("Frame exited event handled",
             size(events) = 1 & events[0] = event);
  handled-events(frame).size := 0;
  let event = make(<frame-destroyed-event>, frame: frame);
  distribute-event(port, event);
  let events = ensure-handled-events(frame);
  check-true("Frame destroyed event handled",
             size(events) = 1 & events[0] = event);
  handled-events(frame).size := 0;
  let event = make(<port-terminated-event>, condition: #t, frame: frame);
  distribute-event(port, event);
  let events = ensure-handled-events(frame);
  check-true("Port terminated event handled",
             size(events) = 1 & events[0] = event);
  handled-events(frame).size := 0;
  exit-frame(frame);
  let events = ensure-handled-events(frame);
  check-true("Final frame exit event received",
             size(events) = 1 & instance?(events[0], <frame-exit-event>));
  handled-events(frame).size := 0;
  frame;
end test frame-events-test;


define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <window-repaint-event>) => ()
  record-event(sheet, event)
end method handle-event;

define test window-events-test ()
  let port = find-test-port();
  let (frame, layout, sheet) = make-event-frame();
  ignore(layout);
  let event = make(<window-repaint-event>,
		   //---- needs region and native region
		   sheet: sheet,
		   region: make-bounding-box(10, 10, 30, 40));
  distribute-event(port, event);
  let events = ensure-handled-events(sheet);
  check-true("Repaint event handled", size(events) = 1 & events[0] = event);
  frame
end test window-events-test;


define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <pointer-motion-event>) => ()
  record-event(sheet, event)
end method handle-event;

define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <pointer-enter-event>) => ()
  record-event(sheet, event)
end method handle-event;

define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <pointer-exit-event>) => ()
  record-event(sheet, event)
end method handle-event;

define test pointer-events-test ()
  let port = find-test-port();
  let (frame, layout, list-box, push-button)
     = make-event-frame(gadget-class-2: <push-button>);
  ignore(layout);
  // Should generate enter/motion on the list box
  distribute-event(port, make(<pointer-motion-event>,
		              sheet: list-box,
		              pointer: port-pointer(port),
		              x: 10, y: 10));
  // Should generate exit on the list box, then enter/motion on the push button
  distribute-event(port, make(<pointer-motion-event>,
		              sheet: push-button,
		              pointer: port-pointer(port),
		              x: 10, y: 10));
  // Should generate exit on the push button, then enter/motion on the list box
  distribute-event(port, make(<pointer-motion-event>,
		              sheet: list-box,
		              pointer: port-pointer(port),
		              x: 10, y: 10));
  //---*** Screws up Vm-tether... put it back in at some point
  // check-equal("event queues shared", 
  //             sheet-event-queue(list-box), sheet-event-queue(push-button));
  let events = ensure-handled-events(list-box);
  check-equal("list box expected number of events",
              size(events), 5);
  check-event("pointer enters list box", 0, <pointer-enter-event>,   list-box);
  check-event("pointer list box motion", 1, <pointer-motion-event>,  list-box);
  check-event("pointer exits list box",  2, <pointer-exit-event>,    list-box);
  check-event("pointer enters list box again", 0, <pointer-enter-event>,   list-box);
  check-event("pointer list box motion again", 1, <pointer-motion-event>,  list-box);
  let events = ensure-handled-events(push-button);
  check-equal("push button expected number of events",
              size(events), 3);
  check-event("pointer enters push button", 0, <pointer-enter-event>,  push-button);
  check-event("pointer push button motion", 1, <pointer-motion-event>, push-button);
  check-event("pointer exits push button",  2, <pointer-exit-event>,   push-button);
  frame;
end test pointer-events-test;


define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <key-press-event>) => ()
  record-event(sheet, event)
end method handle-event;

define test keyboard-events-test ()
  let port = find-test-port();
  let (frame, layout, sheet) = make-event-frame();
  ignore(layout);
  port-input-focus(port)   := sheet;	//--- kludge!
  frame-input-focus(frame) := sheet;	//--- kludge!
  let event = make(<key-press-event>,
		   keysym: #"a",
		   sheet: sheet);
  distribute-event(port, event);
  let events = ensure-handled-events(sheet);
  check-true("Keyboard event handled",
             size(events) = 1 & events[0] = event);
  frame
end test keyboard-events-test;


define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <button-press-event>) => ()
  record-event(sheet, event)
end method handle-event;

define method handle-event 
    (sheet :: <test-gadget-mixin>, event :: <button-release-event>) => ()
  record-event(sheet, event)
end method handle-event;

define method sheet-press-button (sheet)
  let port = port(sheet);
  let event = make(<button-press-event>,
		   sheet: sheet,
		   pointer: port-pointer(port),
		   button: $left-button,
		   x: 10, y: 10);
  distribute-event(port, event);
  let release-event = make(<button-release-event>,
		           sheet: sheet,
		           pointer: port-pointer(port),
		           button: $left-button,
		           x: 10, y: 10);
  distribute-event(port, release-event);
  handle-all-events(top-level-sheet(sheet))
end method sheet-press-button;

define variable *called-callback?* = #f;

define method test-callback (#rest args)
  *called-callback?* := #t
end method test-callback;

define method prepare-for-test-callback()
  *called-callback?* := #f
end method prepare-for-test-callback;

define method test-callback-invoked? ()
  *called-callback?*
end method test-callback-invoked?;

define method test-button-callbacks (button :: <push-button>, name)
  sheet-press-button(button);
  check-true(concatenate("Activate callback called for ", name),
             test-callback-invoked?());
end method test-button-callbacks;

define method test-button-callbacks (button :: <button>, name)
  let button-value = gadget-value(button);
  sheet-press-button(button);
  check-true(concatenate("Value changed callback called for ", name),
             test-callback-invoked?());
  check-true(concatenate("Mouse release toggles ", name),
             gadget-value(button) = ~button-value);
  sheet-press-button(button);
  check-true(concatenate("Mouse release toggles back ", name),
             gadget-value(button) = button-value);
end method test-button-callbacks;

define method test-button-clicking (button-class)
  let done-activate-callback? = #f;
  let done-value-changed-callback? = #f;
  let (frame, layout, button) 
    = make-event-frame
        (gadget-class: button-class,
         activate-callback: test-callback,
         value-changed-callback: test-callback);
  ignore(layout);
  prepare-for-test-callback();
  test-button-callbacks(button, gadget-class-name(button-class));
  frame;
end method test-button-clicking;

define method test-button-box-callbacks (box :: <push-box>, name)
  let button = gadget-box-buttons(box)[0];
  sheet-press-button(button);
  check-true(concatenate("Activate callback called for ", name),
             *called-callback?*);
end method test-button-box-callbacks;

define method test-button-box-callbacks (box :: <button-box>, name)
  let layout = sheet-child(box);
  let buttons = sheet-children(layout);
  sheet-press-button(buttons[1]);
  check-true(concatenate("Value changed callback called for ", name),
             *called-callback?*);
  check-equal(concatenate(name, " selection updated on activating button 2"),
              gadget-selection(box),
              #(1));
  sheet-press-button(buttons[2]);
  check-true(concatenate(name, " selection updated on activating button 3"),
             begin
               let selection = gadget-selection(box);
	       select (box by instance?)
		 <radio-box> => selection = #(2);
		 <check-box> => size(selection) = 2
                                  & every?(rcurry(member?, #(1, 2)), selection)
               end
             end)
end method test-button-box-callbacks;

define method test-button-box-clicking 
    (button-box-class, #key items = #("red", "green", "blue"))
  let done-activate-callback? = #f;
  let done-value-changed-callback? = #f;
  let (frame, layout, button) 
    = make-event-frame
        (gadget-class: button-box-class,
         activate-callback: test-callback,
         value-changed-callback: test-callback,
         items: items);
  ignore(layout);
  prepare-for-test-callback();
  test-button-box-callbacks(button, gadget-class-name(button-box-class));
  frame;
end method test-button-box-clicking;

define test mouse-button-events-test ()
  test-button-clicking(<push-button>);
  test-button-clicking(<radio-button>);
  test-button-clicking(<check-button>);
  test-button-box-clicking(<push-box>);
  test-button-box-clicking(<radio-box>);
  test-button-box-clicking(<check-box>);
end test mouse-button-events-test;


/// Define the events test suite

define suite duim-events-suite ()
  test timer-events-test;
  test frame-events-test;
  test window-events-test;
  test pointer-events-test;
  test keyboard-events-test;
  test mouse-button-events-test;
end suite duim-events-suite;
