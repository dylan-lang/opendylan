Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Input Sheets and Events

/// Event protocol
define protocol <<event-protocol>> ()
  // General events
  getter event-timestamp (event :: <event>) => (integer);
  getter event-client (event :: <event>) => (client);
  getter event-modifier-state (event :: <event>) => (integer);
  // Pointer events and some gadget events, too
  getter event-button (event :: <event>) => (integer);
  getter event-x (event :: <event>) => (x);
  getter event-y (event :: <event>) => (y);
  // Pointer events
  getter event-pointer (event :: <pointer-event>) => (pointer);
  getter boundary-event-kind (event :: <pointer-event>) => (keyword);
  // Keyboard events and some gadget events, too
  getter event-character (event :: <event>) => (char :: false-or(<character>));
  getter event-key-name (event :: <event>) => (keysym);
  // Window events
  getter event-region (event :: <window-event>) => (region :: <region>);
end protocol <<event-protocol>>;


/// Event types

define locked variable *event-timestamp* :: <integer> = 0;

define inline function next-event-timestamp () => (timestamp :: <integer>)
  atomic-increment!(*event-timestamp*)
end function next-event-timestamp;


//--- We could implement an event resource by adding a 'make' method
//--- on <basic-event>, but that would require explicit deallocation
define open abstract primary class <basic-event> (<event>)
  sealed constant slot event-timestamp = next-event-timestamp(),
    init-keyword: timestamp:;
end class <basic-event>;

define method event-client (event :: <basic-event>) => (client)
  #f
end method event-client;


define open abstract primary class <sheet-event> (<basic-event>)
  sealed slot event-sheet :: false-or(<sheet>),
    required-init-keyword: sheet:;
end class <sheet-event>;

define method event-client
    (event :: <sheet-event>) => (sheet :: false-or(<sheet>))
  event-sheet(event)
end method event-client;


define open abstract primary class <frame-event> (<basic-event>)
  sealed slot event-frame :: false-or(<frame>),
    required-init-keyword: frame:;
end class <frame-event>;

define method event-client
    (event :: <frame-event>) => (frame :: false-or(<frame>))
  event-frame(event)
end method event-client;

// This event gets sent when the entire port gets shut down
define sealed class <port-terminated-event> (<frame-event>)
  sealed constant slot port-terminated-condition,
    required-init-keyword: condition:;
end class <port-terminated-event>;


// Note that the 'event-modifier-state' records the state at the time that
// the event occurred, and hence can be different from 'port-modifier-state'
define open abstract primary class <device-event> (<sheet-event>)
  sealed constant slot event-modifier-state :: <integer> = 0,
    init-keyword: modifier-state:;
end class <device-event>;

define inline function event-matches-modifiers?
    (event :: <device-event>, #rest modifiers) => (matches? :: <boolean>)
  dynamic-extent(modifiers);
  ~zero?(logand(event-modifier-state(event), apply(make-modifier-state, modifiers)))
end function event-matches-modifiers?;


define open abstract primary class <input-focus-event> (<sheet-event>)
end class <input-focus-event>;

define sealed class <input-focus-in-event> (<input-focus-event>)
end class <input-focus-in-event>;

define sealed class <input-focus-out-event> (<input-focus-event>)
end class <input-focus-out-event>;


define open abstract primary class <keyboard-event> (<device-event>)
  sealed constant slot event-key-name = #f,
    init-keyword: key-name:;
  // This is #f for keyboard events that don't correspond to
  // characters in the standard character set
  sealed constant slot event-character :: false-or(<character>) = #f,
    init-keyword: character:;
end class <keyboard-event>;

define sealed class <key-press-event> (<keyboard-event>)
end class <key-press-event>;

define sealed class <key-release-event> (<keyboard-event>)
end class <key-release-event>;


define open abstract primary class <pointer-event> (<device-event>)
  sealed constant slot event-x :: false-or(<integer>) = #f,
    init-keyword: x:;
  sealed constant slot event-y :: false-or(<integer>) = #f,
    init-keyword: y:;
  sealed constant slot event-pointer :: false-or(<pointer>),
    required-init-keyword: pointer:;
end class <pointer-event>;

// Note that the 'event-button' records the button state at the time that
// the event occurred, and hence can be different from 'pointer-button-state'
define open abstract primary class <pointer-button-event> (<pointer-event>)
  sealed constant slot event-button = #f,
    init-keyword: button:;
end class <pointer-button-event>;

define sealed class <button-press-event> (<pointer-button-event>)
end class <button-press-event>;

define sealed class <button-release-event> (<pointer-button-event>)
end class <button-release-event>;

// This gets generated when a button press is seen within a certain (small)
// amount of time of a previous button press.  Generating a double click event
// resets the clock, so that the next press will be an ordinary one.
define sealed class <double-click-event> (<button-press-event>)
end class <double-click-event>;

define sealed class <pointer-motion-event> (<pointer-event>)
end class <pointer-motion-event>;

// Like pointer motion, but with a button held down
define sealed class <pointer-drag-event>
    (<pointer-motion-event>, <pointer-button-event>)
end class <pointer-drag-event>;

define constant <boundary-event-kind>
    = one-of(#"ancestor", #"virtual", #"inferior",
	     #"nonlinear", #"nonlinear-virtual", #f);

define sealed class <pointer-boundary-event> (<pointer-motion-event>)
  sealed constant slot boundary-event-kind :: <boundary-event-kind> = #f,
    init-keyword: kind:;
end class <pointer-boundary-event>;

define sealed class <pointer-enter-event> (<pointer-boundary-event>)
end class <pointer-enter-event>;

define sealed class <pointer-exit-event> (<pointer-boundary-event>)
end class <pointer-exit-event>;


define open abstract primary class <window-event> (<sheet-event>)
  sealed constant slot event-region :: <region>,
    required-init-keyword: region:;
end class <window-event>;

define sealed class <window-configuration-event> (<window-event>)
end class <window-configuration-event>;

define sealed class <window-repaint-event> (<window-event>)
end class <window-repaint-event>;


define sealed class <timer-event> (<frame-event>)
end class <timer-event>;


/// Seal some domains for the concrete event classes

define sealed domain make (singleton(<port-terminated-event>));
define sealed domain initialize (<port-terminated-event>);

define sealed domain make (singleton(<input-focus-in-event>));
define sealed domain initialize (<input-focus-in-event>);

define sealed domain make (singleton(<input-focus-out-event>));
define sealed domain initialize (<input-focus-out-event>);

define sealed domain make (singleton(<key-press-event>));
define sealed domain initialize (<key-press-event>);

define sealed domain make (singleton(<key-release-event>));
define sealed domain initialize (<key-release-event>);

define sealed domain make (singleton(<button-press-event>));
define sealed domain initialize (<button-press-event>);

define sealed domain make (singleton(<button-release-event>));
define sealed domain initialize (<button-release-event>);

define sealed domain make (singleton(<double-click-event>));
define sealed domain initialize (<double-click-event>);

define sealed domain make (singleton(<pointer-motion-event>));
define sealed domain initialize (<pointer-motion-event>);

define sealed domain make (singleton(<pointer-drag-event>));
define sealed domain initialize (<pointer-drag-event>);

define sealed domain make (singleton(<pointer-enter-event>));
define sealed domain initialize (<pointer-enter-event>);

define sealed domain make (singleton(<pointer-exit-event>));
define sealed domain initialize (<pointer-exit-event>);

define sealed domain make (singleton(<window-configuration-event>));
define sealed domain initialize (<window-configuration-event>);

define sealed domain make (singleton(<window-repaint-event>));
define sealed domain initialize (<window-repaint-event>);

define sealed domain make (singleton(<timer-event>));
define sealed domain initialize (<timer-event>);


/// Event processing and handling

define protocol <<input-sheet-protocol>> (<<sheet-protocol>>)
  getter sheet-event-queue
    (sheet :: <abstract-sheet>) => (queue :: false-or(<event-queue>));
  setter sheet-event-queue-setter
    (queue :: false-or(<event-queue>), sheet :: <abstract-sheet>)
 => (queue :: false-or(<event-queue>));
end protocol <<input-sheet-protocol>>;

// A "client" is typically either a sheet or a frame
define protocol <<event-handling-protocol>> ()
  // This is called from the port event-processing loop, and run in the
  // event thread (if there is one).  It decides which sheet the event
  // should be dispatched to, and generates crossing events if necessary.
  function distribute-event
    (port :: false-or(<abstract-port>), event :: <event>) => ();
  function do-distribute-event
    (port :: <abstract-port>, event :: <event>) => ();
  // This is called by 'distribute-event' to actually send the event to
  // right sheet in an asynchronous fashion.  This might entail handling
  // the event immediately, or queuing it for handling in the user thread.
  function dispatch-event    (client, event :: <event>) => ();
  function do-dispatch-event (client, event :: <event>) => ();
  // Usually called from 'dispatch-event', which means that the most common
  // way of dealing with the event "in an asynchronous fashion" is to postpone
  // dealing with it until you can deal with it "in a synchronous fashion".
  // The main contract of 'queue-event' is to synchronize events with the
  // application progress, by putting the events in a queue.
  function queue-event (client, event) => ();
  // "Do what needs to be done" to handle the event.
  getter event-handler
    (client) => (handler :: false-or(<event-handler>));
  setter event-handler-setter
    (handler :: false-or(<event-handler>), client)
 => (handler :: false-or(<event-handler>));
  function handle-event
    (handler :: <event-handler>, event :: <event>) => ();
  function handle-button-event
    (handler :: <event-handler>, event :: <pointer-button-event>, button) => ();
  function sheet-handles-events?
    (sheet :: <abstract-sheet>) => (true? :: <boolean>);
  function sheet-handles-keyboard?
    (sheet :: <abstract-sheet>) => (true? :: <boolean>);
  // Primitive event queue management
  function read-event (sheet) => (event :: <event>);
  function read-event-no-hang (sheet) => (event :: false-or(<event>));
  function peek-event (sheet, #key event-class) => (event :: false-or(<event>));
  function unread-event (sheet, event) => ();
  function event-pending? (sheet) => (true? :: <boolean>);
  function wait-for-event (sheet, #key timeout) => (timed-out? :: <boolean>);
  // Force the back-end to generate a null event so that the event loop runs
  function generate-trigger-event
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => ();
end protocol <<event-handling-protocol>>;


define method generate-trigger-event
    (port :: <basic-port>, sheet :: <sheet>) => ()
  #f
end method generate-trigger-event;


define method handle-event
    (handler :: <event-handler>, event :: <event>) => ()
  ignore(handler, event);
  /* warn("Ignoring event %= for event handler %=", handler, event); */
  #f
end method handle-event;

define method handle-event
    (handler :: <event-handler>, event :: <pointer-button-event>) => ()
  handle-button-event(handler, event, event-button(event))
end method handle-event;

// Default method does nothing, but users can do better...
define method handle-button-event
    (handler :: <event-handler>, event :: <pointer-button-event>, button) => ()
  #f
end method handle-button-event;

// Default method on non-sheet event handlers does nothing
define method handle-event 
    (handler :: <event-handler>, event :: <window-repaint-event>) => ()
  #f
end method handle-event;

// Default method on sheets repaints the sheet, which is usually the right thing
define method handle-event 
    (sheet :: <sheet>, event :: <window-repaint-event>) => ()
  repaint-sheet(sheet, event-region(event))
end method handle-event;

define method handle-event
    (handler :: <event-handler>, event :: <port-terminated-event>) => ()
  error("The port for frame %= died unexpectedly", event-frame(event))
end method handle-event;


// Having the event queue (and handler) in each sheet allows different
// sheets in the same frame to have different event queues and handlers.
//--- Would it be better to just get the event queue from the frame?
define open abstract class <sheet-with-event-queue-mixin> (<abstract-sheet>)
  sealed slot sheet-event-queue :: false-or(<event-queue>) = #f,
    init-keyword: event-queue:;
  sealed slot event-handler :: false-or(<event-handler>) = #f,
    init-keyword: event-handler:;
end class <sheet-with-event-queue-mixin>;

define method initialize
    (sheet :: <sheet-with-event-queue-mixin>, #key)
  next-method();
  unless (event-handler(sheet))
    event-handler(sheet) := sheet
  end
end method initialize;

define thread-slot event-handler :: false-or(<event-handler>) of type-union(<abstract-sheet>, <abstract-frame>);

define method event-handler
    (sheet :: <sheet>) => (handler :: <sheet>)
  sheet
end method event-handler;


define method sheet-handles-events?
    (sheet :: <sheet>) => (true? :: <boolean>)
  #f
end method sheet-handles-events?;

define method sheet-handles-events?
    (sheet :: <sheet-with-event-queue-mixin>) => (true? :: <boolean>)
  #t
end method sheet-handles-events?;


define method sheet-handles-keyboard?
    (sheet :: <sheet>) => (true? :: <boolean>)
  #f
end method sheet-handles-keyboard?;


define method dispatch-event (client, event :: <event>) => ()
  do-dispatch-event(client, event)
end method dispatch-event;

define method dispatch-event
    (sheet :: <sheet-with-event-queue-mixin>, event :: <event>) => ()
  do-dispatch-event(sheet, event)
end method dispatch-event;

define method do-note-sheet-attached
    (sheet :: <sheet-with-event-queue-mixin>) => ()
  next-method();
  unless (sheet-event-queue(sheet))
    //--- Maybe instead share the event queue with the nearest parent?
    let top-sheet = top-level-sheet(sheet);
    let queue = top-sheet & sheet-event-queue(top-sheet);
    sheet-event-queue(sheet) := queue
  end
end method do-note-sheet-attached;

define method queue-event
    (sheet :: <sheet>, event :: <event>) => ()
  warn("Ignoring event %= for client %=", event, sheet);
  #f
end method queue-event;

define method queue-event
    (sheet :: <sheet-with-event-queue-mixin>, event :: <event>) => ()
  // Push the new event onto the tail of the event queue,
  // or handle the event directly if there is no queue
  let queue = sheet-event-queue(sheet);
  if (queue)
    event-queue-push-last(queue, event)
  else
    handle-event(event-handler(sheet), event)
  end
end method queue-event;


/// Sheet event queue hacking

define method read-event
    (sheet :: <sheet-with-event-queue-mixin>) => (event :: <event>)
  // Get the first event in the event queue
  wait-for-event(sheet);
  let queue :: <event-queue> = sheet-event-queue(sheet);
  let event = event-queue-pop(queue);
  unless (instance?(event, <pointer-motion-event>))
    duim-debug-message("  Read event %=, client %=", event, event-client(event))
  end;
  event
end method read-event;

define method unread-event
    (sheet :: <sheet-with-event-queue-mixin>, event) => ()
  // Put the event back at the head of the queue
  let queue :: <event-queue> = sheet-event-queue(sheet);
  event-queue-push(queue, event)
end method unread-event;

define method peek-event
    (sheet :: <sheet-with-event-queue-mixin>, #key event-class)
 => (event :: false-or(<event>))
  wait-for-event(sheet);
  let queue :: <event-queue> = sheet-event-queue(sheet);
  let event = event-queue-top(queue);
  when (~event-class | instance?(event, event-class))
    event
  end
end method peek-event;

define method read-event-no-hang
    (sheet :: <sheet-with-event-queue-mixin>) => (event :: false-or(<event>))
  //--- This knows too much about event queues, but we need to avoid race conditions
  let queue :: <event-queue> = sheet-event-queue(sheet);
  with-lock (associated-lock(queue.%non-empty))
    unless (empty?(queue.%deque))
      pop(queue.%deque)
    end
  end
end method read-event-no-hang;

define method event-pending?
    (sheet :: <sheet-with-event-queue-mixin>) => (true? :: <boolean>)
  let queue :: <event-queue> = sheet-event-queue(sheet);
  ~event-queue-empty?(queue)
end method event-pending?;

define method wait-for-event
    (sheet :: <sheet-with-event-queue-mixin>, #key timeout)
 => (timed-out? :: <boolean>)
  let _port = port(sheet);
  until (event-pending?(sheet))
    select (port-event-processor-type(_port))
      #"n" =>
	// Single threaded, so run the event processing loop right here
	process-next-event(_port, timeout: timeout);
      #"n+1" =>
	event-queue-wait(sheet-event-queue(sheet), timeout: timeout);
      #"2n" =>
	event-queue-wait(sheet-event-queue(sheet), timeout: timeout);
    end
  end
end method wait-for-event;


/// Standard input

// Standard input model is to queue most events,
// but to handle window configuration events immediately
define open abstract class <standard-input-mixin> (<sheet-with-event-queue-mixin>)
end class <standard-input-mixin>;

define method do-dispatch-event
    (sheet :: <standard-input-mixin>, event :: <event>) => ()
  queue-event(sheet, event)
end method do-dispatch-event;

define method do-dispatch-event
    (sheet :: <standard-input-mixin>, event :: <window-configuration-event>) => ()
  when (sheet-direct-mirror(sheet))
    // Configuration events on mirrored sheets must update the mirror
    note-mirror-geometry-changed(port(sheet), sheet, event-region(event))
  end;
  handle-event(event-handler(sheet), event)
end method do-dispatch-event;


/// Immediate input

define open abstract class <immediate-input-mixin> (<sheet-with-event-queue-mixin>)
end class <immediate-input-mixin>;

define method do-dispatch-event
    (sheet :: <immediate-input-mixin>, event :: <event>) => ()
  handle-event(event-handler(sheet), event)
end method do-dispatch-event;


/// Delegate input

define open abstract class <delegate-input-mixin> (<abstract-sheet>)
  sealed slot delegate-sheet-delegate = #f,
    init-keyword: delegate:;
end class <delegate-input-mixin>;

define method do-dispatch-event
    (sheet :: <delegate-input-mixin>, event :: <event>) => ()
  let delegate = delegate-sheet-delegate(sheet);
  when (delegate)
    dispatch-event(delegate, event)
  end
end method do-dispatch-event;


/// Null input

define open abstract class <null-input-mixin> (<abstract-sheet>)
end class <null-input-mixin>;

define method do-dispatch-event
    (sheet :: <null-input-mixin>, event :: <event>) => ()
  #f
end method do-dispatch-event;


/// Default input behavior

define method do-dispatch-event
    (sheet :: <sheet>, event :: <event>) => ()
  let parent = sheet-parent(sheet);
  when (parent)
    // If this sheet doesn't handle events, maybe its parent does...
    event-sheet(event) := parent;
    do-dispatch-event(parent, event)
  end
end method do-dispatch-event;


/// Repaint protocol

define protocol <<repaint-handling-protocol>> ()
  function repaint-sheet
    (sheet :: <abstract-sheet>, region :: <region>, #key medium, force?) => ();
  function queue-repaint
    (sheet :: <abstract-sheet>, region :: <region>) => ();
  function do-queue-repaint
    (port :: <abstract-port>, sheet :: <abstract-sheet>, region :: <region>) => ();
  function handle-repaint
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>, region :: <region>) => ();
  // When this returns #f, it means that 'repaint-sheet' will never call
  // 'handle-repaint' to paint this sheet
  function sheet-handles-repaint?
    (sheet :: <abstract-sheet>) => (true? :: <boolean>);
  function do-with-atomic-redisplay
    (sheet :: <abstract-sheet>, continuation :: <function>)
 => (#rest values);
end protocol <<repaint-handling-protocol>>;

// REGION is in the coordinate system of SHEET
define method repaint-sheet
    (sheet :: <sheet>, region :: <region>, #key medium, force?) => ()
  // Don't try to repaint before the whole hierarchy is complete
  when (sheet-mapped?(sheet) | force?)
    unless (region-empty?(region))
      when (sheet-handles-repaint?(sheet))
	with-caret-hidden (sheet)
	  let repaint-region
	    = if (everywhere?(region)) sheet-region(sheet) else region end;
	  if (medium)
	    let old-transform = medium-device-transform(medium);
	    block ()
	      medium-device-transform(medium) := sheet-device-transform(sheet);
	      handle-repaint(sheet, medium, repaint-region)
	    cleanup
	      medium-device-transform(medium) := old-transform;
	    end
	  else
	    with-sheet-medium (medium = sheet)
	      handle-repaint(sheet, medium, repaint-region)
	    end
	  end
	end
      end;
      when (~empty?(sheet-children(sheet)))
	repaint-children(sheet, region, medium: medium, force?: force?)
      end
    end
  end
end method repaint-sheet;

define method repaint-children
    (sheet :: <sheet>, region :: <region>, #key medium, force?) =>()
  let _port = port(sheet);
  for (child :: <basic-sheet> in sheet-children(sheet))
    unless (port-handles-repaint?(_port, child))
      let transform = sheet-transform(child);
      let child-region = transform-region(transform, sheet-region(child));
      let intersecting-region = region-intersection(region, child-region);
      unless (region-empty?(intersecting-region))
        let exposed-region = untransform-region(transform, intersecting-region);
	repaint-sheet(child, exposed-region,
		      medium: medium, force?: force?)
      end
    end
  end
end method repaint-children;

// When the region is a bounding box, we cons fewer intermediate regions
define method repaint-children
    (sheet :: <sheet>, region :: <bounding-box>, #key medium, force?) => ()
  let _port = port(sheet);
  let intersecting-region :: <general-box>
    = make(<general-box>, left: 0, top: 0, right: 0, bottom: 0);
  let (l1, t1, r1, b1) = box-edges(region);
  for (child :: <basic-sheet> in sheet-children(sheet))
    unless (port-handles-repaint?(_port, child))
      let transform = sheet-transform(child);
      let (cl, ct, cr, cb) = box-edges(sheet-region(child));
      let (l2, t2, r2, b2) = transform-box(transform, cl, ct, cr, cb);
      let (intersects?, l3, t3, r3, b3)
	= ltrb-intersects-ltrb?(l1, t1, r1, b1, l2, t2, r2, b2);
      if (intersects?)
	let (l4, t4, r4, b4) = untransform-box(transform, l3, t3, r3, b3);
	set-box-edges(intersecting-region, l4, t4, r4, b4);
	repaint-sheet(child, intersecting-region, 
		      medium: medium, force?: force?)
      end
    end
  end
end method repaint-children;

define method queue-repaint
    (sheet :: <sheet>, region :: <region>) => ()
  let _port = port(sheet);
  when (_port)
    do-queue-repaint(_port, sheet, region)
  end
end method queue-repaint;

define method handle-repaint
    (sheet :: <sheet>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  #f
end method handle-repaint;


define open abstract class <sheet-with-repainting-mixin> (<abstract-sheet>)
end class <sheet-with-repainting-mixin>;

define method sheet-handles-repaint?
    (sheet :: <sheet>) => (true? :: <boolean>)
  #f
end method sheet-handles-repaint?;

define method sheet-handles-repaint?
    (sheet :: <sheet-with-repainting-mixin>) => (true? :: <boolean>)
  #t
end method sheet-handles-repaint?;


/// Standard repainting

define open abstract class <standard-repainting-mixin> (<sheet-with-repainting-mixin>)
end class <standard-repainting-mixin>;

define method queue-repaint
    (sheet :: <standard-repainting-mixin>, region :: <region>) => ()
  let _port = port(sheet);
  when (_port)
    do-queue-repaint(_port, sheet, region)
  end
end method queue-repaint;


/// Immediate repainting

define open abstract class <immediate-repainting-mixin> (<sheet-with-repainting-mixin>)
end class <immediate-repainting-mixin>;

define method queue-repaint
    (sheet :: <immediate-repainting-mixin>, region :: <region>) => ()
  repaint-sheet(sheet, region)
end method queue-repaint;


/// Null repainting

define open abstract class <null-repainting-mixin> (<abstract-sheet>)
end class <null-repainting-mixin>;

define method queue-repaint
    (sheet :: <null-repainting-mixin>, region :: <region>) => ()
  #f
end method queue-repaint;

define method handle-repaint
    (sheet :: <null-repainting-mixin>, medium :: <medium>, region :: <region>) => ()
  #f
end method handle-repaint;


/// Default repainting

// Default method just queues a DUIM repaint event
define method do-queue-repaint
    (_port :: <port>, sheet :: <sheet>, region :: <region>) => ()
  //--- This could try to merge the new damage region with any previously
  //--- queued repaint events
  let (left, top, right, bottom) = box-edges(region);
  queue-event(sheet, make(<window-repaint-event>,
			  sheet: sheet,
			  region: make-bounding-box(left, top, right, bottom)))
end method do-queue-repaint;


/// "Atomic" redisplay

// Default method is a no-op
define method do-with-atomic-redisplay
    (sheet :: <basic-sheet>, continuation :: <function>)
 => (#rest values)
  continuation(sheet)
end method do-with-atomic-redisplay;


/// Crossing events

// We have fun here to generate enter/exit events for unmirrored sheets.
// Three cases:
// 1. Mouse motion event:
//    [Last mirrored sheet, position] --> [new mirrored sheet, position]
// 2. Enter: [last-mirrored sheet, position] -> [new sheet]
// 3. Exit: [last-mirrored sheet, position] -> [new sheet]
//
// Note that exit events on the parent are generated when the pointer goes
// from a parent to a child, and that enter events on the parent are generated
// when the pointer goes back from the child to the parent again.  If these
// events are not interesting, the user should ignore all pointer enter and
// exit events that have a kind of #"inferior".
//
// We boil this down to mouse moved to [new mirrored sheet, position]
//  We have a current stack of sheets 0...n
//  Find m such that 0...m - 1 contain the mouse
//  m...n require exit events
// We can then go deep down inside m sheet generating enter events.
// When we exit, the trace stack has been updated.
//
// The 0th element of the trace is the top level mirrored sheet.
//
// Cases:
// 1. It is the mirrored sheet.
// 2. Ancestor: We have gone deeper.  Perhaps it still counts as exiting.
//    We should just exit from the innermost sheet that encloses the new mirror.
//    We should enter/exit intermediate ancestors.
//    Ancestor-x,y,z --> sheet
// 3. Child: We have gone shallower.  Exit from all of the sheets.
//    We should probably exit from intermediate descendents.
//    Sheet-x,y,z --> child
// 4. Share a common ancestor: exit from all of the sheets.

// This supports deep mirroring, that is, ports where there can be mirrors at
// any level of the sheet hierarchy.  There's a lot of seemingly unnecessary hair
// in this code because a sheet's region isn't always definitively correct...
define method generate-crossing-events 
    (_port :: <basic-port>, event :: <pointer-event>) => (sheet :: false-or(<sheet>))
  with-port-locked (_port)
    let mirrored-sheet = event-sheet(event);
    let top-sheet = mirrored-sheet;
    // X and Y are in "native" coordinates, that is, in the coordinates
    // of the mirrored sheet
    let x = event-x(event);
    let y = event-y(event);
    let modifiers = event-modifier-state(event);
    let pointer = event-pointer(event);
    let mirrored-sheet-ancestors
      = begin
	  let sheets :: <stretchy-object-vector> = _port.%ancestor-stack;
	  sheets.size := 0;
	  //---*** Suspicious! Should maybe use 'sheet-device-parent'
	  for (sheet = sheet-parent(mirrored-sheet) then sheet-parent(sheet),
	       // Null test in case the event thread is running behind and
	       // the mouse moved off of all known sheets in the meanwhile
	       until: display?(sheet) | ~sheet)
	    add!(sheets, sheet);
	  finally
	    sheets
	  end
	end;
    local method generate-enter-event (sheet, kind, update?) => ()
	    when (update?)
	      update-focus-for-enter-event(_port, sheet, pointer)
	    end;
	    dispatch-event(sheet, make(<pointer-enter-event>,
				       sheet: sheet,
				       x: x, y: y,
				       kind: kind,
				       modifier-state: modifiers,
				       pointer: pointer))
	  end method,
          method generate-exit-event (sheet, kind, update?) => ()
	    when (update?)
	      update-focus-for-exit-event(_port, sheet, pointer)
	    end;
	    dispatch-event(sheet, make(<pointer-exit-event>,
				       sheet: sheet,
				       x: x, y: y,
				       kind: kind,
				       modifier-state: modifiers,
				       pointer: pointer))
	  end method;
    // Walk from the mirrored sheet up to the top level sheet,
    // getting X and Y into the top level sheet's coordinate system
    unless (display?(sheet-parent(mirrored-sheet)))
      let new-x = x;
      let new-y = y;
      block (return)
	local method transform-xy (sheet :: <basic-sheet>) => ()
		when (display?(sheet-parent(sheet)))
		  x := new-x;
		  y := new-y;
		  top-sheet := sheet;
		  return()
		end;
		let (_new-x, _new-y)
		  = transform-position(sheet-transform(sheet), new-x, new-y);
		new-x := _new-x;
		new-y := _new-y
	      end method;
	transform-xy(mirrored-sheet);
	for (sheet in mirrored-sheet-ancestors)
	  transform-xy(sheet)
	end
      end
    end;
    let trace-stack :: <stretchy-object-vector> = _port.%trace-stack;
    let exited-sheet?
      = instance?(event, <pointer-exit-event>)
	& ~(boundary-event-kind(event) == #"inferior");
    let entered-from-child?
      = instance?(event, <pointer-enter-event>)
	& boundary-event-kind(event) == #"inferior";
    let exited-top-level-sheet?
      = exited-sheet? & top-sheet == mirrored-sheet;
    when (~exited-sheet? | exited-top-level-sheet?)
      // Pop up the stack of sheets
      unless (empty?(trace-stack))
	let m :: <integer>
	  = if (~exited-top-level-sheet? & trace-stack[0] == top-sheet)
	      let new-x = x;
	      let new-y = y;
	      let mirrored-sheet-in-trace = position(trace-stack, mirrored-sheet);
	      let sheet = #f;
	      when (mirrored-sheet-in-trace & mirrored-sheet-in-trace > 1)
		without-bounds-checks
		  for (i :: <integer> = 1 then i + 1,
		       until: i > mirrored-sheet-in-trace)
		    let (_new-x, _new-y)	// transform to child coordinate space
		      = untransform-position(sheet-transform(trace-stack[i]), new-x, new-y);
		    new-x := _new-x;
		    new-y := _new-y
		  end
		end
	      end;
	      block (return)
		without-bounds-checks
		  for (i :: <integer>
			 = if (mirrored-sheet-in-trace) mirrored-sheet-in-trace + 1 else 0 end
			 then i + 1,
		       until: i >= size(trace-stack))
		    sheet := trace-stack[i];
		    unless (zero?(i))
		      let (_new-x, _new-y)	// transform to child coordinate space
			= untransform-position(sheet-transform(sheet), new-x, new-y);
		      new-x := _new-x;
		      new-y := _new-y
		    end;
		    when ((~mirrored-sheet-in-trace
			   & ~member?(sheet, mirrored-sheet-ancestors))
			  | ~sheet-mapped?(sheet)
			  | ~region-contains-position?(sheet-region(sheet), new-x, new-y))
		      return(i)
		    end;
		  finally
		    size(trace-stack)
		  end
	        end
	      end
	    else
	      0
	    end;
	without-bounds-checks
	  let n :: <integer> = size(trace-stack) - 1;
          for (i :: <integer> = n then i - 1, until: i < m)
	    generate-exit-event(trace-stack[i], #"ancestor", i = n);
	    unless (zero?(i))
	      generate-enter-event(trace-stack[i - 1], #"inferior", i = m)
	    end
	  end
	end;
	trace-stack.size := m
      end
    end;
    when (~exited-sheet?
	  & ~entered-from-child?
	  & region-contains-position?(sheet-region(top-sheet), x, y))
      // If it's empty initialize it with the top level sheet
      when (empty?(trace-stack))
	add!(trace-stack, top-sheet);
	generate-enter-event(top-sheet, #"ancestor", #t)
      end;
      // Now add all sheets between the last sheet on the trace
      // and the mirrored sheet
      unless (position(trace-stack, mirrored-sheet))
	let last-sheet = trace-stack[size(trace-stack) - 1];
	let sheets = #();
	let n :: <integer> = 0;
	for (sheet = mirrored-sheet then sheet-parent(sheet),
	     until: sheet == last-sheet
		    | when (display?(sheet))
			sheets := #();
			#t
		      end)
	  push!(sheets, sheet);
	  inc!(n)
	end;
	for (sheet in sheets,
	     i :: <integer> from 0,
	     first? = #t then #f)
	  generate-exit-event(trace-stack[size(trace-stack) - 1], #"inferior", first?);
	  generate-enter-event(sheet, #"ancestor", i = n);
	  add!(trace-stack, sheet)
	end
      end;
      // We have to get the sheets into the correct coordinate space
      without-bounds-checks
	for (i :: <integer> from 1 below size(trace-stack))
	  let (_x, _y)	// transform to child coordinate space
	    = untransform-position(sheet-transform(trace-stack[i]), x, y);
	  x := _x;
	  y := _y
	end
      end;
      // Finally add progeny of the mirrored sheet
      let new-x = x;
      let new-y = y;
      let sheet = trace-stack[size(trace-stack) - 1];
      let child = #f;
      block (return)
	while (#t)
	  when (empty?(sheet-children(sheet)))
	    return(#f)
	  end;
	  child := child-containing-position(sheet, new-x, new-y);
	  unless (child)
	    return(#f)
	  end;
	  generate-exit-event(sheet, #"inferior", #t);
	  generate-enter-event(child, #"ancestor", #t);
	  let (_new-x, _new-y)	// transform to child coordinate space
	    = untransform-position(sheet-transform(child), new-x, new-y);
	  new-x := _new-x;
	  new-y := _new-y;
	  sheet := child;
	  add!(trace-stack, child)
	end
      end
    end;
    // Return the top sheet on the trace stack
    ~empty?(trace-stack) & trace-stack[size(trace-stack) - 1]
  end
end method generate-crossing-events;


/// Event distribution

// This and the other methods here could be on <port>, but we keep them
// on <basic-port> so that we can get fast slot access
define method distribute-event
    (_port :: <basic-port>, event :: <event>) => ()
  unless (instance?(event, <pointer-motion-event>))
    duim-debug-message("Distributing event %= for %=", event, event-client(event))
  end;
  do-distribute-event(_port, event)
end method distribute-event;

define method distribute-event (_port == #f, event :: <event>) => ()
  warn("Trying to distribute event %= for a null port", event)
end method distribute-event;

define method do-distribute-event
    (_port :: <basic-port>, event :: <event>) => ()
  // Catch all for most events, including <sheet-event> and <frame-event>
  dispatch-event(event-client(event), event)
end method do-distribute-event;

define method do-distribute-event
    (_port :: <basic-port>, event :: <keyboard-event>) => ()
  // Keyboard events go to the sheet with the input focus
  let sheet = event-sheet(event);
  let focus = sheet-input-focus(port-input-focus(_port) | sheet);
  event-sheet(event) := focus;
  dispatch-event(focus, event)
end method do-distribute-event;

define method do-distribute-event
    (_port :: <basic-port>, event :: <pointer-event>) => ()
  // First generate all the correct enter/exit events, and get
  // the trace stack updated
  let pointer  = event-pointer(event);
  let grabbed? = pointer-grabbed?(pointer);
  let sheet    = grabbed? | generate-crossing-events(_port, event);
  // Now dispatch this event to the topmost sheet.
  // X and Y are in "native" coordinates, that is, in the coordinates
  // of the mirrored sheet
  //--- This is not quite right.  We need to transform the coordinates better.
  //--- Also it should probably override the sheet in the trace-stack.
  when (sheet & sheet-mapped?(sheet))
    let x = event-x(event);
    let y = event-y(event);
    let modifiers = event-modifier-state(event);
    let (tx, ty)
      = untransform-position(sheet-device-transform(sheet), x, y);
    // Update the pointer object the primitive way
    pointer.%x-position := tx;
    pointer.%y-position := ty;
    pointer.%position-changed? := #t;
    select (event by instance?)
      <pointer-enter-event> =>
	update-focus-for-enter-event(_port, sheet, pointer);
      <pointer-exit-event> =>
	update-focus-for-exit-event(_port, sheet, pointer);
      <pointer-drag-event> =>
	dispatch-event(sheet, make(<pointer-drag-event>,
				   sheet: sheet,
				   x: tx, y: ty,
				   modifier-state: modifiers,
				   button: event-button(event),
				   pointer: pointer));
      <pointer-motion-event> =>
	dispatch-event(sheet, make(<pointer-motion-event>,
				   sheet: sheet,
				   x: tx, y: ty,
				   modifier-state: modifiers,
				   pointer: pointer));
      <pointer-button-event> =>
	let event-class = object-class(event);
	let new-focus = sheet;
	let old-focus = port-input-focus(_port);
        when (new-focus ~== old-focus
	      & event-class == <button-press-event>
	      & port-focus-policy(_port) == #"click-to-select"
	      & sheet-handles-keyboard?(new-focus))
	  port-input-focus(_port) := new-focus;
	end;
	when (_port.%double-click-interval
	      & event-class == <button-press-event>)
	  // If the port is supposed to be generating double click
	  // events itself, this is where it happens
	  let last-time = _port.%last-button-press-time;
	  let time = get-internal-real-time();
	  if (time < last-time + _port.%double-click-interval)
	    event-class := <double-click-event>;
	    _port.%last-button-press-time := 0
	  else
	    _port.%last-button-press-time := time
	  end
	end;
	dispatch-event(sheet, make(event-class,
				   sheet: sheet,
				   x: tx, y: ty,
				   modifier-state: modifiers,
				   button: event-button(event),
				   pointer: pointer));
      otherwise => #f
    end
  end
end method do-distribute-event;

define method update-focus-for-enter-event
    (_port :: <basic-port>, sheet :: <sheet>, pointer :: <pointer>)
  let new-focus = sheet;
  let old-focus = port-input-focus(_port);
  when (new-focus ~== old-focus
	& port-focus-policy(_port) == #"sheet-under-pointer"
	& sheet-handles-keyboard?(new-focus))
    port-input-focus(_port) := new-focus;
  end;
  unless (sheet == pointer-sheet(pointer))
    // Change the cursor if we moved to a new sheet
    pointer-sheet(pointer) := sheet;
    update-pointer-cursor(pointer)
  end
end method update-focus-for-enter-event;

define method update-focus-for-exit-event
    (_port :: <basic-port>, sheet :: <sheet>, pointer :: <pointer>)
  let old-focus = port-input-focus(_port);
  when (old-focus
	& port-focus-policy(_port) == #"sheet-under-pointer")
    port-input-focus(_port) := #f
  end;
  pointer-sheet(pointer) := #f
end method update-focus-for-exit-event;


/// Function events

define sealed class <function-event> (<frame-event>)
  sealed constant slot event-function :: <function>,
    required-init-keyword: function:;
end class <function-event>;

define sealed domain make (singleton(<function-event>));
define sealed domain initialize (<function-event>);

define method handle-event
    (handler :: <event-handler>, event :: <function-event>) => ()
  event-function(event)()
end method handle-event;

define method distribute-function-event
    (sheet :: <sheet>, function :: <function>) => ()
  let _port = port(sheet);
  when (_port)
    distribute-event(_port, make(<function-event>,
				 frame: sheet-frame(sheet),
				 function: function));
    let top-sheet = top-level-sheet(sheet);
    when (top-sheet)
      generate-trigger-event(_port, top-sheet)
    end
  end
end method distribute-function-event;

define method distribute-function-event
    (frame :: <frame>, function :: <function>) => ()
  let _port = port(frame);
  when (_port)
    distribute-event(_port, make(<function-event>,
				 frame: frame,
				 function: function));
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      generate-trigger-event(_port, top-sheet)
    end
  end
end method distribute-function-event;
