Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Presentations

define protocol <<presentation>> ()
  getter presentation-object
    (presentation :: <presentation>) => (object :: <object>);
  getter presentation-type
    (presentation :: <presentation>) => (type :: <type>);
  function cursor-at-position
    (window :: <window>, x :: <integer>, y :: <integer>) => (cursor);
  function do-cursor-at-position
    (mode :: <major-mode>, window :: <window>, x :: <integer>, y :: <integer>) => (cursor);
  //---*** The division of labor between 'do-presentation-at-position',
  //---*** 'do-handle-presentation-event', and 'handle-event' is dubious...
  function presentation-at-position
    (window :: <window>, x :: <integer>, y :: <integer>,
     #key button, modifiers, event-type)
 => (presentation :: false-or(<presentation>),
     bp :: false-or(<bp>), move-point? :: <boolean>);
  function do-presentation-at-position
    (mode :: <major-mode>, window :: <window>, x :: <integer>, y :: <integer>,
     #key button, modifiers, event-type)
 => (presentation :: false-or(<presentation>),
     bp :: false-or(<bp>), move-point? :: <boolean>);
  function handle-presentation-event
    (window :: <window>, object :: <object>, type :: <type>,
     #key bp, x, y, button, modifiers, event-type) => ();
  function do-handle-presentation-event
    (mode :: <major-mode>, window :: <window>, object :: <object>, type :: <type>,
     #key bp, x, y, button, modifiers, event-type, menu-function) => ();
end protocol <<presentation>>;


define open abstract class <basic-presentation> (<presentation>)
  sealed slot presentation-object :: <object> = #f,
    init-keyword: object:;
  sealed slot presentation-type :: <type> = <object>,
    init-keyword: type:;
end class <basic-presentation>;

define sealed class <simple-presentation> (<basic-presentation>)
end class <simple-presentation>;

define sealed inline method make
    (class == <presentation>, #rest initargs, #key, #all-keys)
 => (presentation :: <presentation>)
  apply(make, <simple-presentation>, initargs)
end method make;

define sealed domain make (singleton(<simple-presentation>));
define sealed domain initialize (<simple-presentation>);


define method cursor-at-position
    (window :: <basic-window>, x :: <integer>, y :: <integer>) => (cursor)
  let buffer = window-buffer(window);
  when (buffer)
    let mode = buffer-major-mode(buffer);
    do-cursor-at-position(mode, window, x, y)
  end
end method cursor-at-position;

define method do-cursor-at-position
    (mode :: <major-mode>, window :: <basic-window>, x :: <integer>, y :: <integer>) => (cursor)
  #f			// use the window's default
end method do-cursor-at-position;


/// Simple presentation handlers

define sealed class <blank-area> (<object>)
end class <blank-area>;


define method presentation-at-position
    (window :: <basic-window>, x :: <integer>, y :: <integer>,
     #key button = $left-button, modifiers = 0, event-type = #"press")
 => (presentation :: false-or(<presentation>),
     bp :: false-or(<basic-bp>), move-point? :: <boolean>)
  let buffer = window-buffer(window);
  if (buffer)
    let mode = buffer-major-mode(buffer);
    do-presentation-at-position(mode, window, x, y, 
				button: button, modifiers: modifiers,
				event-type: event-type)
  else
    values(#f, #f, #f)
  end
end method presentation-at-position;

define method do-presentation-at-position
    (mode :: <major-mode>, window :: <basic-window>, x :: <integer>, y :: <integer>,
     #key button = $left-button, modifiers = 0, event-type = #"press")
 => (presentation :: false-or(<presentation>),
     bp :: false-or(<basic-bp>), move-point? :: <boolean>)
  let (bp, dline) = position->bp(window, x, y);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    gesture-matches?($move-gesture, button, modifiers, event-type: event-type)
    | gesture-matches?($copy-gesture, button, modifiers, event-type: event-type) =>
      // Copy and move happen on button press, but at a low level
      values(#f, bp, #t);
    menu? & dline & x > display-line-width(dline) + 10 =>
      // Mouse-right (press) on blank area gets a presentation for a menu
      values(make(<presentation>,
		  object: #f, type: <blank-area>),
	     bp, #f);
    otherwise =>
      // Everything else just gets a raw BP
      values(#f, bp, #t);
  end
end method do-presentation-at-position;


define method handle-presentation-event
    (window :: <basic-window>, object :: <object>, type :: <type>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press") => ()
  let buffer = window-buffer(window);
  when (buffer)
    let mode = buffer-major-mode(buffer);
    do-handle-presentation-event(mode, window, object, type,
				 bp: bp, x: x, y: y,
				 button: button, modifiers: modifiers,
				 event-type: event-type)
  end
end method handle-presentation-event;

define method do-handle-presentation-event
    (mode :: <major-mode>, window :: <basic-window>, object :: <object>, type :: <type>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
	  menu-function) => ()
  ignore(bp, x, y, button, modifiers, event-type, menu-function);
  #f
end method do-handle-presentation-event;


define macro with-temporary-selection
  { with-temporary-selection (?window:name, ?bp:name)
      ?:body
    selecter
      ?selecter:body
    end }
    => { begin
	   local method temporary-selection-body
		     (?window :: <basic-window>, ?bp :: <basic-bp>) => (#rest values)
		   ignore(?window, ?bp);
		   ?body
		 end method,
		 method temporary-selection-selecter
		     (?window :: <basic-window>, ?bp :: <basic-bp>) => ()
		   ignore(?window, ?bp);
		   ?selecter
		 end method;
	   do-with-temporary-selection
	     (?window, ?bp, temporary-selection-body, temporary-selection-selecter)
	 end }
end macro with-temporary-selection;

define method do-with-temporary-selection
    (window :: <basic-window>, bp :: <basic-bp>,
     continuation :: <function>, selecter :: <function>)
 => (#rest values)
  let old-mark  = window-mark(window);
  let old-point = window-point(window);
  let new-mark  = #f;
  let new-point = #f;
  let already-in-selection?
    = bp & old-mark & bp-within-interval?(bp, make-interval(old-mark, old-point));
  block ()
    unless (already-in-selection?)
      clear-mark!(window: window, redisplay?: #t);
      selecter(window, bp);
      new-mark  := window-mark(window);
      new-point := window-point(window);
      window-temporary-mark?(window) := bp;
      queue-redisplay(window, $display-point);
      redisplay-window(window)
    end;
    continuation(window, bp);
  cleanup
    window-temporary-mark?(window) := #f;
    unless (already-in-selection?)
      when (new-mark = window-mark(window)
	    & new-point = window-point(window))
	// The selection hasn't changed from the one we just made, so clear
	clear-mark!(window: window, redisplay?: #t)
      end
    end
  end
end method do-with-temporary-selection;


define method do-handle-presentation-event
    (mode :: <major-mode>, window :: <basic-window>,
     nothing, type == <blank-area>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
          menu-function = blank-area-menu) => ()
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    menu? =>
      // Pop up a menu of operations for the buffer
      let old-mark  = window-mark(window);
      let old-point = window-point(window);
      let already-in-selection?
	= bp & old-mark & bp-within-interval?(bp, make-interval(old-mark, old-point));
      unless (already-in-selection?)
	clear-mark!(window: window, redisplay?: #t)
      end;
      menu-function(window, mode, nothing, bp: bp, x: x, y: y);
    otherwise => #f;
  end
end method do-handle-presentation-event;

// So other client editors can decide what to do...
define open generic blank-area-menu
    (window :: <window>, mode :: <major-mode>, nothing, #key bp, x, y) => ();

define method blank-area-menu
    (window :: <basic-window>, mode :: <major-mode>, nothing,
     #key bp, x, y) => ()
  ignore(nothing, bp, x, y);
  let frame   = window-frame(window);
  let buffer  = window-buffer(window);
  // Other back ends might choose to do this differently...
  let command
    = choose-from-menu(window,
		       buffer-command-menu-items(mode, buffer),
		       label-key: first, value-key: second,
		       multiple-sets?: #t);
  when (command)
    execute-command(mode, frame, command)
  end
end method blank-area-menu;
