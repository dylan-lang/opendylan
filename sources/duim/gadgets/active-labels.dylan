Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Active labels

define protocol <<active-label>> ()
  function handle-semantic-event
    (frame :: <frame>, gadget :: <active-label>, object :: <object>,
     event :: <pointer-button-event>,
     #key button, modifiers, x, y) => ();
  function handle-semantic-button-event
    (frame :: <frame>, gadget :: <active-label>, object :: <object>,
     event :: <pointer-button-event>, button :: <integer>,
     #key modifiers, x, y) => ();
end protocol <<active-label>>;

define constant %label_selected  :: <integer> = #o01;
define constant %label_underline :: <integer> = #o02;

define open abstract class <active-label>
    (<popup-menu-gadget-mixin>,
     <labelled-gadget-mixin>,
     <value-gadget>,
     <basic-action-gadget>)
  sealed slot gadget-value :: <object> = #f,
    setter: %gadget-value-setter,
    init-keyword: value:;
  sealed slot %label-flags :: <integer> = 0;
end class <active-label>;

define method initialize
    (gadget :: <active-label>, #key underline? = #f)
  next-method();
  label-underline?(gadget) := underline?
end method initialize;


define method gadget-value-setter
    (value, gadget :: <active-label>, #key do-callback? = #f)
 => (value)
  ignore(do-callback?);
  gadget.%gadget-value := value
end method gadget-value-setter;

define method gadget-value-type
    (gadget :: <active-label>) => (type :: <type>)
  object-class(gadget-value(gadget))
end method gadget-value-type;


define method label-selected?
    (gadget :: <active-label>) => (selected? :: <boolean>)
  logand(gadget.%label-flags, %label_selected) = %label_selected
end method label-selected?;

define method label-selected?-setter
    (selected? :: <boolean>, gadget :: <active-label>) => (selected? :: <boolean>)
  gadget.%label-flags
    := logior(logand(gadget.%label-flags, lognot(%label_selected)),
	      if (selected?) %label_selected else 0 end);
  selected?
end method label-selected?-setter;


define method label-underline?
    (gadget :: <active-label>) => (underline? :: <boolean>)
  logand(gadget.%label-flags, %label_underline) = %label_underline
end method label-underline?;

define method label-underline?-setter
    (underline? :: <boolean>, gadget :: <active-label>) => (underline? :: <boolean>)
  gadget.%label-flags
    := logior(logand(gadget.%label-flags, lognot(%label_underline)),
	      if (underline?) %label_underline else 0 end);
  underline?
end method label-underline?-setter;


define method handle-semantic-event
    (frame :: <frame>, gadget :: <active-label>, object :: <object>,
     event :: <pointer-button-event>,
     #key x, y, button = $left-button, modifiers = 0) => ()
  handle-semantic-button-event(frame, gadget, object, event, button,
			       modifiers: modifiers, x: x, y: y)
end method handle-semantic-event;

define method handle-semantic-button-event
    (frame :: <frame>, gadget :: <active-label>, object :: <object>,
     event :: <pointer-button-event>, button :: <integer>,
     #key modifiers, x, y) => ()
  ignore(modifiers, x, y);
  #f
end method handle-semantic-button-event;


/// Concrete implementation of active labels

define sealed class <active-label-pane>
    (<active-label>,
     <standard-input-mixin>,
     <standard-repainting-mixin>,
     <cached-space-requirement-mixin>,
     <leaf-layout-mixin>,
     <basic-sheet>)
  keyword cursor: = #"hand";
end class <active-label-pane>;

define method class-for-make-pane
    (framem :: <frame-manager>, class == <active-label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<active-label-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<active-label-pane>));
define sealed domain initialize (<active-label-pane>);


/// Layout

define method do-compose-space
    (gadget :: <active-label-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let offset :: <integer> = 1;
  let (width, height) = gadget-label-size(gadget);
  make(<space-requirement>,
       width:  width + offset * 2,
       height: height)
end method do-compose-space;

// Draw the active label, highlighting it in inverse video if it's selected
define method handle-repaint
    (gadget :: <active-label-pane>, medium :: <medium>, region :: <region>) => ()
  let offset :: <integer> = 1;
  let selected? = label-selected?(gadget);
  let (left, top, right, bottom) = box-edges(gadget);
  let brush = if (selected?) $foreground else $background end;
  with-drawing-options (medium, brush: brush)
    draw-rectangle(medium, left, top, right, bottom, filled?: #t)
  end;
  let brush = if (selected?) $background else $foreground end;
  draw-gadget-label(gadget, medium, 0 + offset, 0,
                    brush: selected? & $background,
		    underline?: label-underline?(gadget))
end method handle-repaint;


/// Event handling

define method handle-event
    (gadget :: <active-label-pane>, event :: <button-press-event>) => ()
  when (gadget-enabled?(gadget))
    label-selected?(gadget) := #t;
    repaint-sheet(gadget, $everywhere);
    select (event-button(event))
      $left-button, $middle-button =>
	// Left and middle button generate the semantic event
	// when the button is released
	#f;
      $right-button =>
	// Right button generates the semantic event when the
	// button is pressed, since it gets used for pop-up menus
	let x = event-x(event);
	let y = event-y(event);
	if (gadget-popup-menu-callback(gadget))
	  execute-popup-menu-callback
	    (gadget, gadget-client(gadget), gadget-id(gadget), gadget-value(gadget),
	     x: x, y: y)
	else
	  handle-semantic-event
	    (sheet-frame(gadget), gadget, gadget-value(gadget), event,
	     button: event-button(event), modifiers: event-modifier-state(event),
	     x: x, y: y)
	end;
    end
  end
end method handle-event;

define method handle-event 
    (gadget :: <active-label-pane>, event :: <double-click-event>) => ()
  when (gadget-enabled?(gadget))
    label-selected?(gadget) := #t;
    repaint-sheet(gadget, $everywhere);
    // Double click generates the semantic event immediately
    handle-semantic-event
      (sheet-frame(gadget), gadget, gadget-value(gadget), event,
       button: event-button(event), modifiers: event-modifier-state(event),
       x: event-x(event), y: event-y(event));
    // Deselect the label immediately, too
    label-selected?(gadget) := #f;
    repaint-sheet(gadget, $everywhere)
  end
end method handle-event;

define method handle-event 
    (gadget :: <active-label-pane>, event :: <button-release-event>) => ()
  when (gadget-enabled?(gadget) & label-selected?(gadget))
    label-selected?(gadget) := #f;
    repaint-sheet(gadget, $everywhere);
    select (event-button(event))
      $left-button, $middle-button =>
	// Generate the semantic event for these now
	let x = event-x(event);
	let y = event-y(event);
	if (gadget-activate-callback(gadget))
	  execute-activate-callback
	    (gadget, gadget-client(gadget), gadget-id(gadget))
	else
	  handle-semantic-event
	    (sheet-frame(gadget), gadget, gadget-value(gadget), event,
	     button: event-button(event), modifiers: event-modifier-state(event),
	     x: x, y: y)
	end;
      $right-button =>
	// The semantic event has already been generated for this
	#f;
    end
  end
end method handle-event;


define method handle-event 
    (gadget :: <active-label-pane>, event :: <pointer-enter-event>) => ()
  when (gadget-enabled?(gadget))
    let pointer = port-pointer(port(gadget));
    when (pointer-button-state(pointer) ~= 0)
      label-selected?(gadget) := #t;
      repaint-sheet(gadget, $everywhere)
    end
  end
end method handle-event;

define method handle-event 
    (gadget :: <active-label-pane>, event :: <pointer-exit-event>) => ()
  when (gadget-enabled?(gadget))
    let pointer = port-pointer(port(gadget));
    when (pointer-button-state(pointer) ~= 0)
      label-selected?(gadget) := #f;
      repaint-sheet(gadget, $everywhere)
    end
  end
end method handle-event;
