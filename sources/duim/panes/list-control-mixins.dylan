Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Shared list/table/tree control pane functionality

define open abstract class <homegrown-control-mixin> (<collection-gadget>)
  sealed slot %framem :: <frame-manager>,
    required-init-keyword: frame-manager:;
  sealed slot %layout-pane :: false-or(<layout-pane>) = #f;
  sealed slot %old-selection = #[];
end class <homegrown-control-mixin>;

define method frame-manager
    (pane :: <homegrown-control-mixin>) => (framem :: false-or(<frame-manager>))
  pane.%framem | next-method()
end method frame-manager;


define open abstract class <homegrown-control-layout-mixin> (<layout>)
  sealed slot %control-pane :: false-or(<homegrown-control-mixin>) = #f;
end class <homegrown-control-layout-mixin>;


define thread variable *layout-delayed?* = #f;

// Inhibit updating any scroll bars for the duration of the body
define macro delaying-layout
  { delaying-layout (?pane:expression) ?:body end }
    => { begin
	   let _pane = ?pane;
	   block ()
	     dynamic-bind (*layout-delayed?* = _pane)
               ?body
             end
	   cleanup
	     layout-homegrown-control(_pane)
	   end
	 end }
end macro delaying-layout;

define method layout-homegrown-control
    (pane :: <homegrown-control-mixin>) => ()
  when (sheet-mapped?(pane))
    unless (*layout-delayed?*)
      let layout = pane.%layout-pane;
      // Hack to get the screen to redisplay
      clear-box*(sheet-viewport(layout) | layout, sheet-viewport-region(layout));
      // Force relayout of all items, then notify up the sheet tree
      // if anything changed
      relayout-parent(layout);
      // Ensure that all kids are mapped
      sheet-mapped?(pane, do-repaint?: #f) := #t;
      repaint-sheet(layout, $everywhere);
      force-display(pane)
    end
  end
end method layout-homegrown-control;


define method note-gadget-selection-changed
    (pane :: <homegrown-control-mixin>) => ()
  when (sheet-mapped?(pane))
    let layout = pane.%layout-pane;
    let n-items :: <integer> = size(sheet-children(layout));
    select (gadget-selection-mode(pane))
      #"none"   => #f;
      #"single", #"multiple" =>
	for (index :: <integer> in pane.%old-selection)
	  when (index < n-items)
	    let old = index-to-item(pane, index);
	    clear-box*(old, sheet-region(old));
	    repaint-sheet(old, $everywhere)
	  end
	end;
        for (index :: <integer> in gadget-selection(pane))
	  when (index < n-items)
	    let new = index-to-item(pane, index);
	    repaint-sheet(new, $everywhere)
	  end
        end
    end;
    force-display(pane)
  end;
  pane.%old-selection := gadget-selection(pane)
end method note-gadget-selection-changed;


define method handle-event
    (control :: <homegrown-control-mixin>, event :: <button-press-event>) => ()
  when (gadget-enabled?(control))
    select (event-button(event))
      $right-button =>
	let x = event-x(event);
	let y = event-y(event);
	execute-popup-menu-callback
	  (control, gadget-client(control), gadget-id(control), #f, x: x, y: y);
      $left-button =>
        when (gadget-selection-mode(control) == #"multiple")
	  gadget-selection(control, do-callback?: #t) := #[]
	end;
      otherwise =>
	#f;
    end
  end
end method handle-event;

define method handle-event
    (pane :: <homegrown-control-layout-mixin>, event :: <button-press-event>) => ()
  let control = pane.%control-pane;
  when (gadget-enabled?(control))
    handle-event(event-handler(control), event)
  end
end method handle-event;

define method handle-event
    (control :: <homegrown-control-mixin>, event :: <key-press-event>) => ()
  when (gadget-enabled?(control))
    execute-key-press-callback
      (control, gadget-client(control), gadget-id(control), event-key-name(event))
  end
end method handle-event;

define method handle-event
    (pane :: <homegrown-control-layout-mixin>, event :: <key-press-event>) => ()
  let control = pane.%control-pane;
  when (gadget-enabled?(control))
    handle-event(event-handler(control), event)
  end
end method handle-event;


/// Shared list/table/tree control label button functionality

define constant $item-label-border :: <integer> = 5;

define open abstract class <homegrown-control-button-mixin> (<abstract-sheet>)
end class <homegrown-control-button-mixin>;

define method do-compose-space
    (pane :: <homegrown-control-button-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let (width, height) = gadget-label-size(pane);
  let extra-width = $item-label-border * 2;
  make(<space-requirement>,
       width: width + extra-width, height: height)
end method do-compose-space;

define method handle-repaint
    (pane :: <homegrown-control-button-mixin>, medium :: <medium>, region :: <region>) => ()
  let (control, item) = control-and-item-from-button(pane);
  let selected? = item-selected?(control, item);
  let offset = $item-label-border;
  when (selected?)
    let (left, top, right, bottom) = box-edges(pane);
    with-drawing-options (medium, brush: $foreground)
      draw-rectangle(medium, left, top, right, bottom, filled?: #t)
    end
  end;
  draw-gadget-label(pane, medium, 0 + offset, 0, 
                    brush: selected? & $background)
end method handle-repaint;

define method handle-event 
    (pane :: <homegrown-control-button-mixin>, event :: <button-press-event>) => ()
  when (gadget-enabled?(pane))
    // First set the selection
    let control = set-control-selection(pane, event);
    select (event-button(event))
      $left-button => #f;
      $middle-button =>
	activate-gadget(control);
      $right-button =>
	let (control, item) = control-and-item-from-button(pane);
	// Pass the value of the selected object to the callback
	let value = gadget-item-value(control, item-object(item));
	execute-popup-menu-callback
	  (control, gadget-client(control), gadget-id(control), value,
	   x: event-x(event), y: event-y(event));
    end
  end
end method handle-event;

define method handle-event 
    (pane :: <homegrown-control-button-mixin>, event :: <double-click-event>) => ()
  when (gadget-enabled?(pane)
	& event-button(event) == $left-button)
    // Set the selection and activate
    let control = set-control-selection(pane, event);
    activate-gadget(control)
  end
end method handle-event;

define method handle-event
    (pane :: <homegrown-control-button-mixin>, event :: <key-press-event>) => ()
  let (control, item) = control-and-item-from-button(pane);
  when (gadget-enabled?(control))
    execute-key-press-callback
      (control, gadget-client(control), gadget-id(control), event-key-name(event))
  end
end method handle-event;


define method set-control-selection
    (pane :: <homegrown-control-button-mixin>, event) => (control)
  let (control, item) = control-and-item-from-button(pane);
  let index     = item-to-index(control, item);
  let selection = gadget-selection(control);
  select (gadget-selection-mode(control))
    #"none"   => #f;
    #"single" =>
      gadget-selection(control, do-callback?: #t) := vector(index);
    #"multiple" =>
      select (event-modifier-state(event))
	$shift-key =>
	  let min-index = reduce(min, index, selection);
	  let max-index = reduce(max, index, selection);
	  gadget-selection(control, do-callback?: #t)
	    := range(from: min-index, to: max-index);
	$control-key =>
	  if (member?(index, selection))
	    gadget-selection(control, do-callback?: #t) := remove(selection, index);
	  else
	    gadget-selection(control, do-callback?: #t) := add(selection, index);
	  end;
	otherwise =>
	  gadget-selection(control, do-callback?: #t) := vector(index);
      end;
  end;
  control
end method set-control-selection;


define method item-to-index
    (control :: <homegrown-control-mixin>, item)
 => (index :: <integer>)
  let layout = control.%layout-pane;
  let index  = position(sheet-children(layout), item);
  index | error("Failed to find item %= in %=", item, control)
end method item-to-index;

define method index-to-item
    (control :: <homegrown-control-mixin>, index :: <integer>)
 => (item)
  let layout = control.%layout-pane;
  sheet-children(layout)[index]
end method index-to-item;

define method item-selected?
    (control :: <homegrown-control-mixin>, item)
 => (selected? :: <boolean>)
  block (return)
    let layout   = control.%layout-pane;
    let children = sheet-children(layout);
    let n-children :: <integer> = size(children);
    for (index :: <integer> in gadget-selection(control))
      when (index < n-children)
	let child = children[index];
	when (child == item)
	  return(#t)
	end
      end
    end;
    #f
  end
end method item-selected?;


define method control-for-item
    (sheet :: <sheet>) 
 => (sheet :: false-or(<homegrown-control-mixin>))
  let parent = sheet-parent(sheet);
  case
    instance?(parent, <homegrown-control-mixin>) =>
      parent;
    parent =>
      control-for-item(parent);
    otherwise =>
      #f;
  end
end method control-for-item;

define method control-and-item-from-button
    (button :: <homegrown-control-button-mixin>) => (control, item)
  // The parent of a list item button is always, and only, a list item
  let item = sheet-parent(button);
  //--- Can we make this more elegant?
  // The parent of a list item is always, and only, a list control,
  // except for table-controls.
  let parent = control-for-item(item);
  let control
    = if (instance?(parent, <homegrown-control-mixin>))
        parent
      else
        sheet-parent(parent)
      end;
  values(control, item)
end method control-and-item-from-button;
