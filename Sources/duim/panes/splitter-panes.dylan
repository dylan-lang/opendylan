Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple implementation of splitter panes

define sealed class <row-splitter-pane>
    (<row-splitter>,
     <single-child-wrapping-pane>)
end class <row-splitter-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <row-splitter>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<row-splitter-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<row-splitter-pane>));
define sealed domain initialize (<row-splitter-pane>);

define method initialize
    (pane :: <row-splitter-pane>, #key children) => ()
  next-method();
  sheet-child(pane)
    := splitter-pane-layout(pane, children, 
			    child-orientation: #"vertical");
end method initialize;

define method gadget-ratios-setter
    (ratios :: <sequence>, pane :: <row-splitter-pane>)
 => (ratios :: <sequence>)
  next-method();
  let layout :: <layout> = sheet-child(pane);
  layout-x-ratios(layout) := splitter-pane-layout-ratios(pane, ratios);
  if (sheet-layed-out?(layout))
    relayout-children(layout)
  end;
  ratios
end method gadget-ratios-setter;


define sealed class <column-splitter-pane>
    (<column-splitter>,
     <single-child-wrapping-pane>)
end class <column-splitter-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <column-splitter>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<column-splitter-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<column-splitter-pane>));
define sealed domain initialize (<column-splitter-pane>);

define method initialize
    (pane :: <column-splitter-pane>, #key children) => ()
  next-method();
  sheet-child(pane)
    := splitter-pane-layout(pane, children,
			    child-orientation: #"horizontal");
end method initialize;

define method gadget-ratios-setter
    (ratios :: false-or(<sequence>), pane :: <column-splitter-pane>)
 => (ratios :: false-or(<sequence>))
  next-method();
  let layout :: <layout> = sheet-child(pane);
  layout-y-ratios(layout) := splitter-pane-layout-ratios(pane, ratios);
  if (sheet-layed-out?(layout))
    relayout-children(layout)
  end;
  ratios
end method gadget-ratios-setter;


define method splitter-pane-layout
    (pane :: <splitter>, children :: <sequence>, 
     #key child-orientation :: <gadget-orientation>)
 => (layout :: <layout>)
  let ratios = gadget-ratios(pane);
  let new-children :: <simple-object-vector>
    = make(<vector>, size: size(children) * 2 - 1);
  for (child in children,
       i :: <integer> from 0 by 2)
    new-children[i] := child
  end;
  let cursor = select (child-orientation)
		 #"horizontal" => #"vertical-thumb";
		 #"vertical"   => #"horizontal-thumb";
	       end;
  for (i :: <integer> from 1 below size(children) * 2 - 1 by 2)
    new-children[i]
      := make(<splitter-separator-pane>,
	      client: pane, orientation: child-orientation, cursor: cursor,
	      pane-1: new-children[i - 1], pane-2: new-children[i + 1])
  end;
  let layout-class = select (child-orientation)
		       #"horizontal" => <column-layout>;
		       #"vertical"   => <row-layout>;
		     end;
  make(layout-class,
       children: new-children,
       ratios: splitter-pane-layout-ratios(pane, ratios))
end method splitter-pane-layout;

define method splitter-pane-layout-ratios
    (pane :: <splitter>, ratios :: false-or(<sequence>))
 => (ratios :: false-or(<sequence>))
  if (ratios)
    let new-ratios :: <simple-object-vector>
      = make(<vector>, size: size(ratios) * 2 - 1, fill: #f);
    for (ratio in ratios,
	 i :: <integer> from 0 by 2)
      new-ratios[i] := ratio
    end;
    new-ratios
  end
end method splitter-pane-layout-ratios;


/// The separator between resizable panes...

define sealed class <splitter-separator-pane>
    (<oriented-gadget-mixin>, 
     <no-value-gadget-mixin>,
     <basic-gadget>,
     <mirrored-sheet-mixin>,
     <standard-input-mixin>,
     <leaf-pane>)
  sealed slot %initial-position  :: false-or(<integer>) = #f;
  sealed slot %previous-position :: false-or(<integer>) = #f;
  sealed constant slot %pane-1 :: false-or(<sheet>) = #f,
    init-keyword: pane-1:;
  sealed constant slot %pane-2 :: false-or(<sheet>) = #f,
    init-keyword: pane-2:;
  keyword cursor: = #"move";
end class <splitter-separator-pane>;

define sealed domain make (singleton(<splitter-separator-pane>));
define sealed domain initialize (<splitter-separator-pane>);

// This is a mirrored sheet, but we do repainting ourselves...
define method port-handles-repaint?
    (port :: <port>, pane :: <splitter-separator-pane>) => (true? :: <boolean>)
  #f
end method port-handles-repaint?;

// ...but the repainting doesn't actually do anything!
define method handle-repaint
    (pane :: <splitter-separator-pane>, medium :: <medium>, region :: <region>) => ()
  #f
end method handle-repaint;


define constant $splitter-separator-thickness :: <integer> = 4;

define method do-compose-space
    (pane :: <splitter-separator-pane>, #key width, height)
 => (space-requirement :: <space-requirement>)
  select (gadget-orientation(pane))
    #"horizontal" =>
      make(<space-requirement>,
	   min-width: 1, width: width | 1, max-width: $fill,
	   height: $splitter-separator-thickness);
    #"vertical"   =>
      make(<space-requirement>,
	   width: $splitter-separator-thickness,
	   min-height: 1, height: height | 1, max-height: $fill);
  end
end method do-compose-space;


define method handle-event
    (pane :: <splitter-separator-pane>, event :: <button-press-event>) => ()
  let pointer = event-pointer(event);
  pointer-grabbed?(pointer) := pane;
  let (x, y) = sheet-position(pane);
  select (gadget-orientation(pane))
    #"horizontal" =>
      pane.%initial-position  := y;
      pane.%previous-position := y;
    #"vertical"   =>
      pane.%initial-position  := x;
      pane.%previous-position := x;
  end;
  // Ensure the separator is at the top of the Z-order
  raise-sheet(pane, activate?: #f)
end method handle-event;

define method handle-event
    (pane :: <splitter-separator-pane>, event :: <button-release-event>) => ()
  let pointer = event-pointer(event);
  pointer-grabbed?(pointer) := #f;
  when (pane.%initial-position)
    let splitter :: <splitter> = gadget-client(pane);
    let layout   :: <layout>   = sheet-child(splitter);
    let pane1 = pane.%pane-1;
    let pane2 = pane.%pane-2;
    let (x,  y)  = sheet-position(pane);
    let (tx, ty) = transform-position(sheet-transform(pane), event-x(event), event-y(event));
    let (l1, t1, r1, b1) = sheet-edges(pane1);
    let (l2, t2, r2, b2) = sheet-edges(pane2);
    let new-ratios :: <simple-object-vector>
      = select (gadget-orientation(pane))
	  #"horizontal" =>
	    let dy = ty - pane.%initial-position;
	    // Enforce the size constraints
	    //--- We should really enforce both min and max size constraints!
	    let shrink = if (dy < 0) pane1 else pane2 end;
	    let space-req  = compose-space(shrink);
	    let min-height = space-requirement-min-height(shrink, space-req);
	    let (width, height) = sheet-size(shrink);
	    if (dy < 0)
	      when (height + dy < min-height)
		dy := min-height - height
	      end
	    else
	      when (height - dy < min-height)
		dy := height - min-height
	      end
	    end;
	    set-sheet-edges(pane1, l1, t1,      r1, b1 + dy);
	    set-sheet-edges(pane2, l2, t2 + dy, r2, b2     );
	    set-sheet-position(pane, x, pane.%initial-position + dy);
	    local method sheet-height (sheet :: <sheet>) => (height)
		    let (width, height) = sheet-size(sheet);
		    ignore(width);
		    height
		  end method;
	    // Ensure that the resized panes remain OK if the frame resizes
	    let ratios = map-as(<vector>, sheet-height, sheet-children(layout));
	    layout-y-ratios(layout) := ratios;
	  #"vertical"   =>
	    let dx = tx - pane.%initial-position;
	    // Enforce the size constraints
	    //--- We should really enforce both min and max size constraints!
	    let shrink = if (dx < 0) pane1 else pane2 end;
	    let space-req = compose-space(shrink);
	    let min-width = space-requirement-min-width(shrink, space-req);
	    let (width, height) = sheet-size(shrink);
	    let new-width = max(width - abs(dx), min-width);
	    if (dx < 0)
	      when (width + dx < min-width)
		dx := min-width - width
	      end
	    else
	      when (width - dx < min-width)
		dx := width - min-width
	      end
	    end;
	    set-sheet-edges(pane1, l1,      t1, r1 + dx, b1);
	    set-sheet-edges(pane2, l2 + dx, t2, r2     , b2);
	    set-sheet-position(pane, pane.%initial-position + dx, y);
	    local method sheet-width (sheet :: <sheet>) => (height)
		    let (width, height) = sheet-size(sheet);
		    ignore(height);
		    width
		  end method;
	    let ratios = map-as(<vector>, sheet-width, sheet-children(layout));
	    layout-x-ratios(layout) := ratios;
	end;
    let new-ratios-size = size(new-ratios);
    let splitter-ratios :: <simple-object-vector>
      = make(<vector>, size: floor/(new-ratios-size, 2) + 1);
    for (i :: <integer> from 0,
	 j :: <integer> from 0 below new-ratios-size by 2)
      splitter-ratios[i] := new-ratios[j]
    end;
    gadget-ratios(splitter) := splitter-ratios;
    execute-split-bar-moved-callback
      (splitter, gadget-client(splitter), gadget-id(splitter), pane1, pane2);
    pane.%initial-position  := #f;
    pane.%previous-position := #f
  end
end method handle-event;

define method handle-event
    (pane :: <splitter-separator-pane>, event :: <double-click-event>) => ()
  // Ensure that we don't keep the mouse grabbed...
  let pointer = event-pointer(event);
  pointer-grabbed?(pointer) := #f;
  pane.%initial-position    := #f;
  pane.%previous-position   := #f
end method handle-event;

// Give feedback while dragging the separator
define method handle-event
    (pane :: <splitter-separator-pane>, event :: <pointer-drag-event>) => ()
  when (pane.%initial-position)
    let (x,  y)  = sheet-position(pane);
    let (tx, ty) = transform-position(sheet-transform(pane), event-x(event), event-y(event));
    select (gadget-orientation(pane))
      #"horizontal" =>
	let dy    = ty - pane.%previous-position;
	let new-y = y + dy;
	pane.%previous-position := new-y;
	set-sheet-position(pane, x, new-y);
      #"vertical"   =>
	let dx    = tx - pane.%previous-position;
	let new-x = x + dx;
	pane.%previous-position := new-x;
	set-sheet-position(pane, new-x, y);
    end
  end
end method handle-event;

define method handle-event
    (pane :: <splitter-separator-pane>, event :: <key-press-event>) => ()
  // Abort the operation if the user hits Escape
  when (pane.%initial-position & event-key-name(event) == #"escape")
    let (x,  y)  = sheet-position(pane);
    select (gadget-orientation(pane))
      #"horizontal" =>
	let y = pane.%initial-position;
	set-sheet-position(pane, x, y);
      #"vertical"   =>
	let x = pane.%initial-position;
	set-sheet-position(pane, x, y);
    end;
    let pointer = port-pointer(port(event-sheet(event)));
    pointer-grabbed?(pointer) := #f;
    pane.%initial-position    := #f;
    pane.%previous-position   := #f
  end
end method handle-event;
