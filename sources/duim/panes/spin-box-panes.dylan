Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of spin boxes


/// Arrow buttons

define sealed class <arrow-button-pane>
    (<push-button>, <simple-pane>)
  sealed slot %active = #f;
  sealed constant slot arrow-direction = #"down",
    init-keyword: direction:;
end class <arrow-button-pane>;

define sealed domain make (singleton(<arrow-button-pane>));
define sealed domain initialize (<arrow-button-pane>);

define method do-compose-space
    (pane :: <arrow-button-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  make(<space-requirement>,
       width: 12, height: 12)
end method do-compose-space;

define method handle-repaint
    (pane :: <arrow-button-pane>, medium :: <medium>, region :: <region>) => ()
  draw-arrow-button(pane, medium, filled?: pane.%active)
end method handle-repaint;

define method draw-arrow-button 
    (pane :: <arrow-button-pane>, medium :: <medium>, 
     #key filled?) => ()
  let (width, height) = box-size(pane);
  width  := width  - 1;
  height := height - 1;
  let half-width = truncate/(width, 2);
  let direction = arrow-direction(pane);
  let base-y = if (direction = #"down") 0 else height end;
  let tip-y  = if (direction = #"down") height else 0 end;
  draw-polygon(medium,
	       vector(0, base-y, width, base-y, half-width, tip-y),
	       closed?: #t, filled?: filled?);
end method draw-arrow-button;

define method handle-event 
    (pane :: <arrow-button-pane>, event :: <button-press-event>) => ()
  when (gadget-enabled?(pane)
	& event-button(event) == $left-button)
    pane.%active := #t;
    repaint-sheet(pane, $everywhere)
  end
end method handle-event;

define method handle-event 
    (pane :: <arrow-button-pane>, event :: <button-release-event>) => ()
  when (gadget-enabled?(pane)
	& event-button(event) == $left-button)
    with-sheet-medium (medium = pane)
      clear-box*(medium, sheet-region(pane));
      pane.%active := #f;
      repaint-sheet(pane, $everywhere)
    end;
    activate-gadget(pane)
  end
end method handle-event;


/// Spin box pane

define sealed pane <spin-box-pane> 
    (<spin-box>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>)
  pane spin-box-pane-text-field (pane)
    make(<text-field>, 
	 client: pane,
	 value: gadget-label(pane),
	 min-width: 80,
	 foreground: default-foreground(pane),
	 background: default-background(pane),
	 text-style: default-text-style(pane));
  pane spin-box-pane-up-arrow (pane)
    make(<arrow-button-pane>, 
	 client: pane,
	 direction: #"up",
	 activate-callback: method (sheet) 
			      ignore(sheet);
			      spin-box-pane-up(pane)
			    end,
	 foreground: default-foreground(pane),
	 background: default-background(pane));
  pane spin-box-pane-down-arrow (pane)
    make(<arrow-button-pane>,
	 client: pane,
	 direction: #"down",
	 activate-callback: method (sheet)
			      ignore(sheet);
			      spin-box-pane-down(pane)
			    end,
	 foreground: default-foreground(pane),
	 background: default-background(pane));
  layout (pane)
    horizontally (spacing: 2, y-alignment: #"center")
      spin-box-pane-text-field(pane);
      vertically (spacing: 2)
        spin-box-pane-up-arrow(pane);
        spin-box-pane-down-arrow(pane)
      end
    end;
end pane <spin-box-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <spin-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<spin-box-pane>, #f)
end method class-for-make-pane;

define method spin-box-pane-down (pane :: <spin-box-pane>) => (selection)
  let index   :: <integer> = gadget-selection(pane)[0];
  let n-items :: <integer> = size(gadget-items(pane));
  let selection = if (index = n-items - 1)
		    0
		  else
		    index + 1
		  end;
  gadget-selection(pane, do-callback?: #t) := vector(selection)
end method spin-box-pane-down;

define method spin-box-pane-up (pane :: <spin-box-pane>) => (selection)
  let index   :: <integer> = gadget-selection(pane)[0];
  let n-items :: <integer> = size(gadget-items(pane));
  let selection = if (index = 0)
                    n-items - 1
                  else
                    index - 1
                  end;
  gadget-selection(pane, do-callback?: #t) := vector(selection)
end method spin-box-pane-up;

define method note-gadget-selection-changed
    (pane :: <spin-box-pane>) => ()
  next-method();
  let text-field = spin-box-pane-text-field(pane);
  when (text-field)
    gadget-value(text-field) := gadget-label(pane)
  end
end method note-gadget-selection-changed;

define method note-gadget-enabled (client, gadget :: <spin-box-pane>) => ()
  note-spin-box-pane-enable-state-changed(gadget, #t)
end method note-gadget-enabled;

define method note-gadget-disabled (client, gadget :: <spin-box-pane>) => ()
  note-spin-box-pane-enable-state-changed(gadget, #f)
end method note-gadget-disabled;

define method note-spin-box-pane-enable-state-changed
    (gadget :: <spin-box-pane>, enabled?) => ()
  let text-field = spin-box-pane-text-field(gadget);
  when (text-field)
    gadget-enabled?(text-field) := enabled?;
    gadget-enabled?(spin-box-pane-up-arrow(gadget)) := enabled?;
    gadget-enabled?(spin-box-pane-down-arrow(gadget)) := enabled?;
  end
end method note-spin-box-pane-enable-state-changed;
