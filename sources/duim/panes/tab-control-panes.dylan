Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of tab control panes

define sealed class <tab-control-pane> (<column-layout>, <tab-control>)
  slot %button-box :: false-or(<button-box>) = #f;
  keyword y-spacing: = 2;
end class <tab-control-pane>;

define sealed domain make (singleton(<tab-control-pane>));
define sealed domain initialize (<tab-control-pane>);

define sideways method class-for-make-pane
    (framem :: <frame-manager>, class == <tab-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<tab-control-pane>, #f)
end method class-for-make-pane;

define sealed method initialize
    (pane :: <tab-control-pane>, #key frame-manager: framem)
  next-method();
  let pages = tab-control-pages(pane);
  let layout = make(<stack-layout>, children: pages);
  let tabs-at-top? = tab-control-tabs-position(pane) == #"top";
  let button-class
    = if (tabs-at-top?) <top-tab-button> else <bottom-tab-button> end;
  let button-box
    = with-frame-manager (framem)
        make(<radio-box>, 
	     items: pages,
	     label-key: method (page)
			  let label = gadget-label-key(pane)(page);
			  compute-mnemonic-from-label
			    (page, label, remove-ampersand?: #t)
			end,
	     spacing: 0,
	     value: tab-control-current-page(pane),
	     button-class: button-class,
	     value-changed-callback:
	       method (radio-box)
		 tab-control-map-page(pane, gadget-value(radio-box))
	       end method)
      end;
  pane.%button-box := button-box;
  let children
    = case
	~layout      => vector(button-box);
	tabs-at-top? => vector(button-box, layout);
	otherwise    => vector(layout, button-box);
      end;
  sheet-children(pane) := children
end method initialize;

define method tab-control-stack-layout
    (pane :: <tab-control-pane>) => (layout :: <stack-layout>)
  let children = sheet-children(pane);
  if (size(children) = 2) children[1] else children[0] end
end method tab-control-stack-layout;

define method note-pages-changed
    (pane :: <tab-control-pane>) => ()
  let pages = tab-control-pages(pane);
  let old-page = tab-control-mapped-page(pane);
  let new-page = tab-control-current-page(pane);
  let swap-page? = (old-page ~== new-page);
  when (swap-page? & old-page)
    sheet-withdrawn?(old-page) := #t;
    sheet-withdrawn?(new-page) := #f
  end;
  let stack :: <stack-layout> = tab-control-stack-layout(pane);
  sheet-children(stack) := pages;
  //--- This is surely too aggressive, but seems to be needed
  //--- to get things layed out right
  invalidate-space-requirements(stack);
  when (sheet-attached?(stack))
    relayout-parent(stack);
    //---*** Can this be removed if we fix 'set-sheet-edges'?
    update-all-mirror-positions(stack)
  end;
  let buttons = pane.%button-box;
  when (buttons)
    gadget-items(buttons) := pages;
    gadget-value(buttons) := new-page;
    when (sheet-mapped?(buttons))
      repaint-sheet(buttons, sheet-region(buttons))
    end
  end;
  swap-page? & tab-control-map-page(pane, new-page)
end method note-pages-changed;

define method tab-control-mapped-page
    (pane :: <tab-control-pane>) => (child :: false-or(<sheet>))
  let stack :: <stack-layout> = tab-control-stack-layout(pane);
  stack-layout-mapped-page(stack)
end method tab-control-mapped-page;

define method tab-control-map-page
    (pane :: <tab-control-pane>, new-page :: <sheet>) => ()
  let stack :: <stack-layout> = tab-control-stack-layout(pane);
  stack-layout-mapped-page(stack) := new-page
end method tab-control-map-page;

define method note-gadget-value-changed
    (pane :: <tab-control-pane>) => ()
  when (sheet-mapped?(pane))
    tab-control-map-page(pane, tab-control-current-page(pane))
  end
end method note-gadget-value-changed;


/// Tab buttons

define sealed class <tab-button> (<radio-button>, <simple-pane>)
  sealed constant slot %position :: <vertical-position> = #"top",
    init-keyword: position:;
end class <tab-button>;

define sealed domain make (subclass(<tab-button>));
define sealed domain initialize (<tab-button>);

define sealed class <top-tab-button> (<tab-button>)
  keyword position: = #"top";
end class <top-tab-button>;

define sealed class <bottom-tab-button> (<tab-button>)
  keyword position: = #"bottom";
end class <bottom-tab-button>;

define method do-compose-space
    (pane :: <tab-button>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let (width, height) = gadget-label-size(pane);
  make(<space-requirement>,
       width: width + 12, height: height + 8)
end method do-compose-space;

define method handle-repaint
    (pane :: <tab-button>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(pane);
  let bottom = bottom - 1;
  let indentation = 4;
  let selected? = gadget-value(pane);
  let position = pane.%position;
  with-brush (medium, 
	      foreground: if (selected?) $background else $foreground end)
    with-pen (medium, width: 2)
      draw-line(medium, left, bottom, right, bottom)
    end 
  end;
  inc!(left);
  dec!(right);
  inc!(top);
  with-pen (medium,
	    width: if (selected?) 2 else 1 end)
    let (y1, y2, y3)
      = select (position)
          #"top"    => values(bottom, top + indentation, top);
	  #"bottom" => values(top, bottom - indentation, bottom);
	end;
    draw-polygon(medium,
		 vector(left, y1,
			left, y2,
			left + indentation, y3,
			right - indentation, y3,
			right, y2,
			right, y1),
		 filled?: #f,
		 closed?: #f)
  end;
  draw-gadget-label(pane, medium, 6, 6)
end method handle-repaint;

define method handle-event 
    (pane :: <tab-button>, event :: <button-press-event>) => ()
  when (gadget-enabled?(pane)
        & event-button(event) == $left-button
        & ~gadget-value(pane))
    gadget-value(pane, do-callback?: #t) := #t
  end
end method handle-event;

define method note-gadget-value-changed
    (pane :: <tab-button>) => ()
  when (sheet-mapped?(pane))
    clear-box*(pane, sheet-region(pane));
    repaint-sheet(pane, $everywhere)
  end
end method note-gadget-value-changed;
