Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of list control panes

define sealed class <list-control-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <homegrown-control-mixin>,
     <list-control>,
     <single-child-wrapping-pane>)
end class <list-control-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <list-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<list-control-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<list-control-pane>));
define sealed domain initialize (<list-control-pane>);


define sealed class <list-control-layout>
    (<homegrown-control-layout-mixin>, <column-layout>)
end class <list-control-layout>;

define sealed domain make (singleton(<list-control-layout>));
define sealed domain initialize (<list-control-layout>);


define sealed method initialize
    (pane :: <list-control-pane>, #key frame-manager: framem) => ()
  next-method();
  with-frame-manager (framem)
    let layout = make(<list-control-layout>);
    pane.%layout-pane    := layout;
    layout.%control-pane := pane;
    let scroll-bars = gadget-scroll-bars(pane);
    sheet-child(pane) 
      := if (scroll-bars == #"none")
	   layout
	 else
	   scrolling (scroll-bars: scroll-bars, border-type: #f)
	     layout
	    end
         end
  end;
end method initialize;

// Build the items for the first time when the sheet is fully attached
define method note-sheet-attached (pane :: <list-control-pane>) => ()
  next-method();
  note-gadget-items-changed(pane)
end method note-sheet-attached;

// Keep the children of the layout pane in step with 'gadget-items'
define method note-gadget-items-changed 
    (pane :: <list-control-pane>) => ()
  next-method();
  delaying-layout (pane)
    let layout = pane.%layout-pane;
    let framem = frame-manager(pane);
    when (framem)
      sheet-children(layout) := #[];
      for (object in gadget-items(pane))
        let item = make-item(pane, object, frame-manager: framem);
        do-add-item(pane, item)
      end
    end
  end
end method note-gadget-items-changed;


/// Generic implementation of list item panes

define sealed class <list-item-pane>
    (<row-layout>, <labelled-gadget-mixin>, <basic-action-gadget>, <list-item>)
  sealed slot %icon = #f,
    init-keyword: icon:;
end class <list-item-pane>;

define sealed domain make (singleton(<list-item-pane>));
define sealed domain initialize (<list-item-pane>);

define sealed method initialize
    (item :: <list-item-pane>,
     #key label, small-icon, large-icon, frame-manager: framem)
  ignore(large-icon);
  next-method();
  with-frame-manager (framem)
    when (small-icon)
      add-child(item, make(<label>, label: small-icon))
    end;
    when (label)
      add-child(item, make(<list-item-label-button>, label: label))
    end
  end
end method initialize;

define method do-make-item 
    (pane :: <list-control-pane>, class == <list-item>,
     #rest initargs, #key object, frame-manager: framem = frame-manager(pane))
 => (item :: <list-item-pane>)
  let label-function = gadget-label-key(pane);
  let icon-function  = list-control-icon-function(pane);
  let label = (label-function & label-function(object)) | "";
  let (small-icon, large-icon) = (icon-function  & icon-function(object));
  apply(make, <list-item-pane>,
	label: label,
	small-icon: small-icon,
	large-icon: large-icon,
	x-spacing: 4, y-alignment: #"center",
	frame-manager: framem,
	initargs)
end method do-make-item;

define sealed method do-find-item
    (pane :: <list-control-pane>, object, #key)
 => (node :: false-or(<list-item-pane>))
  let key  = gadget-value-key(pane);
  let test = gadget-test(pane);
  let the-key = key(object);
  block (return)
    for (item in sheet-children(pane))
      when (test(key(item-object(item)), the-key))
	return(item)
      end
    end;
    #f
  end
end method do-find-item;

define sealed method do-add-item
    (pane :: <list-control-pane>, item :: <list-item-pane>, #key after) => ()
  let layout = pane.%layout-pane;
  let index = after & position(sheet-children(layout), after);
  add-child(layout, item, index: index | #"end");
  sheet-mapped?(item, do-repaint?: #f) := sheet-mapped?(pane);
  layout-homegrown-control(pane)
end method do-add-item;

define sealed method do-remove-item
    (pane :: <list-control-pane>, item :: <list-item-pane>) => ()
  let layout = pane.%layout-pane;
  remove-child(layout, item);
  layout-homegrown-control(pane)
end method do-remove-item;


define sealed method item-label
    (item :: <list-item-pane>) => (label :: false-or(<string>))
  gadget-label(item)
end method item-label;

define sealed method item-label-setter
    (label :: false-or(<string>), item :: <list-item-pane>) => (label :: false-or(<string>))
  gadget-label(item) := label;
  block (break)
    for (child in sheet-children(item))
      when (instance?(child, <list-item-label-button>))
	gadget-label(child) := label;
	clear-box*(item, sheet-region(item));
	repaint-sheet(item, $everywhere);
	break()
      end
    end
  end;
  label
end method item-label-setter;

define sealed method item-icon
    (item :: <list-item-pane>) => (icon :: false-or(<image>))
  item.%icon
end method item-icon;

define sealed method item-icon-setter
    (icon :: false-or(<image>), item :: <list-item-pane>) => (icon :: false-or(<image>))
  item.%icon := icon;
  block (break)
    for (child in sheet-children(item))
      when (instance?(child, <label>))
	gadget-label(child) := icon;
	clear-box*(item, sheet-region(item));
	repaint-sheet(item, $everywhere);
	break()
      end
    end
  end;
  icon
end method item-icon-setter;


define sealed class <list-item-label-button>
    (<homegrown-control-button-mixin>, <push-button>, <simple-pane>)
end class <list-item-label-button>;

define sealed domain make (singleton(<list-item-label-button>));
define sealed domain initialize (<list-item-label-button>);
