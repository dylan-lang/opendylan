Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of table control panes

define sealed class <table-control-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <homegrown-control-mixin>,
     <table-control>,
     <single-child-wrapping-pane>)
end class <table-control-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <table-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<table-control-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<table-control-pane>));
define sealed domain initialize (<table-control-pane>);


define sealed class <table-control-layout>
    (<homegrown-control-layout-mixin>, <column-layout>)
end class <table-control-layout>;

define sealed domain make (singleton(<table-control-layout>));
define sealed domain initialize (<table-control-layout>);


define sealed method initialize
    (pane :: <table-control-pane>, #key frame-manager: framem) => ()
  next-method();
  let n-columns :: <integer> = size(table-control-columns(pane));
  with-frame-manager (framem)
    let layout  = make(<table-control-layout>,
		       x-spacing: 4,
		       columns:   n-columns);
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
define method note-sheet-attached (pane :: <table-control-pane>) => ()
  next-method();
  note-gadget-items-changed(pane)
end method note-sheet-attached;

define method note-gadget-items-changed 
    (pane :: <table-control-pane>) => ()
  next-method();
  delaying-layout (pane)
    let framem = frame-manager(pane);
    let layout = pane.%layout-pane;
    when (framem)
      sheet-children(layout) := make-table-control-children(frame-manager(pane), pane);
    end
  end
end method note-gadget-items-changed;

// Keep the children of the layout pane in step with 'gadget-items'
//--- This doesn't observe 'table-column-width' or 'table-column-alignment'
define method make-table-control-children
    (framem :: <frame-manager>, pane :: <table-control-pane>)
 => (children :: <stretchy-vector>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  let columns = table-control-columns(pane);
  for (column in columns)
    let heading = table-column-heading(column);
    add!(children, 
	 make(<label-pane>,
	      label: heading,
              frame-manager: framem,
	      text-style: make(<text-style>, weight: #"bold")))
  end;
  for (item in gadget-items(pane))
    for (column in columns)
      let generator :: <function> = table-column-generator(column);
      let object = generator(item);
      add!(children, make-item(pane, object, frame-manager: framem))
    end
  end;
  children
end method make-table-control-children;


/// Generic implementation of table item panes

define sealed class <table-item-pane> 
    (<row-layout>, <labelled-gadget-mixin>, <basic-action-gadget>, <table-item>)
  sealed slot %icon = #f,
    init-keyword: icon:;
end class <table-item-pane>;

define sealed domain make (singleton(<table-item-pane>));
define sealed domain initialize (<table-item-pane>);

define sealed method initialize
    (item :: <table-item-pane>,
     #key label, small-icon, large-icon, frame-manager: framem)
  ignore(large-icon);
  next-method();
  with-frame-manager (framem)
    when (small-icon)
      add-child(item, make(<label>, label: small-icon))
    end;
    when (label)
      add-child(item, make(<table-item-label-button>, label: label))
    end
  end
end method initialize;

define method do-make-item 
    (pane :: <table-control-pane>, class == <table-item>,
     #rest initargs, #key object, frame-manager: framem = frame-manager(pane))
 => (item :: <table-item-pane>)
  let label-function = gadget-label-key(pane);
  let icon-function  = table-control-icon-function(pane);
  let label = (label-function & label-function(object)) | "";
  let (small-icon, large-icon) = (icon-function  & icon-function(object));
  apply(make, <table-item-pane>,
	label: label,
	small-icon: small-icon,
	large-icon: large-icon,
	x-spacing: 4, y-alignment: #"center",
	frame-manager: framem,
	initargs)
end method do-make-item;

define sealed method do-find-item
    (pane :: <table-control-pane>, object, #key)
 => (node :: false-or(<table-item-pane>))
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
    (pane :: <table-control-pane>, item :: <table-item-pane>, #key after) => ()
  let layout = pane.%layout-pane;
  let index = after & position(sheet-children(layout), after);
  add-child(layout, item, index: index | #"end");
  sheet-mapped?(item, do-repaint?: #f) := sheet-mapped?(pane);
  layout-homegrown-control(pane)
end method do-add-item;

define sealed method do-remove-item
    (pane :: <table-control-pane>, item :: <table-item-pane>) => ()
  let layout = pane.%layout-pane;
  remove-child(layout, item);
  layout-homegrown-control(pane)
end method do-remove-item;

define sealed method do-add-column
    (pane :: <table-control-pane>, column :: <table-column>, index :: <integer>) => ()
  //---*** Do this, fixing up all existing items
end method do-add-column;

define sealed method do-remove-column
    (pane :: <table-control-pane>, index :: <integer>) => ()
  //---*** Do this, fixing up all existing items
end method do-remove-column;


define sealed method item-label
    (item :: <table-item-pane>) => (label :: false-or(<string>))
  gadget-label(item)
end method item-label;

define sealed method item-label-setter
    (label :: false-or(<string>), item :: <table-item-pane>) => (label :: false-or(<string>))
  gadget-label(item) := label;
  block (break)
    for (child in sheet-children(item))
      when (instance?(child, <table-item-label-button>))
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
    (item :: <table-item-pane>) => (icon :: false-or(<image>))
  item.%icon
end method item-icon;

define sealed method item-icon-setter
    (icon :: false-or(<image>), item :: <table-item-pane>) => (icon :: false-or(<image>))
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


define sealed class <table-item-label-button>
    (<homegrown-control-button-mixin>, <push-button>, <simple-pane>)
end class <table-item-label-button>;

define sealed domain make (singleton(<table-item-label-button>));
define sealed domain initialize (<table-item-label-button>);


/// Mapping items <-> indices

define sealed method item-to-index
    (table :: <table-control-pane>, item)
 => (index :: <integer>)
  let layout = table.%layout-pane;
  let index  = position(sheet-children(layout), item);
  let n-columns :: <integer> = size(table-control-columns(table));
  if (index)
    floor/(index, n-columns) - 1
  else
    error("Failed to find item %= in %=", item, table)
  end
end method item-to-index;

define sealed method index-to-item
    (table :: <table-control-pane>, index :: <integer>)
 => (item)
  let layout = table.%layout-pane;
  let n-columns :: <integer> = size(table-control-columns(table));
  sheet-children(layout)[index * n-columns + n-columns]
end method index-to-item;

define sealed method item-selected?
    (table :: <table-control-pane>, item)
 => (selected? :: <boolean>)
  block (return)
    let client   = table.%layout-pane;
    let children = sheet-children(client);
    let n-columns :: <integer> = size(table-control-columns(table));
    for (index :: <integer> in gadget-selection(table))
      let child = children[index * n-columns + n-columns];
      when (child == item)
        return(#t)
      end
    end;
    #f
  end
end method item-selected?;
