Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Shared tree/graph control pane functionality

define open abstract class <homegrown-tree-control-mixin> (<homegrown-control-mixin>)
  // A cache for 'gadget-items' in tree and graph controls
  sealed slot %visible-items = #f;
  sealed slot tree-control-expand-icon   :: false-or(<image>) = $tree-expand-icon;
  sealed slot tree-control-contract-icon :: false-or(<image>) = $tree-contract-icon;
end class <homegrown-tree-control-mixin>;

define open generic initialize-tree-control-icons
  (port :: <abstract-port>, tree :: <tree-control>) => ();

define method initialize-tree-control-icons
    (port :: <port>, tree :: <tree-control>) => ()
  // $tree-expand-icon and $tree-contract-icon are reasonable defaults...
  #f
end method initialize-tree-control-icons;
  
define sealed method gadget-items
    (tree :: <homegrown-tree-control-mixin>) => (items :: <sequence>)
  tree.%visible-items
  | begin
      let layout = tree.%layout-pane;
      if (layout)
	let items :: <stretchy-object-vector> = make(<stretchy-vector>);
	for (node in sheet-children(layout))
	  unless (sheet-withdrawn?(node))
	    add!(items, node-object(node))
	  end
	end;
	tree.%visible-items := items
      else
        #[]
      end
    end
end method gadget-items;


/// Gadget update protocol

define sealed method update-gadget
    (tree :: <homegrown-tree-control-mixin>) => ()
  delaying-layout (tree)
    next-method();
    //--- We do this here because if the tree ends up the same size then
    //--- the kids fail to get layed out.
    relayout-children(tree)
  end
end method update-gadget;

define method gadget-state-setter
    (state :: <tree-control-state>, tree :: <homegrown-tree-control-mixin>)
 => (state :: <tree-control-state>)
  delaying-layout (tree)
    next-method();
    relayout-children(tree)
  end;
  state
end method gadget-state-setter;


/*
//---*** I think we need something like the following...

define sealed method item-to-index
    (tree :: <tree-control-pane>, node :: <tree-node-pane>)
 => (index :: <integer>)
  block (return)
    let index :: <integer> = -1;
    for (n :: <tree-node-pane> in sheet-children(tree.%layout-pane))
      unless (sheet-withdrawn?(n))
	inc!(index);
	when (n == node)
	  return(index)
	end
      end
    end;
    error("Failed to find item %= in %=", node, tree)
  end
end method item-to-index;

define sealed method index-to-item
    (tree :: <tree-control-pane>, index :: <integer>)
 => (node :: <tree-node-pane>)
  block (return)
    let i :: <integer> = -1;
    for (node :: <tree-node-pane> in sheet-children(tree.%layout-pane))
      unless (sheet-withdrawn?(node))
	inc!(i);
	when (i == index)
	  return(node)
	end
      end
    end;
    error("Failed to find index %= in %=", index, tree)
  end
end method index-to-item;

define sealed method item-selected?
    (tree :: <tree-control-pane>, node :: <tree-node-pane>)
 => (selected? :: <boolean>)
  let index = item-to-index(tree, node);
  member?(index, gadget-selection(tree))
end method item-selected?;
*/


/// Shared tree/graph control label button functionality

define open abstract class <tree-node-pane-mixin>
    (<row-layout>, <labelled-gadget-mixin>, <basic-action-gadget>, <tree-node>)
  sealed slot %icon = #f,
    init-keyword: icon:;
  sealed slot %selected-icon = #f,
    init-keyword: selected-icon:;
end class <tree-node-pane-mixin>;

define sealed method initialize
    (node :: <tree-node-pane-mixin>,
     #key label, icon, selected-icon, tree, frame-manager: framem)
  ignore(selected-icon);
  next-method();
  when (tree)
    with-frame-manager (framem)
      when (tree-control-show-buttons?(tree)
	    & tree-control-children-predicate(tree)(node-object(node)))
	// Assumes expand and contract icons are the same size.
	// We'll just change the label's icon as necessary
	local method toggle-node (button) => ()
		ignore(button);
		if (node-state(node) == #"expanded")
		  contract-node(tree, node)
		else
		  expand-node(tree, node)
		end;
		distribute-node-state-changed-callback(tree, node)
	      end method;
	add-child(node, make(<tree-node-control-button>,
			     activate-callback: toggle-node))
      end;
      when (icon)
	// Assumes select and deselect icons are the same size.
	// We'll just change the label's icon as necessary
	add-child(node, make(<label>, label: icon))
      end;
      when (label)
	add-child(node, make(<tree-node-label-button>, label: label))
      end
    end
  end
end method initialize;


/// Tree node labels

define sealed class <tree-node-label-button>
    (<homegrown-control-button-mixin>, <push-button>, <simple-pane>)
end class <tree-node-label-button>;

define sealed domain make (singleton(<tree-node-label-button>));
define sealed domain initialize (<tree-node-label-button>);


/// Tree node expand and contract buttons

define sealed class <tree-node-control-button>
    (<push-button>, <simple-pane>)
end class <tree-node-control-button>;

define sealed domain make (singleton(<tree-node-control-button>));
define sealed domain initialize (<tree-node-control-button>);

define constant $tree-control-black = $black;
define constant $tree-control-gray  = make-rgb-color(150.0 / 255.0, 150.0 / 255.0, 150.0 / 255.0);

define pattern $tree-expand-icon
    (list($background, $tree-control-black, $tree-control-gray))
  2, 2, 2, 2, 2, 2, 2, 2, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 0, 0, 0, 1, 0, 0, 0, 2;
  2, 0, 0, 0, 1, 0, 0, 0, 2;
  2, 0, 1, 1, 1, 1, 1, 0, 2;
  2, 0, 0, 0, 1, 0, 0, 0, 2;
  2, 0, 0, 0, 1, 0, 0, 0, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 2, 2, 2, 2, 2, 2, 2, 2;
end pattern $tree-expand-icon;

define pattern $tree-contract-icon
    (list($background, $tree-control-black, $tree-control-gray))
  2, 2, 2, 2, 2, 2, 2, 2, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 0, 1, 1, 1, 1, 1, 0, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 0, 0, 0, 0, 0, 0, 0, 2;
  2, 2, 2, 2, 2, 2, 2, 2, 2;
end pattern $tree-contract-icon;

define sealed method initialize
    (pane :: <tree-node-control-button>, #key)
  next-method();
  // Pick an icon so that 'compose-space' will win...
  gadget-label(pane) := $tree-expand-icon
end method initialize;

define sealed method control-and-item-from-button
    (button :: <tree-node-control-button>) => (tree, node)
  let node = sheet-parent(button);
  let tree = control-for-item(node);
  values(tree, node)
end method control-and-item-from-button;

define method do-compose-space
    (pane :: <tree-node-control-button>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let (width, height) = gadget-label-size(pane);
  make(<space-requirement>,
       width: width, height: height)
end method do-compose-space;

define method handle-repaint
    (pane :: <tree-node-control-button>, medium :: <medium>, region :: <region>) => ()
  let (tree, node) = control-and-item-from-button(pane);
  if (node-state(node) == #"expanded")
    draw-image(medium, tree-control-contract-icon(tree), 0, 0)
  else
    draw-image(medium, tree-control-expand-icon(tree),   0, 0)
  end
end method handle-repaint;

define method handle-event 
    (pane :: <tree-node-control-button>, event :: <button-press-event>) => ()
  when (gadget-enabled?(pane))
    select (event-button(event))
      $left-button =>
        // Use the activation callback to fire the expand/contract
        activate-gadget(pane);
      $right-button =>
        let (tree, node) = control-and-item-from-button(pane);
	ignore(node);
	let x = event-x(event);
	let y = event-y(event);
	execute-popup-menu-callback
	  (tree, gadget-client(tree), gadget-id(tree), #f, x: x, y: y);
      otherwise =>
	#f;
    end
  end
end method handle-event;
