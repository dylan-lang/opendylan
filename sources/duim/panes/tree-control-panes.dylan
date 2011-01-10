Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of tree control panes

define sealed class <tree-control-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <homegrown-tree-control-mixin>,
     <tree-control>,
     <single-child-wrapping-pane>)
end class <tree-control-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <tree-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<tree-control-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<tree-control-pane>));
define sealed domain initialize (<tree-control-pane>);

define sealed method initialize
    (tree :: <tree-control-pane>,
     #key frame-manager: framem)
  next-method();
  with-frame-manager (framem)
    let layout = make(<tree-control-layout>);
    tree.%layout-pane    := layout;
    layout.%control-pane := tree;
    let scroll-bars = gadget-scroll-bars(tree);
    sheet-child(tree) 
      := if (scroll-bars == #"none")
	   layout
	 else
	   scrolling (scroll-bars: scroll-bars, border-type: #f)
	     layout
	    end
         end
  end
end method initialize;


/// Tree control layout

// A streamlined version of column-layouts, with drawing capabilities
define sealed class <tree-control-layout>
    (<homegrown-control-layout-mixin>, <column-layout>)
end class <tree-control-layout>;

define sealed domain make (singleton(<tree-control-layout>));
define sealed domain initialize (<tree-control-layout>);

// Like 'box-pane-allocate-space', but much more streamlined
define method do-allocate-space
    (layout :: <tree-control-layout>, width :: <integer>, height :: <integer>) => ()
  let nodes = sheet-children(layout);
  let indentation = $tree-control-generation-indentation;
  let offset = $tree-control-indentation-offset;
  let y-spacing  :: <integer> = layout-y-spacing(layout);
  let y-position :: <integer> = 0;
  for (node :: <tree-node-pane> in nodes)
    unless (sheet-withdrawn?(node))
      let x-position = indentation * node-generation(node) + offset;
      let space-req = compose-space(node);
      let (w, w-, w+, h, h-, h+) = space-requirement-components(node, space-req);
      ignore(w-, w+, h-, h+);
      let width  :: <integer> = w;
      let height :: <integer> = h;
      set-sheet-edges
	(node, x-position, y-position, x-position + width, y-position + height);
      inc!(y-position, height + y-spacing)
    end
  end
end method do-allocate-space;

define method handle-repaint
    (layout :: <tree-control-layout>, medium :: <medium>, region :: <region>) => ()
  let tree :: <tree-control-pane> = layout.%control-pane;
  when (tree-control-show-edges?(tree))
   with-drawing-options (medium, brush: $tree-control-gray)
      //--- The fudge offsets here are dubious...
      local method draw-edges (node :: <tree-node-pane>) => ()
	      unless (sheet-withdrawn?(node))
		let (from-x, from-y) = sheet-position(node);
		inc!(from-x, 4);
		inc!(from-y, box-height(sheet-region(node)) - 2);
		for (child :: <tree-node-pane> in node-children(node))
		  unless (sheet-withdrawn?(child))
		    let (to-x, to-y) = sheet-position(child);
		    dec!(to-x, 4);
		    inc!(to-y, truncate/(box-height(sheet-region(child)), 2));
		    draw-line(medium, from-x, from-y, from-x, to-y);
		    draw-line(medium, from-x, to-y, to-x, to-y);
		    unless (empty?(node-children(child)))
		      draw-edges(child)
		    end
		  end
		end
	      end
	    end method;
      for (node in tree-control-root-nodes(tree))
	draw-edges(node)
      end
    end
  end
end method handle-repaint;


/// Roots and items

// Build the items for the first time when the sheet is fully attached
define method note-sheet-attached
    (tree :: <tree-control-pane>) => ()
  next-method();
  initialize-tree-control-icons(port(tree), tree);
  //--- Too bad we've lost the 'value:' initarg by the time we get here
  note-tree-control-roots-changed(tree)
end method note-sheet-attached;

define sealed method note-tree-control-roots-changed
    (tree :: <tree-control-pane>, #key value = $unsupplied) => ()
  with-busy-cursor (tree)
    next-method();
    let roots  = tree-control-roots(tree);
    let layout = tree.%layout-pane;
    let state  = sheet-state(tree);
    when (state == #"mapped")
      clear-box*(sheet-viewport(layout) | layout, sheet-viewport-region(layout))
    end;
    gadget-selection(tree) := #[];
    sheet-children(layout) := #[];
    tree.%visible-items := #f;
    let generator :: <function> = tree-control-children-generator(tree);
    let predicate :: <function> = tree-control-children-predicate(tree);
    local method add-one (node, object, depth) => ()
	    let child-node = make-node(tree, object);
	    add-node(tree, node, child-node, setting-roots?: #t);
	    sheet-withdrawn?(child-node, do-repaint?: #f) := #f;
	    when (depth > 0 & predicate(object))
	      for (child in generator(object))
		add-one(child-node, child, depth - 1)
	      end;
	      node-state(child-node) := #"expanded"
	    end
	  end method;
    delaying-layout (tree)
      for (root in roots)
	add-one(tree, root, tree-control-initial-depth(tree))
      end;
      let items = gadget-items(tree);
      // Try to preserve the old value and selection
      select (gadget-selection-mode(tree))
	#"single" =>
	  unless (empty?(items))
	    let index = supplied?(value) & position(items, value);
	    if (index)
	      gadget-selection(tree) := vector(index)
	    else
	      gadget-selection(tree) := #[0]
	    end
	  end;
	#"multiple" =>
	  let selection :: <stretchy-object-vector> = make(<stretchy-vector>);
	  when (supplied?(value))
	    for (v in value)
	      let index = position(items, v);
	      when (index)
		add!(selection, index)
	      end
	    end
	  end;
	  unless (empty?(selection))
	    gadget-selection(tree) := selection
	  end;
	otherwise =>
	  #f;
      end;
    end
  end
end method note-tree-control-roots-changed;


/// Generic implementation of tree node panes

define sealed class <tree-node-pane>
    (<tree-node-pane-mixin>, <tree-node>)
end class <tree-node-pane>;

define sealed domain make (singleton(<tree-node-pane>));
define sealed domain initialize (<tree-node-pane>);

define constant $tree-control-generation-indentation :: <integer> = 16;
define constant $tree-control-indentation-offset     :: <integer> =  4;

// Add generation spacing to each row to account for the indentation
define method do-compose-space
    (node :: <tree-node-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let space-req   = next-method();
  let indentation = $tree-control-generation-indentation;
  let offset      = $tree-control-indentation-offset;
  let extra       = indentation * node-generation(node) + offset;
  space-requirement+(node, space-req,
		     width: extra, min-width: extra, max-width: extra)
end method do-compose-space;

define method do-make-node 
    (tree :: <tree-control-pane>, class == <tree-node>,
     #rest initargs, #key object)
 => (item :: <tree-node-pane>)
  let framem = frame-manager(tree);
  let label-function = gadget-label-key(tree);
  let icon-function  = tree-control-icon-function(tree);
  let label = (label-function & label-function(object)) | "";
  let (icon, selected-icon)
    = if (icon-function) icon-function(object) else values(#f, #f) end;
  apply(make, <tree-node-pane>,
	tree:  tree,
	label: label,
	icon:  icon,
	selected-icon: selected-icon,
	x-spacing: 4, y-alignment: #"center",
	frame-manager: framem,
	initargs)
end method do-make-node;

define sealed method do-find-node
    (tree :: <tree-control-pane>, object, #key node: parent-node)
 => (node :: false-or(<tree-node-pane>))
  let key  = gadget-value-key(tree);
  let test = gadget-test(tree);
  let the-key = key(object);
  block (return)
    for (node in sheet-children(tree.%layout-pane))
      when (test(key(node-object(node)), the-key))
	// Is it a child of the requested node?
	when (~parent-node | member?(node, node-children(parent-node)))
	  return(node)
	end
      end
    end;
    #f
  end
end method do-find-node;

// Note that we don't relayout the tree -- that only happens
// in 'expand-node' and 'contract-node'
define sealed method do-add-node
    (tree :: <tree-control-pane>, parent, node :: <tree-node-pane>, #key after) => ()
  tree.%visible-items := #f;
  unless (after)
    // The idea here is to ensure that the new node comes after
    // the last node (and all of that node's descendents) in its
    // own generation
    local method last-child (node) => (child)
	    if (empty?(node-children(node)))
	      node
	    else
	      last-child(last(node-children(node)))
	    end
	  end method;
    unless (empty?(node-parents(node)))
      after := last-child(node-parents(node)[0])
    end
  end;
  let layout = tree.%layout-pane;
  let index = after & position(sheet-children(layout), after);
  add-child(layout, node, index: if (index) index + 1 else #"end" end);
  sheet-withdrawn?(node, do-repaint?: #f) := #t
end method do-add-node;

define sealed method do-add-nodes
    (tree :: <tree-control-pane>, parent, nodes :: <sequence>, #key after) => ()
  let selected-nodes = gadget-selected-nodes(tree);
  tree.%visible-items := #f;
  gadget-selection(tree) := #[];
  for (node in nodes)
    add-node(tree, parent, node, after: after)
  end;
  gadget-selection(tree) := compute-gadget-selection(tree, selected-nodes)
end method do-add-nodes;

// Note that we don't relayout the tree -- that only happens
// in 'expand-node' and 'contract-node'
define sealed method do-remove-node
    (tree :: <tree-control-pane>, node :: <tree-node-pane>) => ()
  tree.%visible-items := #f;
  remove-child(tree.%layout-pane, node)
end method do-remove-node;

define sealed method node-children-setter
    (children :: <sequence>, node :: <tree-node-pane>)
 => (children :: <sequence>)
  let tree = control-for-item(node);
  let old-children = node-children(node);
  let selected-nodes = gadget-selected-nodes(tree);
  tree.%visible-items := #f;
  gadget-selection(tree) := #[];
  next-method();
  delaying-layout (tree)
    for (n in old-children)
      remove-child(tree.%layout-pane, n)
    end;
    for (n in children)
      add-node(tree, node, n)
    end;
    gadget-selection(tree) := compute-gadget-selection(tree, selected-nodes)
  end;
  children
end method node-children-setter;

define sealed method gadget-selected-nodes
    (tree :: <tree-control-pane>) => (nodes :: <sequence>)
  let nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let selection = gadget-selection(tree);
  let index :: <integer> = -1;
  for (node :: <tree-node-pane> in sheet-children(tree.%layout-pane))
    unless (sheet-withdrawn?(node))
      inc!(index);
      when (member?(index, selection))
	add!(nodes, node)
      end
    end
  end;
  nodes
end method gadget-selected-nodes;

//--- Not correct if the same object appears twice in the tree
define sealed method compute-gadget-selection 
    (tree :: <tree-control-pane>, nodes) => (selection :: <sequence>)
  let new-selection :: <stretchy-object-vector> = make(<stretchy-vector>);
  let items = gadget-items(tree);
  for (node :: <tree-node-pane> in nodes)
    let index = position(items, node-object(node));
    when (index)
      add!(new-selection, index)
    end
  end;
  new-selection
end method compute-gadget-selection;

define sealed method do-expand-node
    (tree :: <tree-control-pane>, node :: <tree-node-pane>) => ()
  tree.%visible-items := #f;
  delaying-layout (tree)
    let mapped? = sheet-mapped?(node);
    local method unwithdraw (node :: <tree-node-pane>)
	    // Only map in the first level
	    sheet-withdrawn?(node, do-repaint?: #f) := #f;
	    sheet-mapped?(node, do-repaint?: #f) := mapped?;
	  end method;
    do(unwithdraw, node-children(node));
  end
end method do-expand-node;

define sealed method do-contract-node
    (tree :: <tree-control-pane>, node :: <tree-node-pane>) => ()
  tree.%visible-items := #f;
  delaying-layout (tree)
    local method withdraw (node :: <tree-node-pane>)
	    sheet-withdrawn?(node, do-repaint?: #f) := #t;
	    when (node-state(node) == #"expanded")
	      node-state(node) := #"contracted"
	    end;
	    // Withdraw all the way to the bottom
	    do(withdraw, node-children(node))
	  end method;
    do(withdraw, node-children(node))
  end
end method do-contract-node;


define sealed method node-label
    (node :: <tree-node-pane>) => (label :: false-or(<string>))
  gadget-label(node)
end method node-label;

define sealed method node-label-setter
    (label :: false-or(<string>), node :: <tree-node-pane>) => (label :: false-or(<string>))
  gadget-label(node) := label;
  block (break)
    for (child in sheet-children(node))
      when (instance?(child, <tree-node-label-button>))
	gadget-label(child) := label;
	clear-box*(node, sheet-region(node));
	repaint-sheet(node, $everywhere);
	break()
      end
    end
  end;
  label
end method node-label-setter;

define sealed method node-icon
    (node :: <tree-node-pane>) => (icon :: false-or(<image>))
  node.%icon
end method node-icon;

define sealed method node-icon-setter
    (icon :: false-or(<image>), node :: <tree-node-pane>) => (icon :: false-or(<image>))
  node.%icon := icon;
  block (break)
    for (child in sheet-children(node))
      when (instance?(child, <label>))
	gadget-label(child) := icon;
	clear-box*(node, sheet-region(node));
	repaint-sheet(node, $everywhere);
	break()
      end
    end
  end;
  icon
end method node-icon-setter;
