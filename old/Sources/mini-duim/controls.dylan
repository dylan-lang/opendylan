Module:    mini-duim
Synopsis:  Mini-DUIM "controls"
Author:    Scott McKay, Andrew Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// List, table, and tree controls

// <list-control>, <table-control>, and <tree-control> all use this
define open abstract class <basic-list-control>
    (<scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <choice-gadget>)
end class <basic-list-control>;

// A <basic-list-item> always has a <basic-list-control> as a parent.
define open abstract class <basic-list-item> 
    (<action-gadget-mixin>,
     <basic-gadget>)
  slot item-object = #f,
    init-keyword: object:;
end class <basic-list-item>;

define open generic make-item
    (list-control, object, #key frame-manager) => (list-item);

define open generic find-item (list-control, object, #key) => (list-item);

define open generic add-item (list-control, item, #key after) => (item);

// The concrete implementation should add the list item to the
// list control, and then re-layout and redisplay the list control
define open generic do-add-item (list-control, item, #key after) => ();

define open generic remove-item (list-control, item) => ();

// The concrete implementation should remove the list item from the
// list control, and then re-layout and redisplay the list control
define open generic do-remove-item (list-control, item) => ();


/// List controls

define constant <list-control-view>
  = one-of(#"small-icon", #"large-icon", #"list", #"details");

// The children of a <list-control> are a bunch of <list-item>'s,
// all layed out in a vertical column
define open abstract class <list-control> (<basic-list-control>)
  slot list-control-icon-function = #f,		// takes an object, produces an icon
    init-keyword: icon-function:;
  slot list-control-view :: false-or(<list-control-view>) = #"list",
    init-keyword: view:;
end class <list-control>;

define open abstract class <list-item> (<basic-list-item>)
  slot list-item-icon = #f,
    init-keyword: icon:;
end class <list-item>;


define method make-item
    (pane :: <list-control>, object,
     #key frame-manager: framem = frame-manager(pane))
 => (list-item :: <list-item>)
  let label-function = gadget-name-key(pane);
  let icon-function = list-control-icon-function(pane);
  let label = label-function(object);
  let icon = icon-function & icon-function(object);
  with-frame-manager (framem)
    make-pane(<list-item>,
              list: pane,
	      object: object,
	      label: label,
	      icon: icon)
  end
end method make-item;

define method find-item
    (pane :: <list-control>, object, #key)
 => (node :: false-or(<list-item>))
  let key = gadget-value-key(pane);
  let test = gadget-test(pane);
  let the-key = key(object);
  block (return)
    for (child in sheet-children(pane))
      when (test(key(item-object(child)), the-key))
	return(child)
      end
      finally #f
    end
  end
end method find-item;


// AFTER indicates which item to place the new item after
define method add-item
    (pane :: <list-control>, item :: <list-item>, #key after)
 => (item :: <list-item>)
  do-add-item(pane, item, after: after);
  item
end method add-item;

define method remove-item
    (pane :: <list-control>, item :: <list-item>) => ()
  do-remove-item(pane, item)  
end method remove-item;


/// Table controls

define constant <table-control-view>
  = one-of(#"small-icon", #"large-icon", #"list", #"details");

// The children of a <table-control> are a bunch of <table-item>'s,
// all layed out in a series of vertical columns.  The headings
// give both the headings and the space requirements for the columns
// in each <table-item>.
define open abstract class <table-control> (<basic-list-control>)
  slot table-control-headings = make(<stretchy-vector>),
    init-keyword: headings:;
  slot table-control-generators = make(<stretchy-vector>),
    init-keyword: generators:;
  slot table-control-view :: false-or(<table-control-view>) = #"details",
    init-keyword: view:;
end class <table-control>;

// A table item always has a <table-control> as a parent.  It just lays
// its kids in the columns specified by the table control.
define open abstract class <table-item> (<basic-list-item>)
end class <table-item>;


define method make-item
    (pane :: <table-control>, object,
     #key frame-manager: framem = frame-manager(pane))
 => (table-item :: <table-item>)
  with-frame-manager (framem)
    make-pane(<table-item>,
	      table: pane,
	      object: object)
  end
end method make-item;

define method find-item
    (pane :: <table-control>, object, #key)
 => (node :: false-or(<table-item>))
  let key = gadget-value-key(pane);
  let test = gadget-test(pane);
  let the-key = key(object);
  block (return)
    for (child in sheet-children(pane))
      when (test(key(item-object(child)), the-key))
	return(child)
      end
      finally #f
    end
  end
end method find-item;


// AFTER indicates which item to place the new item after
define method add-item
    (pane :: <table-control>, item :: <table-item>, #key after)
 => (item :: <table-item>)
  do-add-item(pane, item, after: after);
  item
end method add-item;

define method remove-item
    (pane :: <table-control>, item :: <table-item>) => ()
  do-remove-item(pane, item)  
end method remove-item;


define method add-column
    (pane :: <table-control>, heading, generator, index) => ()
  insert-at!(table-control-headings(pane), heading, index);
  insert-at!(table-control-generators(pane), generator, index);
  do-add-column(pane, heading, generator, index)
end method add-column;

define open generic do-add-column
    (pane :: <table-control>, heading, generator, index) => ();

define method remove-column
    (pane :: <table-control>, index) => ()
  do-remove-column(pane, index);
  remove-at!(table-control-headings(pane), index);
  remove-at!(table-control-generators(pane), index)
end method remove-column;

define open generic do-remove-column
    (pane :: <table-control>, index) => ();


/// Tree controls

// The children of a <tree-control> are a bunch of <tree-node>'s,
// all layed out in a vertical column
define open abstract class <tree-control> (<basic-list-control>)
  slot %roots = make(<stretchy-vector>);	// the set of root nodes
  slot tree-control-inferior-producer,		// takes an object, produces child objects
    required-init-keyword: inferior-producer:;
  slot tree-control-inferior-predicate = default-inferior-predicate,
    init-keyword: inferior-predicate:;
  slot tree-control-icon-function = #f,		// takes an object, produces two icons
    init-keyword: icon-function:;
  slot tree-control-show-edges? = #t,
    init-keyword: show-edges?:;
  slot tree-control-show-root-edges? = #f,
    init-keyword: show-root-edges?:;
  slot tree-control-show-buttons? = #t,
    init-keyword: show-buttons?:;
end class <tree-control>;

define function default-inferior-predicate (node) => (true? :: <boolean>)
  ignore(node);
  #t
end function default-inferior-predicate;

define open generic tree-control-roots
    (tree :: <tree-control>) => (roots :: <sequence>);
define open generic tree-control-roots-setter
    (roots :: <sequence>, tree :: <tree-control>, #key frame-manager)
 => (roots :: <sequence>);

define method tree-control-roots
    (tree :: <tree-control>) => (roots :: <sequence>)
  tree.%roots
end method tree-control-roots;

define method tree-control-roots-setter
    (roots :: <sequence>, tree :: <tree-control>, #key frame-manager: framem)
 => (roots :: <sequence>)
  ignore(framem);
  // The back end is responsible for filling this with tree node objects...
  tree.%roots := make(<stretchy-vector>)
end method tree-control-roots-setter;


// A tree node always has a <tree-control> as a parent.  It just lays
// out the button, icon, and label in a row, with the appropriate indentation.
define open abstract class <tree-node> (<basic-list-item>)
  slot node-parents = make(<stretchy-vector>),
    init-keyword: node-parents:;
  slot node-children = make(<stretchy-vector>),
    init-keyword: node-children:;
  slot node-expanded? = #f;
  slot node-selected-icon = #f,
    init-keyword: selected-icon:;
  slot node-deselected-icon = #f,
    init-keyword: deselected-icon:;
  slot node-generation = 0,
    init-keyword: generation:;
end class <tree-node>;


//--- Windows has "before expand" and "after expand" notifications
define method expand-node (node :: <tree-node>, #key sort-function) => ()
  unless (node-expanded?(node))
    let tree = sheet-parent(node);
    let inferior-producer = tree-control-inferior-producer(tree);  
    let inferiors = inferior-producer(item-object(node));
    when (sort-function)
      inferiors := sort-function(inferiors)
    end;
    node-expanded?(node) := #t;
    do-expand-node(tree, node, inferiors)
  end
end method expand-node;

define open generic do-expand-node 
    (tree :: <tree-control>, node :: <tree-node>, inferiors) => ();


define method contract-node (node :: <tree-node>) => ()
  when (node-expanded?(node))
    let tree = sheet-parent(node);
    node-expanded?(node) := #f;
    do-contract-node(tree, node)
  end
end method contract-node;

define open generic do-contract-node 
    (tree :: <tree-control>, node :: <tree-node>) => ();


//--- This should recompute the gadget's contents from its dynamic
//--- functions.  In the tree-control case, it should try and preserve
//--- the expanded/contracted nature of the tree.
define method update-gadget (gadget :: <tree-control>) => ()
  let expanded-items = make(<stretchy-vector>);
  for (node in sheet-children(gadget))
    if (node-expanded?(node))
      add!(expanded-items, item-object(node))
    end
  end;
  do(method (node)
       contract-node(node);
       update-node-inferiors(gadget, node, expanded-items)
     end,
     tree-control-roots(gadget))
end method update-gadget;

define method update-node-inferiors 
    (gadget :: <tree-control>, node :: <tree-node>,
     expanded-items :: <sequence>)
 => ()
  if (member?(item-object(node), expanded-items, test: gadget-test(gadget)))
    expand-node(node);
    for (child-node in node-children(node))
      update-node-inferiors(gadget, child-node, expanded-items)
    end
  end
end method update-node-inferiors;


define method make-item
    (tree :: <tree-control>, object,
     #key frame-manager: framem = frame-manager(tree))
 => (node :: <tree-node>)
  let label-function = gadget-name-key(tree);
  let icon-function = tree-control-icon-function(tree);
  let label = label-function(object);
  let (selected-icon, deselected-icon)
    = if (icon-function) icon-function(object) else values(#f, #f) end;
  with-frame-manager (framem)
    make-pane(<tree-node>,
	      tree: tree,
	      object: object,
	      label: label,
	      selected-icon: selected-icon,
	      deselected-icon: deselected-icon | selected-icon)
  end
end method make-item;

define method find-item
    (tree :: <tree-control>, object, #key node)
 => (node :: false-or(<tree-node>))
  let key = gadget-value-key(tree);
  let test = gadget-test(tree);
  let the-key = key(object);
  block (return)
    for (child in sheet-children(tree))
      when (test(key(item-object(child)), the-key))
	// Is it a child of the requested node?
	when (~node | member?(child, node-children(node)))
	  return(child)
	end
      end
      finally #f
    end
  end
end method find-item;


// AFTER indicates which root to place the new root after
define method add-item
    (tree :: <tree-control>, node :: <tree-node>, #key after)
 => (node :: <tree-node>)
  let roots = tree-control-roots(tree);
  node-generation(node) := 0;
  node-parents(node) := #[];
  do-add-item(tree, node, after: after);
  let index = after & find-key(roots, curry(\==, after));
  // Insert the new node into the set of roots, but avoid calling
  // 'tree-roots-setter' because it expects objects (not nodes)
  insert-at!(roots, node, index | #"end");
  node
end method add-item;

// AFTER indicates which of NODE's children to place the new node after
define method add-item
    (parent :: <tree-node>, node :: <tree-node>, #key after)
 => (node :: <tree-node>)
  let tree = sheet-parent(parent);
  let children = node-children(parent);
  node-generation(node) := node-generation(parent) + 1;
  node-parents(node) := make(<stretchy-vector>, size: 1, fill: parent);
  do-add-item(tree, node, after: after);
  let index = after & find-key(children, curry(\==, after));
  insert-at!(children, node, index | #"end");
  node
end method add-item;


define method remove-item
    (tree :: <tree-control>, node :: <tree-node>) => ()
  let roots = tree-control-roots(tree);
  // Avoid calling 'tree-roots-setter' because it deals in objects,
  // not in nodes
  remove!(roots, node);
  do-remove-item(tree, node)
end method remove-item;

define method remove-item
    (parent :: <tree-node>, node :: <tree-node>) => ()
  let tree = sheet-parent(parent);
  remove!(node-children(parent), node);
  do-remove-item(tree, node)
end method remove-item;
