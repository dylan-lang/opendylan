Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Tree controls

// The nodes in a tree control are layed out in a column (with indentation)
// Note that nodes are not modelled as sheets!
define open abstract class <tree-control>
    (<bordered-gadget-mixin>,
     <scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <key-press-gadget-mixin>,
     <popup-menu-gadget-mixin>,
     <basic-choice-gadget>)
  sealed constant slot gadget-lines :: false-or(<integer>) = #f,
    init-keyword: lines:;
  sealed slot tree-control-roots :: <stretchy-object-vector> = make(<stretchy-vector>),
    setter: %roots-setter;
  sealed slot tree-control-root-nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  // How deep should the tree be initially?
  sealed slot tree-control-initial-depth :: <integer> = 0,
    init-keyword: depth:;
  // Takes an object, produces child objects
  sealed slot tree-control-children-generator :: <function>,
    required-init-keyword: children-generator:;
  // Takes an object, returns #t iff there are any child objects
  sealed slot tree-control-children-predicate :: <function> = default-children-predicate,
    init-keyword: children-predicate:;
  // Takes an object, produces two icons -- a normal and a selected icon
  sealed constant slot tree-control-icon-function :: false-or(<function>) = #f,
    init-keyword: icon-function:;
  // Compressed tree control flag word
  sealed slot tree-control-flags :: <integer> = $initial-tree-control-flags;
  // Callback for node-state changed
  sealed slot tree-node-state-changed-callback :: <callback-type> = #f,
    init-keyword: node-state-changed-callback:;
end class <tree-control>;

// Bits 0..2 are some basic boolean flags
define constant %tree_show_edges      :: <integer> = #o01;
define constant %tree_show_root_edges :: <integer> = #o02;
define constant %tree_show_buttons    :: <integer> = #o04;

define constant $initial-tree-control-flags :: <integer>
    = logior(%tree_show_edges, %tree_show_root_edges, %tree_show_buttons);

define method initialize
    (tree :: <tree-control>,
     #key roots = #[], show-edges? = #t, show-root-edges? = #t, show-buttons? = #t)
  next-method();
  let bits = logior(if (show-edges?)      %tree_show_edges      else 0 end,
		    if (show-root-edges?) %tree_show_root_edges else 0 end,
		    if (show-buttons?)    %tree_show_buttons    else 0 end);
  tree-control-flags(tree) := bits;
  tree.%roots := as(<stretchy-vector>, roots);
  tree.%items := make(<stretchy-vector>)
end method initialize;


define sealed inline method tree-control-show-edges?
    (tree :: <tree-control>) => (show-edges? :: <boolean>)
  logand(tree-control-flags(tree), %tree_show_edges) = %tree_show_edges
end method tree-control-show-edges?;

define sealed inline method tree-control-show-edges?-setter
    (show-edges? :: <boolean>, tree :: <tree-control>) => (show-edges? :: <boolean>)
  tree-control-flags(tree)
    := logior(logand(tree-control-flags(tree), lognot(%tree_show_edges)),
	      if (show-edges?) %tree_show_edges else 0 end);
  show-edges?
end method tree-control-show-edges?-setter;

define sealed inline method tree-control-show-root-edges?
    (tree :: <tree-control>) => (show-root-edges? :: <boolean>)
  logand(tree-control-flags(tree), %tree_show_root_edges) = %tree_show_root_edges
end method tree-control-show-root-edges?;

define sealed inline method tree-control-show-root-edges?-setter
    (show-root-edges? :: <boolean>, tree :: <tree-control>) => (show-root-edges? :: <boolean>)
  tree-control-flags(tree)
    := logior(logand(tree-control-flags(tree), lognot(%tree_show_root_edges)),
	      if (show-root-edges?) %tree_show_root_edges else 0 end);
  show-root-edges?
end method tree-control-show-root-edges?-setter;

define sealed inline method tree-control-show-buttons?
    (tree :: <tree-control>) => (show-buttons? :: <boolean>)
  logand(tree-control-flags(tree), %tree_show_buttons) = %tree_show_buttons
end method tree-control-show-buttons?;

define sealed inline method tree-control-show-buttons?-setter
    (show-buttons? :: <boolean>, tree :: <tree-control>) => (show-buttons? :: <boolean>)
  tree-control-flags(tree)
    := logior(logand(tree-control-flags(tree), lognot(%tree_show_buttons)),
	      if (show-buttons?) %tree_show_buttons else 0 end);
  show-buttons?
end method tree-control-show-buttons?-setter;


// #f means that no items have ever been added to the node
define constant <node-state> = one-of(#"expanded", #"contracted", #f);

define open abstract class <tree-node> (<item>)
  sealed constant slot node-object = #f,
    init-keyword: object:;
  sealed slot node-parents :: <sequence> = make(<stretchy-vector>),
    init-keyword: node-parents:;
  sealed slot node-children :: <sequence> = make(<stretchy-vector>),
    init-keyword: node-children:,
    setter: %node-children-setter;
  sealed slot node-state :: <node-state> = #f;
  sealed slot node-generation :: <integer> = 0,
    init-keyword: generation:;
end class <tree-node>;

define protocol <<tree-control>> ()
  getter tree-control-roots
    (tree :: <tree-control>) => (roots :: <sequence>);
  setter tree-control-roots-setter
    (roots :: <sequence>, tree :: <tree-control>)
 => (roots :: <sequence>);
  getter tree-control-root-nodes
    (tree :: <tree-control>) => (nodes :: false-or(<sequence>));
  setter tree-control-root-nodes-setter
    (nodes :: false-or(<sequence>), tree :: <tree-control>)
 => (nodes :: false-or(<sequence>));
  function note-tree-control-roots-changed
    (tree :: <tree-control>, #key value) => ();
  getter tree-control-expanded-objects
    (tree :: <tree-control>)
 => (objects :: <sequence>, depth :: <integer>);
  setter tree-control-expanded-objects-setter
    (objects :: <sequence>, tree :: <tree-control>, #key depth)
 => (objects :: <sequence>);
  getter tree-control-expanded-object-count
    (tree :: <tree-control>)
 => (count :: <integer>, depth :: <integer>);
  function make-node (tree-control, object, #key, #all-keys) => (node);
  function find-node (tree-control, object, #key, #all-keys) => (node);
  function add-node (tree-control, parent, node, #key after, setting-roots?) => ();
  function remove-node (tree-control, node) => ();
  function expand-node (tree-control, node) => ();
  function contract-node (tree-control, node) => ();
  function do-make-node (tree-control, node-class, #key, #all-keys) => (node);
  function do-find-node (tree-control, object, #key, #all-keys) => (node);
  function do-add-node (tree-control, parent, node, #key after) => ();
  function do-add-nodes (tree-control, parent, nodes :: <sequence>, #key after) => ();
  function do-remove-node (tree-control, node) => ();
  function do-expand-node (tree-control, node) => ();
  function do-contract-node (tree-control, node) => ();
  // Node state changed notification callback
  getter tree-node-state-changed-callback
    (tree :: <tree-control>) => (callback :: <callback-type>);
  setter tree-node-state-changed-callback-setter
    (callback :: <callback-type>, tree :: <tree-control>)
 => (callback :: <callback-type>);
end protocol <<tree-control>>;

define protocol <<tree-node>> ()
  getter node-object
    (node :: <tree-node>) => (object);
  getter node-parents
    (node :: <tree-node>) => (parents :: <sequence>);
  setter node-parents-setter
    (parents :: <sequence>, node :: <tree-node>) => (parents :: <sequence>);
  getter node-children
    (node :: <tree-node>) => (children :: <sequence>);
  setter node-children-setter
    (children :: <sequence>, node :: <tree-node>) => (children :: <sequence>);
  getter node-label
    (node :: <tree-node>) => (label :: false-or(<string>));
  setter node-label-setter
    (label :: false-or(<string>), node :: <tree-node>) => (label :: false-or(<string>));
  getter node-icon
    (node :: <tree-node>) => (icon :: false-or(<image>));
  setter node-icon-setter
    (icon :: false-or(<image>), node :: <tree-node>) => (icon :: false-or(<image>));
end protocol <<tree-node>>;


// For convenience...
define sealed inline method item-object
    (node :: <tree-node>) => (object)
  node-object(node)
end method item-object;


define function default-children-predicate (node) => (true? :: <boolean>)
  ignore(node);
  #t
end function default-children-predicate;

define method tree-control-roots-setter
    (roots :: <sequence>, tree :: <tree-control>)
 => (roots :: <sequence>)
  let value = gadget-value(tree);
  tree.%roots := as(<stretchy-vector>, roots);
  note-tree-control-roots-changed(tree, value: value);
  roots
end method tree-control-roots-setter;

define method note-tree-control-roots-changed
     (tree :: <tree-control>, #key value = $unsupplied) => ()
  ignore(value);
  // The back end needs to fill these in...
  gadget-items(tree).size := 0;
  tree-control-root-nodes(tree).size := 0;
end method note-tree-control-roots-changed;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <tree-control>)
 => (items :: <sequence>)
  error("Use 'tree-control-roots-setter' to set the items of a tree control");
  items
end method gadget-items-setter;


/// Gadget state

define method update-gadget
    (tree :: <tree-control>) => ()
  let state = gadget-state(tree);
  tree-control-roots(tree) := #[];
  gadget-state(tree) := state
end method update-gadget;

define method tree-control-expanded-objects
    (tree :: <tree-control>)
 => (objects :: <stretchy-vector>, depth :: <integer>)
  let objects :: <stretchy-object-vector> = make(<stretchy-vector>);
  let depth :: <integer> = 0;
  local method walk-nodes (node :: <tree-node>) => ()
	  when (node-state(node) == #"expanded")
	    add!(objects, node-object(node));
	    max!(depth, node-generation(node));
	    do(walk-nodes, node-children(node))
	  end
	end method;
  do(walk-nodes, tree-control-root-nodes(tree));
  values(objects, depth)
end method tree-control-expanded-objects;

define method tree-control-expanded-objects-setter
    (objects :: <sequence>, tree :: <tree-control>, #key depth = 1)
 => (objects :: <sequence>)
  local method expand-one (node :: <tree-node>) => ()
	  when (member?(node-object(node), objects, test: gadget-test(tree)))
	    expand-node(tree, node);
	    when (node-generation(node) <= depth)
	      do(expand-one, node-children(node))
	    end
	  end
	end method;
  do(expand-one, tree-control-root-nodes(tree));
  objects
end method tree-control-expanded-objects-setter;

define method tree-control-expanded-object-count
    (tree :: <tree-control>)
 => (count :: <integer>, depth :: <integer>)
  let count :: <integer> = 0;
  let depth :: <integer> = 0;
  local method walk-nodes (node :: <tree-node>) => ()
	  when (node-state(node) == #"expanded")
	    inc!(count, 1);
	    max!(depth, node-generation(node));
	    do(walk-nodes, node-children(node))
	  end
	end method;
  do(walk-nodes, tree-control-root-nodes(tree));
  values(count, depth)
end method tree-control-expanded-object-count;


define sealed class <tree-control-state> (<value-gadget-state>)
  sealed constant slot %state-roots :: <sequence>,
    required-init-keyword: roots:;
  sealed constant slot %state-expanded-objects :: <sequence>,
    required-init-keyword: expanded-objects:;
  sealed constant slot %state-expanded-depth :: <integer>,
    required-init-keyword: expanded-depth:;
end class <tree-control-state>;

define sealed domain make (singleton(<tree-control-state>));
define sealed domain initialize (<tree-control-state>);

define method gadget-state
    (tree :: <tree-control>) => (state :: <tree-control-state>)
  let (objects, depth) = tree-control-expanded-objects(tree);
  make(<tree-control-state>,
       value: gadget-value(tree),
       roots: tree-control-roots(tree),
       expanded-objects: objects,
       expanded-depth: depth)
end method gadget-state;

define method gadget-state-setter
    (state :: <tree-control-state>, tree :: <tree-control>)
 => (state :: <tree-control-state>)
  with-busy-cursor (tree)
    tree-control-roots(tree) := state.%state-roots;
    tree-control-expanded-objects(tree, depth: state.%state-expanded-depth)
      := state.%state-expanded-objects;
    next-method()
  end
end method gadget-state-setter;


/// Tree controls nodes

define sealed method make-node
    (tree :: <tree-control>, object, #rest initargs, #key)
 => (node :: <tree-node>)
  apply(do-make-node, tree, <tree-node>, object: object, initargs)
end method make-node;

define sealed method find-node
    (tree :: <tree-control>, object, #key node: parent-node)
 => (node :: false-or(<tree-node>))
  do-find-node(tree, object, node: parent-node)
end method find-node;

// AFTER indicates which root to place the new root after
define sealed method add-node
    (tree :: <tree-control>, parent :: <tree-control>, node :: <tree-node>,
     #key after, setting-roots?) => ()
  let roots      = tree-control-roots(tree);
  let root-nodes = tree-control-root-nodes(tree);
  node-generation(node) := 0;
  node-parents(node) := #[];
  add!(gadget-items(tree), node-object(node));
  do-add-node(tree, parent, node, after: after);
  let index = after & position(root-nodes, after);
  // Insert the new node into the set of roots
  unless (setting-roots?)	//--- I'm ashamed...
    insert-at!(roots, node-object(node), index | #"end")
  end;
  insert-at!(root-nodes, node, index | #"end")
end method add-node;

// AFTER indicates which of NODE's children to place the new node after
define sealed method add-node
    (tree :: <tree-control>, parent :: <tree-node>, node :: <tree-node>,
     #key after, setting-roots?) => ()
  ignore(setting-roots?);
  let children = node-children(parent);
  node-generation(node) := node-generation(parent) + 1;
  node-parents(node) := make(<stretchy-vector>, size: 1, fill: parent);
  add!(gadget-items(tree), node-object(node));
  do-add-node(tree, parent, node, after: after);
  let index = after & position(children, after);
  insert-at!(children, node, index | #"end");
  node-state(parent) := node-state(parent) | #"contracted"
end method add-node;

define sealed method remove-node
    (tree :: <tree-control>, node :: <tree-node>) => ()
  let roots      = tree-control-roots(tree);
  let root-nodes = tree-control-root-nodes(tree);
  remove!(gadget-items(tree), node-object(node));
  remove!(roots, node-object(node));	// just in case...
  remove!(root-nodes, node);
  do-remove-node(tree, node)
end method remove-node;

define method node-children-setter
    (children :: <sequence>, node :: <tree-node>)
 => (children :: <sequence>)
  node-state(node) := #f;
  node.%node-children := as(<stretchy-vector>, children)
end method node-children-setter;

define sealed method expand-node
    (tree :: <tree-control>, node :: <tree-node>) => ()
  unless (node-state(node))
    with-busy-cursor (tree)
      // If no items have ever been added, do it now
      let children-predicate = tree-control-children-predicate(tree);
      when (children-predicate(node-object(node)))
	let children-generator = tree-control-children-generator(tree);  
	let objects = children-generator(node-object(node));
	let nodes = map-as(<simple-vector>,
			   method (object) make-node(tree, object) end, objects);
	do-add-nodes(tree, node, nodes)
      end;
      node-state(node) := #"contracted"
    end
  end;
  when (node-state(node) == #"contracted")
    node-state(node) := #"expanded";
    do-expand-node(tree, node)
  end
end method expand-node;

define sealed method contract-node
    (tree :: <tree-control>, node :: <tree-node>) => ()
  when (node-state(node) == #"expanded")
    node-state(node) := #"contracted";
    do-contract-node(tree, node)
  end
end method contract-node;

define method ensure-node-visible
    (tree :: <tree-control>, node :: <tree-node>) => ()
  // The back-end is expected to fill this in
  #f
end method ensure-node-visible;


/// Node state changed callback

define sealed method execute-node-state-changed-callback
    (tree :: <tree-control>, client, id, node :: <tree-node>) => ()
  ignore(client, id);
  let callback = tree-node-state-changed-callback(tree);
  if (callback)
    execute-callback(tree, callback, tree, node)
  else
    do-execute-node-state-changed-callback(tree, client, id, node)
  end
end method execute-node-state-changed-callback;

define method do-execute-node-state-changed-callback
    (tree :: <tree-control>, client, id, node :: <tree-node>) => ()
  ignore(client, id, node);
  #f
end method do-execute-node-state-changed-callback;

define sealed class <node-state-changed-gadget-event> (<gadget-event>)
  sealed constant slot event-tree-node,
    required-init-keyword: node:;
end class <node-state-changed-gadget-event>;

define sealed domain make (singleton(<node-state-changed-gadget-event>));
define sealed domain initialize (<node-state-changed-gadget-event>);

define sealed method handle-event
    (tree :: <tree-control>, event :: <node-state-changed-gadget-event>) => ()
  execute-node-state-changed-callback
    (tree, gadget-client(tree), gadget-id(tree), event-tree-node(event))
end method handle-event;

define function distribute-node-state-changed-callback
    (tree :: <tree-control>, node :: <tree-node>) => ()
  distribute-event(port(tree),
		   make(<node-state-changed-gadget-event>,
			gadget: tree,
			node: node))
end function distribute-node-state-changed-callback;
