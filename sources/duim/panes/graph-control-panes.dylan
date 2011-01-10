Module:       duim-gadget-panes-internals
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of graph control panes

define sealed class <graph-control-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <homegrown-tree-control-mixin>,
     <graph-control>,
     <single-child-wrapping-pane>)
end class <graph-control-pane>;

define sideways method class-for-make-pane 
    (framem :: <frame-manager>, class == <graph-control>, #key graph-type = #"tree")
 => (class :: <class>, options :: false-or(<sequence>))
  select (graph-type)
    #"tree" => values(<tree-graph-pane>, #f);
    #"DAG"  => values(<DAG-graph-pane>,  #f);
  end
end method class-for-make-pane;

define sealed domain make (subclass(<graph-control-pane>));
define sealed domain initialize (<graph-control-pane>);

define method initialize
    (graph :: <graph-control-pane>,
     #key frame-manager: framem)
  next-method();
  with-frame-manager (framem)
    let layout-class
      = layout-class-for-graph(graph);
    let layout
      = make(layout-class, graph-control: graph);
    graph.%layout-pane   := layout;
    layout.%control-pane := graph;
    let scroll-bars = gadget-scroll-bars(graph);
    sheet-child(graph) 
      := if (scroll-bars == #"none")
	   layout
	 else
	   scrolling (scroll-bars: scroll-bars, border-type: #f)
	     layout
	   end
         end
  end;
end method initialize;


/// Roots and items

// Build the items for the first time when the sheet is fully attached
define method note-sheet-attached
    (graph :: <graph-control-pane>) => ()
  next-method();
  initialize-tree-control-icons(port(graph), graph);
  //--- Too bad we've lost the 'value:' initarg by the time we get here
  note-tree-control-roots-changed(graph)
end method note-sheet-attached;

define sealed method note-tree-control-roots-changed
    (graph :: <graph-control-pane>, #key value = $unsupplied) => ()
  with-busy-cursor (graph)
    next-method();
    let roots  = tree-control-roots(graph);
    let layout = graph.%layout-pane;
    let state  = sheet-state(graph);
    when (state == #"mapped")
      clear-box*(sheet-viewport(layout) | layout, sheet-viewport-region(layout))
    end;
    gadget-selection(graph) := #[];
    sheet-children(layout) := #[];
    graph.%visible-items := #f;
    delaying-layout (graph)
      generate-graph-nodes(graph, roots);
      let items = gadget-items(graph);
      // Try to preserve the old value and selection
      select (gadget-selection-mode(graph))
	#"single" =>
	  unless (empty?(items))
	    let index = supplied?(value) & position(items, value);
	    if (index)
	      gadget-selection(graph) := vector(index)
	    else
	      gadget-selection(graph) := #[0]
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
	    gadget-selection(graph) := selection
	  end;
	otherwise =>
	  #f;
      end;
    end
  end
end method note-tree-control-roots-changed;

define sealed method gadget-items
    (graph :: <graph-control-pane>) => (items :: <sequence>)
  graph.%visible-items
  | begin
      let layout = graph.%layout-pane;
      if (layout)
	let items :: <stretchy-object-vector> = make(<stretchy-vector>);
	for (node in sheet-children(layout))
	  unless (sheet-withdrawn?(node))
	    add!(items, node-object(node))
	  end
	end;
	when (graph-edge-generator(graph))
	  for (edge :: <graph-edge> in layout.%edges)
	    when (node-state(graph-edge-from-node(edge)) == #"expanded")
	      unless (sheet-withdrawn?(graph-edge-to-node(edge)))
		add!(items, graph-edge-object(edge))
	      end
	    end
	  end
	end;
	graph.%visible-items := items
      else
        #[]
      end
    end
end method gadget-items;


/// Generic implementation of graph node panes

define sealed class <graph-node-pane>
    (<tree-node-pane-mixin>, <graph-node>)
end class <graph-node-pane>;

define sealed domain make (singleton(<graph-node-pane>));
define sealed domain initialize (<graph-node-pane>);

define method do-make-node 
    (graph :: <graph-control-pane>, class == <graph-node>,
     #rest initargs, #key object)
 => (item :: <graph-node-pane>)
  let framem = frame-manager(graph);
  let label-function = gadget-label-key(graph);
  let icon-function  = tree-control-icon-function(graph);
  let label = (label-function & label-function(object)) | "";
  let (icon, selected-icon)
    = if (icon-function) icon-function(object) else values(#f, #f) end;
  apply(make, <graph-node-pane>,
	tree:  graph,
	label: label,
	icon:  icon,
	selected-icon: selected-icon,
	x-spacing: 4, y-alignment: #"center",
	frame-manager: framem,
	initargs)
end method do-make-node;

define method node-x
    (node :: <graph-node-pane>) => (x :: <integer>)
  let (x, y) = sheet-position(node);
  ignore(y);
  x
end method node-x;

define method node-x-setter
    (new-x :: <integer>, node :: <graph-node-pane>) => (x :: <integer>)
  let (x, y) = sheet-position(node);
  ignore(x);
  set-sheet-position(node, new-x, y);
  new-x
end method node-x-setter;

define method node-y
    (node :: <graph-node-pane>) => (y :: <integer>)
  let (x, y) = sheet-position(node);
  ignore(x);
  y
end method node-y;

define method node-y-setter
    (new-y :: <integer>, node :: <graph-node-pane>) => (y :: <integer>)
  let (x, y) = sheet-position(node);
  ignore(y);
  set-sheet-position(node, x, new-y);
  new-y
end method node-y-setter;

define sealed method do-find-node
    (graph :: <graph-control-pane>, object, #key node: parent-node)
 => (node :: false-or(<graph-node-pane>))
  let key  = gadget-value-key(graph);
  let test = gadget-test(graph);
  let the-key = key(object);
  block (return)
    for (node in sheet-children(graph.%layout-pane))
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

// Note that we don't relayout the graph -- that only happens
// in 'expand-node' and 'contract-node'
define sealed method do-add-node
    (graph :: <graph-control-pane>, parent, node :: <graph-node-pane>, #key after) => ()
  graph.%visible-items := #f;
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
  let layout = graph.%layout-pane;
  let index = after & position(sheet-children(layout), after);
  add-child(layout, node, index: if (index) index + 1 else #"end" end);
  sheet-withdrawn?(node, do-repaint?: #f) := #t
end method do-add-node;

define sealed method do-add-nodes
    (graph :: <graph-control-pane>, parent, nodes :: <sequence>, #key after) => ()
  let selected-nodes = gadget-selected-nodes(graph);
  graph.%visible-items := #f;
  gadget-selection(graph) := #[];
  for (node in nodes)
    add-node(graph, parent, node, after: after)
  end;
  gadget-selection(graph) := compute-gadget-selection(graph, selected-nodes)
end method do-add-nodes;

// Note that we don't relayout the graph -- that only happens
// in 'expand-node' and 'contract-node'
define sealed method do-remove-node
    (graph :: <graph-control-pane>, node :: <graph-node-pane>) => ()
  graph.%visible-items := #f;
  remove-child(graph.%layout-pane, node)
end method do-remove-node;

define sealed method gadget-selected-nodes
    (graph :: <graph-control-pane>) => (nodes :: <sequence>)
  let nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let selection = gadget-selection(graph);
  let index :: <integer> = -1;
  for (node :: <graph-node-pane> in sheet-children(graph.%layout-pane))
    unless (sheet-withdrawn?(node))
      inc!(index);
      when (member?(index, selection))
	add!(nodes, node)
      end
    end
  end;
  nodes
end method gadget-selected-nodes;

//--- Not correct if the same object appears twice in the graph
define sealed method compute-gadget-selection 
    (graph :: <graph-control-pane>, nodes) => (selection :: <sequence>)
  let new-selection :: <stretchy-object-vector> = make(<stretchy-vector>);
  let items = gadget-items(graph);
  for (node :: <graph-node-pane> in nodes)
    let index = position(items, node-object(node));
    when (index)
      add!(new-selection, index)
    end
  end;
  new-selection
end method compute-gadget-selection;

define sealed method do-expand-node
    (graph :: <graph-control-pane>, node :: <graph-node-pane>) => ()
  graph.%visible-items := #f;
  delaying-layout (graph)
    let mapped? = sheet-mapped?(node);
    local method unwithdraw (node :: <graph-node-pane>)
	    // Only map in the first level
	    sheet-withdrawn?(node, do-repaint?: #f) := #f;
	    sheet-mapped?(node, do-repaint?: #f) := mapped?;
	  end method;
    do(unwithdraw, node-children(node));
  end
end method do-expand-node;

define sealed method do-contract-node
    (graph :: <graph-control-pane>, node :: <graph-node-pane>) => ()
  graph.%visible-items := #f;
  delaying-layout (graph)
    local method withdraw (node :: <graph-node-pane>)
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
    (node :: <graph-node-pane>) => (label :: false-or(<string>))
  gadget-label(node)
end method node-label;

define sealed method node-label-setter
    (label :: false-or(<string>), node :: <graph-node-pane>) => (label :: false-or(<string>))
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
    (node :: <graph-node-pane>) => (icon :: false-or(<image>))
  node.%icon
end method node-icon;

define sealed method node-icon-setter
    (icon :: false-or(<image>), node :: <graph-node-pane>) => (icon :: false-or(<image>))
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


/// Graph control edges

define open abstract primary class <graph-edge-pane> (<graph-edge>)
  sealed slot %x1 :: <integer>,
    required-init-keyword: x1:;
  sealed slot %y1 :: <integer>,
    required-init-keyword: y1:;
  sealed slot %x2 :: <integer>,
    required-init-keyword: x2:;
  sealed slot %y2 :: <integer>,
    required-init-keyword: y2:;
end class <graph-edge-pane>;

define sealed domain make (subclass(<graph-edge-pane>));
define sealed domain initialize (<graph-edge-pane>);


define sealed class <line-graph-edge> (<graph-edge-pane>)
end class <line-graph-edge>;

define sealed method draw-edge
    (edge :: <line-graph-edge>, medium :: <basic-medium>, region :: <region>) => ()
  let (x1, y1, x2, y2) = values(edge.%x1, edge.%y1, edge.%x2, edge.%y2);
  when (region-contains-position?(region, x1, y1)
	| region-contains-position?(region, x2, y2))
    draw-line(medium, x1, y1, x2, y2)
  end
end method draw-edge;


define sealed class <arrow-graph-edge> (<graph-edge-pane>)
  sealed slot %direction-flags :: <integer> = %to_head;
end class <arrow-graph-edge>;

define constant %from_head :: <integer> = #o01;
define constant %to_head   :: <integer> = #o02;

define method initialize
    (edge :: <arrow-graph-edge>,
     #key from-head? = #f, to-head? = #t)
  next-method();
  let bits = logior(if (from-head?) %from_head else 0 end,
		    if (to-head?)   %to_head   else 0 end);
  edge.%direction-flags := bits
end method initialize;

define sealed method draw-edge
    (edge :: <arrow-graph-edge>, medium :: <basic-medium>, region :: <region>) => ()
  let (x1, y1, x2, y2) = values(edge.%x1, edge.%y1, edge.%x2, edge.%y2);
  when (region-contains-position?(region, x1, y1)
	| region-contains-position?(region, x2, y2))
    draw-arrow(medium, x1, y1, x2, y2,
	       from-head?: logand(edge.%direction-flags, %from_head) ~= 0,
	       to-head?:   logand(edge.%direction-flags, %to_head)   ~= 0)
  end;
end method draw-edge;


define method edge-attachment-points
    (parent :: <graph-node-pane>, child :: <graph-node-pane>, orientation)
 => (x1, y1, x2, y2)
  local method north (node) => (x, y)
	  let (left, top, right, bottom) = sheet-edges(node);
	  ignore(bottom);
	  values(floor/(right + left, 2), top - 1)
	end method,
        method south (node) => (x, y)
	  let (left, top, right, bottom) = sheet-edges(node);
	  ignore(top);
	  values(floor/(right + left, 2), bottom + 1)
	end method,
        method west (node) => (x, y)
	  let (left, top, right, bottom) = sheet-edges(node);
	  ignore(right);
	  values(left - 1, floor/(bottom + top, 2))
	end method,
        method east (node) => (x, y)
	  let (left, top, right, bottom) = sheet-edges(node);
	  ignore(left);
	  values(right + 1, floor/(bottom + top, 2))
	end method;
  select (orientation)
    #"vertical", #"down" =>
      let (x1, y1) = south(parent);
      let (x2, y2) = north(child);
      values(x1, y1, x2, y2);
    #"up" =>
      let (x1, y1) = north(parent);
      let (x2, y2) = south(child);
      values(x1, y1, x2, y2);
    #"horizontal", #"right" =>
      let (x1, y1) = east(parent);
      let (x2, y2) = west(child);
      values(x1, y1, x2, y2);
    #"left" =>
      let (x1, y1) = west(parent);
      let (x2, y2) = east(child);
      values(x1, y1, x2, y2);
  end
end method edge-attachment-points;


/// Basic graph layout pane class

define abstract class <graph-control-layout>
    (<standard-repainting-mixin>,
     <layout-border-mixin>,
     <homegrown-control-layout-mixin>,
     <layout-pane>)
  sealed slot %edges :: <vector> = #[];
end class <graph-control-layout>;

define sealed domain make (subclass(<graph-control-layout>));
define sealed domain initialize (<graph-control-layout>);

define method do-compose-space
    (layout :: <graph-control-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let (width, height) = layout-graph-nodes(layout);
  layout-graph-edges(layout, reuse-edges?: #f);
  make(<space-requirement>,
       width: width, height: height)
end method do-compose-space;

define method do-allocate-space
    (layout :: <graph-control-layout>, width :: <integer>, height :: <integer>) => ()
  ignore(width, height);
  layout-graph-nodes(layout);
  layout-graph-edges(layout, reuse-edges?: #t)
end method do-allocate-space;

//---*** Still need to highlight the selected edges somehow...
//---*** Edges probably need to have labels, then we can highlight the labels
define method handle-repaint
    (layout :: <graph-control-layout>, medium :: <medium>, region :: <region>) => ()
  next-method();
  let graph = layout.%control-pane;
  when (tree-control-show-edges?(graph))
    with-drawing-options (medium, brush: $tree-control-gray)
      for (edge in layout.%edges)
	draw-edge(edge, medium, region)
      end
    end
  end
end method handle-repaint;


// Useful macro
define macro with-node-breadth-and-depth-functions
  { with-node-breadth-and-depth-functions
       ((?breadthfun:name, ?depthfun:name,
	 ?breadth-start-setter:name, ?depth-start-setter:name,
	 ?depth-incrementer:name, ?depth-startfun:name) = ?orientation:expression)
      ?:body end }
    => { local method node-bottom (node)
		 node-y(node) + box-height(node)
	       end method,
	       method node-bottom-setter (b, node)
		 node-y(node) := b - box-height(node)
	       end method,
	       method node-right (node)
		  node-x(node) + box-width(node)
	       end method,
	       method node-right-setter (r, node)
		 node-x(node) := r - box-width(node)
	       end method;
	 let (?breadthfun :: <function>, ?depthfun :: <function>,
	      ?breadth-start-setter :: <function>, ?depth-start-setter :: <function>,
	      ?depth-startfun :: <function>)
	   = select (?orientation)
	       #"vertical", #"down", #"up" =>
		 values(box-width, box-height,
			node-x-setter,
			if (?orientation == #"up") node-bottom-setter else node-y-setter end,
			if (?orientation == #"up") node-bottom else node-y end);
	       #"horizontal", #"right", #"left" =>
		 values(box-height, box-width,
			node-y-setter,
			if (?orientation == #"left") node-right-setter else node-x-setter end,
			if (?orientation == #"left") node-right else node-x end);
	     end;
	 let ?depth-incrementer :: <function>
	   = select (?orientation)
	       #"vertical", #"down", #"horizontal", #"right" => \+;
	       #"up", #"left" => \-;
	     end;
	 ?body }
end macro with-node-breadth-and-depth-functions;


/// Event handling for edges

define method handle-event 
    (pane :: <graph-control-layout>, event :: <button-press-event>) => ()
  let graph = pane.%control-pane;
  when (gadget-enabled?(graph))
    let edge = find-edge-at-position(pane, event-x(event), event-y(event));
    if (edge)
      set-edge-selection(graph, edge, event);
      select (event-button(event))
	$left-button => #f;
	$middle-button =>
	  activate-gadget(graph);
	$right-button =>
	  let value = graph-edge-object(edge);
	  execute-popup-menu-callback
	    (graph, gadget-client(graph), gadget-id(graph), value,
	     x: event-x(event), y: event-y(event));
      end
    else
      next-method()
    end
  end
end method handle-event;

define method handle-event 
    (pane :: <graph-control-layout>, event :: <double-click-event>) => ()
  let graph = pane.%control-pane;
  when (gadget-enabled?(graph)
	& event-button(event) == $left-button)
    let edge = find-edge-at-position(pane, event-x(event), event-y(event));
    if (edge)
      set-edge-selection(graph, edge, event);
      activate-gadget(graph)
    else
      next-method()
    end
  end
end method handle-event;

define sealed method find-edge-at-position
    (layout :: <graph-control-layout>, x :: <integer>, y :: <integer>)
 => (edge :: false-or(<graph-edge>))
  let graph = layout.%control-pane;
  when (graph-edge-generator(graph))
    block (return)
      for (edge :: <graph-edge-pane> in layout.%edges)
	when (position-close-to-line?(x, y,
				      edge.%x1, edge.%y1, edge.%x2, edge.%y2, thickness: 4))
	  return(edge)
	end
      end
    end
  end
end method find-edge-at-position;

define sealed method set-edge-selection
    (graph :: <graph-control-pane>, edge :: <graph-edge>, event) => ()
  let index     = position(gadget-items(graph), graph-edge-object(edge));
  let selection = gadget-selection(graph);
  select (gadget-selection-mode(graph))
    #"none"   => #f;
    #"single" =>
      gadget-selection(graph, do-callback?: #t) := vector(index);
    #"multiple" =>
      select (event-modifier-state(event))
	$shift-key =>
	  let min-index = reduce(min, index, selection);
	  let max-index = reduce(max, index, selection);
	  gadget-selection(graph, do-callback?: #t)
	    := range(from: min-index, to: max-index);
	$control-key =>
	  if (member?(index, selection))
	    gadget-selection(graph, do-callback?: #t) := remove(selection, index);
	  else
	    gadget-selection(graph, do-callback?: #t) := add(selection, index);
	  end;
	otherwise =>
	  gadget-selection(graph, do-callback?: #t) := vector(index);
      end;
  end
end method set-edge-selection;


/// Tree graphs

define sealed class <tree-graph-pane> (<graph-control-pane>)
end class <tree-graph-pane>;

define sealed class <tree-graph-layout> (<graph-control-layout>)
end class <tree-graph-layout>;

define sealed method layout-class-for-graph
    (graph :: <tree-graph-pane>) => (class :: subclass(<graph-control-layout>))
  <tree-graph-layout>
end method layout-class-for-graph;


define method generate-graph-nodes
    (graph :: <tree-graph-pane>, roots :: <sequence>,
     #key key = identity, test = \==) => ()
  let generator :: <function> = tree-control-children-generator(graph);
  let predicate :: <function> = tree-control-children-predicate(graph);
  local method add-one (node, object, depth :: <integer>) => ()
	  let child-node = make-node(graph, object);
	  add-node(graph, node, child-node, setting-roots?: #t);
	  sheet-withdrawn?(child-node, do-repaint?: #f) := #f;
	  when (depth > 0 & predicate(object))
	    for (child in generator(object))
	      add-one(child-node, child, depth - 1)
	    end;
	    node-state(child-node) := #"expanded"
	  end
	end method;
  for (root in roots)
    add-one(graph, root, tree-control-initial-depth(graph))
  end
end method generate-graph-nodes;

//--- If anyone wanted, it would be easy to add an option which said which way
//--- the kids ran -- e.g. left-to-right or right-to-left in the case of vertical
//--- graphs.  Just change the one remaining '+' to call a new breadth incrementer.
define method layout-graph-nodes
    (layout :: <tree-graph-layout>)
 => (width :: <integer>, height :: <integer>)
  let graph :: <graph-control-pane> = layout.%control-pane;
  let root-nodes       = tree-control-root-nodes(graph);
  let orientation      = graph-orientation(graph);
  let inter-generation = graph-inter-generation-spacing(graph);
  let intra-generation = graph-intra-generation-spacing(graph);
  let most-negative-depth :: <integer> = 0;
  let start-breadth :: <integer> = 0;
  let width  :: <integer> = 0;
  let height :: <integer> = 0;
  //--- There might be a more elegant way to do this!
  for (node in sheet-children(layout))
    unless (sheet-withdrawn?(node))
      let space-req = compose-space(node);
      let (w, w-, w+, h, h-, h+) = space-requirement-components(node, space-req);
      ignore(w-, w+, h-, h+);
      set-sheet-size(node, w, h)
    end
  end;
  with-node-breadth-and-depth-functions
     ((breadthfun, depthfun,
       breadth-start-setter, depth-start-setter,
       depth-incrementer, depth-startfun) = orientation)
    local method layout-graph
	      (root, start-depth :: <integer>, start-breadth :: <integer>, tallest-sibling)
	    let children = node-children(root);
	    let breadth :: <integer> = start-breadth;
	    let root-breadth   :: <integer> = breadthfun(root);
	    let breadth-margin :: <integer> = floor/(intra-generation, 2);
	    let tallest-child  :: <integer> = 0;
	    let n-children     :: <integer> = 0;
	    for (child in children)
	      unless (sheet-withdrawn?(child))
		max!(tallest-child, depthfun(child));
		inc!(n-children);
	      end
	    end;
	    for (child in children)
	      unless (sheet-withdrawn?(child))
		inc!(breadth, breadth-margin);
		let child-breadth :: <integer>
		  = layout-graph(child,
				 depth-incrementer(start-depth, tallest-sibling + inter-generation),
				 breadth,
				 tallest-child);
		inc!(breadth, child-breadth);
		inc!(breadth, breadth-margin)
	      end
	    end;
	    let total-child-breadth :: <integer>
	      = breadth - start-breadth;
	    let my-breadth :: <integer>
	      = start-breadth + floor/(max(0, total-child-breadth - root-breadth), 2);
	    depth-start-setter(start-depth, root);
	    breadth-start-setter(my-breadth, root);
	    let (left, top, right, bottom) = sheet-edges(root);
	    ignore(left, top);
	    max!(width,  right);
	    max!(height, bottom);
	    when (~zero?(n-children))
	      min!(most-negative-depth,
		   depth-incrementer(start-depth, tallest-sibling))
	    end;
	    // Returns the breadth of the graph as a result
	    max(total-child-breadth, root-breadth)
	  end method;
    for (root-node in root-nodes)
      inc!(start-breadth,
           layout-graph(root-node, 0, start-breadth, depthfun(root-node)))
    end;
    // For up and right, we've laid out into negative coordinates.  Correct this.
    when (negative?(most-negative-depth))
      do-sheet-children(method (node)
			  depth-start-setter(depth-startfun(node) - most-negative-depth, node)
			end, layout)
    end
  end;
  values(width, height)
end method layout-graph-nodes;

define method layout-graph-edges
    (layout :: <tree-graph-layout>, #key reuse-edges? = #f) => ()
  let graph :: <graph-control-pane> = layout.%control-pane;
  let orientation = graph-orientation(graph);
  if (reuse-edges?)
    let edges :: <stretchy-object-vector> = layout.%edges;
    for (edge :: <graph-edge-pane> in edges)
      let from-node = graph-edge-from-node(edge);
      let to-node   = graph-edge-to-node(edge);
      let (x1, y1, x2, y2)
	= edge-attachment-points(from-node, to-node, orientation);
      edge.%x1 := x1;
      edge.%y1 := y1;
      edge.%x2 := x2;
      edge.%y2 := y2
    end
  else
    let root-nodes    = tree-control-root-nodes(graph);
    let edge-class    = graph-edge-class(graph) | <line-graph-edge>;
    let edge-initargs = graph-edge-initargs(graph);
    let edge-function = graph-edge-generator(graph);
    let edges :: <stretchy-object-vector> = make(<stretchy-vector>);
    local method make-edges (from-node :: <graph-node>)
	    // By definition, expanded nodes aren't withdrawn, so we
	    // do the more general test
	    when (node-state(from-node) == #"expanded")
	      for (to-node :: <graph-node> in node-children(from-node))
		when (~sheet-withdrawn?(to-node))
		  unless (empty?(node-children(to-node)))
		    make-edges(to-node)
		  end;
		  let (x1, y1, x2, y2)
		    = edge-attachment-points(from-node, to-node, orientation);
		  let object
		    = edge-function & edge-function(node-object(from-node), node-object(to-node));
		  let edge
		    = apply(make, edge-class,
			    x1: x1, y1: y1, x2: x2, y2: y2, 
			    from-node: from-node, to-node: to-node,
			    object: object,
			    edge-initargs);
		  add!(edges, edge)
		end
	      end
	    end
	  end method;
    for (root-node in root-nodes)
      make-edges(root-node)
    end;
    layout.%edges := edges
  end
end method layout-graph-edges;


/// Directed graphs, both acyclic and cyclic

define sealed class <DAG-graph-pane> (<graph-control-pane>)
  sealed slot %node-table :: <object-table> = make(<table>),
    init-keyword: node-table:;
  sealed slot %n-generations :: <integer> = 0;
end class <DAG-graph-pane>;

define sealed class <DAG-graph-layout> (<graph-control-layout>)
end class <DAG-graph-layout>;

define sealed method layout-class-for-graph
    (graph :: <DAG-graph-pane>) => (class :: subclass(<graph-control-layout>))
  <DAG-graph-layout>
end method layout-class-for-graph;


// Note that we don't use 'add-node' directly to do any of this
// because it's too simple-minded to do the sharing we need here
define method generate-graph-nodes
    (graph :: <DAG-graph-pane>, roots :: <sequence>,
     #key key = identity, test = \==) => ()
  let layout = graph.%layout-pane;
  let generator :: <function> = tree-control-children-generator(graph);
  let predicate :: <function> = tree-control-children-predicate(graph);
  let node-table :: <object-table> = graph.%node-table;
  let root-nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let max-depth = tree-control-initial-depth(graph);
  graph.%n-generations := 0;
  local method do-children (function :: <function>, node)
	  do(function, generator(node))
	end method,
        method create-node (parent-object, parent-node, child-object, nothing)
          ignore(nothing);
	  let child-node = make-node(graph, child-object);
	  add!(gadget-items(graph), child-object);
	  add-child(layout, child-node);
	  when (parent-node)
	    node-state(parent-node) := #"expanded"
	  end;
	  // This guarantees that the next phase will have at least one
	  // node from which to start.  Otherwise the entire graph gets
	  // lost.  If the first node isn't really a root, it will be
	  // deleted from the set of roots when the cycle is detected.
	  when (empty?(root-nodes))
	    add!(root-nodes, child-node)
	  end;
	  initialize-node(parent-object, parent-node, child-object, child-node)
        end method,
        method initialize-node (parent-object, parent-node, child-object, child-node)
	  ignore(parent-object, child-object);
	  let old-generation = node-generation(child-node);
	  // Set the generation of this node to 1 greater than the parent,
	  // and keep track of the highest generation encountered.
	  max!(graph.%n-generations,
	       max!(node-generation(child-node),
		    if (parent-node) node-generation(parent-node) + 1 else 0 end));
	  // If the child node got its generation adjusted, then we must
	  // adjust the generation number of already-processed children,
	  // and their children, etc.
	  unless (node-generation(child-node) == old-generation)
	    increment-generation(child-node)
	  end;
	  // Preserve the ordering of the nodes
	  when (parent-node)
	    unless (member?(parent-node, node-parents(child-node)))
	      add!(node-parents(child-node), parent-node)
	    end;
	    unless (member?(child-node, node-children(parent-node)))
	      add!(node-children(parent-node), child-node)
	    end
	  end;
	  child-node
	end method,
        method increment-generation (node)
	  let new-generation = node-generation(node) + 1;
	  for (child in node-children(node))
	    // Remember which generation the child belonged to.
	    let old-generation = node-generation(child);
	    max!(graph.%n-generations,
		 max!(node-generation(child), new-generation));
	    // If it has changed, fix up the next generation recursively.
	    unless (node-generation(child) >= old-generation)
	      increment-generation(child)
	    end
	  end
	end method;
  traverse-graph(roots, do-children, node-table, key,
		 create-node, initialize-node, max-depth: max-depth);
  do(method (node)
       when (empty?(node-parents(node)))
	 add-new!(root-nodes, node)
       end
     end method,
     node-table);
  tree-control-root-nodes(graph) := root-nodes
end method generate-graph-nodes;

define sealed method do-add-nodes
    (graph :: <DAG-graph-pane>, parent, nodes :: <sequence>, #key after) => ()
  let layout = graph.%layout-pane;
  let node-table :: <object-table> = graph.%node-table;
  let root-nodes :: <stretchy-object-vector> = tree-control-root-nodes(graph);
  let selected-nodes = gadget-selected-nodes(graph);
  graph.%visible-items := #f;
  gadget-selection(graph) := #[];
  local method do-children (function :: <function>, node)
	  do(function, node-children(node))
	end method,
        method record-node (parent, hash, child, depth)
	  ignore(parent, hash, depth);
	  child
	end method;
  // Record all the existing nodes
  traverse-graph(root-nodes, do-children, node-table, node-object,
		 record-node, ignore);
  // These two local methods are copied from 'generate-graph-nodes' above
  local method initialize-node (parent-node, child-node)
	  let old-generation = node-generation(child-node);
	  max!(graph.%n-generations,
	       max!(node-generation(child-node),
		    if (parent-node) node-generation(parent-node) + 1 else 0 end));
	  unless (node-generation(child-node) == old-generation)
	    increment-generation(child-node)
	  end;
	  // Preserve the ordering of the nodes
	  when (parent-node)
	    unless (member?(parent-node, node-parents(child-node)))
	      add!(node-parents(child-node), parent-node)
	    end;
	    unless (member?(child-node, node-children(parent-node)))
	      add!(node-children(parent-node), child-node)
	    end
	  end
	end method,
	method increment-generation (node)
	  let new-generation = node-generation(node) + 1;
	  for (child in node-children(node))
	    // Remember which generation the child belonged to.
	    let old-generation = node-generation(child);
	    max!(graph.%n-generations,
		 max!(node-generation(child), new-generation));
	    // If it has changed, fix up the next generation recursively.
	    unless (node-generation(child) >= old-generation)
	      increment-generation(child)
	    end
	  end
	end method;
  for (new-child in nodes)
    // The following is based on 'create-node' in 'generate-graph-nodes' above
    let object    = node-object(new-child);
    let old-child = gethash(node-table, object);
    unless (old-child)
      add!(gadget-items(graph), object);
      add-child(layout, new-child);
      gethash(node-table, object) := new-child
    end;
    when (parent)
      node-state(parent) := #"expanded"
    end;
    initialize-node(parent, old-child | new-child);
  end;
  gadget-selection(graph) := compute-gadget-selection(graph, selected-nodes)
end method do-add-nodes;

define sealed method do-contract-node
    (graph :: <DAG-graph-pane>, node :: <graph-node-pane>) => ()
  graph.%visible-items := #f;
  delaying-layout (graph)
    local method withdraw (node :: <graph-node-pane>)
	    // Need to be careful not to withdraw a child node if there are
	    // any parents that still points to it...
	    when (every?(method (n) node-state(n) == #"contracted" end, node-parents(node)))
	      sheet-withdrawn?(node, do-repaint?: #f) := #t;
	      when (node-state(node) == #"expanded")
		node-state(node) := #"contracted"
	      end;
	      // Withdraw all the way to the bottom
	      do(withdraw, node-children(node))
	    end
	  end method;
    do(withdraw, node-children(node))
  end
end method do-contract-node;

define sealed class <generation-descriptor> (<object>)
  sealed slot generation-generation  :: <integer> = 0,	// generation number
    init-keyword: generation:;
  sealed slot generation-breadth     :: <integer> = 0;	// sum of breadth of all nodes in this generation
  sealed slot generation-depth       :: <integer> = 0;	// maximum depth of any node in this generation
  sealed slot generation-start-depth :: <integer> = 0,	// starting depth position for this generation
    init-keyword: start-depth:;
  sealed slot generation-size        :: <integer> = 0;	// number of nodes in this generation
  // "Temporaries" used during placement
  sealed slot generation-breadth-so-far :: <integer> = 0, // running placement on the breadth axis
    init-keyword: start-breadth:;
  sealed slot generation-inner-breadth-separation :: <integer> = 0;
  sealed slot generation-edge-breadth-separation  :: <integer> = 0;
  sealed slot generation-touched? :: <boolean> = #f;	// if #t, use inner breadth separation
end class <generation-descriptor>;

define method layout-graph-nodes
    (layout :: <DAG-graph-layout>)
 => (width :: <integer>, height :: <integer>)
  let graph :: <graph-control-pane> = layout.%control-pane;
  let node-table :: <object-table> = graph.%node-table;
  let root-nodes :: <stretchy-object-vector> = tree-control-root-nodes(graph);
  let n-generations    = graph.%n-generations;
  let orientation      = graph-orientation(graph);
  let inter-generation = graph-inter-generation-spacing(graph);
  let intra-generation = graph-intra-generation-spacing(graph);
  let center-nodes?    = graph-center-nodes?(graph);
  let start-x :: <integer> = 0;
  let start-y :: <integer> = 0;
  let width   :: <integer> = 0;
  let height  :: <integer> = 0;
  //--- There might be a more elegant way to do this!
  for (node in sheet-children(layout))
    unless (sheet-withdrawn?(node))
      let space-req = compose-space(node);
      let (w, w-, w+, h, h-, h+) = space-requirement-components(node, space-req);
      ignore(w-, w+, h-, h+);
      set-sheet-size(node, w, h)
    end
  end;
  unless (empty?(root-nodes))
    local method do-children (function :: <function>, node)
	    do(function, node-children(node))
	  end method,
          method traverse (new-node-function :: <function>, old-node-function :: <function>)
	    traverse-graph(root-nodes, do-children, node-table, identity,
			   new-node-function, old-node-function)
	  end method,
	  method set-sheet-position-yx (sheet, y, x)
	    set-sheet-position(sheet, x, y)
	  end method;
    let (breadthfun :: <function>, depthfun :: <function>, position-setter :: <function>,
	 start-breadth :: <integer>, start-depth :: <integer>)
      = select (orientation)
	  #"vertical", #"down", #"up" =>
	    values(box-width, box-height, set-sheet-position,    start-x, start-y);
	  #"horizontal", #"right", #"left" =>
	    values(box-height, box-width, set-sheet-position-yx, start-y, start-x);
	end;
    let generation-descriptors :: <simple-object-vector>
      = make(<vector>, size: n-generations + 1);
    let max-gen-breadth :: <integer> = 0;
    let broadest-gen-descr = #f;
    for (generation :: <integer> from 0 to n-generations)
      generation-descriptors[generation] := make(<generation-descriptor>,
						 generation: generation,
						 start-breadth: start-breadth)
    end;
    when (orientation == #"up" | orientation == #"left")
      generation-descriptors := reverse!(generation-descriptors)
    end;
    // Determine the breadth and depth of each generation
    local method collect-node-size (parent, hash, child, depth)
	    ignore(parent, hash, depth);
	    unless (sheet-withdrawn?(child))
	      let descr = find-value(generation-descriptors, 
				     method (gd)
				       generation-generation(gd) = node-generation(child)
				     end);
	      inc!(generation-size(descr));
	      inc!(generation-breadth(descr), breadthfun(child));
	      max!(generation-depth(descr),   depthfun(child))
	    end
	  end method;
    traverse(collect-node-size, ignore);
    // Determine max-breadth and starting-depth
    let depth-so-far :: <integer> = start-depth;
    for (descr in generation-descriptors)
      let gen-breadth = generation-breadth(descr);
      when (~broadest-gen-descr | (gen-breadth > max-gen-breadth))
	max-gen-breadth := gen-breadth;
	broadest-gen-descr := descr
      end;
      generation-start-depth(descr) := depth-so-far;
      inc!(depth-so-far, inter-generation + generation-depth(descr))
    end;
    // Determine breadth-spacing
    inc!(max-gen-breadth, intra-generation * generation-size(broadest-gen-descr));
    for (descr in generation-descriptors)
      let excess = floor/(max-gen-breadth - generation-breadth(descr),
			  max(generation-size(descr), 1));
      generation-inner-breadth-separation(descr) := excess;
      generation-edge-breadth-separation(descr) := floor/(excess, 2)
    end;
    local method place-node (parent, hash, child, depth)
	    ignore(parent, hash, depth);
	    unless (sheet-withdrawn?(child))	    
	      let descr = find-value(generation-descriptors,
				     method (gd)
				       generation-generation(gd) = node-generation(child)
				     end);
	      inc!(generation-breadth-so-far(descr),
		   if (generation-touched?(descr))
		     generation-inner-breadth-separation(descr)
		   else
		     generation-touched?(descr) := #t;
		     generation-edge-breadth-separation(descr)
		   end);
	      position-setter(child,
			      generation-breadth-so-far(descr),
			      if (center-nodes?)
				generation-start-depth(descr)
				+ floor/(generation-depth(descr) - depthfun(child), 2)
			      else
				generation-start-depth(descr)
			      end);
	      let (left, top, right, bottom) = sheet-edges(child);
	      ignore(left, top);
	      max!(width,  right);
	      max!(height, bottom);
	      inc!(generation-breadth-so-far(descr), breadthfun(child))
	    end
	  end method;
    traverse(place-node, ignore)
  end;
  values(width, height)
end method layout-graph-nodes;

define method layout-graph-edges
    (layout :: <DAG-graph-layout>, #key reuse-edges? = #f) => ()
  let graph :: <graph-control-pane> = layout.%control-pane;
  let orientation = graph-orientation(graph);
  if (reuse-edges?)
    let edges :: <stretchy-object-vector> = layout.%edges;
    for (edge :: <graph-edge-pane> in edges)
      let from-node = graph-edge-from-node(edge);
      let to-node   = graph-edge-to-node(edge);
      let (x1, y1, x2, y2)
	= edge-attachment-points(from-node, to-node, orientation);
      edge.%x1 := x1;
      edge.%y1 := y1;
      edge.%x2 := x2;
      edge.%y2 := y2
    end
  else
    let node-table :: <object-table> = graph.%node-table;
    let root-nodes :: <stretchy-object-vector> = tree-control-root-nodes(graph);
    let edge-class    = graph-edge-class(graph) | <arrow-graph-edge>;
    let edge-initargs = graph-edge-initargs(graph);
    let edge-function = graph-edge-generator(graph);
    let edges :: <stretchy-object-vector> = make(<stretchy-vector>);
    unless (empty?(root-nodes))
      local method do-children (function :: <function>, node)
	      do(function, node-children(node))
	    end method,
	    method create-edge (from-node, hash, to-node, depth)
	      ignore(hash, depth);
	      // By definition, expanded nodes aren't withdrawn, so we
	      // do the more general test; we don't want to draw edges
	      // from unexpanded nodes to "shared" child nodes
	      when (from-node & node-state(from-node) == #"expanded"
		    & ~sheet-withdrawn?(to-node))
		let (x1, y1, x2, y2)
		  = edge-attachment-points(from-node, to-node, orientation);
		let object
		  = edge-function & edge-function(node-object(from-node), node-object(to-node));
		let edge
		  = apply(make, edge-class,
			  x1: x1, y1: y1, x2: x2, y2: y2, 
			  from-node: from-node, to-node: to-node,
			  object: object,
			  edge-initargs);
		add!(edges, edge)
	      end
	    end method;
      traverse-graph(root-nodes, do-children, node-table, identity,
		     create-edge, create-edge)
    end;
    layout.%edges := edges
  end
end method layout-graph-edges;

// 'roots' is a sequence of the roots of the graph.  
// 'child-mapper' is a function of two arguments, a function and an object
// over whose child objects the function should be applied.
// 'node-table' is a table that is used to record and detect when an object has
// already been included in the graph.
// 'key' is a function of one argument used to produce the hash table key.
// There is no 'test' function, since it is already captured in the hash table.
// 'new-node-function' is a function of four arguments (the parent object, the
// parent object's hash value, the child object, and "nothing").  Its return
// value will be stored as the hash value of the child object.
// 'old-node-function' is a function of four arguments (the parent object, the
// parent object's hash value, the child object, and the child object's hash 
// value).  Its return value is ignored.
// 'max-depth' is the cutoff depth of the tree, or #f for no cutoff.
//--- Potential bug: the cutoff may fall short in that you may reach a certain
//--- node at the maximum depth, mark that node as seen and decline to descend
//--- into its children, then find that node again through a shorter path.  If
//--- you really want to fix this, write a breadth-first descent of the graph.
define method traverse-graph
    (roots :: <sequence>, child-mapper :: <function>,
     node-table :: <object-table>, key :: <function>,
     new-node-function :: <function>, old-node-function :: <function>,
     #key max-depth :: false-or(<integer>) = #f) => ()
  remove-all-keys!(node-table);
  local method traverse (parent-object, parent-hashval, object, max-depth)
	  let object-hashval
            = new-node-function(parent-object, parent-hashval, object, #f);
	  gethash(node-table, key(object)) := object-hashval;
	  when (max-depth) dec!(max-depth) end;
	  when (~max-depth | max-depth >= 0)
	    local method traverse1 (child-object)
		    let child-key = key(child-object);
		    let (child-hashval, found?)
		      = gethash(node-table, child-key);
		    if (found?)
		      old-node-function(object, object-hashval, child-object, child-hashval)
		    else
		      traverse(object, object-hashval, child-object, max-depth)
		    end
		  end method;
	    child-mapper(traverse1, object)
	  end
	end method;
  do(method (root) traverse(#f, #f, root, max-depth) end, roots)
end method traverse-graph;
