Module:       DUIM-formatting-internals
Synopsis:     DUIM formatted output
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Graph formatting

// The basic graph output record class
define open abstract class <basic-graph-record> (<sequence-record>)
  // The output records corresponding to the root nodes
  sealed slot graph-root-nodes :: <sequence> = #[],
    init-keyword: root-nodes:;
  sealed slot graph-node-table :: false-or(<table>) = #f,
    init-keyword: node-table:;
  // Stores things like :ORIENTATION, :MERGE-DUPLICATES?, etc.
  sealed slot graph-properties :: <sequence> = #[],
    init-keyword: properties:;
  // A few things to make this act like a tree control...
  sealed slot tree-control-children-generator :: <function>,
    required-init-keyword: children-generator:;
  sealed slot tree-control-children-predicate :: false-or(<function>),
    required-init-keyword: children-predicate:;
end class <basic-graph-record>;

// Skip the 'update-region-for-new-child', because we're going to do
// layout and 'recompute-region' right away
define method note-child-added
    (record :: <basic-graph-record>, child :: <output-record>) => ()
  note-child-added-1(record, child)
end method note-child-added;

// Recover the hash table during incremental redisplay
define method match-output-records
    (record :: <basic-graph-record>, #key node-table, #all-keys) => (true? :: <boolean>)
  block ()
    next-method()
  afterwards
    when (node-table)
      graph-node-table(record) := node-table
    end
  end
end method match-output-records;


// The basic graph node output record class
//--- Should we store the in and out edges for each node?
define sealed class <graph-node-record> (<sequence-record>)
  // The output records corresponding to this node's parents and children
  sealed slot node-parents :: <sequence>  = #[],
    init-keyword: node-parents:;
  sealed slot node-children :: <sequence> = #[],
    init-keyword: node-children:;
  sealed slot node-object,
    required-init-keyword: object:;
  sealed slot node-generation :: <integer> = 0,
    init-keyword: generation:;
end class <graph-node-record>;

define method graph-node? (object) => (true? :: <boolean>) #f end;
define method graph-node? (object :: <graph-node-record>) => (true? :: <boolean>) #t end;

define method initialize (node :: <graph-node-record>, #key key, test)
  ignore(key, test);
  next-method()
end method initialize;

define output-record-constructor <graph-node-record>
    (#key parent, sheet, region, transform,
          node-children: children, node-parents: parents, object, key, test)
  parent: parent, sheet: sheet,
  region: region, transform: transform,
  node-children: children, node-parents: parents, object: object
end;

define method graph-node-x (node :: <graph-node-record>) => (x)
  let (x, y) = sheet-position(node);
  ignore(y);
  x
end method graph-node-x;

define method graph-node-x-setter (new-x, node :: <graph-node-record>) => (x)
  let (x, y) = sheet-position(node);
  ignore(x);
  %set-sheet-position(node, new-x, y)
end method graph-node-x-setter;

define method graph-node-y (node :: <graph-node-record>) => (y)
  let (x, y) = sheet-position(node);
  ignore(x);
  y
end method graph-node-y;

define method graph-node-y-setter (new-y, node :: <graph-node-record>) => (y)
  let (x, y) = sheet-position(node);
  ignore(y);
  %set-sheet-position(node, x, new-y)
end method graph-node-y-setter;

// Graph node output records match if their objects match
define method match-output-records
    (record :: <graph-node-record>,
     #key object, key = identity, test = \==, #all-keys) => (true? :: <boolean>)
  when ((~object & ~node-object(record))
	| test(key(object), key(node-object(record))))
    node-children(record).size := 0;
    #t
  end
end method match-output-records;


// The basic graph edge output record class
define open abstract class <graph-edge-record> (<basic-leaf-record>)
  sealed slot graph-edge-from-node :: <graph-node-record>,
    required-init-keyword: from-node:;
  sealed slot graph-edge-to-node :: <graph-node-record>,
    required-init-keyword: to-node:;
  sealed slot %x1 :: <integer>,
    required-init-keyword: x1:;
  sealed slot %y1 :: <integer>,
    required-init-keyword: y1:;
  sealed slot %x2 :: <integer>,
    required-init-keyword: x2:;
  sealed slot %y2 :: <integer>,
    required-init-keyword: y2:;
end class <graph-edge-record>;

define method match-output-records
    (record :: <graph-edge-record>,
     #key from-node, to-node, #all-keys) => (true? :: <boolean>)
  graph-edge-from-node(node) == from-node
  & graph-edge-to-node(node) == to-node
end method match-output-records;

define sealed class <line-graph-edge> (<graph-edge-record>)
end class <line-graph-edge>;

define method handle-repaint
    (record :: <line-graph-edge>, region :: <region>, medium :: <medium>) => ()
  ignore(region);
  with-record-medium-state (medium, record)
    draw-line(medium, record.%x1, record.%y1, record.%x2, record.%y2)
  end
end method handle-repaint;

define sealed class <arrow-graph-edge> (<graph-edge-record>)
  sealed slot %from-head? = #f, init-keyword: from-head?:;
  sealed slot %to-head?   = #f, init-keyword: to-head?:;
end class <arrow-graph-edge>;

define method handle-repaint
    (record :: <arrow-graph-edge>, region :: <region>, medium :: <medium>) => ()
  ignore(region);
  with-record-medium-state (medium, record)
    draw-arrow(medium, record.%x1, record.%y1, record.%x2, record.%y2,
	       from-head?: record.%from-head?, to-head?: record.%to-head?)
  end
end method handle-repaint;

define method edge-attachment-points
    (parent :: <graph-node-record>, child :: <graph-node-record>, orientation)
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


/// Useful macros

define macro with-node-breadth-and-depth-functions
  { with-node-breadth-and-depth-functions
       ((?breadthfun:name, ?depthfun:name,
	 ?breadth-start-setter:name, ?depth-start-setter:name,
	 ?depth-incrementor:name, ?depth-startfun:name) = ?orientation:expression)
      ?:body end }
    => { let node-bottom
	  = method (node) graph-node-y(node) + box-height(node) end;
	 let node-bottom-setter
	  = method (b, node) graph-node-y(node) := b - box-height(node) end;
         let node-right
	  = method (node) graph-node-x(node) + box-width(node) end;
	 let node-right-setter
	  = method (r, node) graph-node-x(node) := r - box-width(node) end;
	 let (?breadthfun, ?depthfun,
	      ?breadth-start-setter, ?depth-start-setter, ?depth-startfun)
	   = select (?orientation)
	       #"vertical", #"down", #"up" =>
		 values(box-width, box-height,
			graph-node-x-setter,
			if (?orientation == #"up") node-bottom-setter else graph-node-y-setter end,
			if (?orientation == #"up") node-bottom else graph-node-y end);
	       #"horizontal", #"right", #"left" =>
		 values(box-height, box-width,
			graph-node-y-setter,
			if (?orientation == #"left") node-right-setter else graph-node-x-setter end,
			if (?orientation == #"left") node-right else graph-node-x end);
	     end;
	 let ?depth-incrementor
	   = select (?orientation)
	       #"vertical", #"down", #"horizontal", #"right" => \+;
	       #"up", #"left" => \-;
	     end;
	 ?body }
end macro with-node-breadth-and-depth-functions;


/// Tree graphs

define sealed class <tree-graph-record> (<basic-graph-record>)
end class <tree-graph-record>;

define method generate-graph-nodes
    (graph :: <tree-graph-record>, sheet :: <output-recording-mixin>, roots :: <sequence>,
     object-printer :: <function>, children-generator :: <function>,
     #key key = identity, test = \==) => ()
  let properties = graph-properties(graph);
  let cutoff-depth = get-property(properties, cutoff-depth:);
  local method format-node (object, #key depth = 1)
	  block (return)
	    when (cutoff-depth & (depth > cutoff-depth))
	      return()
	    end;
	    let children :: <stretchy-object-vector> = make(<stretchy-vector>);
	    for (child in children-generator(object))
	      let node = format-node(child, depth: depth + 1);
	      when (node & ~member?(node, children))
		// Don't add the same node to the children more than
		// once, which can come up during redisplay
		add!(children, node)
	      end
	    end;
	    let this-node
	      = with-caret-position-saved (sheet)
	          do-with-new-output-record-1
	            (sheet, method (record) object-printer(object, sheet) end,
		     <graph-node-record>, <graph-node-record>-constructor, #f,
		     object: object,
		     node-children: children,
		     key: key, test: test)
	        end;
            let parents = vector(this-node);
            for (child in children)
	      node-parents(child) := parents
	    end;
	    this-node
          end
        end method;
  graph-root-nodes(graph) := map-as(<stretchy-vector>, format-node, roots)
end method generate-graph-nodes;

//--- If anyone desired, it would be extremely easy to add an option which said
//--- which way the kids ran - e.g. left-to-right or right-to-left in the case
//--- of vertical graphs.  Just change the one remaining "+" to funcalling
//--- a new breadth-incrementor function.  - jga 25 Nov 94
define method layout-graph-nodes
    (graph :: <tree-graph-record>, sheet :: <output-recording-mixin>) => ()
  let root-nodes = graph-root-nodes(graph);
  let properties = graph-properties(graph);
  let orientation = get-property(properties, orientation:);
  let inter-generation = get-property(properties, inter-generation-separation:);
  let intra-generation = get-property(properties, intra-generation-separation:);
  let most-negative-depth = 0;
  let start-breadth = 0;
  with-node-breadth-and-depth-functions
     ((breadthfun, depthfun,
       breadth-start-setter, depth-start-setter,
       depth-incrementor, depth-startfun) =  orientation)
    local method layout-graph (root, start-depth, start-breadth, tallest-sibling)
	    let children = node-children(root);
	    let breadth = start-breadth;
	    let root-breadth = breadthfun(root);
	    let breadth-margin = floor/(intra-generation, 2);
	    let tallest-child = 0;
	    for (child in children)
	      max!(tallest-child, depthfun(child))
	    end;
	    for (child in children)
	      inc!(breadth, breadth-margin);
	      let child-breadth
		= layout-graph(child,
			       depth-incrementor(start-depth, tallest-sibling, inter-generation),
			       breadth,
			       tallest-child);
	      inc!(breadth, child-breadth);
	      inc!(breadth, breadth-margin)
	    end;
	    let total-child-breadth = breadth - start-breadth;
	    let my-breadth = start-breadth + floor/(max(0, total-child-breadth - root-breadth), 2);
	    depth-start-setter(start-depth, root);
	    breadth-start-setter(my-breadth, root);
	    when (empty?(children))
	      min!(most-negative-depth,
		   depth-incrementor(start-depth, tallest-sibling))
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
			end, graph)
    end
  end
end method layout-graph-nodes;

define method layout-graph-edges
    (graph :: <tree-graph-record>, sheet :: <output-recording-mixin>,
     #key edge-class = <line-graph-edge>, edge-initargs = #[]) => ()
  let root-nodes = graph-root-nodes(graph);
  let properties = graph-properties(graph);
  let orientation = get-property(properties, orientation:);
  let medium-state = sheet-medium-state(sheet);
  let medium = sheet-medium(sheet);
  with-identity-transform (medium)
    with-output-recording-options (sheet, draw?: #f, record?: #t)
      with-new-output-record (record = sheet,
                              record-class: <sequence-record>,
                              parent: graph)
	local method make-edges (parent)
		for (child in node-children(parent))
		  unless (empty?(node-children(child)))
		    make-edges(child)
		  end;
		  let (x1, y1, x2, y2)
		    = edge-attachment-points(parent, child, orientation);
                  let transform = make(<mutable-translation-transform>, tx: x1, ty: y1);
                  let region = make-bounding-box(0, 0, x2 - x1, y2 - y1);
		  let edge = apply(make, edge-class,
				   region: region, transform: transform,
                                   x1: 0, y1: 0, x2: x2 - x1, y2: y2 - y1, 
				   from-node: parent, to-node: child,
				   medium-state: medium-state,
				   edge-initargs);
		  add-child(record, edge)
                end
	      end method;
        for (root-node in root-nodes)
          make-edges(root-node)
        end
      end
    end
  end
end method layout-graph-edges;


/// Directed graphs, both acyclic and cyclic

define sealed class <DAG-graph-record> (<basic-graph-record>)
  sealed slot %n-generations :: <integer> = 0;
end class <DAG-graph-record>;

define method generate-graph-nodes
    (graph :: <DAG-graph-record>, sheet :: <output-recording-mixin>, roots :: <sequence>,
     object-printer :: <function>, children-generator :: <function>,
     #key key = identity, test = \==) => ()
  let properties = graph-properties(graph);
  let cutoff-depth = get-property(properties, cutoff-depth:);
  let node-table = graph-node-table(graph);
  let root-nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method do-children (function, node)
	  do(function, children-generator(node))
	end method,
        method new-node-function (parent-object, parent-record, child-object, nothing)
          ignore(nothing);
	  let child-record
	    = with-caret-position-saved (sheet)
	        do-with-new-output-record-1
	          (sheet, method (record) object-printer(child-object, sheet) end,
		   <graph-node-record>, <graph-node-record>-constructor, #f,
		   object: child-object,
		   node-children: make(<stretchy-vector>),
		   node-parents:  make(<stretchy-vector>),
		   key: key, test: test)
	      end;
          // This guarantees that the next phase will have at least one
          // node from which to start.  Otherwise the entire graph gets
	  // lost.  If the first node isn't really a root, it will be
	  // deleted from the set of roots when the cycle is detected.
	  when (empty?(root-nodes))
	    add!(root-nodes, child-record)
	  end;
	  old-node-function(parent-object, parent-record, child-object, child-record)
        end method,
        method old-node-function (parent-object, parent-record, child-object, child-record)
	  ignore(parent-object, child-object);
	  let old-generation = node-generation(child-record);
	  // Set the generation of this node to 1 greater than the parent,
	  // and keep track of the highest generation encountered.
	  max!(graph.%n-generations,
	       max!(node-generation(child-record),
		    if (parent-record) node-generation(parent-record) + 1 else 0 end));
	  // If the child-record got its generation adjusted, then we must
	  // adjust the generation-number of already-processed children,
	  // and their children, etc.
	  unless (node-generation(child-record) == old-generation)
	    increment-generation(child-record)
	  end;
	  // Preserve the ordering of the nodes
	  when (parent-record)
	    unless (member?(parent-record, node-parents(child-record)))
	      add!(node-parents(child-record), parent-record)
	    end;
	    unless (member?(child-record, node-children(parent-record)))
	      add!(node-children(parent-record), child-record)
	    end
	  end;
	  child-record
	end method,
        method increment-generation (record)
	  let new-generation = node-generation(record) + 1;
	  for (child in node-children(record))
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
		 new-node-function, old-node-function, max-depth: cutoff-depth);
  do(method (node)
       when (graph-node?(node)
	     & empty?(node-parents(node)))
	 add-new!(root-nodes, node)
       end
     end method,
     node-table);
  graph-root-nodes(graph) := root-nodes
end method generate-graph-nodes;

define sealed class <generation-descriptor> (<object>)
  sealed slot generation-generation :: <integer> = 0,	// generation number
    init-keyword: generation:;
  sealed slot generation-breadth :: <integer> = 0;	// sum of breadth of all nodes in this generation
  sealed slot generation-depth :: <integer> = 0;	// maximum depth of any node in this generation
  sealed slot generation-start-depth :: <integer> = 0,	// starting depth position for this generation
    init-keyword: start-depth:;
  sealed slot generation-size :: <integer> = 0;		// number of nodes in this generation
  // "Temporaries" used during placement
  sealed slot generation-breadth-so-far :: <integer> = 0, // running placement on the breadth axis
    init-keyword: start-breadth:;
  sealed slot generation-inner-breadth-separation :: <integer> = 0;
  sealed slot generation-edge-breadth-separation  :: <integer> = 0;
  sealed slot generation-touched? :: <boolean> = #f;	// if #t, use inner breadth separation
end class <generation-descriptor>;

define method layout-graph-nodes
    (graph :: <DAG-graph-record>, sheet :: <output-recording-mixin>) => ()
  let root-nodes = graph-root-nodes(graph);
  let node-table = graph-node-table(graph);
  let n-generations = graph.%n-generations;
  let properties = graph-properties(graph);
  let orientation = get-property(properties, orientation:);
  let center-nodes? = get-property(properties, center-nodes?:);
  let inter-generation = get-property(properties, inter-generation-separation:);
  let intra-generation = get-property(properties, intra-generation-separation:);
  let start-x = 0;
  let start-y = 0;
  unless (empty?(root-nodes))
    local method %set-sheet-position-yx (record, y, x)
	    %set-sheet-position(record, x, y)
	  end method;
    local method do-children (function, node)
	    do(function, node-children(node))
	  end method,
          method traverse (new-node-function, old-node-function)
	    traverse-graph(root-nodes, do-children, node-table, identity,
			   new-node-function, old-node-function)
	  end method;
    let (breadthfun, depthfun, set-positionfun, start-breadth, start-depth)
      = select (orientation)
	  #"vertical", #"down", #"up" =>
	    values(box-width, box-height, %set-sheet-position, start-x, start-y);
	  #"horizontal", #"right", #"left" =>
	    values(box-height, box-width, %set-sheet-position-yx, start-y, start-x);
	end;
    let generation-descriptors = make(<simple-vector>, size: n-generations + 1);
    let max-gen-breadth = 0;
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
    local method collect-node-size (p, ph, child-node, nothing)
	    ignore(p, ph, nothing);
	    let descr = find-value(generation-descriptors, 
				   method (gd)
				     generation-generation(gd) = node-generation(child-node)
				   end);
	    inc!(generation-size(descr));
	    inc!(generation-breadth(descr), breadthfun(child-node));
	    max!(generation-depth(descr),   depthfun(child-node))
	  end method;
    traverse(collect-node-size, ignore);
    // Determine max-breadth and starting-depth
    let depth-so-far = start-depth;
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
    local method place-node (p, ph, child-node, nothing)
	    ignore(p, ph, nothing);
	    let descr = find-value(generation-descriptors,
				   method (gd)
				     generation-generation(gd) = node-generation(child-node)
				   end);
	    inc!(generation-breadth-so-far(descr),
		 if (generation-touched?(descr))
		   generation-inner-breadth-separation(descr)
		 else
		   generation-touched?(descr) := #t;
		   generation-edge-breadth-separation(descr)
		 end);
	    set-positionfun(child-node,
			    generation-breadth-so-far(descr),
			    if (center-nodes?)
			      generation-start-depth(descr)
			      + floor/(generation-depth(descr) - depthfun(child-node), 2)
			    else
			      generation-start-depth(descr)
			    end);
	    inc!(generation-breadth-so-far(descr), breadthfun(child-node))
	  end method;
    traverse(place-node, ignore)
  end
end method layout-graph-nodes;

define method layout-graph-edges
    (graph :: <DAG-graph-record>, sheet :: <output-recording-mixin>,
     #key edge-class = <line-graph-edge>, edge-initargs = #[]) => ()
  let root-nodes = graph-root-nodes(graph);
  let node-table = graph-node-table(graph);
  let properties = graph-properties(graph);
  let orientation = get-property(properties, orientation:);
  let medium-state = sheet-medium-state(sheet);
  let medium = sheet-medium(sheet);
  unless (empty?(root-nodes))
    local method do-children (function, node)
	    do(function, node-children(node))
	  end method;
    with-identity-transform (medium)
      with-output-recording-options (sheet, draw?: #f, record?: #t)
        with-new-output-record (record = sheet,
				record-class: <sequence-record>,
				parent: graph)
	  local method make-edge (parent, ph, child, ch)
                  ignore(ph, ch);
                  when (parent)
		    let (x1, y1, x2, y2)
		      = edge-attachment-points(parent, child, orientation);
                    let transform = make(<mutable-translation-transform>, tx: x1, ty: y1);
                    let region = make-bounding-box(0, 0, x2 - x1, y2 - y1);
                    let edge = apply(make, edge-class,
                                     region: region, transform: transform,
                                     x1: 0, y1: 0, x2: x2 - x1, y2: y2 - y1, 
				     from-node: parent, to-node: child,
				     medium-state: medium-state,
				     edge-initargs);
		    add-child(record, edge)
                  end
		end method;
          traverse-graph(root-nodes, do-children, node-table, identity,
                         make-edge, make-edge)
        end
      end
    end
  end
end method layout-graph-edges;

// ROOTS is a sequence of the roots of the graph.  
// CHILD-MAPPER is a function of two arguments, a function and an object
// over whose child objects the function should be applied.
// NODE-TABLE is a table that is used to record and detect when an object has
// already been included in the graph.
// KEY is a function of one argument used to produce the hash table key.
// There is no TEST function, since it is already captured in the hash table.
// NEW-NODE-FUNCTION is a function of four arguments, the parent object, the
// parent object's hash value, the child object, and "nothing".  Its returned
// value will be stored as the hash value of the child object.
// OLD-NODE-FUNCTION is a function of four arguments, the parent object, the
// parent object's hash value, the child object, and the child object's hash 
// value.  Its returned value is ignored.
// MAX-DEPTH is the cutoff depth of the tree, or NIL for no cutoff.
//--- Potential bug: the cutoff (MAX-DEPTH) may fall short in that you may
//--- reach a certain node at the maximum depth, mark that node as seen and
//--- decline to descend into its children, then find that node again
//--- through a shorter path.  If you really want to fix this, write a
//--- breadth-first descent of the graph.
define method traverse-graph
    (roots :: <sequence>, child-mapper :: <function>, node-table, key :: <function>,
     new-node-function :: <function>, old-node-function :: <function>,
     #key max-depth :: false-or(<integer>) = #f) => ()
  remove-all-keys!(node-table);
  local method traverse (parent-object, parent-hashval, object, max-depth)
	  let object-hashval
            = new-node-function(parent-object, parent-hashval, object, #f);
	  gethash(node-table, key(object)) := object-hashval;
	  when (max-depth) dec!(max-depth) end;
	  unless (max-depth = 0)
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


/// The grapher

define variable *inter-generation-separation* :: <integer> = 20;
define variable *intra-generation-separation* :: <integer> = 10;

define method format-graph-from-roots
    (sheet :: <output-recording-mixin>, roots :: <sequence>,
     object-printer :: <function>, children-generator :: <function>,
     #key x, y, orientation = #"horizontal", 
          merge-duplicates? = #f,
          key = identity, test = \==, table-class = <object-table>,
          children-predicate = #f,
          graph-class = if (merge-duplicates?) <DAG-graph-record> else <tree-graph-record> end,
          center-nodes? = #f, maximize-generations? = #f,
          cutoff-depth :: false-or(<integer>) = #f,
          edge-class = <line-graph-edge>, edge-initargs = #[],
          inter-generation-separation :: <integer> = *inter-generation-separation*,
          intra-generation-separation :: <integer> = *intra-generation-separation*,
          store-objects? = #t, move-caret? = #t)
 => (record :: <basic-graph-record>)
  unless (merge-duplicates?)
    // Guarantees that each object in the graph is unique...
    key  := vector;
    test := \==
  end;
  let node-table :: <table> = make(table-class);
  let properties = vector(orientation:, orientation,
			  center-nodes?:, center-nodes?,
			  cutoff-depth:, cutoff-depth,
			  merge-duplicates?:, merge-duplicates?,
			  maximize-generations:, maximize-generations?,
			  inter-generation-separation:, inter-generation-separation,
			  intra-generation-separation:, intra-generation-separation);
  let graph-record
    = with-output-recording-options (sheet, draw?: #f, record?: #t)
        with-end-of-line-action (sheet, #"allow")
	  with-end-of-page-action (sheet, #"allow")
	    with-new-output-record (graph-record = sheet,
                                    record-class: graph-class,
				    children-generator: children-generator,
				    children-predicate: children-predicate,
				    node-table: node-table,
				    properties: properties)
	      generate-graph-nodes(graph-record, sheet,
				   roots, object-printer, children-generator,
				   key: key, test: test);
	      graph-record
	    end
	  end
        end
      end;
  block ()
    layout-graph-nodes(graph-record, sheet);
    when (find-key(graph-root-nodes(graph-record),
                   method(node) ~empty?(node-children(node)) end))
      // Don't bother with edges if there are only root nodes
      layout-graph-edges(graph-record, sheet,
                         edge-class: edge-class, edge-initargs: edge-initargs)
    end
  cleanup
    // Flush any references to the user's objects if he doesn't want
    // them stored
    unless (store-objects?)
      do-sheet-children(method (node) node-object(node) := #f end,
			graph-record)
    end;
    // We're going to free the node table as we exit, so make sure
    // there are no pointers to it
    graph-node-table(graph-record) := #f
  end; 
  if (x & y)
    %set-sheet-position(graph-record, x, y)
  else
    let (x, y) = sheet-caret-position(sheet);
    %set-sheet-position(graph-record, x, y)
  end;
  recompute-region(graph-record);
  when (sheet-drawing?(sheet))
    repaint-sheet(graph-record, $everywhere)
  end;
  when (move-caret?)
    move-caret-beyond-output-record(sheet, graph-record)
  end;
  graph-record
end method format-graph-from-roots;
