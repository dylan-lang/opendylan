Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Graph controls

define constant <graph-orientation>
  = one-of(#"horizontal", #"vertical", #"up", #"down", #"left", #"right");

// The nodes in a tree control are layed out in a genuine graph
// Note that nodes and edges are not modelled as sheets in the front end!
//--- In a perfect world, <tree-control> would be a subclass of <graph-control>
define open abstract class <graph-control> (<tree-control>)
  // Various user-settable properties
  sealed constant slot graph-edge-class :: false-or(subclass(<graph-edge>)) = #f,
    init-keyword: edge-class:;
  sealed constant slot graph-edge-initargs :: <sequence> = #[],
    init-keyword: edge-initargs:;
  sealed constant slot graph-edge-generator :: false-or(<function>) = #f,
    init-keyword: edge-generator:;
  sealed constant slot graph-orientation :: <graph-orientation> = #"horizontal",
    init-keyword: orientation:;
  sealed constant slot graph-center-nodes? :: <boolean> = #f,
    init-keyword: center-nodes?:;
  sealed constant slot graph-inter-generation-spacing :: <integer> = 20,
    init-keyword: inter-generation-spacing:;
  sealed constant slot graph-intra-generation-spacing :: <integer> =  8,
    init-keyword: intra-generation-spacing:;
end class <graph-control>;


// Nodes within a graph control
//--- In a perfect world, <tree-node> would be a subclass of <graph-node>
define open abstract class <graph-node> (<tree-node>)
end class <graph-node>;

define protocol <<graph-node>> (<<tree-node>>)
  getter node-x
    (node :: <tree-node>) => (x :: <integer>);
  getter node-x-setter
    (x :: <integer>, node :: <tree-node>) => (x :: <integer>);
  getter node-y
    (node :: <tree-node>) => (y :: <integer>);
  getter node-y-setter
    (y :: <integer>, node :: <tree-node>) => (y :: <integer>);
end protocol <<graph-node>>;

define sealed method make-node
    (graph :: <graph-control>, object, #rest initargs, #key)
 => (node :: <graph-node>)
  apply(do-make-node, graph, <graph-node>, object: object, initargs)
end method make-node;

define sealed method find-node
    (graph :: <graph-control>, object, #key node: parent-node)
 => (node :: false-or(<graph-node>))
  do-find-node(graph, object, node: parent-node)
end method find-node;


// Edges within a graph control
define open abstract primary class <graph-edge> (<object>)
  sealed constant slot graph-edge-from-node :: <graph-node>,
    required-init-keyword: from-node:;
  sealed constant slot graph-edge-to-node   :: <graph-node>,
    required-init-keyword: to-node:;
  sealed constant slot graph-edge-object    :: <object> = #f,
    init-keyword: object:;
end class <graph-edge>;

define protocol <<graph-edge>> ()
  getter draw-edges
    (edge :: <graph-edge>, medium :: <abstract-medium>, region :: <region>) => ();
end protocol <<graph-edge>>;
