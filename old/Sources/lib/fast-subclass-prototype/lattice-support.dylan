module: fast-subclass
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// This is lattice structure support code for the algorithms in
/// Efficient Handling of Multiple Inheritance Hierarchies
/// by Yves Caseau form Oopsla 93
/// 


// should be limited(<sequence>, of: <lattice-node>)
define constant <lattice-node-sequence> = <sequence>;


define abstract class <lattice-node> (<object>)
  slot lattice-direct-predecessors-slot :: <lattice-node-sequence>,
     init-value: #(),
     init-keyword: direct-predecessors:;
  slot lattice-direct-successors-slot :: <lattice-node-sequence>,
     init-value: #();
  slot lattice-predecessors :: <lattice-node-sequence>,
     init-value: #(),
    init-keyword: predecessors:;
  // These are the 3 'functions' on each node described in the paper
  // subclass-bits is lower case gamma
  slot subclass-bits :: <subclass-bits>, init-function: make-subclass-bits;
  // forbidden-bits is 'v' (lower case greek nu?)
  slot forbidden-bits :: <subclass-bits>, init-function: make-subclass-bits;
  // Gene-bit is 'g' in the paper
  slot gene-bit :: false-or(<integer>), init-value: #f;
  slot node-number :: <integer>, init-value: -1;
end;

define class <ordinary-node> (<lattice-node>)
end;


/// nodes corresponding to a dylan class
define class <class-node> (<ordinary-node>)
  slot class-node-class :: <class>, required-init-keyword: class:;
end;

define constant *top-node* = make (<class-node>, class: <object>);



define constant *bottom-node* = make (<lattice-node>,
				      direct-predecessors: list(*top-node*),
				      predecessors: list(*top-node*));


define inline method lattice-direct-predecessors (node :: <lattice-node>)
  lattice-direct-predecessors-slot(node)
end;

define variable *node-counter* = 0;

define method lattice-direct-predecessors-setter
    (new-predecessors :: <lattice-node-sequence>, node :: <lattice-node>)
  lattice-direct-predecessors-slot(node) 
    := remove-duplicates(new-predecessors);
  // add this node to the successors of all the new parents
  for (predecessor :: <lattice-node> in new-predecessors)
    lattice-direct-successors-slot(predecessor) :=
      add-new!(lattice-direct-successors-slot(predecessor), 
	       node);
  end;
  node-number(node) := *node-counter*; *node-counter* := *node-counter* + 1;
  // add bottom to successors of node
  unless (node == *bottom-node*)
    // lattice-direct-successors-slot(node) := list(*bottom-node*);
    lattice-direct-predecessors-slot(*bottom-node*) 
      := add!(lattice-direct-predecessors-slot(*bottom-node*), node);
    lattice-predecessors(*bottom-node*)
      := add!(lattice-predecessors(*bottom-node*), node)
  end;
  // calculate all-predecessors of node.
  lattice-predecessors(node) 
    := reduce(method(set, p :: <lattice-node>)
		  union(set, lattice-predecessors(p))
	      end,
	      list(node),
	      new-predecessors);
  new-predecessors;
end;

define inline method lattice-direct-successors (node :: <lattice-node>)
  lattice-direct-successors-slot(node)
end;

define method lattice-class-subnodes (node :: <class-node>)
 => (nodes :: <lattice-node-sequence>)
  list(node)
end;

define method lattice-class-subnodes (node :: <combination-node>)
 => (nodes :: <lattice-node-sequence>)
  add!(reduce(concatenate,#(), map(lattice-class-subnodes,
				   lattice-direct-successors(node))),
       node)
end;

define method lattice-class-subnodes (node :: <lattice-node>)
 => (nodes :: <lattice-node-sequence>)
  reduce(concatenate,#(), map(lattice-class-subnodes,
			      lattice-direct-successors(node)))

end;



// This should really just return the first successor class nodes, plus
// any intermediate nodes that actually have a gene bit.
define method lattice-true-direct-successors (node :: <lattice-node>)
  reduce(concatenate, #(), map(lattice-class-subnodes, lattice-direct-successors(node)));
end;


/// The 'tau' function from the paper only applies to join nodes, but
/// it makes it simpler it is identity on class nodes
define method tau (c :: <ordinary-node>)
  list(c);
end;

/// These nodes make up the set J in the paper.
// a <join-node> is not "ordinary"
define class <join-node> (<lattice-node>)
  slot join-node-node-1 :: <lattice-node>, required-init-keyword: node-1:;
  slot join-node-node-2 :: <lattice-node>, required-init-keyword: node-2:;
  slot tau :: <simple-object-vector>, required-init-keyword: tau:;
end;

// experimental, and not currently used
define class <branching-node> (<ordinary-node>)
end;

// needed??
// not currently used.
// This could be used to install a single node under each unique set
// of multiple parents.
define class <joining-node> (<ordinary-node>)
end;

// experimental
// This one is being used.  
// Each one should have exactly 1 parent.
// These nodes are installed under class nodes with many children.
// Each direct subclass of the parent of any combintion node will inherit
// from a unique combination of exactly 2 combination nodes.
define class <combination-node> (<ordinary-node>)
  slot true-child-count, init-value: 0;
end;


// the set of all classes, and the map to encoding info
define constant *s* =
     begin
       let t = make(<table>);
       element(t, <object>) := *top-node*;
       t
     end;

/// lookup the node for a class
define function get-class-node (x :: <class>)
  element(*s*, x) |
  (element(*s*, x) := make(<class-node>, class: x));
end;

/// The following 6 methods get from a <class> to it's information from the
/// algorithm.  These are mostly here so we can treat <class> objects
/// pretty much as nodes in the completed lattice.

define method subclass-bits (c :: <class>) => (s :: <subclass-bits>);
  subclass-bits(*s*[c]);
end;  

/// probably not needed
define method forbidden-bits (c :: <class>) => (s :: <subclass-bits>);
  forbidden-bits(*s*[c]);
end;  

/// probably not needed
define method subclass-bits-setter (new :: <subclass-bits>, c :: <class>)
 => (s :: <subclass-bits>);
  subclass-bits(*s*[c]) := new;
end;  

/// probably not needed
define method forbidden-bits-setter (new :: <subclass-bits>, c :: <class>)
 => (s :: <subclass-bits>);
  forbidden-bits(*s*[c]) := new;
end;  

/// probably not needed
define method gene-bit (class :: <class>) => (gene :: false-or(<integer>));
  gene-bit(*s*[class]);
end;

/// probably not needed
define method gene-bit-setter
    (new :: false-or(<integer>), class :: <class>)
 => (gene :: false-or(<integer>));
  gene-bit(*s*[class]) := new;
end;

/// The <= function from the paper.
define method l-<= (a :: <lattice-node>, b :: <lattice-node>) => result :: <boolean>;
  a == b | member?(b, lattice-predecessors(a))
end;

/// this is the /\ (logical 'and' symbol) operator applied to lattice nodes
/// It returns the greatest lower bound for the 2 nodes
define function find-join-node (a :: <lattice-node>, b :: <lattice-node>)
  local method walk-up (node :: <lattice-node>)
          block (return)
            for (p in lattice-direct-predecessors(node))
              if (l-<=(p, a) & l-<=(p, b))
                return(walk-up(p))
              end if;
            end for;
            return(node)
          end block;
        end method;
  walk-up(*bottom-node*)
end;


/// reset the state so we can do some debugging.
define method reset-subclass-bits (root)
  size(*s*) := 0;
  // hack for emulator ...
  for (key in key-sequence(*s*))
    remove-key!(*s*, key)
  end; 
  // end hack for emulator
  *node-counter* := 0;
  element(*s*, root) := *top-node*;
  class-node-class(*top-node*) := root;
  *last-gene* := #f;
  lattice-direct-successors-slot(*top-node*) := #();
  lattice-direct-predecessors(*top-node*) := #();
  lattice-direct-predecessors-slot(*bottom-node*) := list(*top-node*);
end;


/// run function on each node in the lattice below root.
/// Assure that all the parents of a node that are also under root are 
/// visited before the node
define method top-down
    (root :: <lattice-node>, function :: <function>)
  top-down-internal(root, function, lattice-direct-predecessors,
		    lattice-direct-successors);
end;

/// optimization
define method top-down
    (root == *bottom-node*, function :: <function>)
  // do nothing
end;


/// top-down on <class>es
define method top-down
    (root :: <class>, function :: <function>)
  top-down-internal(root, function, d-direct-superclasses,
		    d-direct-subclasses);
end;


/// Walk the lattice and call the function for every node.
/// The function is not called on a node until it has been called on
/// every parent of the node
/// There may be a way to optimize this better.
define method top-down-internal
    (root, function :: <function>,
     direct-predecessors :: <function> , direct-successors :: <function>)
 => ();
  let universe = make(<table>);
  let pending = make(<deque>);
  let processed = make(<table>);
  local method add-to-universe (top)
	  unless (element(universe, top))
	    element(universe, top) := #t;
	    do(add-to-universe, direct-successors(top));
	  end;
	end;
  local method processed?(node) => (yes? :: <boolean>);
	  element(processed, node, default: #f)
	end;
  local method enqueue (node)
	  unless (processed?(node))
	    element(processed, node) := #t;
	    push-last(pending, node);
	  end;
	end;
  local method process (node)
	  if (every?(method(p)
		       ~element(universe, p)
			 | (processed?(p) & ~member?(p, pending))
		     end,
		     direct-predecessors(node)))
	    function(node);
	    do(rcurry(enqueue), direct-successors(node))
	  else
	    push-last(pending, node);
	  end if;
	end;
  add-to-universe(root);
  element(processed, root) := #t;
  for (sub in direct-successors(root))
    enqueue(sub);
  end;
  while (~empty?(pending))
    let elt = pop(pending);
    process(elt);
  end;
end;



/// walk the lattice and call the function for every node under node
/// no need to worry about whether the parents of the node have already
/// been visited, but don't visit any nodes more than once.
// on a class
define method do-all-subnodes (node :: <class>, fun :: <function>)
  let universe = make(<table>);
  local method add-to-universe (top)
	  unless (element(universe, top))
            fun(top);
	    element(universe, top) := #t;
	    do(add-to-universe, direct-subclasses(top));
	  end;
	end;
  add-to-universe(node);
end;


// on a lattice node
define method do-all-subnodes (node :: <lattice-node>, fun :: <function>)
  let universe = make(<table>);
  local method add-to-universe (top)
	  unless (element(universe, top))
            fun(top);
	    element(universe, top) := #t;
	    do(add-to-universe, lattice-direct-successors(top));
	  end;
	end;
  add-to-universe(node);
end;

/// the rest of this file is for debugging

define method print-gene-bits (n :: <lattice-node>, level :: <integer>)
  for(i from 0 below level)
    format-out(" ");
  end;
  print-node(n);
  for(node in lattice-direct-successors(n))
    print-gene-bits(node, level + 1);
  end;
end;


define method maximum-genes ()
  next-gene(reduce(subclass-bit-union, make-subclass-bits(), 
                   map(subclass-bits, 
                       lattice-direct-predecessors(*bottom-node*))));
end;


define method test-subclass-short ()
  let losers = #();
  for (n1 in key-sequence(*s*), i from 0)
    if (modulo(i, 4) = 0)
      for (n2 in key-sequence(*s*), j from 0)
	if (modulo(j, 5) = 0)
	  unless (~subtype?(n1, n2) == ~fast-subclass(n1, n2))
	    unless (member?(n1, losers) | member?(n2, losers))
	      format-out("losing on %s, %s, subtype?:  %s, bit-subtype: %s\n", 
			 n1, n2, subtype?(n1, n2), fast-subclass(n1, n2));
	      losers := add!(losers, n1);
	      losers := add!(losers, n2);
	    end;
	  end;
	end;
      end;
    end;
  end;
  losers
end;


define method test-subclass ()
  let losers = #();
  for (n1 in key-sequence(*s*))
    for (n2 in key-sequence(*s*))
      unless (~subtype?(n1, n2) == ~fast-subclass(n1, n2))
	unless (member?(n1, losers) | member?(n2, losers))
	  format-out("losing on %s, %s, subtype?:  %s, bit-subtype: %s\n", 
		     n1, n2, subtype?(n1, n2), fast-subclass(n1, n2));
	  losers := add!(losers, n1);
	  losers := add!(losers, n2);
	end;
      end;
    end;
  end;
  losers
end;

define method test-some-subclasses (seq)
  let losers = #();
  for (n1 in seq)
    for (n2 in seq)
      unless (~subtype?(n1, n2) == ~fast-subclass(n1, n2))
        unless (member?(n1, losers) | member?(n2, losers))
          format-out("losing on %s, %s, subtype?:  %s, bit-subtype: %s\n", 
                      n1, n2, subtype?(n1, n2), fast-subclass(n1, n2));
          losers := add!(losers, n1);
          losers := add!(losers, n2);
        end;
      end
    end;
  end;
  losers
end;



define variable *last-gene* = #f;

define method print-subclass-bits (bits :: <subclass-bits>)
  unless (*last-gene*)
    *last-gene* := maximum-genes();
  end;
  let high-bit = block (return)
                   for (i from *last-gene* to 0 by -1)
                     if (subclass-bit?(bits, i))
                       return(i + 1);
                     end;
		   end;
		   0
                 end;
  for (i from *last-gene* to 0 by -1)
    if (subclass-bit?(bits, i))
      format-out("1");
    else
      format-out("0");
    end;
  end;
end;



define method print-node (node :: <class-node>)
  format-out("{|");
  print-subclass-bits(subclass-bits(node));
  format-out("| %s class-node %s ",
	     gene-bit(node) | "|",
	     debug-name(class-node-class(node)));
  print-node-numbers(node);
  format-out("}\n");
end;
	     
define method print-node-numbers (node)
  format-out("#%s, (", node-number(node));
  for (n in lattice-direct-predecessors(node))
    format-out("%s ", node-number(n));
  end;
  format-out(")");
end;



define method print-node (node :: <join-node>)
  format-out("{|");
  print-subclass-bits(subclass-bits(node));
  format-out("| || join-node  ");
  print-node-numbers(node);
  format-out("[");
  for (n in tau(node))
    format-out("%s ", node-number(n));
  end;
  format-out("]}\n");
end;

define method print-node (node :: <branching-node>)
  format-out("{|");
  print-subclass-bits(subclass-bits(node));
  format-out("| %s branching-node  ",
	     gene-bit(node));
  print-node-numbers(node);
  format-out("}\n");
end;

define method print-node (node :: <combination-node>)
  format-out("{|");
  print-subclass-bits(subclass-bits(node));
  format-out("| %s combo-node  ",
	     gene-bit(node));
  print-node-numbers(node);
  format-out("}\n");
end;


define method print-node (node :: <lattice-node>)
  format-out("{|");
  print-subclass-bits(subclass-bits(node));
  format-out("| %s node  ",
	     gene-bit(node) | "|");
  print-node-numbers(node);
  format-out("}\n");
end;

define method print-meet (node1 :: <class>, node2 :: <class>)
  print-meet(get-class-node(node1), get-class-node(node2));
end;

define method print-meet (node1 :: <lattice-node>, node2 :: <lattice-node>)
  let supers1 = lattice-predecessors(node1);
  let supers2 = lattice-predecessors(node2);
  print-some-gene-bits(*top-node*, union(supers1, supers2));

end;


define method print-some-gene-bits (start :: <lattice-node>, included-nodes :: <lattice-node-sequence>)
  top-down(start,
	   method (node)
	     if(member?(node, included-nodes))
	       print-node(node)
	     end
	   end);
end;
