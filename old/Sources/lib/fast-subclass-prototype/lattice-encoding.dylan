module: fast-subclass
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// This is the incremental encoding algorithm from the paper
/// Efficient Handling of Multiple Inheritance Hierarchies
/// by Yves Caseau form Oopsla 93
/// 


/// top level function that builds bit table for all the classes
define method build-bits-for-classes (root)
  // need something here to lock out other threads from doing the
  // same thing.
  reset-subclass-bits(root);
  top-down(root, add-class-node)
end;

/// add an existing class to the lattice
define method add-class-node (c :: <class>)
  let supers = d-direct-superclasses(c);
  // for classes with exactly one superclass we pass on the number of
  // sibling classes to create-class.  This is to facilitate the creation of
  // intermediate nodes for classes with lots of direct subclasses.
  if (size(supers) = 1)
    create-class(c, supers, size(d-direct-subclasses(supers[0])));
  else
    create-class(c, supers, 0);
  end;
end;

define constant $branching-limit = 8;


/// from the paper
/// I have added the branching limit stuff, and the extra 
/// branching-factor parameter so we can decide if we need to add 
/// intermediate nodes
define method create-class (x :: <class>, ys :: <sequence>,
			    branching-factor :: <integer>)
  let x-node = get-class-node(x);
  let ys-nodes = map(get-class-node, ys);
    
  let parents = if (branching-factor > $branching-limit
		      & size(ys-nodes) = 1)
		  find-free-combination-node-pair(ys-nodes, branching-factor);
		else
		  ys-nodes;
		end;
  lattice-direct-predecessors(x-node) := check(x-node, parents);
  encode(x-node);
  x-node;
end;

/// From the paper
define method encode (class :: <lattice-node>)
  let supers = lattice-direct-predecessors(class);
  if (size(supers) = 1)
    // single inheritance is relatively simple
    let super = supers[0];
    forbidden-bits(super) := calculate-forbidden-bits(super);
    gene-bit(class) := next-gene(subclass-bits(super), forbidden-bits(super));
    subclass-bits(class)
      := set-subclass-bit(subclass-bits-copy(subclass-bits(super)),
			  gene-bit(class));
  else
    solve-conflicts(class);
    subclass-bits(class) := apply(subclass-bit-union,
				  map(subclass-bits,
				      lattice-direct-predecessors(class)));
  end if;
end method;


/// optimization for *bottom-node*/  
define method calculate-forbidden-bits (class == *bottom-node*)
 => (bits :: <subclass-bits>)
  make-subclass-bits();
end;


/// the 'forbidden' function from the paper.
// I don't think it goes far enough.
define method calculate-forbidden-bits (class :: <lattice-node>)
 => (bits :: <subclass-bits>);
  let new-bits = make-subclass-bits();
  for (superior in lattice-predecessors(class))
    for (candidate-y in lattice-true-direct-successors(superior))
      if(size(lattice-direct-predecessors(candidate-y)) = 1)
	unless (class == candidate-y
		  | l-<=(class, candidate-y))
	  new-bits := set-subclass-bit(new-bits, gene-bit(candidate-y));
	end;
      end;
    end for;
  end for;
  do-all-subnodes
    (class,
     method (descendent)
	 new-bits := set-subclass-bit(new-bits, gene-bit(descendent))
     end);
  new-bits
end method;


/// from the paper
define method solve-conflicts (class :: <lattice-node>)
 => ();
  let forbidden = calculate-forbidden-bits(class);
  for (parent in lattice-direct-predecessors(class))
    for (ancestor in lattice-predecessors(parent))
      if (subclass-bit?(forbidden, gene-bit(ancestor)))
	mutation(ancestor, forbidden);
      end;
    end;
  end;
end;


/// from the paper
define method mutation (class :: <lattice-node>, forbidden :: <subclass-bits>)
  let new-forbidden = remove-gene(class, gene-bit(class), forbidden);
  gene-bit(class) := next-gene(new-forbidden, subclass-bits(class));
  do-all-subnodes
    (class,
     method (sub)
       set-subclass-bit(subclass-bits(sub), gene-bit(class));
     end);
end;

/// from the paper
define method remove-gene (class :: <lattice-node>, gene :: <integer>,
			   forbidden :: <subclass-bits>)
 => ();
  let result :: <subclass-bits> = forbidden;
  gene-bit(class) := #f;
  unset-subclass-bit(subclass-bits(class), gene);
  for (super in lattice-direct-predecessors(class))
    result := subclass-bit-union(result, calculate-forbidden-bits(super));
  end;
  top-down(class,
	   method (c)
	     result := subclass-bit-union(result, calculate-forbidden-bits(c));
	     if (gene-bit(c))
	       if (~any?(method (super) gene-bit(super) = gene end,
			 lattice-predecessors(c)))
		 unset-subclass-bit(subclass-bits(c), gene);
	       end;
	     else // union of superclasses subclass bits
	       subclass-bits(c)
		 := reduce(subclass-bit-union, make-subclass-bits(),
			   map(subclass-bits, lattice-direct-predecessors(c)));
	     end
	   end method);
  result;
end;

// ----------
// combination nodes
// create intermediate nodes under a node with many children so that 
// each child is under a unique pair of the intermediate node


define method find-free-combination-node-pair (parents, branching-factor)
  let combination-nodes
    = choose(rcurry(instance?, <combination-node>),
	     lattice-direct-successors(parents[0]));
  let (p1, p2)
    = choose-combination-nodes(combination-nodes, parents, branching-factor);
  list(p1, p2);
end;  

define method choose-combination-nodes
    (combination-nodes, parents, branching-factor)
  let child-limit = size(combination-nodes);
  local method find-node (found)
	  let found-successors = found & lattice-direct-successors(found);
          block (return)
	    for (node in combination-nodes)
	      if (node.true-child-count < child-limit
		    & node ~= found
		    & if (found)
			empty?(intersection(lattice-direct-successors(node),
					    found-successors))
		      else
			#t
		      end)
		node.true-child-count := node.true-child-count + 1;
		return(node)
	      end;
	    end for;
	    // didn't find one so add one to parent
	    child-limit := child-limit + 1;
	    let new = new-combination-node(parents[0]);
	    new.true-child-count := 1;
	    return(new);
	  end block;
	end method;
  let p1 = find-node(#f);
  values(p1, find-node(p1));
end;

define method new-combination-node (parent)
  let new = make(<combination-node>);
  lattice-direct-predecessors(new) := list(parent);
  encode(new);
  new;
end;


//---------
// playing around ...
// number of combinations of n things chosen from m things
define method comb (n,m)
  if (n > m) 0
  elseif (m = 1 | m = n) 1
  elseif (n = 1) m
  else comb(n - 1, m - 1) + comb1(n - 1, m - 2)
  end
end;

define method comb1 (n, m)
  comb(n, m)
    + if (n > m) 0
      else comb1(n, m - 1)
      end
end;

// -------------------
// branching nodes
// not used...
// make many children of one node into a $bushiness-ary tree rooted
// under the node

define constant $bushiness = 3;


define method find-free-branching-node (parent, branching-factor)
 => (new-parent :: <lattice-node-sequence>)
  let children = lattice-direct-successors(parent);
  let (next-branching, left) = truncate/(branching-factor, $bushiness);
  block (return)
    if (size(children) < $bushiness)
      // not enough children, so add some
      if (next-branching <= 1)
	if (next-branching = 1)
	  // this parent has some leaf, and some branching nodes
	  for(i from size(children) below left)
	    add-branching-node(parent);
	  end;
	end;
	// now return parent since the rest of its nodes will be leaves
	return(list(parent));
      else // all leaf nodes directly below here
	// just add up to $bushiness branching nodes
	for(i from size(children) below $bushiness)
	  add-branching-node(parent);
	end;
      end if; 
    end if; // finished adding nodes
    // refetch children since it may have changed
    let children = lattice-direct-successors(parent);
    for(child in children,
	extra = left then extra - 1)
      // look for a child that has a free slot under a branching node
      if (instance?(child, <branching-node>))
	let found? = if(extra > 0)
		       find-free-branching-node(child, next-branching + 1);
		     else
		       find-free-branching-node(child, next-branching);
		     end;
	// found one.
	if (found?)
	  return(found?);
	end if;
      end if;
    end for;
  end block;
  // falling through so return false
end;


define method add-branching-node (parent)
  let new-node = make(<branching-node>);
  lattice-direct-predecessors(new-node) := list(parent);
  encode(new-node);
end;


define method find-or-create-branching-node
    (node :: <lattice-node>,
     parents :: <lattice-node-sequence>,
     branching-factor :: <integer>)
 => (new-parents :: <lattice-node-sequence>)
  let new-parents = find-free-branching-node(parents[0], branching-factor);
  if(~new-parents)
    format-out("failed to find branching node for %s\n", class-node-class(node));
    parents
  else
    new-parents
  end;
end;
