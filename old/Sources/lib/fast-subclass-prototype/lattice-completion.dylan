language: infix-dylan
module: fast-subclass
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// This is the lattice completion algorithm from the paper
/// Efficient Handling of Multiple Inheritance Hierarchies
/// by Yves Caseau form Oopsla 93
/// 



/// this is not in the paper, but it is needed if 2 classes inherit from
/// the same set of superclasses then we need to create a joining node,
/// all the subclasses with matching parents inherit from that node.
// not currently used
define function check2 (x :: <lattice-node>,
			s :: <lattice-node-sequence>)
 => (s- :: <lattice-node-sequence>);
  let s-size = size(s);
  if (s-size > 1)
    let conflicts
      = choose(method(sibling)
		   size(lattice-direct-predecessors(sibling)) = s-size
	       end,
	       reduce(intersection,
		      lattice-direct-successors(s[0]),
		      map(lattice-direct-successors, tail(s))));
    
    if (~empty?(conflicts))
      // install a new node, and inherit from only that one.
      let previous-joining-node
	= find-if(method (x)
		    instance?(x, <joining-node>)
		      | instance?(x, <join-node>)
		  end,
		  conflicts);
      if (previous-joining-node)
	// return the already existing one
	list(previous-joining-node)
      else
	// return the new superclass
	list(install-joining-node(s, conflicts));
      end;
    else
      check(x,s)		// check multiple inheritance
    end if;
  else
    s				// just return s
  end;
end;

/// also not in the paper.  aux function used by check2 above.
define method install-joining-node
    (parents :: <lattice-node-sequence>,
     conflicted-children :: <lattice-node-sequence>)
 => (joining-node :: <joining-node>)
  let new-node = make(<joining-node>);
  lattice-direct-predecessors(new-node) := check(new-node, parents);
  encode(new-node);
  for (child in conflicted-children)
    if(instance?(child, <class-node>))
      lattice-direct-predecessors(child) := list(new-node);
      encode(child);
      let gene = gene-bit(child);
      // propagate the new gene.
      do-all-subnodes(child,
		      method (descendent)
			subclass-bits(descendent)
			  := set-subclass-bit(subclass-bits(descendent),
					      gene);
		      end);
    end;
  end;
  new-node;
end;
		    




/// From the paper
/// returns the augmented parents list for the node x
/// s is the set of parents specified for this node.
define function check (x :: <lattice-node>,
		       s :: <lattice-node-sequence>)
  => (s- :: <lattice-node-sequence>);
  let s- = s;
  // we use lists and only do the lower half of s X s
  for (y1-tail = s then tail(y1-tail),
       until: empty?(y1-tail))
    let y1 = head(y1-tail);
    for (y2-tail = tail(y1-tail) then tail(y2-tail),
	 until: empty?(y2-tail))
      let y2 = head(y2-tail);
      unless (y1 == y2)
	for (A in lattice-predecessors(y1))
	  for (B in lattice-predecessors(y2))
	    unless (A == B)
	      // the following line in the paper had a typo.
	      // The paper had '&' instead of '|'
	      // The intent is 'if (A and B are not comparable) ...'
	      if (~(l-<=(A, B) | l-<=(B, A)))
		let i = find-join-node(A,B);
		if (i ~= x & ~any?(rcurry(l-<=, i), s))
		  if (instance?(i, <join-node>) &
			every?(method (w) any?(rcurry(l-<=, w), s) end,
			       tau(i)))
		    unless (member?(i, s-))
		      if (~any?(curry(l-<=, i), s-))
			// don't add this node if it's already covered by an
			// existing join node
			do(method (n)
			     if (~instance?(n, <combination-node>)
				   & (y1 == n | y2 == n))
			       s- := remove!(s-, n);
			     end;
			   end,
			   tau(i));
			s- := add!(s- , i);
		      else
			format-out("\n Not adding parent node\n");
			print-node(i);
			format-out("to: \n");
			print-node(x);
			format-out("in favor of\n");
			print-node(find-if(curry(l-<=, i), s-))
		      end;
		    end unless;
		  else
		    let new-node = simplify(i, s, A, B);
		    unless (member?(new-node, s-))
		      for (n in s-)
			// remove any elements of s- that are superseded by
			// the new node 
			if (~instance?(n, <combination-node>)
			      & l-<=(new-node, n))
			  format-out("\n Removing parent node\n");
			  print-node(n);
			  format-out("from: \n");
			  print-node(x);
			  format-out("in favor of:\n");
			  print-node(new-node);
			  s- := remove(s-, n);
			end if;
		      end for;
		      s- := add!(s-, new-node);
		    end unless;
		  end if;
		end if;
	      end if;
	    end unless;
	  end for;
	end for;
      end unless;
    end for;
  end for;
  format-out("Returning node(s):\n");
  for(n in s-)
    print-node(n);
  end;
  s-    // finally!
end;


/// support function 
define function find-if (fn, sequence) => (object)
  block (return)
  for (item in sequence)
    if (fn(item))
      return(item);
    end;
  end for;
  return(#f);
  end block;
end;

/// from the paper
/// returns the new node to insert above the node being added
define function simplify (i :: <lattice-node>,
			  s :: <lattice-node-sequence>,
			  A :: <lattice-node>,
			  B :: <lattice-node>)
  => (join :: <lattice-node>);
  let z = find-if(method (z)
                    l-<=(i, z) & any?(rcurry(l-<=, z), s)
                  end,
                  lattice-direct-successors(A));
  if (z)
    simplify(i, s, z, B)
  else
    let z = find-if(method (z)
                      l-<=(i, z) & any?(rcurry(l-<=, z), s)
                    end,
                    lattice-direct-successors(B));
    if (z)
      simplify(i, s, A, z);
    else
      create-join(i, A, B);
    end if;
  end if;
end;


/// from the paper.
/// returns a new node with parents A and B that maintains the
/// lattice structure.
define function create-join
    (i :: <lattice-node>, A :: <lattice-node>, B :: <lattice-node>)
 => (A-B :: <lattice-node>);
  let A-B = make(<join-node>,
		 node-1: A, node-2: B,
		 tau: union(tau(A), tau(B)));
  lattice-direct-predecessors(A-B) := check(i, list(A, B));
  encode(A-B);
  A-B
end;

