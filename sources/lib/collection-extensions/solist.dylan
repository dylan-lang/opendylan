module: 	self-organizing-list
author: 	Robert Stockton (rgs@cs.cmu.edu)
synopsis:	Provides "self-organizing lists".  These explicit key
		collections provide roughly the semantics of hash tables, but
		use a probabilistic implementation which provides O(n) worst
		case performance but can provide very fast constant time
		access in the best case.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

//======================================================================
// The "Self Organizing List" is a "poor man's hash table".  More
// precisely, <self-organizing-list> is a subclass of <dictionary> for
// which addition and retrieval are both linear in the worst case, but
// which use a probabilistic strategy which yields nearly constant
// time in the best case.
//
// Because they have a very low overhead, self-organizing lists may
// provide better peformance than hash tables in cases where
// references have a high degree of temporal locality.  They may also
// be useful in situations where it is difficult to create a proper
// hash function.
//
// Define new self-organizing-lists with 
//
//   make(<self-organizing-list>, test: test)
//
// Test is expected to be an equality function.  In particular, it is
// expected to satisfy the identity and transitivity requirements
// described in chapter 5.  If not specified, test defaults to \==.
//======================================================================


define class <self-organizing-list>
    (<mutable-explicit-key-collection>, <stretchy-collection>)
  slot data :: <list>, init-value: #();
  // slot accessor provides method for standard collection op "key-test"
  sealed slot key-test :: <function>, init-value: \==, init-keyword: test:;
end class;

define sealed domain make (singleton(<self-organizing-list>));
define sealed domain initialize (<self-organizing-list>);

define inline function sol-fip-next-state
    (list :: <self-organizing-list>, state :: <list>) 
    => (result :: <list>);
  tail(state);
end function;

define inline function sol-fip-finished-state?
    (list :: <self-organizing-list>, state :: <list>, limit)
    => result :: <boolean>;
  state == #();
end function;

define inline function sol-fip-current-key
    (list :: <self-organizing-list>, state :: <list>) 
    => (result :: <object>);
  head(head(state));
end function;


define inline function sol-fip-current-element
    (list :: <self-organizing-list>, state :: <list>) 
    => (result :: <object>);
  tail(head(state));
end function;

define inline function sol-fip-current-element-setter
    (value :: <object>, list :: <self-organizing-list>, state :: <list>) 
    => (result :: <object>);
  tail(head(state)) := value;
end function;

define inline function sol-fip-copy-state
    (list :: <self-organizing-list>, state :: <list>) 
    => (result :: <list>);
  state;
end function;

define sealed inline method forward-iteration-protocol
    (table :: <self-organizing-list>)
    => (initial-state :: <object>,
	limit :: <object>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(table.data, #f, sol-fip-next-state, sol-fip-finished-state?,
	 sol-fip-current-key, sol-fip-current-element,
	 sol-fip-current-element-setter, sol-fip-copy-state);
end method forward-iteration-protocol;

define constant sol-no-default = pair(#f, #f);

// Auxiliary function for element.  Search the tail of the list for an element
// for which test(elem, key) is true, and then return the pair which
// *precedes* that element (or #() if not found)
//
define function elem-search (prev :: <list>, test :: <function>, key)
  let list = prev.tail;
  if (list == #())
    #();
  else
    let elem = list.head;
    if (test(elem.head, key))
      prev;
    else
      elem-search(list, test, key);
    end if;
  end if;
end function elem-search;

define method element(table :: <self-organizing-list>, key :: <object>,
		      #key default: default = sol-no-default)
    => res :: <object>;
  let list :: <list> = table.data;
  let test :: <function> = table.key-test;

  let first = list.head;	// depend upon head(#()) being defined
  case
    list == #() =>
      if (default == sol-no-default)
	error("Key %= not in %=", key, table)
      else
	default
      end if;
    test(first.head, key) =>
      first.tail;
    otherwise =>
      let cell :: <list> = elem-search(list, test, key);
      if (cell ~= #())
	// Move the matching element to the beginning
	let next = cell.tail;
	cell.tail := next.tail;
	next.tail := list;
	table.data := next;
	// Return the actual value
	next.head.tail;
      elseif (default == sol-no-default)
	error("Key %= not in %=", key, table)
      else
	default;
      end if;
  end case;
end method element;

define constant sol-no-value = pair(#f, #f);

define method element-setter(value, table :: <self-organizing-list>, 
			     key :: <object>)
    => value :: <object>;
  // Bring the existing element (if any) to the front of the list
  if (element(table, key, default: sol-no-value) == sol-no-value)
    // It wasn't there, so add it
    table.data := pair(pair(key, value), table.data);
  else
    // It was there, so change the value of the first element.
    tail(head(table.data)) := value;
  end if;
  value;
end method element-setter;

define method remove-key!
    (table :: <self-organizing-list>, key :: <object>)
 => (nuked? :: <boolean>);
  // Bring the existing element (if any) to the front of the list
  if (element(table, key, default: sol-no-value) ~= sol-no-value)
    // It was there, so remove it.
    table.data := table.data.tail;
    #t;
  else
    #f;
  end if;
end method remove-key!;
