Module: t-lists-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This is only debugged in the emulator.  

define open abstract primary class <t-list> (<deque>)
  slot first-pair :: <list>, init-value: #(), init-keyword: first-pair:;
  slot last-pair :: <list>, init-value: #(), init-keyword: last-pair:;
end class;

define sealed concrete class <actual-t-list> (<t-list>)
end class;

// could speed this up using unsafe push-last
define method make 
    (class == <t-list>, 
     #rest initialization-arguments,
     #key size: the-real-size :: <integer> = 0, 
          fill: the-real-fill :: <object> = #f) 
 => (result :: <actual-t-list>);
  let the-t-list = 
    apply(make, <actual-t-list>, size: 0, fill: #f,
	  initialization-arguments);
  for (counter = the-real-size then counter - 1,
       while: counter > 0)
    push-last(the-t-list, the-real-fill);
  end for;
  the-t-list
end method;

define method print-object (the-t-list :: <t-list>, the-stream :: <stream>)
 => ();
  let (initial-state, limit, next-state, finished?, current-key, 
       current-element,current-element-setter) 
    = forward-iteration-protocol(the-t-list);
  if (~empty?(the-t-list))
    format(the-stream, "(<t-list> of (");
    print(current-element(the-t-list, initial-state), the-stream);
    for (state = next-state(the-t-list, initial-state) 
	   then next-state(the-t-list, state),
	 until: finished?(the-t-list, state, limit))
      format(the-stream, ", ");
      print(current-element(the-t-list, state), the-stream);
    end for;
    format(the-stream, "))");
  else
    format(the-stream, "(empty <t-list>)");
  end if;
end method;

define method empty? (the-t-list :: <t-list>) => (result :: <boolean>);
    empty?(the-t-list.first-pair)
end method;

define method push 
    (t-list :: <t-list>, new-element :: <object>) => (result :: <object>);
  if (empty?(t-list))
    t-list.first-pair := t-list.last-pair := pair(new-element, #());
  else
    t-list.first-pair := pair(new-element, t-list.first-pair);
  end if;
  new-element
end method;
    
define method push-last 
    (t-list :: <t-list>, new-element :: <object>) => (result :: <object>);
  if (empty?(t-list))
    t-list.first-pair := t-list.last-pair := pair(new-element, #());
  else
    let new-pair = pair(new-element, t-list.last-pair.tail);
    t-list.last-pair.tail := new-pair;
    t-list.last-pair := new-pair;
  end if;
  new-element
end method;

define method pop (t-list :: <t-list>) => (result :: <object>);
  if (empty?(t-list))
    error("pop from an empty <t-list>");
  else
    let result = t-list.first-pair.head;
    if (t-list.first-pair == t-list.last-pair)
      t-list.first-pair := t-list.last-pair := #();
    else
      t-list.first-pair := tail(t-list.first-pair);
    end if;
    result
  end if
end method;

// This isn't efficient as a doubly linked list deque would be.  If you
// really need pop-last to be fast used doubly linked lists and pay the
// extra overhead of keeping backward links. 
define method pop-last (t-list :: <t-list>) => (result :: <object>);
  if (empty?(t-list))
    error("pop-last from an empty <t-list>");
  else
    let result = t-list.last-pair.head;
    if (t-list.first-pair == t-list.last-pair)
      t-list.first-pair := t-list.last-pair := #();
    else
      for (next-to-last-pair = t-list.first-pair 
	     then next-to-last-pair.tail,
	   until: next-to-last-pair.tail == t-list.last-pair)
	finally
	  next-to-last-pair.tail := #();
	  t-list.last-pair := next-to-last-pair;
      end for;
    end if;
    result
  end if
end method;

// could speed this up using unsafe push-last
define method shallow-copy (the-t-list :: <t-list>)
 => (new-t-list :: <t-list>);
  let new-t-list = make(<actual-t-list>);
  for (thing in the-t-list)
    push-last(new-t-list, thing);
  end for;
  new-t-list
end method;

// Splice the two lists together, modifying the first argument.
define method append! (first-list :: <t-list>, second-list :: <t-list>)
 => (modified-first-list :: <t-list>);
  if ( ~ empty?(second-list))
    if (empty?(first-list))
      first-list.first-pair := second-list.first-pair;
      first-list.last-pair := second-list.last-pair;
    else
      first-list.last-pair.tail := second-list.first-pair;
      first-list.last-pair := second-list.last-pair;
    end if;
  end if;
  first-list
end method;

// Not concatenate. This method modifies all but the last list in
// more-sequences.
define method concatenate! (the-t-list :: <t-list>, #rest more-sequences)
 => (result-t-list :: <t-list>);
  let result-t-list = the-t-list;
  for (the-sequence in more-sequences)
    result-t-list := append!(result-t-list, as(<t-list>, the-sequence));
  end for;
  result-t-list
end method;

/* OBSOLETE:
 * The function call find-element(test-function, the-t-list, default: foo)
 * should be replaced by find-element(the-t-list, test-function, failure: foo)
 * and find-element(test-function, the-t-list)
 * replaced by find-element(the-t-list, test-function)

define method find-element
    (test-function :: <function>, the-t-list :: <t-list>,
     #key default = #f)
 => (result :: <object>);
  let (initial-state, limit, next-state, finished?, current-key, 
       current-element) 
    = forward-iteration-protocol(the-t-list);
  let found? = #f;
  let result = default;
  for (state = initial-state then next-state(the-t-list, state),
       until: found? | finished?(the-t-list, state, limit))
    found? := test-function(current-element(the-t-list, state));
    if (found?) result := current-element(the-t-list, state) end if;
  end for;
  result
end method;
*/

define method as (class == <t-list>, t-list :: <t-list>)
 => (result :: <t-list>);
  t-list
end method;

define method as (class == <t-list>, collection :: <collection>)
 => (result :: <t-list>);
  let result = make(<t-list>);
  for (thing in collection)
    push-last(result, thing);
  end for;
  result
end method;

define method as (class == <list>, t-list :: <t-list>)
 => (result :: <list>);
  t-list.first-pair
end method;

define method as (class == <t-list>, the-list :: <list>)
 => (result :: <t-list>);
  for (the-last-pair = the-list
	 then the-last-pair.tail,
       until: the-last-pair.tail == #())
  finally
    make(<t-list>, first-pair: the-list, last-pair: the-last-pair)
  end for
end method;

define method size (t :: <t-list>) => (result :: <integer>);
  size(t.first-pair)
end method;

define constant $unsupplied-default = pair(#"unsupplied", #"unsupplied");

// Methods on element and element-setter for 0, 1, 2 so that first, second
// and third don't use the default method for element.  (First, second and
// third aren't generic functions.)
define method element 
    (t :: <t-list>, index == 0, #key default = $unsupplied-default)
 => (result :: <object>);
  if (~empty?(t.first-pair))
    t.first-pair.head
  elseif (default == $unsupplied-default)
    error("element: <t-list> has no first element");
  else
    default
  end if
end method;

define method element 
    (t :: <t-list>, index == 1, #key default = $unsupplied-default)
 => (result :: <object>);
  if (~empty?(t.first-pair) & ~empty?(t.first-pair.tail))
    t.first-pair.tail.head
  elseif (default == $unsupplied-default)
    error("element: <t-list> has no second element");
  else
    default
  end if
end method;

define method element 
    (t :: <t-list>, index == 2, #key default = $unsupplied-default)
 => (result :: <object>);
  if (~empty?(t.first-pair) 
	& ~empty?(t.first-pair.tail) 
	& ~empty?(t.first-pair.tail.tail))
    t.first-pair.tail.tail.head
  elseif (default == $unsupplied-default)
    error("element: <t-list> has no third element");
  else
    default
  end if
end method;

define method element-setter 
    (new-value :: <object>, t :: <t-list>, index == 0)
 => (result :: <object>);
  if (~empty?(t.first-pair))
    t.first-pair.head := new-value;
  else 
    error("element-setter: <t-list> has no first element");
  end if;
  new-value
end method;

define method element-setter 
    (new-value :: <object>, t :: <t-list>, index == 1)
 => (result :: <object>);
  if (~empty?(t.first-pair) & ~empty?(t.first-pair.tail))
    t.first-pair.tail.head := new-value;
  else
    error("element-setter: <t-list> has no second element");
  end if;
  new-value
end method;

define method element-setter 
    (new-value :: <object>, t :: <t-list>, index == 2)
 => (result :: <object>);
  if (~empty?(t.first-pair)
	& ~empty?(t.first-pair.tail) 
	& ~empty?(t.first-pair.tail.tail))
    t.first-pair.tail.tail.head := new-value;
  else
    error("element-setter: <t-list> has no third element");
  end if;
  new-value
end method;

define method last (t :: <t-list>, #key default = $unsupplied-default) 
 => (result :: <object>);
  if (~empty?(t.last-pair))
    t.last-pair.head
  elseif (default == $unsupplied-default)
    error("last: <t-list> has no last element");
  else
    default
  end
end method;

define method last-setter (new-value :: <object>, t :: <t-list>) 
 => (result :: <object>);
  if (~empty?(t.last-pair))
    t.last-pair.head := new-value;
  else
    error("last-setter: <t-list> has no last element");
  end if;
  new-value
end method;

define method element
    (the-t-list :: <t-list>, index :: <integer>, 
     #key default = $unsupplied-default)
 => (result :: <object>);
  let (initial-state, limit, next-state, finished?, current-key, 
       current-element,current-element-setter) 
    = forward-iteration-protocol(the-t-list);
  for (state = initial-state then next-state(the-t-list, state),
       counter = index then counter - 1,
       until: (counter <= 0) | finished?(the-t-list, state, limit))
  finally
    if (counter == 0)
      current-element(the-t-list, state)
    elseif (default == $unsupplied-default)
      error("element: <t-list> has no %dth element", index);
    else
      default
    end if
  end for
end method;

define method element-setter
    (new-value :: <object>, the-t-list :: <t-list>, index :: <integer>)
 => (result :: <object>);
  let (initial-state, limit, next-state, finished?, current-key, 
       current-element, current-element-setter) 
    = forward-iteration-protocol(the-t-list);
  for (state = initial-state then next-state(the-t-list, state),
       counter = index then counter - 1,
       until: (counter <= 0) | finished?(the-t-list, state, limit))
  finally
    if (counter == 0)
      current-element-setter(new-value, the-t-list, state)
    else
      error("element-setter: <t-list> has no %dth element", index);      
    end if;
  end for;
  new-value
end method;

    
define method forward-iteration-protocol (t-list :: <t-list>)
  => (initial-state :: <object>, limit :: <object>,
      next-state :: <function>, finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>)
  let key-counter = -1;
  values(t-list.first-pair, // initial-state
	 #(), // limit
	 method  // next-state,
	     (the-t-list :: <t-list>, current-state :: <list>)
	  => (result :: <list>);
	     current-state.tail end method, 
	 method  // finished-state?,
	     (the-t-list :: <t-list>, current-state :: <list>, limit :: <list>)
	  => (result :: <boolean>); 
	     current-state == limit end method,
	 method  // current-key 
	     (the-t-list :: <t-list>, current-pair :: <list>)
	  => (result :: <integer>);
	     key-counter := key-counter + 1; key-counter end method,
	 method  // current-element
	     (the-t-list :: <t-list>, current-state :: <list>) 
	  => (result :: <object>);
	     current-state.head end method,
	 method  // current-element-setter
	     (value :: <object>, s :: <t-list>, current-state :: <list>)
	  => (result :: <object>);
	     current-state.head := value end method,
	 method  // copy-state
	     (s :: <t-list>, current-state :: <list>)
	     pair(current-state.head, current-state.tail) end method)
end method;
