Module:    internal
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <limited-list> (<mutable-sequence>) end;

define open generic limited-list 
    (of :: <type>) => (type :: subclass(<limited-list>));

define open generic limited-list-first
    (list :: <limited-list>) => (object);

define open generic limited-list-first-setter
    (object, list :: <limited-list>) => (object);

define open generic limited-list-rest
    (list :: <limited-list>) => (list :: <limited-list>);

define open generic limited-list-rest-setter
    (list :: <limited-list>, list :: <limited-list>)
 => (list :: <limited-list>);

define open generic prepend 
    (object, list :: <limited-list>) => (new-list :: <limited-list>);

//// Convenient access.

define inline sealed method list-first 
    (list :: <limited-list>, #key default) => (first)
  // We just don't support default...
  limited-list-first(list)
end method;

define inline sealed method list-first-setter
    (value :: <object>, list :: <limited-list>, #key default) => (first)
  limited-list-first(list) := value
end method;

define inline sealed method list-rest 
    (list :: <limited-list>) => (rest :: <limited-list>)
  limited-list-rest(list)
end method;

define inline sealed method list-rest-setter 
    (new-rest :: <limited-list>, list :: <limited-list>) => (rest :: <limited-list>)
  limited-list-rest(list) := new-rest
end method;

//// Shared implementation.

define open primary class <non-empty-limited-list> (<limited-list>)
  slot %limited-list-rest :: <limited-list>,
    required-init-keyword: rest:;
end class;

define open primary class <empty-limited-list> (<limited-list>)
end class;

//
// AS
// 
 
define method as 
    (class :: subclass(<limited-list>), collection :: <collection>) 
 => (l :: <limited-list>)
  let result :: <limited-list> = empty(class);
  for (item in collection)
    result := prepend(item, result);
  end for;
  reverse!(result)
end method as;

define sealed method as 
    (class :: subclass(<limited-list>), v :: <simple-object-vector>) 
 => (l :: <limited-list>)
  for (result = empty(class) then prepend(vector-element(v, index), result),
       index :: <integer> from v.size - 1 to 0 by -1)
  finally
    result
  end
end;

define sealed method as 
    (class :: subclass(<limited-list>), v :: <object-deque>) 
 => (l :: <limited-list>)
  let rep = v.representation;
  for (result = empty(class) 
         then prepend(island-deque-element(rep, index), result),
       index :: <integer> from rep.last-index to rep.first-index by -1)
  finally
    result
  end
end;

//
// SIZE
// 

define sealed method size 
    (list :: <limited-list>) => (s :: false-or(<integer>))
  let nil = empty(object-class(list));
  if (list == nil) 
    0
  else
    let list :: <non-empty-limited-list> = list;
    iterate sum (count :: <integer> = 0, 
                 fast :: <non-empty-limited-list> = list, 
                 slow :: <non-empty-limited-list> = list)
      let fast-tail = fast.%limited-list-rest;
      if (fast-tail == nil)
        count + 1
      elseif (fast == slow & count > 0)
        #f
      else
        let fast-tail :: <non-empty-limited-list> = fast-tail;
	let fast-tail-tail = fast-tail.%limited-list-rest;
        case	
	  fast-tail-tail == nil 
            => count + 2;
	  otherwise
            => let fast-tail-tail :: <non-empty-limited-list> 
                 = fast-tail-tail;
	       let slowtail :: <non-empty-limited-list>
                 = %limited-list-rest(slow);
  	       sum(count + 2, fast-tail-tail, slowtail);
        end
      end if
    end iterate
  end
end method size;

//
// ELEMENT
// 

define sealed method element
    (lst :: <limited-list>, key :: <integer>, #key default = unsupplied())
        => (o :: <object>)
  if (key < 0)
    if (unsupplied?(default)) element-range-error(lst, key) else default end
  else
    iterate loop (l :: <limited-list> = lst, i :: <integer> = 0)
      if (~empty?(l))
	let l :: <non-empty-limited-list> = l;
	if (i == key) 
          limited-list-first(l) 
        else 
          loop(%limited-list-rest(l), i + 1) 
        end;
      elseif (unsupplied?(default))
	element-range-error(lst, key)
      else
	default
      end if
    end iterate
  end if
end method element;      

//
// ELEMENT-NO-BOUNDS-CHECK
// 

define sealed method element-no-bounds-check
    (lst :: <limited-list>, key :: <integer>, #key default)
 => (result :: <object>)
  for (k :: <integer> from 0 below key,
       remain :: <limited-list> = lst then remain.%limited-list-rest)
  finally 
    remain.limited-list-first
  end for
end method element-no-bounds-check;      

//
// ELEMENT-SETTER
// 

define sealed method element-setter
    (new-value, lst :: <limited-list>, key :: <integer>)
 => (new-value :: <object>)
  iterate loop (l :: <limited-list> = lst, i :: <integer> = 0)
    if (~empty?(l))
      let l :: <non-empty-limited-list> = l;
      if (i == key)
        limited-list-first(l) := new-value
      else
        loop(%limited-list-rest(l), i + 1) 
      end;
    else
      element-range-error(lst, key)
    end if
  end iterate
end method element-setter;      

//
// ELEMENT-NO-BOUNDS-CHECK-SETTER
// 
 
define sealed method element-no-bounds-check-setter
    (new-value, lst :: <limited-list>, key :: <integer>) => new-value;
  for (k :: <integer> from 0 below key,
       remain :: <limited-list> = lst then remain.%limited-list-rest)
  finally
    remain.limited-list-first := new-value
  end for
end method element-no-bounds-check-setter;      

define inline sealed method first
    (list :: <limited-list>, #key default) => (first)
  if (instance?(list, <non-empty-limited-list>))
    limited-list-first(list)
  else
    element-range-error(list, 0);
  end;
end method;

define inline sealed method rest
    (list :: <limited-list>) => (rest :: <limited-list>)
  if (instance?(list, <non-empty-limited-list>))
    limited-list-rest(list)
  else
    error("Cannot take rest of the empty limited list %=", list);
  end;
end method;

define inline sealed method rest
    (sequence :: <list>) => (sequence-tail :: <list>)
  sequence.tail;
end method rest;

//// Iteration.

define inline function limited-list-next-state
    (collection :: <limited-list>, state :: <limited-list>)
 => (l :: <limited-list>)
  limited-list-rest(state)
end function;

define inline function limited-list-finished-state?
    (collection :: <limited-list>, state :: <limited-list>, limit)
 => (result :: <boolean>)
  empty?(state)
end function;

define inline function limited-list-copy-state
    (collection :: <limited-list>, state :: <limited-list>)
 => (l :: <limited-list>)
  state
end function;

define inline function limited-list-current-key
    (collection :: <limited-list>, state :: <limited-list>) 
 => (result :: <integer>)
  iterate search (l :: <limited-list> = collection, k :: <integer> = 0)
    if (l == state)
      k
    else
      search(limited-list-rest(l), k + 1)
    end if
  end iterate
end function;

define inline function limited-list-current-element
    (collection :: <limited-list>, state :: <limited-list>) => (result)
  limited-list-first(state)
end function;

define inline function limited-list-current-element-setter
    (new-value, collection :: <limited-list>, state :: <limited-list>)
 => (result)
  limited-list-first(state) := new-value
end function;

define inline method forward-iteration-protocol
    (list :: <limited-list>) 
 => (initial-state :: <limited-list>, limit :: <limited-list>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(list,
	 empty(object-class(list)),
	 limited-list-next-state,
	 limited-list-finished-state?,
	 limited-list-current-key,
	 limited-list-current-element,
	 limited-list-current-element-setter,
	 limited-list-copy-state)
end method;

define macro limited-list-definer
  { define limited-list ?class:name of ?element:name ?opt-fill }
    => { define sealed class ?class (<limited-list>) end;

         define sealed domain make (subclass(?class));
         define sealed domain initialize (?class);

         define sideways sealed inline method limited-list
             (type == ?element) => (list-class == ?class)
           ?class
         end method;

         define sealed method type-for-copy 
             (list :: ?class) => (class :: singleton(?class))
           ?class
         end;

         define sealed method make 
             (class == ?class, 
                #key size :: <integer> = 0, fill :: ?element = ?opt-fill) 
          => (list :: ?class)
           for (i :: <integer> from 0 below size,
                result :: ?class = empty(?class) then prepend(fill, result))
           finally
             result
           end;
         end method;

         define sealed /* made-inline */ class "non-empty-" ## ?class
             (<non-empty-limited-list>, ?class)
           sealed slot limited-list-first :: ?element,
             required-init-keyword: first:;
         end class;

         define sealed inline method limited-list-rest
             (l :: "non-empty-" ## ?class) => (rest :: ?class)
           // Eek!!!
           %guarantee-type(l.%limited-list-rest, ?class)
           // l.limited-list-rest
         end;

         define sealed inline method limited-list-rest-setter
             (rest :: ?class, l :: "non-empty-" ## ?class) => (rest :: ?class)
           %limited-list-rest(l) := rest
         end;

         define sealed domain limited-list-first-setter (<object>, ?class);
         define sealed domain limited-list-rest-setter (<object>, ?class);

         define sealed class "empty-" ## ?class
             (<empty-limited-list>, ?class)
         end;

         define constant "$empty-" ## ?class :: "empty-" ## ?class
           = make("empty-" ## ?class);

         define inline sealed method prepend 
             (object :: ?element, list :: ?class) 
          => (list :: "non-empty-" ## ?class)
           make("non-empty-" ## ?class, first: object, rest: list);
         end method;

         define sealed domain prepend (<object>, ?class);

         define inline sealed method empty
             (class :: subclass(?class)) => (empty :: "empty-" ## ?class)
           "$empty-" ## ?class;
         end method; 

         define inline sealed method empty?
             (list :: ?class) => (well? :: <boolean>)
           list == "$empty-" ## ?class;
         end method; 

         define inline function ?class ## "-next-state"
             (collection :: ?class, state :: ?class)
          => (l :: ?class)
           let state :: "non-empty-" ## ?class 
             = %guarantee-type(state, "non-empty-" ## ?class);
           limited-list-rest(state)
         end function;

         define inline function ?class ## "-current-element"
             (collection :: ?class, state :: ?class)
          => (e :: ?element)
           let state :: "non-empty-" ## ?class 
             = %guarantee-type(state, "non-empty-" ## ?class);
           limited-list-first(state)
         end function;

         define inline function ?class ## "-current-element-setter"
             (e :: ?element, collection :: ?class, state :: ?class)
          => (e :: ?element)
           let state :: "non-empty-" ## ?class 
             = %guarantee-type(state, "non-empty-" ## ?class);
           limited-list-first(state) := e
         end function;

         define inline sealed method forward-iteration-protocol
             (list :: ?class) 
          => (initial-state :: ?class, limit :: ?class,
              next-state :: <function>, finished-state? :: <function>,
              current-key :: <function>,
              current-element :: <function>, 
	      current-element-setter :: <function>,
	      copy-state :: <function>)
           values(list,
		  empty(?class),
                  ?class ## "-next-state",
		  limited-list-finished-state?,
		  limited-list-current-key,
		  ?class ## "-current-element",
		  ?class ## "-current-element-setter",
		  limited-list-copy-state)
         end method; }
  { define limited-list "<" ## ?element:name ## ">" ?opt-fill }
    => { define limited-list "<simple-" ## ?element ## "-list>" 
           of "<" ## ?element ## ">" = ?opt-fill }
opt-fill:
  { } 
    => { #f }
  { = ?fill:expression }
    => { ?fill }
end macro;

//// Standard limited list classes.

define limited-list <object>;
define limited-list <class>;
define limited-list <method>;

define inline method limited-list 
    (type :: <type>) => (list-type :: subclass(<limited-list>))
  <simple-object-list>
end method;
