Module:    union-collection
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <union-collection> (<collection>)
  // @@@@ should be required-init-keyword
  slot members :: <collection>, init-value: #(), init-keyword: members:;
end class <union-collection>;

define class <cached-union-collection> (<union-collection>)
end class <cached-union-collection>;

define class <id?-cached-union-collection> (<cached-union-collection>)
  slot cache :: <table>,
    init-function: method ()
                     make(<table>)
                   end method;
end class <id?-cached-union-collection>;

define class <=-cached-union-collection> (<cached-union-collection>)
  slot cache :: <table>,
    init-function: method ()
                     make(<table>, test: \=)
                   end method;
end class <=-cached-union-collection>;

define method make (class == <cached-union-collection>, #rest all-keys)
  apply(make, <id?-cached-union-collection>, all-keys)
end method make;

define method remove-key! (collection :: <union-collection>, key)
  block (return)
    for (member in collection.members)
      let value = element(member, key, default: not-found());
      unless (value.not-found?)
        remove-key!(member, key);
        return()
      end unless;
    end for
  end block;
  collection
end method remove-key!;

define method element
    (collection :: <union-collection>, key, #key default = unsupplied())
  block (return)
    for (member in collection.members)
      let value = element(member, key, default: not-found());
      unless (value.not-found?)
        return(value)
      end unless;
    end for;
    if (default.unsupplied?)
      error("Element outside of range: %= %=", collection, key)
    else
      default
    end if
  end block
end method element;

define method element-setter (new-value, collection :: <union-collection>, key)
  block (return)
    for (member in collection.members)
      if (element(member, key, default: #f))
        member[key] := new-value;
        return()
      end if;
    end for;
    current-element(collection.members, collection.members.initial-state)[key]
      := new-value
  end block;
  new-value
end method element-setter;

//// ITERATION PROTOCOL

// <iterator>:  encapsulate full iteration state

define class <iterator> (<object>)   // TODO: make all but state setter: #f
  slot it-collection :: <collection>, required-init-keyword: collection:;
  slot it-state :: <object>, required-init-keyword: state:;
  slot it-limit :: <object>, required-init-keyword: limit:;
  slot it-next-state :: <function>, required-init-keyword: next-state:;
  slot it-finished-state? :: <function>,
    required-init-keyword: finished-state?:;
  slot it-current-key :: <function>, required-init-keyword: current-key:;
  slot it-current-element :: <function>,
    setter: set-it-current-keyword!,  // HACK
    required-init-keyword: current-element:;
  slot it-current-element-setter :: <function>,
    required-init-keyword: current-element-setter:;
  slot it-copy-state :: <function>, required-init-keyword: copy-state:;
end class <iterator>;

define constant <iterator-or-false> = union(<iterator>, singleton(#f));

define method initial-iterator (collection :: <collection>)
  let (initial-state, limit, next-state, finished-state?,
       current-key, current-element, current-element-setter, copy-state)
    = forward-iteration-protocol(collection);
  make(<iterator>,
       collection: collection,
       state: initial-state,
       limit: limit,
       next-state: next-state,
       finished-state?: finished-state?,
       current-key: current-key,
       current-element: current-element,
       current-element-setter: current-element-setter,
       copy-state: copy-state)
end method initial-iterator;

define method iterator-finished? (iterator :: <iterator>) => (_ :: <boolean>);
  iterator.it-finished-state?(iterator.it-collection,
			      iterator.it-state,
			      iterator.it-limit)
end method iterator-finished?;

define method iterator-next-state! (iterator :: <iterator>)
  iterator.it-state
    := iterator.it-next-state(iterator.it-collection, iterator.it-state)
end method iterator-next-state!;

define method iterator-current-element (iterator :: <iterator>)
  iterator.it-current-element(iterator.it-collection, iterator.it-state)
end method iterator-current-element;

define method iterator-current-element-setter
    (new-value, iterator :: <iterator>)
  iterator.it-current-element-setter
    (new-value, iterator.it-collection, iterator.it-state)
end method iterator-current-element-setter;

define method iterator-current-key (iterator :: <iterator>)
  iterator.it-current-key(iterator.it-collection, iterator.it-state)
end method iterator-current-key;

define method copy-iterator (iterator :: <iterator>)
  make(<iterator>,
       collection: iterator.it-collection,
       state:
	 iterator.it-copy-state(iterator.it-collection, iterator.it-state),
       limit: iterator.it-limit,
       next-state: iterator.it-next-state,
       finished-state?: iterator.it-finished-state?,
       current-key: iterator.it-current-key,
       current-element: iterator.it-current-element,
       current-element-setter: iterator.it-current-element-setter,
       copy-state: iterator.it-copy-state)
end method copy-iterator;

// <union-state>

define class <union-state> (<object>)
  slot member-iterator :: <iterator>, required-init-keyword: member-iterator:;
  slot subiterator :: <iterator-or-false>, required-init-keyword: subiterator:;
end class <union-state>;

define method next-subiterator (super :: <iterator>)
 => (_ :: <iterator-or-false>);
  if (iterator-finished?(super))
    #f
  else
    let subiterator = initial-iterator(iterator-current-element(super));
    iterator-next-state!(super);
    if (iterator-finished?(subiterator))
      next-subiterator(super)
    else
      subiterator
    end if;
  end if;
end method next-subiterator;

// iteration methods

define constant union-collection-initial-state
  = method (union :: <union-collection>) => (_ :: <union-state>)
      let member-iterator = initial-iterator(union.members);
      make(<union-state>,
	   member-iterator: member-iterator,
	   subiterator: next-subiterator(member-iterator))
    end method;

define constant union-collection-finished-state?
  = method (union :: <union-collection>, state :: <union-state>, limit)
      state.subiterator == #f
    end method;

define constant union-collection-next-state
  = method (union :: <union-collection>, state :: <union-state>)
      iterator-next-state!(state.subiterator);
      if (iterator-finished?(state.subiterator))
	state.subiterator := next-subiterator(state.member-iterator);
      end if;
      state
    end method;

define constant union-collection-current-element
  = method (union :: <union-collection>, state :: <union-state>)
      iterator-current-element(state.subiterator)
    end method;

define constant union-collection-current-element-setter
  = method (new-value, union :: <union-collection>, state :: <union-state>)
      iterator-current-element(state.subiterator) := new-value
    end method;

define constant union-collection-current-key
  = method (union :: <union-collection>, state :: <union-state>)
      iterator-current-key(state.subiterator)
    end method;

define constant union-collection-copy-state
  = method (union :: <union-collection>, state :: <union-state>)
      make(<union-state>,
	   member-iterator: copy-iterator(state.member-iterator),
	   subiterator: state.subiterator & copy-iterator(state.subiterator))
    end method;

define method forward-iteration-protocol (collection :: <union-collection>)
  values(collection.union-collection-initial-state,
	 #f,
	 union-collection-next-state,
	 union-collection-finished-state?,
	 union-collection-current-key,
	 union-collection-current-element,
	 union-collection-current-element-setter,
	 union-collection-copy-state)
end method forward-iteration-protocol;

//// CACHED

define method element
    (collection :: <cached-union-collection>, key, #key default = unsupplied())
  let cached-value = element(collection.cache, key, default: not-found());
  if (cached-value.not-found?)
    collection.cache[key] := next-method()
  else
    cached-value
  end if
end method element;

define method element-setter
    (new-value, collection :: <cached-union-collection>, key)
  next-method();
  collection.cache[key] := new-value;
  new-value
end method element-setter;

define method remove-key! (collection :: <cached-union-collection>, key)
  remove-key!(collection.cache, key);
  next-method()
end method remove-key!;

define method flush (collection :: <cached-union-collection>)
  clear!(collection.cache);
  values()
end method flush;

define constant cached-union-collection-current-element-setter
  = method
        (new-value, union :: <cached-union-collection>, state :: <union-state>)
      union.cache[iterator-current-key(state.subiterator)]
        := iterator-current-element(state.subiterator)
        := new-value
    end method;

define method forward-iteration-protocol (union :: <cached-union-collection>)
  values(union.union-collection-initial-state,
	 #f,
	 union-collection-next-state,
	 union-collection-finished-state?,
	 union-collection-current-key,
	 union-collection-current-element,
	 cached-union-collection-current-element-setter,
	 union-collection-copy-state)
end method forward-iteration-protocol;
