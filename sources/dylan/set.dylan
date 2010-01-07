module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <set> (<mutable-explicit-key-collection>)
end class <set>;

define sealed inline method make (class == <set>, #rest all-keys, #key) 
    => (object)
  apply(make, <object-set>, all-keys)
end method make;


////
//// <OBJECT-SET>
////

define constant $empty-table-set-elements = make(<object-table>, size: 0);

define sealed class <object-set> (<set>)
  slot set-elements :: <object-table>, init-value: $empty-table-set-elements;
end class <object-set>;

define sealed domain make (singleton(<object-set>));

define sealed method initialize (set :: <object-set>, #key size = unsupplied())
  next-method();
  set.set-elements := 
    if (size.supplied?) 
      make(<object-table>, size: size, values?: #f)
    else 
      make(<object-table>, values?: #f)
    end;
end method initialize;


//
// SIZE
// 

define sealed inline method size (set :: <object-set>) => (size :: <integer>)
  set.set-elements.size
end method size;


// This is a simplified version of gethash.  Read the comments in table.dylan
// for further explanation of how this works.

define sealed method member? (object, set :: <object-set>, #key test) 
    => (bool :: <boolean>)
  let table :: <object-table> = set.set-elements;
  let tv = table-vector(table);
  let token = rehash-token(tv);
  // Ensure token fetched before computing hash code.
  sequence-point();
  // Don't need hash state here.
  let id = hash-for-lookup(tv, object);
  let (index, table-key) = search(tv, object, id);
  if (table-key ~== $table-entry-empty)
    sequence-point();
    if (rehash-token-valid?(tv, token))
      #t
    else
      // Rehash has been initiated.
//      rehash-table(table, tv, #f);      // Why do this?
      with-table-vector-locked (tv) end;  // Just wait on lock instead.
      member?(object, set);	// try again
    end if;
  elseif (needs-rehash?(tv, token))
    rehash-table(table, tv, #f);
    member?(object, set);	// try again
  else
    #f
  end if;
end method member?;


// This is a simplified version of puthash.  Read the comments in table.dylan
// for further explanation of how this works.

define sealed method add! (set :: <object-set>, key) => (set :: <object-set>)
  let table :: <object-table> = set.set-elements;
  let tv = table-vector(table);
  let token = rehash-token(tv);
  // Ensure that the above occurs before computing the hash code.
  sequence-point();
  let (id, hstate) = hash(tv, key);
  let (index, fkey) = search(tv, key, id);
  let success? =
    if (fkey ~== $table-entry-empty)
      #t
    else
      with-table-vector-locked (tv)
        merge-hash-state!(hash-state(tv), hstate);
        let keys = entry-keys(tv);
        if (entry-key(keys, index) == $table-entry-empty
             & ~full?(tv)
             & rehash-token-valid?(tv, token)
             & ~is-stale?(tv.hash-state))
          additions(tv) := additions(tv) + 1;
          entry-key(keys, index) := key;
          #t;				// success flag
        else
          #f;				// failure flag
        end if;
      end with-table-vector-locked;
    end if;

  if (success?)
    set
  else
    // Store failed for some reason.  Rehash if needed and retry.
    if (needs-rehash?(tv, token))
      rehash-table(table, tv, full?(tv));
    elseif (full?(tv))
      rehash-table(table, tv, #t);
    end;
    add!(set, key);	// try again
  end if;
end method add!;

define sealed method remove! (set :: <object-set>, object, #key test, count) 
    => (set :: <object-set>)
  remove-key!(set.set-elements, object);
  set
end method remove!;

define sealed method remove-all-keys! (set :: <object-set>)
  remove-all-keys!(set.set-elements)
end method;

define sealed method element (set :: <object-set>, key, #key default = unsupplied())
    => (key-or-default-or-error :: <object>)
  if (member?(key, set)) 
    key
  elseif (supplied?(default)) 
    default
  else 
    error(make(<not-found-error>, 
          format-string: "No such element %= in %=", 
          format-arguments: list(key, set)))
  end
end method element;

// element-setter for sets, given that a set's key-sequence is a vector
// of its elements.  element-setter(o, s, o) adds o if not already present.
// element-setter(o, s, m) replaces m by o if m already present, otherwise
// adds o.  (yuk - who decided on this spec?)

define sealed method element-setter (object, set :: <object-set>, key) => (object)
  if (key == object)
    add!(set, object)
  else
    if (member?(key, set))
      remove!(set, key)
    end if;
    add!(set, object)
  end if;
  object
end method element-setter;


define sealed method key-test (set :: <object-set>)
  => test :: <function>;
  \==
end method key-test;


////
//// iteration protocol
////

define inline function set-finished-state?
     (set, state :: <iteration-state>, limit)
  => finished? :: <boolean>;
  // ignore(set, limit);
  finished-state-index?(state-index(state))
end;


define sealed inline method forward-iteration-protocol (set :: <object-set>)
  => (initial-state		:: <iteration-state>,
      limit			:: <object>,
      next-state		:: <function>,
      finished-state?		:: <function>,
      current-key		:: <function>,
      current-element		:: <function>,
      current-element-setter	:: <function>,
      copy-state		:: <function>);
  let (initial, limit, next, finished, current-key, current-element,
       current-element-setter, copy-state) = 
    set.set-elements.forward-iteration-protocol;

  values(initial, #f, next, set-finished-state?, current-key, current-key,
         method (value, table, state :: <iteration-state>)
           error(make(<immutable-error>,
                      format-string: "Cannot update current element of a set during iteration."))
         end,
         copy-state)
end method forward-iteration-protocol;

///
/// LIMITED TABLES
/// 

define method limited-set
     (of :: <type>, size :: false-or(<integer>)) => (type :: <limited-set-type>)
  make(<limited-set-type>,
       class:          <set>,
       element-type:   of,
       concrete-class: <object-set>,
       size:           size);
end method;

/// TODO: COULD BE EXPENSIVE UNLESS TYPES ARE CACHED

define inline method type-for-copy (x :: <set>) 
    => (type :: <type>)
  let elt-type = element-type(x);
  if (elt-type == <object>)
    object-class(x)
  else 
    limited-set(element-type(x), #f)
  end if
end method type-for-copy;
