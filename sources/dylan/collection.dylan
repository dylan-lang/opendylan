Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// BOOTED: define ... class <collection> ... end;


// Useful Functional Objects extensions

// These constants are explicitly declared <pair> so that callers can
// trust that they're pointer objects. Their actually "pairness" should
// never be seen.

// since pairs aren't inlineable, we use a thunk which is.

define generic not-found-object ();
define inline constant not-found = method () not-found-object end;
define inline constant not-found? = method (x) x == not-found-object; end;
// define inline constant found? = method (x) ~(x == not-found-object); end;

define generic unsupplied-object ();
define inline constant unsupplied = method () unsupplied-object end;
define inline constant unsupplied? = method (x) x == unsupplied-object; end;
define inline constant supplied? = method (x) ~(x == unsupplied-object); end;


// Methods with only one collection argument are specialized in the
// appropriate file.  If a method has more than one collection argument
// then all the specializations are presented together.


////////////
// INTERFACE
////////////

// Functions on <collection>.
// Make these sealed generics so we can specialize if necessary.
// We need to ensure that the dispatching of any uses of these functions
// in the dispatch code is resolved at compile time.

define sealed generic do
    (fn :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => (false :: singleton(#f));

define sealed generic map
    (fn :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => (new-collection :: <collection>);

define constant <collection-type>
  = type-union(subclass(<collection>), <limited-collection-type>);

define constant <mutable-collection-type>
  = type-union(subclass(<mutable-collection>), <limited-mutable-collection-type>);

define sealed generic map-as
    (type :: <mutable-collection-type>, fn :: <function>,
     collection :: <collection>, #rest more-collections :: <collection>)
 => (new-collection :: <mutable-collection>);

define sealed generic map-into
    (mutable-collection :: <mutable-collection>, function :: <function>,
     collection :: <collection>, #rest more-collections :: <collection>)
 => (mutable-collection :: <mutable-collection>);

define sealed generic any?
    (test :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => value;

define sealed generic every?
    (test :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => (value :: <boolean>);


// Open generics on <collection>

define open generic element
  (collection :: <object-with-elements>, key, #key default) => object;

define open generic key-sequence
  (collection :: <collection>) => (keys :: <sequence>);

define open generic reduce
  (fn :: <function>, initial-value, collection :: <collection>) => object;

define open generic reduce1
  (fn :: <function>, collection :: <collection>) => object;

define open generic member?
  (value, collection :: <collection>, #key test) => boolean :: <boolean>;

define open generic find-key
    (collection :: <collection>, predicate :: <function>, #key skip, failure)
 => key;

define open generic key-test
  (collection :: <collection>) => (test :: <function>);

define open generic forward-iteration-protocol (collection :: <collection>)
  => (initial-state :: <object>, limit :: <object>,
      next-state :: <function>, finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>);

define open generic backward-iteration-protocol (collection :: <collection>)
  => (initial-state :: <object>, limit :: <object>,
      next-state :: <function>, finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>);


define open generic add!
  (coll :: <collection>, new-element) => (possibly-new-coll :: <collection>);

define open generic remove!
    (coll :: <collection>, value, #key test, count)
 => (possibly-new-coll :: <collection>);


// Access conditions
// TODO: need a better hierarchy than this.

define class <key-test-error> (<simple-error>) end;

define class <not-found-error> (<simple-error>) end;

// define class <key-not-found-error> (<not-found-error>) end;

// define class <value-not-found-error> (<not-found-error>) end;

define class <invalid-index-error> (<not-found-error>) end;

define class <subscript-out-of-bounds-error> (<invalid-index-error>) end;

define class <iteration-error> (<simple-error>) end;

define class <empty-collection-error> (<simple-error>) end;



/////////////////
// IMPLEMENTATION
/////////////////


//
// FORWARD-ITERATION-PROTOCOL
//

/// support for set-by

define inline function %curry-current-element-setter
    (collection, state, current-element-setter)
  method (new-value)
    current-element(collection, state) := new-value
  end method
end function;

//
// DO
//

define sealed method do
    (fn :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => (false :: singleton(#f));
  if (more-colls.empty?)
    for (e in coll) fn(e) end
  else
    for (v in apply(multiple-collection, coll, more-colls)) apply(fn, v) end
  end if
end method do;


// The compiler optimizes a call to "do" with a single collection to
// a call to do-one

define generic do-one
    (function :: <function>, collection ::  <collection>)
 => (false :: singleton(#f));

define inline method do-one
    (function :: <function>, collection :: <collection>)
 => (false :: singleton(#f))
  for (e in collection) function(e) end
end method do-one;

ignore(do-one);  // Silence "unused" warning.


//
// MAP
//

define generic map-as-one
    (type :: <mutable-collection-type>,
     function :: <function>, collection ::  <collection>)
 => (new-collection :: <mutable-collection>); // actually :: type

define sealed method map
    (fn :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => (new-collection :: <collection>);
  let tfc :: <mutable-collection-type> = type-for-copy(coll);
  if (empty?(more-colls))
    map-as-one(tfc, fn, coll)
  else
    map-as-one(tfc, method (v) apply(fn, v) end,
               apply(multiple-collection, coll, more-colls))
  end if
end method map;


// The compiler optimizes a call to map with a single collection to
// a call to map-as-one

//
// MAP-AS
//

define method map-as
    (type :: <mutable-collection-type>,
     function :: <function>,
     collection :: <collection>, #rest more-collections :: <collection>)
 => (result :: <mutable-collection>);  // actually :: type;
  if (empty?(more-collections))
    map-as-one(type, function, collection)
  else // Might be worth splitting out the singleton case eventually?
    map-as-one(type, method (v) apply(function, v) end,
      apply(multiple-collection, collection, more-collections))
  end if
end method map-as;


// First the two covering cases:

// We don't know how big the new collection will be in advance, and it may
// be expensive to precompute the size, so accumulate the result as we go
// along.

define method map-as-one
    (type :: <mutable-collection-type>,
     function :: <function>, collection ::  <explicit-key-collection>)
 => (new-collection :: <mutable-collection>); // actually :: type
  let acc = make(<keyed-accumulator>, key-test: collection.key-test);
  for (e keyed-by k in collection) acc[k] := function(e) end for;
  convert-accumulator-as(type, acc)
end method map-as-one;

define method map-as-one
    (type :: <mutable-collection-type>,
     function :: <function>, collection ::  <sequence>)
 => (new-collection :: <mutable-collection>); // actually :: type
  let acc = make(<sequence-accumulator>, key-test: collection.key-test);
  for (e in collection) add!(acc, function(e)) end for;
  convert-accumulator-as(type, acc)
end method map-as-one;

// In the list and deque case we don't need to know the size in advance,
// so no need for accumulators.

define inline method map-as-one
    (type == <list>, function :: <function>, collection ::  <sequence>)
 => (new-collection :: <list>);
  for (result = #() then pair(function(e), result),
       e in collection)
  finally
    reverse!(result)
  end for;
end method map-as-one;

// If we have an <array> or <vector> then we assume we can find the size
// efficiently.

define method map-as-one
    (type :: subclass(<array>),
     function :: <function>, collection ::  <array>)
 => (new-collection :: <array>); // actually :: type
  let collection-size = collection.size;
  if (collection-size = 0)
    make(type, size: 0)
  else
    let result =
      make(type, dimensions: collection.dimensions,
           fill: function(collection.first));
    without-bounds-checks
      for (i :: <integer> from 1 below collection-size)
        result[i] := function(collection[i])
      end for
    end without-bounds-checks;
    result
  end if
end method map-as-one;

define method map-as-one
    (type :: subclass(<vector>),
     function :: <function>, collection ::  <array>)
 => (new-collection :: <vector>); // actually :: type
  let collection-size = collection.size;
  if (collection-size = 0)
    make(type, size: 0)
  else
    let result =
      make(type, size: collection.size,
           fill: function(collection.first));
    without-bounds-checks
      for (i :: <integer> from 1 below collection-size)
        result[i] := function(collection[i])
      end for
    end without-bounds-checks;
    result
  end if
end method map-as-one;

/*
define inline copy-down-method map-as-one
    (type == <simple-object-vector>,
     function :: <function>, collection :: <simple-object-vector>) =>
    (new-collection :: <simple-object-vector>);
*/

define inline method map-as-one
    (type == <simple-object-vector>,
     function :: <function>, collection :: <simple-object-vector>)
 => (new-collection :: <vector>); // actually :: type
  let result = make(<simple-object-vector>, size: collection.size);
  without-bounds-checks
    for (i :: <integer> from 0 below collection.size)
      result[i] := function(collection[i])
    end for
  end without-bounds-checks;
  result
end method map-as-one;


// And now some tie-breakers...

define copy-down-method map-as-one (type :: subclass(<vector>),
                                    function :: <function>,
                                    collection ::  <explicit-key-collection>) =>
  (new-collection :: <vector>);

define copy-down-method map-as-one (type == <list>,
                                    function :: <function>,
                                    collection ::  <explicit-key-collection>) =>
  (new-collection :: <vector>);

/*
define method map-as-one
    (type :: subclass(<vector>),
     function :: <function>, collection ::  <explicit-key-collection>)
 => (new-collection :: <vector>); // actually :: type
  let acc = make(<keyed-accumulator>);
  for (e keyed-by k in collection) acc[k] := function(e) end for;
  convert-accumulator-as(type, acc)
end method map-as-one;

define method map-as-one
    (type == <list>,
     function :: <function>, collection ::  <explicit-key-collection>)
 => (new-collection :: <list>); // actually :: type
  let acc = make(<keyed-accumulator>);
  for (e keyed-by k in collection) acc[k] := function(e) end for;
  convert-accumulator-as(type, acc)
end method map-as-one;
*/


//
// MAP-INTO
//

define method map-into
    (target :: <mutable-collection>,
     function :: <function>,
     collection :: <collection>, #rest more-collections :: <collection>)
 => (target :: <mutable-collection>);
  if (empty?(more-collections))
    unless (target.key-test == collection.key-test)
      error(make(<key-test-error>,
                 format-string: "Collections %= and %= have different key tests",
                 format-arguments: list(target, collection)))
    end;
    if (instance?(target, <stretchy-collection>))
      map-into-stretchy-one(function, target, collection)
    else
      map-into-rigid-one(function, target, collection)
    end if
  else // Don't bother doing anything too smart for now.
    map-into(target, method (v) apply(function, v) end,
             apply(multiple-collection, collection, more-collections))
  end if
end method map-into;



// STRETCHY CASE

// QUESTION:
//   The target collection should not be involved in the alignment in the
//   stretchy case, but what if the target is a sequence and the source
//   contains keys that aren't natural numbers?  For now we just ignore
//   such keys, so that the target has a "stretchy" effect on alignment.

define method map-into-stretchy-one
    (fun :: <function>, target :: <mutable-collection>, coll :: <collection>)
 => (target :: <mutable-collection>);
  for (val keyed-by key in coll) target[key] := fun(val) end for;
  target
end;

define copy-down-method map-into-stretchy-one
    (fun :: <function>, target :: <mutable-collection>, coll :: <sequence>)
 => (target :: <mutable-collection>);

/*
define method map-into-stretchy-one
    (fun :: <function>, target :: <mutable-collection>, coll :: <sequence>)
 => (target :: <mutable-collection>);
  for (val in coll, key from 0) target[key] := fun(val) end;
  target
end;
*/

define method map-into-stretchy-one
    (fun :: <function>, target :: <mutable-sequence>, coll :: <collection>)
 => (target :: <mutable-collection>);
  let max-key = maximum-sequence-key(coll);

  with-fip-of target
    for (state = initial-state then next-state(target, state),
         key from 0 to max-key,
         until: finished-state?(target, state, limit))
      let val = element(coll, key, default: not-found());
      unless (not-found?(val))
        current-element-setter(fun(val), target, state)
      end;

    finally
      if (key > max-key)
        target
      else // We are in trouble as the target isn't big enough.
        target.size := max-key + 1;

        // We can't continue the iteration as we have resized, so
        // start again, skipping the keys we have already processed.
        with-fip-of target
          for (key from 0 below key,
               state = initial-state then next-state(target, state))
          finally // Process the remaining keys
            for (state = state then next-state(target, state),
                 key from key to max-key)
              let val = element(coll, key, default: not-found());
              unless (not-found?(val))
                current-element-setter(fun(val), target, state)
              end
            end for
          end for
        end with-fip-of
      end if
    end for
  end with-fip-of;
  target
end method map-into-stretchy-one;

define method map-into-stretchy-one
    (fun :: <function>, target :: <mutable-sequence>, coll :: <sequence>)
 => (target :: <mutable-collection>);

  with-fip-of coll with prefix c-
    with-fip-of target with prefix t-
      let t-state = t-initial-state;
      for (c-state = c-initial-state then c-next-state(coll, c-state),
           key from 0,
           until: c-finished-state?(coll, c-state, c-limit))
        if (t-finished-state?(target, t-state, t-limit))
          // Arghh.  Now things are really grim.
          target.size := coll.size;

          // We can't continue the iteration on target as we have resized, so
          // start again, skipping the keys we have already processed.
          with-fip-of target with prefix t-
            for (key from 0 below key,
                 t-state = t-initial-state then t-next-state(target, t-state))
            finally // Process the remaining keys
              for (t-state = t-state then t-next-state(target, t-state))
                t-current-element-setter
                  (fun(c-current-element(coll, c-state)), target, t-state);
                c-state := c-next-state(coll, c-state);
              end for
            end for
          end
        else
          t-current-element-setter
            (fun(c-current-element(coll, c-state)), target, t-state);
          t-state := t-next-state(target, t-state)
        end if
      end for
    end
  end;
  target
end method map-into-stretchy-one;

// Subclasses of array should have sublinear implementations of element,
// so let's exploit this.
// Perhaps it would be better to find the maximum key to avoid repeated
// expansions in the worst case?  Or will implementations of things like
// <stretchy-vector> be smart about this?

define method map-into-stretchy-one
    (fun :: <function>, target :: <array>, coll :: <collection>)
 => (target :: <mutable-collection>);
  for (val keyed-by key in coll)
    unless (~instance?(key, <integer>) | key < 0) target[key] := fun(val) end;
  end for;
  target
end;

// If the source is a sequence then life is even easier...

define method map-into-stretchy-one
    (fun :: <function>, target :: <array>, coll :: <sequence>)
 => (target :: <mutable-collection>);
  for (val in coll, key from 0) target[key] := fun(val) end;
  target
end;

// some more useful ones
define inline copy-down-method map-into-stretchy-one
  (fun :: <function>, target :: <array>, coll :: <list>) =>
  (target :: <mutable-collection>);
define inline copy-down-method map-into-stretchy-one
  (fun :: <function>, target :: <array>, coll :: <simple-object-vector>) =>
  (target :: <mutable-collection>);



// RIGID CASE

// First we define the default case.

define method map-into-rigid-one
    (fun :: <function>, target :: <mutable-collection>, coll :: <collection>)
 => (target :: <mutable-collection>);
  for (val keyed-by key in coll)
    unless (not-found?(element(target, key, default: not-found())))
      target[key] := fun(val)
    end
  end for;
  target
end;


// When the source or target is a sequence then iterate over that.

define method map-into-rigid-one
    (fun :: <function>, target :: <mutable-sequence>, coll :: <collection>)
 => (target :: <mutable-collection>);
  let max-key = maximum-sequence-key(coll);
  with-fip-of target  /* Use with-setter? */
    for (key from 0 to max-key,
         state = initial-state then next-state(target, state),
         until: finished-state?(target, state, limit))
      let val = element(coll, state, default: not-found());
      unless (not-found?(val))
        current-element(target, state) := fun(val)
      end
    end for
  end;
  target
end method map-into-rigid-one;

define method map-into-rigid-one
    (fun :: <function>, target :: <mutable-collection>, coll :: <sequence>)
 => (target :: <mutable-collection>);
  for (key from 0,  val in coll)
    unless (not-found?(element(target, key, default: not-found())))
      target[key] := fun(val)
    end unless
  end for;
  target
end method map-into-rigid-one;

// markt, some more useful copy-downs
define inline copy-down-method map-into-rigid-one
  (fun :: <function>, target :: <mutable-collection>, coll :: <list>) =>
  (target :: <mutable-collection>);
define inline copy-down-method map-into-rigid-one
  (fun :: <function>, target :: <mutable-collection>, coll :: <simple-object-vector>) =>
  (target :: <mutable-collection>);

// Subclasses of array should have sublinear implementations of element,
// so let's exploit this.

define method map-into-rigid-one
    (fun :: <function>, target :: <array>, coll :: <collection>)
 => (target :: <mutable-collection>);
  let end-key = target.size;
  for (val keyed-by key in coll)
    unless (~instance?(key, <integer>) | key < 0 | key >= end-key)
      target[key] := fun(val)
    end
  end;
  target
end method map-into-rigid-one;

define method map-into-rigid-one
    (fun :: <function>, target :: <array>, coll :: <sequence>)
 => (target :: <mutable-collection>);
  for (val in coll, key from 0 below target.size)
    target[key] := fun(val)
  end;
  target
end method map-into-rigid-one;

define method map-into-rigid-one
    (fun :: <function>, target :: <mutable-collection>, coll :: <array>)
 => (target :: <mutable-collection>);
  with-fip-of coll /* Use with-setter? */
    let end-key = coll.size;
    for (state = initial-state then next-state(target, state),
         until: finished-state?(target, state, limit))
      let key = current-key(coll, state);
      unless (~instance?(key, <integer>) | key < 0 | key >= end-key)
        current-element-setter(fun(coll[key]), target, state)
      end
    end
  end;
  target
end method map-into-rigid-one;

define method map-into-rigid-one
    (fun :: <function>, target :: <mutable-sequence>, coll :: <array>)
 => (target :: <mutable-collection>);
  with-fip-of coll /* Use with-setter? */
    for (state = initial-state then next-state(target, state),
         key from 0 below coll.size,
         until: finished-state?(target, state, limit))
      current-element-setter(fun(coll[key]), target, state)
    end
  end;
  target
end method map-into-rigid-one;

// Now the case where both source and target are sequences.

define method map-into-rigid-one
    (fun :: <function>, target :: <mutable-sequence>, coll :: <sequence>)
 => (target :: <mutable-collection>);
  with-fip-of target /* Use with-setter? */
    for (state = initial-state then next-state(target, state),
         val in coll,
         until: finished-state?(target, state, limit))
      current-element-setter(fun(val), target, state);
    end for
  end;
  target
end method map-into-rigid-one;

// markt
define inline copy-down-method map-into-rigid-one
  (fun :: <function>, target :: <mutable-sequence>, coll :: <list>) =>
  (target :: <mutable-collection>);
define inline copy-down-method map-into-rigid-one
  (fun :: <function>, target :: <mutable-sequence>, coll :: <simple-object-vector>) =>
  (target :: <mutable-collection>);


// And finally the case where both source and target are arrays.

define method map-into-rigid-one
    (fun :: <function>, target :: <array>, coll :: <array>)
 => (target :: <mutable-collection>);
  let sz = min(target.size, coll.size);
  without-bounds-checks
    for (i from 0 below min(target.size, coll.size))
      target[i] := fun(coll[i])
    end
  end;
  target
end method map-into-rigid-one;


//
// ANY?
//

define sealed method any?
    (test :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => value;
  select (size(more-colls))
    0 =>
      any?-one(test, coll);
    1 =>
      any?-two(test, coll, vector-element(more-colls, 0));
    otherwise =>
      any?-one(curry(apply, test), apply(multiple-collection, coll, more-colls));
  end select
end method any?;


define sealed generic any?-one
  (test :: <function>, coll :: <collection>) => value;

define inline sealed method any?-one
    (test :: <function>, coll :: <collection>) => value;
  for (item in coll, result = #f then test(item), until: result)
  finally
    result
  end for
end method any?-one;

ignore(any?-one);  // Silence "unused" warning.

define sealed generic any?-two
  (test :: <function>, c1 :: <collection>, c2 :: <collection>) => value;

define inline sealed method any?-two
    (test :: <function>, c1 :: <sequence>, c2 :: <sequence>) => value;
  for (e1 in c1, e2 in c2, result = #f then test(e1, e2), until: result)
  finally
    result
  end for
end method any?-two;

define inline sealed method any?-two
    (test :: <function>, c1 :: <collection>, c2 :: <collection>) => value;
  any?-one(curry(apply, test), multiple-collection(c1, c2))
end method any?-two;

ignore(any?-two);  // Silence "unused" warning.

//
// EVERY?
//

define sealed method every?
    (test :: <function>, coll :: <collection>, #rest more-colls :: <collection>)
 => (value :: <boolean>);
  select (size(more-colls))
    0 =>
      every?-one(test, coll);
    1 =>
      every?-two(test, coll, vector-element(more-colls, 0));
    otherwise =>
      every?-one(curry(apply, test), apply(multiple-collection, coll, more-colls));
  end select
end method every?;


define sealed generic every?-one
  (test :: <function>, coll :: <collection>) => value :: <boolean>;

define sealed inline method every?-one
    (test :: <function>, coll :: <collection>) => value :: <boolean>;
  for (item in coll, result = #t then test(item), while: result)
  finally
    result ~== #f
  end for
end method every?-one;

ignore(every?-one);  // Silence "unused" warning.

define sealed generic every?-two
  (test :: <function>, c1 :: <collection>, c2 :: <collection>) => value;

define inline sealed method every?-two
    (test :: <function>, c1 :: <sequence>, c2 :: <sequence>) => value;
  for (e1 in c1, e2 in c2, result = #t then test(e1, e2), while: result)
  finally
    result ~== #f
  end for
end method every?-two;

define inline sealed method every?-two
    (test :: <function>, c1 :: <collection>, c2 :: <collection>) => value;
  every?-one(curry(apply, test), multiple-collection(c1, c2))
end method every?-two;

ignore(every?-two);  // Silence "unused" warning.

//
// REDUCE
//

define inline method reduce (fn :: <function>, init-value, collection :: <collection>)
 => (object)
  for (result = init-value then fn(result, item),
       item in collection)
  finally
    result
  end for
end method reduce;


//
// REDUCE1
//

define inline method reduce1 (fn :: <function>, collection :: <collection>)
 => (object)
  with-fip-of collection
    if (finished-state?(collection, initial-state, limit))
      // Is there a more informative error class that's appropriate here?
      error(make(<empty-collection-error>,
                 format-string: "Reduce1 undefined for empty collections"))
    else
      let result = current-element(collection, initial-state);
      for (state = next-state(collection, initial-state)
                   then next-state(collection, state),
           until: finished-state?(collection, state, limit))
        result := fn(result, current-element(collection, state));
      finally
        result
      end for
    end if
  end with-fip-of
end method reduce1;



//
// MEMBER?
//

define method member? (value, collection :: <collection>, #key test = \==)
 => (boolean :: <boolean>)
  for (item in collection,
       result = #f then test(value, item),
       until: result)
  finally
    result & #t
  end for
end method member?;


//
// FIND-KEY
//

define inline method find-key
    (collection :: <collection>, fn :: <function>,
     #key skip :: <integer> = 0, failure = #f) => (key)
  for (e keyed-by k in collection,
       found = #f then fn(e) & ((skip := skip - 1) < 0),
       kludge = 0 then k,
       until: found)
  finally
    if (found) kludge else failure end if
  end for
end method;


//
// Specialized inherited generic methods
//


//
// AS
//

define method as
    (type :: <collection-type>, coll :: <collection>)
 => (new-coll :: <collection>)
  if (instance?(coll, type)) coll else map-as(type, identity, coll) end
end method as;


//
// SHALLOW-COPY
//

define method shallow-copy (coll :: <collection>) => (new-coll :: <collection>)
  map(identity, coll)
end method shallow-copy;


//
// SIZE
//

define method size (collection :: <collection>) => (result :: <integer>)
  for (item in collection, size :: <integer> from 0)
  finally
    size;
  end for
end method size;



//
// EMPTY?
//

define method empty? (collection :: <collection>) => (result :: <boolean>)
  with-fip-of collection
    finished-state?(collection, initial-state, limit)
  end
end method empty?;


//
// =
//

// Let's start with the simple (i.e. inefficient) version, that won't work
// on circular lists.

define method \= (c1 :: <collection>, c2 :: <collection>) => (eq :: <boolean>)
  unless (c1.key-test ~== c2.key-test | c1.size ~= c2.size)
    for (e1 keyed-by k in c1,
         eq = #t then begin
                        let e2 = element(c2, k, default: not-found());
                        unless (not-found?(e2)) e1 = e2 end
                      end,
         while: eq)
    finally
      eq
    end
  end
end;


// We now consider the two cases where one of the collections is a sequence
// and the other one isn't.  We assume that it will be faster to do the
// random accesses on the non-sequence.

define method \= (c1 :: <sequence>, c2 :: <collection>) => (eq :: <boolean>)
  unless (c2.key-test ~== \==)
    let eq = #t;
    for (e1 in c1, key from 0,
         eq = #t then begin
                        let e2 = element(c2, key, default: not-found());
                        unless (not-found?(e2)) e1 = e2 end
                      end,
         while: eq)
    finally
      eq & key = c2.size
    end for
  end unless
end;

define method \= (c1 :: <collection>, c2 :: <sequence>) => (eq :: <boolean>)
  c2 = c1
end;


// In the two sequence case we iterate through both in parallel.

define method \= (c1 :: <sequence>, c2 :: <sequence>) => (eq :: <boolean>)
  with-fip-of c1 with prefix one-
    with-fip-of c2 with prefix two-
      iterate compare (s1 = one-initial-state, s2 = two-initial-state)
        case
          one-finished-state?(c1, s1, one-limit) =>
            two-finished-state?(c2, s2, two-limit);
          two-finished-state?(c2, s2, two-limit) =>
            #f;
          otherwise                              =>
            one-current-element(c1, s1) = two-current-element(c2, s2) &
              compare(one-next-state(c1, s1), two-next-state(c2, s2))
        end case
      end iterate
    end
  end
end;

// In the array case we assume that size is fast.
define method \= (c1 :: <array>, c2 :: <array>) => (eq :: <boolean>)
  unless (c1.size ~= c2.size)
    for (e1 in c1, e2 in c2,
         eq = #t then e1 = e2,
         while: eq)
    finally
      eq
    end
  end
end;


// Lists are a pain because we have to deal with the dotted case.
// We are allowed to diverge if the list has a cycle.

define method \= (c1 :: <list>, c2 :: <collection>) => (eq :: <boolean>)
  block (return)
    for (l = c1 then l.tail, key from 0)
      case
        l == #() => return (key = c2.size);
        ~(instance?(l, <pair>)) => return(#f);
        otherwise =>
          let e = element(c2, key, default: not-found());
          if (not-found?(e) | e ~= l.head) return(#f) end;
      end case
    end for
  end block
end method \=;


define inline method \= (c1 :: <collection>, c2 :: <list>) => (eq :: <boolean>)
  c2 = c1
end method \=;

define method \= (c1 :: <list>, c2 :: <sequence>) => (eq :: <boolean>)
  with-fip-of c2
    block (return)
      for (l = c1 then l.tail,
           state = initial-state then next-state(c2, state))
        case
          l == #()
            => return(finished-state?(c2, state, limit));
          (finished-state?(c2, state, limit)
          | ~instance?(l, <pair>)
          | current-element(c2, state) ~= l.head)
            => return(#f);
        end case
      end for
    end block
  end with-fip-of
end method \=;


define inline method \= (c1 :: <sequence>, c2 :: <list>) => (eq :: <boolean>)
  c2 = c1
end method \=;


// Now for the case of two lists.

define sealed method \= (c1 :: <pair>, c2 :: <pair>) => (eq :: <boolean>)
  c1 == c2 | ((c1.head = c2.head) & (c1.tail = c2.tail))
end method \=;

define sealed method \= (c1 :: <empty-list>, c2 :: <empty-list>)
 => (eq :: <boolean>)
  #t
end method \=;

define sealed method \= (c1 :: <list>, c2 :: <list>) => (eq :: <boolean>)
  #f  // Only called if previous two methods inapplicable
end method \=;



//
// ELEMENT-NO-BOUNDS-CHECK
//

define open generic element-no-bounds-check
  (collection :: <collection>, key, #key default) => object;

define inline method element-no-bounds-check
    (collection :: <collection>, key, #key default = unsupplied()) => object;
  element(collection, key, default: default)
end method element-no-bounds-check;


define function element-range-error
    (collection :: <collection>, key)
 => (will-never-return :: <bottom>)
  // We don't embed the collection in the condition as it will prevent the
  // collection having dynamic extent.  A debugger should be able to display
  // the collection.
  error(make(<invalid-index-error>,
             format-string: "ELEMENT outside of range: %=",
             format-arguments: list(key)))
end function element-range-error;


define class <natural-number-error> (<simple-error>) end;

define inline function check-nat(object) => (nat :: <integer>)
  unless (check-type(object, <integer>) >= 0)
    error(make(<natural-number-error>,
               format-string: "number >= 0 expected instead of %=",
               format-arguments: list(object)))
  end;
  object
end;

// Another useful check method is check-key-test-eq, defined in accumulators.dylan.


// define inline function current-element-setter
//     (coll :: <collection>, state, setter :: <function>)
//         => (setter :: <function>)
//   method (value) setter(value, coll, state) end
// end;


define constant <element-type> = <object>; // KLUDGE FOR LIMITED COLLECTIONSXS

/// define open abstract primary class <limited-collection> ... end;

// The element type for limited collections.
define open generic element-type (t :: <collection>)
  => type :: <type>;

define sealed domain element-type (<limited-collection>);

define inline method element-type (t :: <collection>) => (type == <object>)
  <object>
end method;


// This function helps compute an upper bound on the maximum
// integer key in a collection.

define generic maximum-sequence-key(collection :: <collection>)
  => key :: <integer>;




