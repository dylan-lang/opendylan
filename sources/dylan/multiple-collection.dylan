Module:    internal
Culprit:   Kevin Mitchell (partially based on previous Apple code)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// A number of collection functions can take multiple collections as
// arguments.  Efficiently iterating over multiple collections in parallel
// requires some care, and a non-trivial amount of code.  Rather than making
// this code purely internal to the collections functions,  we define a set of
// classes for dealing with multiple collections.  The result will be slightly
// less efficient than the dedicated versions, but we then have the option of
// making the classes visible to the user if we want.


// The basic idea is to take a sequence of collections and to build a
// collection from them.  Iterating through this collection will iterate
// through it's constituent collections in parallel.  There are three
// separate cases to consider, depending on whether the constituents are all
// sequences, explicit keyed collections, or a mixture.

// In the multiple sequence case we just store the sequences.

define class <multiple-sequence>(<sequence>)
  constant slot collections :: limited(<vector>, of: <sequence>),
         required-init-keyword: collections:;
end class <multiple-sequence>;

// In the explicit key case we cache the index of the smallest collection,
// but delay computing it until needed in case some of the collections have
// slow size methods.

define class <multiple-explicit-key-collection>(<explicit-key-collection>)
  constant slot collections :: limited(<vector>, of: <explicit-key-collection>),
         required-init-keyword: collections:;
  slot smallest-collection-index :: false-or(<integer>) = #f;
end class <multiple-explicit-key-collection>;

// In the mixed case we separate the sequence collections from the rest.  This
// is so we can iterate through the sequences whilst performing random access
// on the other collections.  However, we need to record the position of each
// collection in the original sequence, and we use a couple of maps for this
// purpose. We also cache an upper bound on the largest key in the resulting
// collection.

define class <multiple-mixed-collection>(<explicit-key-collection>)
  constant slot sequence-collections :: limited(<vector>, of: <sequence>),
         required-init-keyword: sequences:;
  constant slot sequence-map :: limited(<vector>, of: <integer>),
         required-init-keyword: smap:;
  constant slot explicit-collections ::
           limited(<vector>, of: <explicit-key-collection>),
         required-init-keyword: explicits:;
  constant slot explicit-map :: limited(<vector>, of: <integer>),
         required-init-keyword: emap:;
  slot key-upper-bound :: false-or(<integer>) = #f;
end class <multiple-mixed-collection>;


// The multiple-collection method takes a sequence of collections and returns
// a collection.

define function multiple-collection
    (coll :: <collection>, #rest colls :: <collection>)
 => (collection :: <collection>);
  if (empty?(colls)) coll
  else
    let coll-class =
      if (instance?(coll,<sequence>)) <sequence>
      else <explicit-key-collection> end;

    let kt = coll.key-test;
    let all-the-same = #t;
    for (c in colls)
      all-the-same := all-the-same & instance?(c, coll-class);
      unless (c.key-test == kt)
        error(make(<key-test-error>,
                format-string: "Collection %= and %= have different key tests",
                format-arguments: list(coll, c)))
      end
    end;
    let cv = apply(vector, coll, colls);
    case
      ~all-the-same
        => mixed-collection(cv);
      coll-class == <sequence>
        => make(<multiple-sequence>,
                collections: as(limited(<vector>, of: <sequence>), cv));
      otherwise
        => make(<multiple-explicit-key-collection>,
                collections: as(limited(<vector>, of: <explicit-key-collection>), cv));
    end case
  end if
end function multiple-collection;


// The following helper method builds instances of <multiple-mixed-collection>

define function mixed-collection
    (colls :: limited(<sequence>, of: <collection>)) => (coll :: <collection>);
  let seq-count = 0;
  for (c in colls)
    if (instance?(c, <sequence>)) seq-count := seq-count + 1 end if
  end for;
  let exp-count = colls.size - seq-count;

  let scols =
    make(limited(<simple-vector>, of: <sequence>), size: seq-count);
  let smap =
    make(limited(<simple-vector>, of: <integer>), size: seq-count);
  let ecols =
    make(limited(<simple-vector>, of: <explicit-key-collection>), size: exp-count);
  let emap =
    make(limited(<simple-vector>, of: <integer>), size: exp-count);

  let scnt = 0; let ecnt = 0;
  for (c in colls, i from 0)
    if (instance?(c, <sequence>))
      scols[scnt] := c; smap[scnt] := i; scnt := scnt + 1
    else
      ecols[ecnt] := c; emap[ecnt] := i; ecnt := ecnt + 1
    end if
  end for;

  make(<multiple-mixed-collection>,
    sequences: scols, smap: smap, explicits: ecols, emap: emap)
end function mixed-collection;



//
// ITERATION PROTOCOL
//

// We must define a forward-iteration-protocol for each of the multiple
// collections.  First we treat the easiest case, <multiple-sequence>.

// We just need to iterate through each sequence in parallel, stopping as
// soon as one of them is exhausted.   The current-element method returns a
// vector containing the current element of each constituent sequence.

// The state of the iteration is just the vector of constituent states, and we
// use closures to access the elements vector and iteration protocols of the
// constituents.  This assumes that creating closures will be fast in the
// run-time, and that we don't need to inline the iteration methods in this
// case.  If this assumption turns out to be false we can include these items
// as additional components of the state.

define method forward-iteration-protocol(coll :: <multiple-sequence>)
 => (init :: <object>, limit :: <object>, next :: <function>,
     finished? :: <function>, key :: <function>, elem :: <function>,
     elem-setter :: <function>, copy :: <function>);

  let collections = coll.collections;

  let (inits, limits, nexts, finished?s, keys, elems, elem-setters, copies)
    = extend-function(forward-iteration-protocol, collections);

  let sz = collections.size;

  values (
    inits,

    limits,

    method (c, states) // next-state
      for (i from 0 below sz)
        states[i] := nexts[i](collections[i], states[i])
      end for;
      states
    end,

    method (c, states, limits) // finished-state?
      for (i from 0 below sz,
           finished? = #f
             then finished?s[i](collections[i], states[i], limits[i]),
           until: finished?)
        finally finished?
      end for
    end,

    method (seq, states) keys[0](collections[0], states[0]) end,

    method (seq, states) // current-element
      let elements :: <simple-object-vector>
        = make(<simple-object-vector>, size: sz);
      for (i from 0 below sz)
         elements[i] := elems[i](collections[i], states[i])
      end for;
      elements
    end,

    method (vals, seq, states) // current-element-setter
      error("Immutable collection %=", seq)
    end,

    method (seq, states) // copy-state
      let copy-state = make(<simple-object-vector>, size: sz);
      for (i from 0 below sz)
        copy-state[i] := copies[i](collections[i], states[i])
      end for;
      copy-state
    end)
end method forward-iteration-protocol;


// Given a function that takes an argument and returns n values,
// this helper method applies the function point-by-point to a vector,
// returning n vectors of corresponding results.

define function fake-values (x, #rest r) // HACK: TO AVOID COMPILER BUG
  apply(values, x, r)
end function;

define function extend-function(function :: <function>, vec :: <vector>)
  let (#rest results) = function(vec[0]);
  let results-list :: <simple-object-vector>
    = map(method (r) make(<vector>, size: vec.size, fill: r) end, results);
  for (i from 1 below vec.size)
    let (#rest next-results) = function(vec[i]);
    for (r in next-results, v in results-list) v[i] := r end for;
  end for;
  apply(fake-values, results-list)
end function extend-function;


// Now for the next case, <multiple-explicit-key-collection>.  The strategy
// here is to find the smallest collection in the sequence, and then iterate
// through that.  For each key in the collection we check to see if each of
// the other collections contains the key.  If they do then this key appears
// in the iteration.  If not, we skip it.  The skip-until-common method
// performs this task.  This method also calculates the corresponding elements
// for this key, so the current-element method just needs to return the
// vector.

define method forward-iteration-protocol
    (coll :: <multiple-explicit-key-collection>)
 => (init :: <object>, limit :: <object>, next :: <function>,
     finished? :: <function>, key :: <function>, elem :: <function>,
     elem-setter :: <function>, copy :: <function>);

  let collections = coll.collections;
  let sz = collections.size;

  let index :: <integer>  // Cache smallest if not found before.
    = coll.smallest-collection-index
    | (coll.smallest-collection-index := minimum-collection(collections));

  let iterating-collection = collections[index];

  with-fip-of iterating-collection
    let state = initial-state;
    let elements :: <simple-object-vector>
      = make(<simple-object-vector>, size: sz);

    local method skip-until-common(state)
      block (return)
        until (finished-state?(iterating-collection, state, limit))
          let key = current-key(iterating-collection, state);
          let common-key? = #t;
          for (i from 0 below sz, while: common-key?)
            case
              i = index
                => elements[i] := current-element(iterating-collection, state);
              not-found?(elements[i] :=
                           element(collections[i], key, default: not-found()))
                => common-key? := #f;
            end case
          end for;
          if (common-key?)
            return(state)
          else
            state := next-state(iterating-collection, state)
          end if;
        end until;
        state;
      end
    end;

    values (
      skip-until-common(initial-state),

      limit,

      method (c, state) // next-state
        skip-until-common(next-state(iterating-collection, state))
      end,

      method (c, state, limit)
        finished-state?(iterating-collection, state, limit)
      end,

      method (c, state)
        current-key (iterating-collection, state)
      end,

      method (c, state) shallow-copy(elements) end, // current-element

      method (vals, seq, state) // current-element-setter
        error("Immutable collection %=", seq)
      end,

      copy-state )
  end
end method forward-iteration-protocol;


// We iterate through the smallest explicit-key collection.  This helper
// method finds this collection.

define function minimum-collection(collections :: <sequence>)
 => (index :: <integer>);
  let index = 0;
  let min-size = collections[0].size;
  for (i from 1 below collections.size)
    let s = collections[i].size;
    if (s < min-size) index := i; min-size := s end if
  end for;
  index
end function minimum-collection;


// Now life gets tricky, as we must deal with the <multiple-mixed-collection>
// case.  The strategy here is to iterate through all of the sequences in
// parallel, skipping over those elements not contained in the explicitly
// keyed collections.  If we knew the maximum integer key common to all the
// explicit collections then we could stop once this key was reached.
// Unfortunately it might be expensive to compute this.  So as a compromise we
// find the maximum integer key in the explicit collection with the smallest
// size.  Of course it is easy to construct examples where this doesn't help
// us, but this seems true of any strategy in this situation.

define method forward-iteration-protocol
    (coll :: <multiple-mixed-collection>)
 => (init :: <object>, limit :: <object>, next :: <function>,
     finished? :: <function>, key :: <function>, elem :: <function>,
     elem-setter :: <function>, copy :: <function>);

  let scolls = coll.sequence-collections;
  let ecolls = coll.explicit-collections;
  let emap = coll.explicit-map;
  let smap = coll.sequence-map;

  let key-upper-bound :: <integer> // Cache the upper bound
    = coll.key-upper-bound
    | (coll.key-upper-bound :=
          maximum-sequence-key(
            ecolls[minimum-collection(ecolls)]));

  let (inits, limits, nexts, finished?s, keys, elems, elem-setters, copies)
    = extend-function(forward-iteration-protocol, scolls);

  let ssz = scolls.size;
  let esz = ecolls.size;
  let elements :: <simple-object-vector>
    = make(<simple-object-vector>, size: ssz + esz);

  // An iteration state is a pair of a key and a vector of iteration states
  // for the sequences.  We store the key explicitly because the sequences
  // may have slow current-key methods.

  local method finished?(c, state, limits)
    let index = state.head;
    let states = state.tail;

      index > key-upper-bound
    | begin
        for (i from 0 below ssz,
             finished? = #f
               then finished?s[i](scolls[i], states[i], limits[i]),
             until: finished?)
          finally finished?
        end for
      end
  end;

  local method next-states(states)
    for (i from 0 below ssz)
      states[i] := nexts[i](scolls[i], states[i])
    end for;
    states
  end;

  local method skip-until-common(state)
    let key = state.head;
    let states = state.tail;

    block (return)
      until (finished?(scolls, state, limits))
        let common-key? = #t;
        for (i from 0 below esz, while: common-key?)
          if (not-found?(elements[emap[i]] :=
                           element(ecolls[i], key, default: not-found())))
            common-key? := #f
          end if
        end for;
        if (common-key?)
          for (i from 0 below ssz)
            elements[smap[i]] :=
              elems[i](scolls[i], states[i])
          end for;
          return()
        else
          states := next-states(states);  key := key + 1
        end if;
      end until;
    end;
    state.head := key;
    state.tail := states;
    state
  end;

  values (
    skip-until-common(pair(0,inits)),

    limits,

    method (c, state) // next-state
      state.head := state.head + 1;  state.tail := next-states(state.tail);
      skip-until-common(state)
    end,

    finished?,

    method (seq, state) state.head end,

    method (seq, state) shallow-copy(elements) end, // current-element

    method (vals, seq, state) // current-element-setter
      error("Immutable collection %=", seq)
    end,

    method (seq, state) // copy-state
      let states = state.tail;
      let copy-state = make(<simple-object-vector>, size: ssz);
      for (i from 0 below ssz)
        copy-state[i] := copies[i](scolls[i], states[i])
      end for;
      pair(state.head, copy-state)
    end )
end method forward-iteration-protocol;


//
// ELEMENT
//

// We must now define the element method for these classes.  Unlike
// the iteration case, we generate a new vector each call.

define method element
    (collection :: <multiple-sequence>,
     key :: <integer>,
     #key default = unsupplied())
 => element :: <vector>;
  let collections = collection.collections;
  let sz = collections.size;
  let result = make(<simple-object-vector>, size: sz);

  let missing = #f;
  for (i from 0 below sz, until: missing)
    missing :=
      not-found?(
        result[i] := element(collections[i], key, default: not-found()))
  end for;

  case
    ~missing => result;
    unsupplied?(default)
      =>  element-range-error(collection, key);
    otherwise => as(<vector>, default)
  end case
end method element;


define method element
    (collection :: <multiple-explicit-key-collection>,
     key :: <object>,
     #key default = unsupplied())
 => (element :: <vector>);
  let collections = collection.collections;
  let sz = collections.size;
  let result = make(<simple-object-vector>, size: sz);

  let missing = #f;
  for (i from 0 below sz, until: missing)
    missing :=
      not-found?(
        result[i] := element(collections[i], key, default: not-found()))
  end for;

  case
    ~missing => result;
    unsupplied?(default)
      =>  element-range-error(collection, key);
    otherwise => as(<vector>, default)
  end case
end method element;


// The <multiple-mixed-collection> case is a bit messier as the collections
// are split.

define method element
    (collection :: <multiple-mixed-collection>, key :: <integer>,
     #key default = unsupplied())
 => (element :: <vector>);
  let scolls = collection.sequence-collections;
  let ecolls = collection.explicit-collections;
  let ssz = scolls.size;
  let esz = ecolls.size;
  let smap = collection.sequence-map;
  let emap = collection.explicit-map;
  let result = make(<simple-object-vector>, size: ssz + esz);

  let missing = #f;
  for (i from 0 below ssz, until: missing)
    missing :=
      not-found?(
        result[smap[i]] := element(scolls[i], key, default: not-found()))
  end for;

  for (i from 0 below esz, until: missing)
    missing :=
      not-found?(
        result[emap[i]] := element(ecolls[i], key, default: not-found()))
  end for;

  case
    ~missing => result;
    unsupplied?(default)
      => element-range-error(collection, key);
    otherwise => as(<vector>, default)
  end case
end method element;


//
// KEY-TEST
//

// The key-test method returns == for those collections involving sequences,
// and the key-test function for the first collection in the explicit case.

define method key-test
    (coll :: <multiple-sequence>) => test-fun :: <function>;
  \==
end method key-test;

define method key-test
    (coll :: <multiple-mixed-collection>) => test-fun :: <function>;
  \==
end method key-test;


define method key-test
    (coll :: <multiple-explicit-key-collection>) => test-fun :: <function>;
  coll.collections[0].key-test
end method key-test;


//
// TYPE-FOR-COPY
//

define method type-for-copy(
    mc :: type-union(<multiple-sequence>,
                     <multiple-explicit-key-collection>,
                     <multiple-mixed-collection>)) => (t :: <type>)
  error("TYPE-FOR-COPY not implemented on the multiple collections %=", mc)
end method type-for-copy;


