Module:       collections-internals
Author:       Keith Dennison
Synopsis:     Define <bit-set> and its operations
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sealed domain make(singleton(<bit-set>));

define sealed primary class <bit-set> (<set>)
  slot member-vector-pad :: <bit> = 0, init-keyword: pad:;
  slot member-vector :: <bit-vector> = make(<bit-vector>, size: 0),
    init-keyword: member-vector:;
end;


//
// INITIALIZE
//
define sealed method initialize
    (set :: <bit-set>, #key member-vector = unsupplied(),
                            pad = unsupplied(),
                            upper-bound-hint = unsupplied(),
                            members = unsupplied(),
                            all-members-from :: false-or(<integer>) = #f)
 => ()
  next-method();
  if (~supplied?(member-vector))
    let pad :: <bit> = 0;
    if (all-members-from)
      upper-bound-hint := all-members-from;
      pad := 1;
    end if;
    if (supplied?(upper-bound-hint))
      let vector :: <bit-vector> = make(<bit-vector>, size: upper-bound-hint);
      set.member-vector-pad := pad;
      set.member-vector := vector;
    end if;
  end if;
  if (supplied?(members))
//    do(method(i :: <integer>) set-add!(set, i) end, members);
    do(curry(set-add!, set), members);
  end if;
end method;


//
// \=
//
define sealed method \=
    (set1 :: <bit-set>, set2 :: <bit-set>) => (result :: <boolean>)
  if (set1 == set2)
    #t;
  elseif (set1.member-vector-pad ~= set2.member-vector-pad)
    #f;
  else
    let vector1 = set1.member-vector;
    let vector2 = set2.member-vector;
    let min-size = min(size(vector1), size(vector2));
    let result = #t;
    for (i :: <integer> from 0 below min-size)
      result := result & (vector1[i] = vector2[i]);
    end for;
    for (i :: <integer> from min-size below size(vector1))
      result := result & (vector1[i] = set1.member-vector-pad);
    end for;
    for (i :: <integer> from min-size below size(vector2))
      result := result & (vector2[i] = set2.member-vector-pad);
    end for;
    result;
  end if;
end method;


//
// MEMBER?
//
define sealed method member?
    (i :: <integer>, set :: <bit-set>, #key test) => (result :: <boolean>)
  if (i < 0)
    element-range-error(set, i);
  end if;
  if (i < size(set.member-vector))
    element-no-bounds-check(set.member-vector, i) = 1;
//    set.member-vector[i] = 1;
  else
    set.member-vector-pad = 1;
  end if;
  // ALTERNATIVE?
  // element(set, i, default: not-found()) = i;
end method;


//
// SET-ADD
//
define sealed method set-add
    (set :: <bit-set>, i :: <integer>) => (new-set :: <bit-set>)
  if (i < 0)
    element-range-error(set, i);
  end if;
  let vector
    = if (i >= size(set.member-vector) & set.member-vector-pad = 0)
	make(<bit-vector>, size: (i + 1), round-up-size?: #t, fill: 0,
	     copy-from: set.member-vector);
      else
	copy-sequence(set.member-vector);
      end if;
  if (i < size(vector))
    element-no-bounds-check-setter(1, vector, i);
//    vector[i] := 1;
  end if;
  make(<bit-set>, pad: set.member-vector-pad, member-vector: vector);
end method;


//
// SET-ADD!
//
define sealed method set-add!
    (set :: <bit-set>, i :: <integer>) => (set :: <bit-set>)
  if (i < 0)
    element-range-error(set, i);
  end if;
  if (i >= size(set.member-vector))
    if (set.member-vector-pad = 0)
      set.member-vector := make(<bit-vector>, size: (i + 1),
                                round-up-size?: #t,
				fill: 0, copy-from: set.member-vector);
      element-no-bounds-check-setter(1, set.member-vector, i);
//      set.member-vector[i] := 1;
    end if;
  else
    element-no-bounds-check-setter(1, set.member-vector, i);
//    set.member-vector[i] := 1;
  end if;
  set
end method;


//
// ADD!
//
define sealed inline method add!
    (set :: <bit-set>, i :: <integer>) => (set :: <bit-set>)
  set-add!(set, i);
end method;


//
// SET-REMOVE
//
define sealed method set-remove
    (set :: <bit-set>, i :: <integer>) => (new-set :: <bit-set>)
  if (i < 0)
    element-range-error(set, i);
  end if;
  let vector
    = if (i >= size(set.member-vector) & set.member-vector-pad = 1)
	make(<bit-vector>, size: (i + 1), round-up-size?: #t, fill: 1,
	     copy-from: set.member-vector);
      else
	copy-sequence(set.member-vector);
      end if;
  if (i < size(vector))
    element-no-bounds-check-setter(0, vector, i);
//    vector[i] := 0;
  end if;
  make(<bit-set>, pad: set.member-vector-pad, member-vector: vector);
end method;


//
// SET-REMOVE!
//
define sealed method set-remove!
    (set :: <bit-set>, i :: <integer>) => (new-set :: <bit-set>)
  if (i < 0)
    element-range-error(set, i);
  end if;
  if (i >= size(set.member-vector))
    if (set.member-vector-pad = 1)
      set.member-vector := make(<bit-vector>, size: (i + 1),
                                round-up-size?: #t,
				fill: 1, copy-from: set.member-vector);
      element-no-bounds-check-setter(0, set.member-vector, i);
//      set.member-vector[i] := 0;
    end if;
  else
    element-no-bounds-check-setter(0, set.member-vector, i);
//    set.member-vector[i] := 0;
  end if;
  set
end method;


//
// REMOVE!
//
define sealed inline method remove!
    (set :: <bit-set>, i :: <integer>, #key test, count)
 => (new-set :: <bit-set>)
  set-remove!(set, i);
end method;


//
// ELEMENT
//
define sealed method element
    (set :: <bit-set>, key :: <integer>, #key default = unsupplied())
 => (element)
  let bit = element(set.member-vector, key, default: not-found());
  if (found?(bit))
    bit == 1
  elseif (set.infinite?)
    #t
  elseif (supplied?(default))
    default
  else
    element-range-error(set, key);
  end
end method;


//
// ELEMENT-SETTER
//
define sealed method element-setter
    (object :: <integer>, set :: <bit-set>, key :: <integer>)
 => (el1 :: <integer>)
  if (object ~= key)
    remove!(set, key);
  end if;
  set-add!(set, object);
  object;
end method;


//
// INFINITE?
//
define sealed method infinite?(set :: <bit-set>) => (result :: <boolean>)
  set.member-vector-pad = 1;
end method;


//
// EMPTY?
//
define sealed method empty?(set :: <bit-set>) => (result :: <boolean>)
//  ~infinite?(set) & empty?(set.member-vector);
  size(set) = 0;
end method;


//
// SIZE
//
define sealed method size(set :: <bit-set>) => (size :: false-or(<integer>))
  if (set.member-vector-pad = 1)
    #f;
  else
    bit-count(set.member-vector, bit-value: 1);
  end if;
end method;


//
// SET-UNION
//
define sealed method set-union
    (set1 :: <bit-set>, set2 :: <bit-set>) => (new-set :: <bit-set>)
  let (vector, pad)
    = bit-vector-or(set1.member-vector, set2.member-vector,
        pad1: set1.member-vector-pad,  pad2: set2.member-vector-pad);
  make(<bit-set>, member-vector: vector, pad: pad);
end method;


//
// SET-UNION!
//
define sealed method set-union!
    (set1 :: <bit-set>, set2 :: <bit-set>) => (set1 :: <bit-set>)
  let (vector, pad)
    = bit-vector-or!(set1.member-vector, set2.member-vector,
        pad1: set1.member-vector-pad,  pad2: set2.member-vector-pad);
  set1.member-vector := vector;
  set1.member-vector-pad := pad;
  set1;  
end method;


//
// SET-INTERSECTION
//
define sealed method set-intersection
    (set1 :: <bit-set>, set2 :: <bit-set>) => (new-set :: <bit-set>)
  let (vector, pad)
    = bit-vector-and(set1.member-vector, set2.member-vector,
        pad1: set1.member-vector-pad,  pad2: set2.member-vector-pad);
  make(<bit-set>, member-vector: vector, pad: pad);
end method;


//
// SET-INTERSECTION!
//
define sealed method set-intersection!
    (set1 :: <bit-set>, set2 :: <bit-set>) => (set1 :: <bit-set>)
  let (vector, pad)
    = bit-vector-and!(set1.member-vector, set2.member-vector,
        pad1: set1.member-vector-pad,  pad2: set2.member-vector-pad);
  set1.member-vector := vector;
  set1.member-vector-pad := pad;
  set1;
end method;


//
// SET-DIFFERENCE
//
define sealed method set-difference
    (set1 :: <bit-set>, set2 :: <bit-set>) => (new-set :: <bit-set>)
  let (vector, pad)
    = bit-vector-andc2(set1.member-vector, set2.member-vector,
        pad1: set1.member-vector-pad, pad2: set2.member-vector-pad);
  make(<bit-set>, member-vector: vector, pad: pad);
end method;


//
// SET-DIFFERENCE!
//
define sealed method set-difference!
    (set1 :: <bit-set>, set2 :: <bit-set>) => (set1 :: <bit-set>)
  let (vector, pad)
    = bit-vector-andc2!(set1.member-vector, set2.member-vector,
        pad1: set1.member-vector-pad, pad2: set2.member-vector-pad);
  set1.member-vector := vector;
  set1.member-vector-pad := pad;
  set1;
end method;


//
// SET-COMPLEMENT
//
define sealed method set-complement
    (set :: <bit-set>) => (new-set :: <bit-set>)
  let (vector, pad)
    = bit-vector-not(set.member-vector, pad: set.member-vector-pad);
  make(<bit-set>, member-vector: vector, pad: pad);
end method;


//
// SET-COMPLEMENT!
//
define sealed method set-complement!
    (set :: <bit-set>) => (set :: <bit-set>)
  let (vector, pad)
    = bit-vector-not!(set.member-vector, pad: set.member-vector-pad);
  set.member-vector := vector;
  set.member-vector-pad := pad;
  set;
end method;


//
// COPY-BIT-SET!
//
define function copy-bit-set!
    (set1 :: <bit-set>, set2 :: <bit-set>) => ()
  set1.member-vector-pad := set2.member-vector-pad;
  set1.member-vector := copy-sequence(set2.member-vector);
end function;


//
//
// EMPTY-BIT-SET!
//
define function empty-bit-set!(set :: <bit-set>) => ()
  set.member-vector-pad := 0;
  fill!(set.member-vector, 0);
end function;


//
// UNIVERSAL-BIT-SET!
//
define function universal-bit-set!(set :: <bit-set>) => ()
  set.member-vector-pad := 1;
  fill!(set.member-vector, 1);
end function;



//
// ITERATION PROTOCOLS
//

define generic current-word (state :: <bit-set-iteration-state>)
 => (word :: <raw-machine-word>);

define generic current-word-setter
    (new :: <raw-machine-word>, state :: <bit-set-iteration-state>)
 => (word :: <raw-machine-word>);

define sealed domain make(singleton(<bit-set-iteration-state>));

define primary sealed class <bit-set-iteration-state> (<object>)
  raw slot current-word :: <raw-machine-word>;
  slot current-element :: <integer>, required-init-keyword: current-element:;
  slot word-offset :: <integer>, required-init-keyword: word-offset:;
  slot bit-offset :: <integer>, required-init-keyword: bit-offset:;
end class;

define inline-only method current-word (state :: <bit-set-iteration-state>)
 => (word :: <raw-machine-word>)
  primitive-cast-pointer-as-raw(primitive-initialized-slot-value(state, integer-as-raw(0)))
end method current-word;

define inline-only method current-word-setter
    (word :: <raw-machine-word>, state :: <bit-set-iteration-state>)
 => (word :: <raw-machine-word>)
  primitive-slot-value(state, integer-as-raw(0)) := primitive-cast-raw-as-pointer(word);
  word
end method current-word-setter;

define sealed method initialize
    (state :: <bit-set-iteration-state>, #key word :: <machine-word>)
 => ()
  next-method();
  state.current-word := primitive-unwrap-machine-word(word)
end method initialize;

define inline function bs-ip-current-key
    (collection :: <bit-set>, state :: <bit-set-iteration-state>)
 => (key :: <integer>)
  state.current-element;
end function;

define inline function bs-ip-current-element
    (collection :: <bit-set>, state :: <bit-set-iteration-state>)
 => (element :: <integer>)
  state.current-element;
end function;

define inline function bs-ip-current-element-setter
    (value :: <integer>, collection :: <bit-set>,
     state :: <bit-set-iteration-state>)
 => (value :: <integer>)
  error("Cannot update current element of a set during iteration.");
end function;

define inline function bs-ip-copy-state
    (collection :: <bit-set>, state :: <bit-set-iteration-state>)
 => (new-state :: <bit-set-iteration-state>)
  make(<bit-set-iteration-state>,
       word: primitive-wrap-machine-word(state.current-word),
       bit-offset: state.bit-offset,
       word-offset: state.word-offset,
       current-element: state.current-element);
end function;


//
// FORWARD-ITERATION-PROTOCOL
//

define inline function bs-fip-initial-state
    (set :: <bit-set>) => (initial-state :: <bit-set-iteration-state>)
  let current-word :: <raw-machine-word> = integer-as-raw(0);
  let word-offset :: <integer> = word-size(set.member-vector) - 1;
  let bit-offset :: <integer> = $machine-word-size;
  block (return)
    for (j :: <integer> from 0 below word-size(set.member-vector))
      for (i :: <integer> from 0 below $machine-word-size,
	   word :: <raw-machine-word>
	     = bit-vector-word(set.member-vector, j)
	     then primitive-machine-word-unsigned-shift-right(word, integer-as-raw(1)))
	if (raw-as-integer(primitive-machine-word-logand(word, integer-as-raw(1))) = 1)
	  current-word := word;
	  word-offset := j;
	  bit-offset := i;
	  return()
	end if
      end for
    end for
  end block;
  make(<bit-set-iteration-state>,
       word: primitive-wrap-machine-word(current-word),
       word-offset: word-offset,
       bit-offset: bit-offset,
       current-element: ((word-offset * $machine-word-size) + bit-offset));
end function;

define inline function bs-fip-limit
    (set :: <bit-set>) => (limit :: <integer>)
  size(set.member-vector);
end function;

define inline function bs-fip-next-state
    (collection :: <bit-set>, state :: <bit-set-iteration-state>)
 => (new-state :: <bit-set-iteration-state>)
  if (state.current-element >= size(collection.member-vector))
    state.current-element := state.current-element + 1;
  else
    block(return)
      for (i :: <integer> from state.bit-offset + 1 below $machine-word-size,
	   word :: <raw-machine-word>
	     = primitive-machine-word-unsigned-shift-right(state.current-word, 
							   integer-as-raw(1))
	       then primitive-machine-word-unsigned-shift-right(word, integer-as-raw(1)))
        state.current-element := state.current-element + 1;
        if (raw-as-integer(primitive-machine-word-logand(word, integer-as-raw(1))) = 1)
	  state.current-word := word;
          state.bit-offset := i;
          return();
        end if;
      end for;
      for (j :: <integer> from state.word-offset + 1
	                  below word-size(collection.member-vector))
	for (i :: <integer> from 0 below $machine-word-size,
	     word :: <raw-machine-word>
	       = bit-vector-word(collection.member-vector, j)
                 then primitive-machine-word-unsigned-shift-right(word, integer-as-raw(1)))
	  state.current-element := state.current-element + 1;
          if (raw-as-integer(primitive-machine-word-logand(word, integer-as-raw(1))) = 1)
            state.current-word := word;
            state.bit-offset := i;
            state.word-offset := j;
            return();
          end if;
        end for;
      end for;
      state.current-element := size(collection.member-vector);
    end block;
  end if;
  state;
end function;

define inline function bs-fip-finished-state?
    (collection :: <bit-set>, state :: <bit-set-iteration-state>,
     limit :: <integer>)
 => (boolean :: <boolean>)
  (collection.member-vector-pad = 0 & state.current-element >= limit)
end function;

define sealed method forward-iteration-protocol(set :: <bit-set>)
  => (initial-state :: <object>, limit :: <object>, next-state :: <function>,
      finished-state? :: <function>, current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>)
  values(bs-fip-initial-state(set),
         bs-fip-limit(set),
         bs-fip-next-state,
         bs-fip-finished-state?,
         bs-ip-current-key,
         bs-ip-current-element,
         bs-ip-current-element-setter,
         bs-ip-copy-state);
end method;


//
// BACKWARD-ITERATION-PROTOCOL
//
define inline function bs-bip-initial-state
    (set :: <bit-set>) => (initial-state :: <bit-set-iteration-state>)
  let current-word :: <raw-machine-word> = integer-as-raw(0);
  let word-offset :: <integer> = 0;
  let bit-offset :: <integer> = -1;
  block (return)
    for (woff :: <integer> from word-size(set.member-vector) - 1 to 0 by -1)
      let word :: <raw-machine-word> = bit-vector-word(set.member-vector, woff);
      for (boff :: <integer> from $machine-word-size - 1 to 0 by -1)
	word := primitive-machine-word-unsigned-rotate-left(word, integer-as-raw(1));
	if (raw-as-integer(primitive-machine-word-logand(word, integer-as-raw(1))) = 1)
	  current-word := word;
	  word-offset := woff;
	  bit-offset := boff;
	  return()
	end if
      end for
    end for
  end block;
  let state = make(<bit-set-iteration-state>,
       word: primitive-wrap-machine-word(current-word),
       word-offset: word-offset,
       bit-offset: bit-offset,
       current-element: ((word-offset * $machine-word-size) + bit-offset));
  state;
end function;

define inline function bs-bip-limit
    (set :: <bit-set>) => (limit :: <integer>)
  0;
end function;

define inline function bs-bip-next-state
    (collection :: <bit-set>, state :: <bit-set-iteration-state>)
 => (new-state :: <bit-set-iteration-state>)
  block(return)
    let word :: <raw-machine-word> = state.current-word;
    for (i :: <integer> from state.bit-offset - 1 above -1 by -1)
      state.current-element := state.current-element - 1;
      word := primitive-machine-word-unsigned-rotate-left
                (word, integer-as-raw(1));
      if (raw-as-integer(primitive-machine-word-logand
                           (word, integer-as-raw(1))) = 1)
        state.bit-offset := i;
        state.current-word := word;
        return();
      end if;
    end for;
    for (j :: <integer> from state.word-offset - 1 above -1 by -1)
      let word :: <raw-machine-word> = bit-vector-word(collection.member-vector, j);
      for (i :: <integer> from $machine-word-size above 0 by -1)
        word := primitive-machine-word-unsigned-rotate-left
                  (word, integer-as-raw(1));
        state.current-element := state.current-element - 1;
        if (raw-as-integer(primitive-machine-word-logand
                             (word, integer-as-raw(1))) = 1)
          state.bit-offset := i - 1;
          state.word-offset := j;
          state.current-word := word;
          return();
        end if;
      end for;
    end for;
    state.bit-offset := -1;
    state.word-offset := -1;
    state.current-element := -1;
  end block;
  state;
end function;

define inline function bs-bip-finished-state?
    (collection :: <bit-set>, state :: <bit-set-iteration-state>,
     limit :: <integer>)
 => (boolean :: <boolean>)
  state.current-element < 0;
end function;

define sealed method backward-iteration-protocol(set :: <bit-set>)
  => (initial-state :: <object>, limit :: <object>, next-state :: <function>,
      finished-state? :: <function>, current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>)
  if (infinite?(set))
    error("BACKWARD-ITERATION-PROTOCOL called on infinite set %=", set);
  end if;
  values(bs-bip-initial-state(set),
         bs-bip-limit(set),
         bs-bip-next-state,
         bs-bip-finished-state?,
         bs-ip-current-key,
         bs-ip-current-element,
         bs-ip-current-element-setter,
         bs-ip-copy-state);
end method;
