Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define open abstract class <deque> (<stretchy-mutable-sequence>)
end class <deque>;


////////////
// INTERFACE
////////////

define open generic pop 
  (deque :: <deque>) => object;

define open generic pop-last 
  (deque :: <deque>) => object;
  
define open generic push 
  (deque :: <deque>, new-value) => (new-value :: <object>);

define open generic push-last 
  (deque :: <deque>, new-value) => (new-value :: <object>);



/////////////////
// IMPLEMENTATION
/////////////////


//
// MAKE
//

define sealed inline method make
    (class == <deque>, #rest all-keys, #key size, fill) 
 => (deque :: <object-deque>)
  apply(make, <object-deque>, all-keys)
end method make;


//
// AS
//

define method as 
    (class == <deque>, object :: <collection>) => (deque :: <deque>)
  as(<object-deque>, object)
end method as;

define sealed inline method as 
    (class == <deque>, deque :: <deque>) => (deque :: <deque>)
  deque
end method as;


//
// ADD!
//

define method add! (deque :: <deque>, new-element) => (deque :: <deque>)
  push(deque, new-element);
  deque
end method add!;


//
// ADD
//

define method add (deque :: <deque>, new-element) => (new :: <deque>)
  let new = deque.copy-sequence;
  push(new, new-element);
  new
end method add;


//
// REMOVE
//

define method remove 
    (deque :: <deque>, value, #key test = \==, count = deque.size)
         => (new :: <deque>)
  remove!(deque.copy-sequence, value, test: test, count: count)
end method remove;


//
// REVERSE
//

define method reverse (deque :: <deque>) => (new :: <deque>)
  reverse!(deque.copy-sequence)
end method reverse;




//
// <OBJECT-DEQUE>
//

define class <object-deque> (<deque>, <limited-collection>)
  slot representation :: <island-deque>, 
    init-value: $empty-island-deque;
end class <object-deque>;


define sealed domain element-type (<object-deque>);

///
/// LIMITED DEQUES
/// 

define method limited-deque
     (of :: <type>) => (type :: <limited-deque-type>)
  make(<limited-deque-type>,
       class:          <deque>,
       element-type:   of,
       concrete-class: <object-deque>);
end method;

define method limited 
    (class == <deque>, #key of, #all-keys) => (type :: <type>)
  limited-deque(of)
end method;

/// TODO: COULD BE EXPENSIVE UNLESS TYPES ARE CACHED

define sealed inline method type-for-copy (x :: <object-deque>) 
    => (type :: <type>)
  let elt-type = element-type(x);
  if (elt-type == <object>)
    object-class(x)
  else 
    limited-deque(elt-type)
  end if
end method type-for-copy;

define constant $minimum-island-deque-data-size = 4;

// Note that first-index > last-index when the queue is empty

define class <island-deque> (<deque>)
  slot first-index :: <integer>, init-value: 0;
  slot last-index :: <integer>, init-value: 0;
  repeated slot island-deque-element,
    init-keyword: fill:,
    init-value: #f,
    size-getter: size,
    size-init-keyword: size:,
    size-init-value: 0;
end class <island-deque>;

define constant $empty-island-deque = make(<island-deque>, size: 0);


//
// INITIALIZE
//

define method initialize (deque :: <object-deque>, #key size = 0, fill)
  next-method();
  check-nat(size);
  let data-size = max(size * 2, $minimum-island-deque-data-size);
  let rep = make(<island-deque>, size: data-size, fill: #f);
  let rep-first-index = truncate/(data-size, 2);
  let rep-last-index = rep-first-index + size - 1;
  if (fill)
    for (i :: <integer> from rep-first-index to rep-last-index)
      island-deque-element(rep, i) := fill 
    end;
  end;
  rep.first-index := rep-first-index;
  rep.last-index := rep-last-index;
  deque.representation := rep;
end method initialize;


//
// AS
//

define sealed method as 
    (class == <object-deque>, object :: <collection>) => (d :: <object-deque>)
  let new-deque :: <object-deque> = make(<object-deque>);
  for (element in object)
    push-last(new-deque, element);
  end for;
  new-deque
end method as;

define sealed method as 
    (class == <object-deque>, object :: <array>) => (d :: <object-deque>)
  let new-deque :: <object-deque> = make(<object-deque>, size: object.size);
  let rep = new-deque.representation;
  for (i :: <integer> from rep.first-index, val in object)
    island-deque-element(rep, i) := val
  end for;
  new-deque
end method as;

define sealed method as 
    (class == <object-deque>, object :: <object-deque>) => (d :: <object-deque>)
  object
end method as;


//
// SIZE
// 

define sealed inline method size (collection :: <object-deque>) => (size :: <integer>)
  let rep = collection.representation;
  (rep.last-index - rep.first-index) + 1
end method size;


//
// SIZE-SETTER
// 

define sealed inline method trusted-size-setter
    (new-size :: <integer>, collection :: <object-deque>) 
 => (new-size :: <integer>)
  // TODO: write a faster version of this method.
  let difference = new-size - collection.size;
  case
    difference < 0 => 
      for (i :: <integer> from 0 below - difference) 
        pop-last(collection) 
      end;
    difference > 0 => 
      for (i :: <integer> from 0 below difference) 
        trusted-push-last(collection, #f) 
      end;
  end case;
  new-size
end method trusted-size-setter;

define sealed method size-setter (new-size :: <integer>, collection :: <object-deque>) 
 => (new-size :: <integer>)
  // TODO: write a faster version of this method.
  check-nat(new-size);
  let size = size(collection);
  unless (new-size <= size)
    // expected to fail when #f is incompatible with element-type
    check-type(#f, element-type(collection))
  end unless;
  trusted-size(collection) := new-size;
end method size-setter;


//
// EMPTY?
// 

define sealed method empty? (collection :: <object-deque>) => (result :: <boolean>)
  let rep = collection.representation;
  rep.last-index < rep.first-index
end method empty?;


//
// ELEMENT
// 

define sealed method element
    (collection :: <object-deque>, index :: <integer>, 
     #key default = unsupplied()) => (object)
  let rep = collection.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  let rep-size-minus-1 = rep-last-index - rep-first-index;
  if (index < 0 | index > rep-size-minus-1)
    if (unsupplied?(default))
      element-range-error(collection, index)
    else
      check-type(default, element-type(collection));
      default
    end if
  else
    // Even if multiple threads are running, and rep-first-index and
    // rep-last-index are incorrect, they should be within the bounds of
    // rep, and so if we get here the following should be within bounds.
    island-deque-element(rep, index + rep-first-index)
  end if
end method element;


//
// ELEMENT-NO-BOUNDS-CHECK
// 

// We can't trust skipping the bounds check because the deque might move under
// our feet.

define sealed inline method element-no-bounds-check
    (collection :: <object-deque>, index :: <integer>, 
     #key default = unsupplied()) => (object)
  element(collection, index, default: default)
end method element-no-bounds-check;


//
// ELEMENT-SETTER
// 

define sealed method element-setter
    (new-value, collection :: <object-deque>, index :: <integer>)
 => (object)
  check-type(new-value, element-type(collection));
  let rep = collection.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  let rep-size-minus-1 = rep-last-index - rep-first-index;
  if (index < 0) element-range-error(collection, index) end;
  if (index > rep-size-minus-1) 
    if (collection.size = index)
      trusted-size(collection) := index + 1;
    else 
      collection.size := index + 1;
    end if;
    collection[index] := new-value // Let's try again
  else
    // Even if multiple threads are running, and rep-first-index and
    // rep-last-index are incorrect, they should be within the bounds of
    // rep, and so if we get here the following should be within bounds.
    island-deque-element(rep, index + rep-first-index) := new-value;
  end if
end method element-setter;


//
// ELEMENT-NO-BOUNDS-CHECK-SETTER
// 

// We can't trust skipping the bounds check because the deque might move under
// our feet.

define inline sealed method element-no-bounds-check-setter
    (new-value, collection :: <object-deque>, index :: <integer>)
        => (object)
  element-setter(new-value, collection, index)
end method element-no-bounds-check-setter;


//
// REMOVE!
// 

define sealed method remove!
    (deque :: <object-deque>, target,
     #key test = \==, count = deque.size) => (d :: <deque>)
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  iterate grovel (count :: <integer> = count,
		  src-index :: <integer> = rep-first-index,
		  dst-index :: <integer> = rep-first-index)
    if (src-index > rep-last-index)
      for (i :: <integer> from dst-index below src-index) 
        island-deque-element(rep, i) := #f 
      end;
      rep.last-index := dst-index - 1
    else
      let item = island-deque-element(rep, src-index);
      case
	count > 0 & test(item, target) =>
	  grovel(count - 1, src-index + 1, dst-index);
	otherwise =>
	  island-deque-element(rep, dst-index) := item;
	  grovel(count, src-index + 1, dst-index + 1)
      end case
    end if
  end iterate;
  deque
end method remove!;


//
// REVERSE!
// 

define sealed method reverse! (deque :: <object-deque>) => (result :: <deque>)
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  let stopping-index = floor/(rep-first-index + rep-last-index, 2);
  for (i :: <integer> from rep-first-index below stopping-index,
       j :: <integer> from rep-last-index by -1)
    let tmp = island-deque-element(rep, i);
    island-deque-element(rep, i) := island-deque-element(rep, j);
    island-deque-element(rep, j) := tmp;
  end for;
  deque
end method reverse!;


// PRIVATE

define method grow! (deque :: <object-deque>)
  let old-rep = deque.representation;
  let old-rep-first-index = old-rep.first-index;
  let old-rep-last-index = old-rep.last-index;
  let old-rep-size = (old-rep-last-index - old-rep-first-index) + 1;
  let new-rep = make(<island-deque>, size: old-rep-size * 2, fill: #f);
  new-rep.first-index := truncate/(old-rep-size, 2);
  for (src-index :: <integer> 
         from old-rep-first-index to old-rep-last-index,
       dst-index :: <integer> from new-rep.first-index)
    island-deque-element(new-rep, dst-index) := 
      island-deque-element(old-rep, src-index);
  finally
    new-rep.last-index := dst-index - 1;
  end for;
  deque.representation := new-rep;
  deque
end method grow!;


define method make-room-at-first! (deque :: <object-deque>)
  if (size(deque.representation) = deque.size)
    grow!(deque)
  end if;
  // assume grow leaves room at first
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  let rep-size-minus-1 = rep.size - 1;
  let delta :: <integer> = 
    rep-size-minus-1 - (rep-last-index - rep-first-index);
  unless ( rep-last-index = rep-size-minus-1 
         | delta < 0 /* i.e. thread problem */)
    // unlucky?
    for (dst-index :: <integer> from rep-size-minus-1 to delta by -1,
         src-index :: <integer>
           from (rep-size-minus-1 - delta) + rep-first-index by -1)
      island-deque-element(rep, dst-index) := island-deque-element(rep, src-index);
    end for;
    rep.first-index := delta;
    rep.last-index := rep-size-minus-1;
  end unless;
  values()
end method make-room-at-first!;


define method make-room-at-last! (deque :: <object-deque>)
  if (size(deque.representation) = deque.size)
    grow!(deque)
  end if;
  // assume grow leaves room at last
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  let new-last :: <integer> = rep-last-index - rep-first-index;
  unless (rep-first-index = 0 | new-last < -1 /* i.e. thread problem */)
    // unlucky?
    for (dst-index :: <integer> from 0 to new-last,
	 src-index :: <integer> from rep-first-index)
      island-deque-element(rep, dst-index) := island-deque-element(rep, src-index);
    end for;
    rep.first-index := 0;
    rep.last-index := new-last
  end unless;
  values()
end method make-room-at-last!;


//
// PUSH
// 

define sealed method push 
    (deque :: <object-deque>, new-element) => (new-element :: <object>)
  check-type(new-element, element-type(deque));
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  while (rep-first-index = 0) // If threads are used properly this loop should
                              // be executed at most once.
    make-room-at-first!(deque);
    rep := deque.representation;
    rep-first-index := rep.first-index;
  end while;
  rep.first-index := rep-first-index := rep-first-index - 1;
  island-deque-element(rep, rep-first-index) := new-element
end method push;


//
// POP
// 

define sealed method pop (deque :: <object-deque>) => (object)
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  if (rep-last-index < rep-first-index)
    error(make(<empty-collection-error>,
               format-string: "POP empty deque %=", 
               format-arguments: list(deque)))
  else
    let value = island-deque-element(rep, rep-first-index);
    island-deque-element(rep, rep-first-index) := #f;
    rep.first-index := rep-first-index + 1;
    value
  end if
end method pop;


//
// PUSH-LAST
// 

define sealed inline method trusted-push-last 
    (deque :: <object-deque>, new-element) => (result :: <deque>)
  let rep = deque.representation;
  let rep-last-index = rep.last-index;
  while (rep-last-index = (rep.size - 1))
      // If threads are used properly this loop should
      // be executed at most once.
    make-room-at-last!(deque);
    rep := deque.representation;
    rep-last-index := rep.last-index;
  end while;
  rep.last-index := rep-last-index := rep-last-index + 1;
  island-deque-element(rep, rep-last-index) := new-element;
  deque
end method trusted-push-last;

define sealed method push-last 
    (deque :: <object-deque>, new-element) => (result :: <deque>)
  check-type(new-element, element-type(deque));
  trusted-push-last(deque, new-element);
end method push-last;


//
// POP-LAST
// 

define sealed method pop-last (deque :: <object-deque>) => (object)
  let rep = deque.representation;
  let rep-first-index = rep.first-index;
  let rep-last-index = rep.last-index;
  if (rep-last-index < rep-first-index)
    error(make(<empty-collection-error>,
               format-string: "POP empty deque %=", 
               format-arguments: list(deque)))
  else
    let value = island-deque-element(rep, rep-last-index);
    island-deque-element(rep, rep-last-index) := #f;
    rep.last-index := rep-last-index - 1;
    value
  end if
end method pop-last;


//
// ITERATION PROTOCOL
// 

// We assume that the underlying vector can only ever get bigger.  If this
// ceases to hold then the iteration code needs to be rethought.

define inline function object-deque-current-key
    (deque :: <object-deque>, state :: <integer>)
  state - deque.representation.first-index
end function;

define inline function object-deque-current-element
    (deque :: <object-deque>, state :: <integer>)
  island-deque-element(deque.representation, state)
end function;

define inline function object-deque-current-element-setter
    (new-value, deque :: <object-deque>, state :: <integer>)
  check-type(new-value, element-type(deque));
  island-deque-element(deque.representation, state) := new-value
end function;


define inline method forward-iteration-protocol (deque :: <object-deque>)
    => (initial-state, limit,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>)
  values(deque.representation.first-index,
	 deque.representation.last-index + 1,
	 sequence-next-state,
	 sequence-finished-state?,
	 object-deque-current-key,
	 object-deque-current-element,
	 object-deque-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;

define inline method backward-iteration-protocol (deque :: <object-deque>)
    => (final-state, limit,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>)
  values(deque.representation.last-index + 1,
	 deque.representation.first-index,
	 sequence-previous-state,
	 sequence-finished-state?,
	 sequence-current-key,
	 object-deque-current-element,
	 object-deque-current-element-setter,
	 identity-copy-state)
end method backward-iteration-protocol;

 
//
// COPY-SEQUENCE
// 

define sealed method copy-sequence
    (source :: <object-deque>, 
     #key start: first :: <integer> = 0, end: last = unsupplied()) 
        => (result-sequence :: <deque>);
  if (first ~= 0 | supplied?(last)) next-method()
  else
    let rep = source.representation;
    let rep-first-index = rep.first-index;
    let rep-last-index = rep.last-index;
    let deque-size = (rep-last-index - rep-first-index) + 1;
    let target = make(<object-deque>, size: deque-size, element-type: element-type(source));
    let target-rep = target.representation;
    for (from :: <integer> from rep-first-index to rep-last-index,
         to :: <integer> from target-rep.first-index to target-rep.last-index)
      island-deque-element(target-rep, to) := island-deque-element(rep, from)
    end for;
    target
  end
end copy-sequence;



//
// CONCATENATE-AS-TWO
// 

define method concatenate-as-two
    (type == <deque>, first-seq :: <sequence>, second-seq :: <sequence>)
        => result-seq :: <deque>; 
  case 
    empty?(first-seq)
      => as(<deque>, second-seq);
    empty?(second-seq)
      => as(<deque>, first-seq);
    otherwise
      => let d = make(<deque>);
         for (val in first-seq) push-last(d, val) end;
         for (val in second-seq) push-last(d, val) end;
         d
  end
end;

define sealed domain make (singleton(<object-deque>));
define sealed domain element-type (<object-deque>);

define sealed method as (class == <list>, v :: <object-deque>) 
    => (l :: <list>)
  let rep = v.representation;
  for (result = #() then pair(island-deque-element(rep, index), result),
       index :: <integer> from rep.last-index to rep.first-index by -1)
  finally
    result
  end
end;

/* ambiguity resolvers */
define copy-down-method map-as-one (type == <deque>,
				    function :: <function>,
				    collection ::  <infinite-range>) => 
  (new-collection :: <vector>); // actually :: type
define copy-down-method map-as-one (type == <object-deque>,
				    function :: <function>,
				    collection ::  <infinite-range>) => 
  (new-collection :: <vector>); // actually :: type

define copy-down-method map-as-one (type == <deque>,
				    function :: <function>, 
				    collection ::  <explicit-key-collection>) =>
  (new-collection :: <vector>);

define copy-down-method map-as-one (type == <object-deque>,
				    function :: <function>, 
				    collection ::  <explicit-key-collection>) =>
  (new-collection :: <vector>);

/*
define method map-as-one
    (type == <deque>,
     function :: <function>, collection ::  <explicit-key-collection>)
        => new-collection :: <deque>; // actually :: type
  let acc = make(<keyed-accumulator>);
  for (e keyed-by k in collection) acc[k] := function(e) end for;
  convert-accumulator-as(type, acc)
end method map-as-one;

define method map-as-one
    (type == <object-deque>,
     function :: <function>, collection ::  <explicit-key-collection>)
        => new-collection :: <object-deque>; 
  let acc = make(<keyed-accumulator>);
  for (e keyed-by k in collection) acc[k] := function(e) end for;
  convert-accumulator-as(type, acc)
end method map-as-one;
*/

define inline method map-as-one
    (type == <deque>, function :: <function>, collection ::  <sequence>)
        => new-collection :: <deque>;
  let result = make(<deque>);
  for (e in collection) push-last(result, function(e)) end for;
  result
end method map-as-one;

// markt, not quite a copy-down, because of == specializer above
define inline method map-as-one
    (type == <object-deque>, function :: <function>, collection ::  <sequence>)
        => new-collection :: <deque>;
  let result = make(<deque>);
  for (e in collection) push-last(result, function(e)) end for;
  result
end method map-as-one;

// eof 

