Module:    internal
Author:    Paul Haahr, Jonathan Bachrach, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <range> classes

define abstract primary class <range> (<sequence>)
  slot range-from :: <number>, required-init-keyword: from:;
  slot range-by   :: <number>, required-init-keyword: by:;
end class <range>;

define sealed class <empty-range> (<range>)
end class <empty-range>;

define sealed class <constant-range> (<range>)
  sealed slot size :: type-union(<integer>, singleton(#f)),
    required-init-keyword: size:;
end class <constant-range>;

define sealed class <finite-range> (<range>)
  sealed slot size :: <integer>,
    required-init-keyword: size:;
end class <finite-range>;

define sealed class <infinite-range> (<range>)
end class <infinite-range>;

define class <infinite-range-error> (<simple-error>) end;


/// initialization

define constant $empty-range-password :: <pair> = pair("empty-range", #());

define constant $empty-range :: <empty-range>
  = make(<empty-range>, from: 0, by: 0, password: $empty-range-password);

define method make
    (class == <empty-range>, #key password, #all-keys) => (result :: <range>)
  if (password == $empty-range-password)
    next-method()
  else
    $empty-range
  end if;
end method make;

define method make
    (class == <range>,
     #key from = 0, by = 1, to, below, above, size, #all-keys)
 => (result :: <range>)
  // constant ranges (by: 0)
  if (zero?(by))
    if ((~above | (from > above)) & (~below | (from < below)))
      make(<constant-range>, from: from, by: 0, size: size)
    else
      $empty-range
    end if;
  else
    // convert above: or below: to to:
    if (by > 0)
      if (below & (~to | to < (below - by)))
	to := below - by
      end if;
    else
      if (above & (~to | to > (above - by)))
	to := above - by
      end if;
    end if;
    // infinite ranges
    if (~to & ~size)
      make(<infinite-range>, from: from, by: by)
    else
      if (size)
	if (to)
	  let new-to = from + by * (size + 1);
	  if (new-to <= to)
	    to := new-to
	  else
	    size := floor/(to + by - from, by);
	  end if;
	else
	  to := from + by * (size + 1);
	end if;
      else
	size := floor/(to + by - from, by);
      end if;
      if (size = 0)
	$empty-range
      else
	make(<finite-range>, from: from, by: by, size: size)
      end if;
    end if;
  end if;
end method make;

define constant range
  = method (#rest keys, #key from, by, to, below, above, size) => (range :: <range>)
      apply(make, <range>, keys)
    end method;


/// collection and sequence operations

define sealed inline method type-for-copy
    (range :: <range>) => (result == <list>)
  <list>
end method type-for-copy;

define method shallow-copy 
    (range :: <range>) => (list :: <list>)
  for (e in range using backward-iteration-protocol, 
       list = #() then pair(e, list))
  finally
    list
  end
end method shallow-copy;


define method empty?
    (range :: <range>) => (result == #f)
  #f
end method empty?;

define sealed method empty?
    (range :: <empty-range>) => (result == #t)
  #t
end method empty?;


define sealed method size
    (range :: <empty-range>) => (result == 0)
  0
end method size;

define sealed method size
    (range :: <infinite-range>) => (result == #f)
  #f
end method size;


define sealed method element
    (range :: <empty-range>, key :: <integer>, #key default = unsupplied())
 => (object)
  if (unsupplied?(default))
    element-range-error(range, key)
  else
    default
  end if
end method element;

define sealed method element
    (range :: <constant-range>, key :: <integer>, #key default = unsupplied())
 => (object)
  if (~range.size | key < range.size)
    range.range-from
  else
    if (unsupplied?(default))
      element-range-error(range, key)
    else
      default
    end if
  end if;
end method element;

define sealed method element
    (range :: <infinite-range>, key :: <integer>, #key default)
 => (object)
  range.range-from + key * range.range-by
end method element;

define sealed method element
    (range :: <finite-range>, key :: <integer>, #key default = unsupplied())
 => (object)
  if (key >= 0 & key < range.size)
    range.range-from + key * range.range-by
  else
    if (unsupplied?(default))
      element-range-error(range, key)
    else
      default
    end if
  end if;
end method element;


define sealed method last
    (range :: <infinite-range>, #key default) => (object)
  error(make(<infinite-range-error>, 
             format-string: "Cannot apply 'last' to an unbounded range"))
end method last;

define sealed method last
    (range :: <constant-range>, #key default) => (object)
  range.range-from
end method last;

define sealed method last
    (range :: <finite-range>, #key default) => (object)
  range.range-from + range.range-by * (range.size - 1)
end method last;


define sealed method find-key-for-element
    (value, range :: <empty-range>)
  #f
end method find-key-for-element;

define sealed method find-key-for-element
    (value, range :: <constant-range>)
  if (value = range.range-from)
    0
  else
    #f
  end if
end method find-key-for-element;

define sealed method find-key-for-element
    (value, range :: <infinite-range>)
  let (key, rem) = truncate/(value - range.range-from, range.range-by);
  if (rem = 0 & 0 <= key)
    key
  else
    #f
  end if;
end method find-key-for-element;

define sealed method find-key-for-element
    (value, range :: <finite-range>)
  let (key, rem) = truncate/(value - range.range-from, range.range-by);
  if (rem = 0 & 0 <= key & key < range.size)
    key
  else
    #f
  end if;
end method find-key-for-element;


define method as
    (class == <range>, range :: <range>) => (result :: <range>)
  range
end method as;


define method \=
    (range-1 :: <range>, range-2 :: <range>) => (result == #f)
  #f
end method;

define sealed method \= 
    (range-1 :: <empty-range>, range-2 :: <empty-range>) => (result == #t)
  #t
end method;

define sealed method \= 
    (range-1 :: <constant-range>, range-2 :: <constant-range>) => (result :: <boolean>)
  range-1.range-from = range-2.range-from
  & range-1.size = range-2.size
end method;

define sealed method \=
    (range-1 :: <infinite-range>, range-2 :: <infinite-range>) => (result :: <boolean>)
  range-1.range-from = range-2.range-from
  & range-1.range-by = range-2.range-by
end method;

define sealed method \=
    (range-1 :: <finite-range>, range-2 :: <finite-range>) => (result :: <boolean>)
  range-1.range-from = range-2.range-from
  & range-1.range-by = range-2.range-by
  & range-1.size = range-2.size
end method;


define method member?
    (value :: <number>, range :: <range>, #key test = \==)
 => (result :: <boolean>)
  if (test == \==)
    find-key-for-element(value, range) & #t
  else
    next-method()
  end if
end method member?;


define class <incompatible-range-error> (<simple-error>) end;

define method add!
    (range :: <range>, elt) => (result :: <sequence>)
  error(make(<type-error>, value: elt, type: <number>))
end method add!;

define method add 
    (range :: <range>, elt :: <number>) => (result :: <sequence>)
  add!(copy-sequence(range), elt)
end method add;

define sealed method add!
    (range :: <empty-range>, elt :: <number>) => (result :: <sequence>)
  make(<range>, from: elt, size: 1)
end method add!;

define sealed method add! 
    (range :: <infinite-range>, elt :: <number>) => (result :: <sequence>)
  let prev = range.range-from - range.range-by;
  unless (elt = prev)
    error(make(<incompatible-range-error>,
	       format-string: "You can't add %= to the infinite range %=", 
	       format-arguments: list(elt, range)))
  end unless;
  range.range-from := prev;
  range
end method add!;

define sealed method add! 
    (range :: <constant-range>, elt :: <number>) => (result :: <sequence>)
  case
    (elt = range.range-from) =>
      if (range.size)
	range.size := range.size + 1
      end if;
      range;
    (range.size = 1) =>
      make(<range>, from: range.range-from,
	   by: elt - range.range-from, size: 2);
    otherwise =>
      add!(shallow-copy(range), elt);
  end case;
end method add!;

define sealed method add! 
    (range :: <finite-range>, elt :: <number>) => (result :: <sequence>)
  case
    (elt = range.last + range.range-by) =>
      range.size := range.size + 1;
      range;
    (elt = range.range-from - range.range-by) =>
      range.size := range.size + 1;
      range.range-from := range.range-from - range.range-by;
      range;
    (range.size = 1) =>
      if (elt = range.range-from)
	make(<range>, from: elt, by: 0, size: 2);
      else
        range.size := range.size + 1;
	range.range-by := elt - range.range-from;
	range;
      end if;
    otherwise =>
      add!(shallow-copy(range), elt);
  end case;
end method add!;


define method remove!
    (range :: <range>, elt, #key test, count) => (result :: <sequence>)
  error(make(<type-error>, value: elt, type: <number>))
end method remove!;

define method remove 
    (range :: <range>, elt :: <number>, #key test, count) => (result :: <sequence>)
  remove!(copy-sequence(range), elt, test: test, count: count)
end method remove;

define sealed method remove!
    (range :: <empty-range>, elt :: <number>, #key test, count) => (result :: <sequence>)
  range	  
end method remove!;

define sealed method remove! 
    (range :: <infinite-range>, elt :: <number>, #key test, count) => (result :: <sequence>)
  //--- What about 'test'?
  unless (elt = range.range-from)
    error(make(<incompatible-range-error>,
	       format-string: "You can't remove %= from the infinite range %=", 
	       format-arguments: list(elt, range)))
  end unless;
  if (~count | count > 0)
    let next = range.range-from + range.range-by;
    range.range-from := next;
  end if;
  range
end method remove!;

define sealed method remove! 
    (range :: <constant-range>, elt :: <number>, #key test, count) => (result :: <sequence>)
  //--- What about 'test'?
  if (~count | count > 0)
    case
      (elt = range.range-from) =>
	if (range.size)
	  range.size := range.size - 1
	end if;
	range;
      (range.size = 1) =>
	$empty-range;
      otherwise =>
	if (member?(elt, range))
	  remove!(shallow-copy(range), elt, test: test, count: count)
	else
	  range
	end if;
    end case;
  else
    range
  end if;
end method remove!;

define sealed method remove! 
    (range :: <finite-range>, elt :: <number>, #key test, count) => (result :: <sequence>)
  //--- What about 'test'?
  if (~count | count > 0)
    case
      (elt = range.last) =>
	range.size := range.size - 1;
	range;
      (elt = range.range-from) =>
	range.size := range.size - 1;
	range.range-from := range.range-from + range.range-by;
	range;
      (range.size = 1 & elt = range.range-from) =>
	$empty-range;
      otherwise =>
	if (count & count > 0 & member?(elt, range))
	  remove!(shallow-copy(range), elt, test: test, count: count)
	else
	  range
	end if;
    end case;
  else
    range
  end if;
end method remove!;


define sealed method copy-sequence 
    (range :: <empty-range>, #key start, end: endp) => (result :: <empty-range>)
  $empty-range
end method copy-sequence;

define method copy-sequence
    (range :: <range>, #key start = 0, end: endp = range.size) => (result :: <range>)
  let from = range[start];
  let by = range.range-by;
  if (endp)
    let new-end = if (range.size & endp > range.size)
		    range.size
		  else
		    endp
		  end;
    make(<range>, from: from, by: by, size: new-end - start)
  else
    make(<range>, from: from, by: by)
  end if;
end method copy-sequence;


define method id?-intersection (r1 :: <empty-range>, r2 :: <empty-range>)
  $empty-range
end method id?-intersection;

define method id?-intersection (r1 :: <range>, r2 :: <empty-range>)
  $empty-range
end method id?-intersection;

define method id?-intersection (r1 :: <empty-range>, r2 :: <range>)
  $empty-range
end method id?-intersection;

define method id?-intersection (r1 :: <constant-range>, r2 :: <constant-range>)
  if (r1.range-from == r2.range-from)
    if (if (r1.size & r2.size) r1.size < r2.size else r1.size end)
      r1
    else
      r2
    end if;
  else
    $empty-range
  end if;
end method id?-intersection;

define constant id?-intersection-with-constant
  = method (value :: <number>, range :: <range>)
      if (member?(value, range))
	make(<range>, from: value, size: 1)
      else
	$empty-range
      end if
    end method;

define method id?-intersection (r1 :: <constant-range>, r2 :: <finite-range>)
  id?-intersection-with-constant(r1.range-from, r2)
end method id?-intersection;

define method id?-intersection (r1 :: <constant-range>, r2 :: <infinite-range>)
  id?-intersection-with-constant(r1.range-from, r2)
end method id?-intersection;

define method id?-intersection (r1 :: <finite-range>, r2 :: <constant-range>)
  id?-intersection-with-constant(r2.range-from, r1)
end method id?-intersection;

define method id?-intersection (r1 :: <infinite-range>, r2 :: <constant-range>)
  id?-intersection-with-constant(r2.range-from, r1)
end method id?-intersection;

define constant same-sign? // assumes x ~= 0 & y ~= 0
  = method (x, y)
      (if (positive?(x)) positive? else negative? end)(y)
    end method;

define constant first-intersection
  = method (from1, by1, from2, by2)
      if (by1 < 0)
	let neg = first-intersection(-from1, -by1, -from2, -by2);
	if (neg)
	  -neg
	else
	  #f
	end if;
      elseif (from1 < from2)
	first-intersection(from2, by2, from1, by1)
      else
	// assert(from1 >= from2 & by1 > 0 &  by2 > 0)
	block (return)
	  for (i from 0 below by2,
	       n from from1 by by1)
	    if (remainder(n - from2, by2) = 0)
	      return(n)
	    end if;
	  finally
	    #f
	  end for
	end block
      end if
    end method;

define constant ordered-finite-intersection
  // assumes by1 > 0 & by2 > 0
  = method (lo1, by1, hi1, lo2, by2, hi2)
      if (hi1 < lo2 | hi2 < lo1)
	$empty-range
      else
	let from = first-intersection(lo1, by1, lo2, by2);
	if (from & from >= lo1 & from >= lo2)
	  let to = first-intersection(hi1, -by1, hi2, -by2);
	  if (to & to <= hi1 & to <= hi2)
	    make(<range>, from: from, to: to, by: lcm(by1, by2))
	  else
	    $empty-range
	  end if
	else
	  $empty-range
	end if
      end if
    end method;

define constant finite-intersection
  = method (from1, by1, to1, from2, by2, to2)
      if (by1.negative?)
	if (by2.negative?)
	  ordered-finite-intersection(to1, -by1, from1, to2, -by2, from2)
	else
	  ordered-finite-intersection(to1, -by1, from1, from2, by2, to2)
	end if
      else
	if (by2.negative?)
	  ordered-finite-intersection(from1, by1, to1, to2, -by2, from2)
	else
	  ordered-finite-intersection(from1, by1, to1, from2, by2, to2)
	end if
      end if
    end method;

define constant last-of-in
  = method (of :: <infinite-range>, in :: <range>)
      // |in| is either a finite range or an infinite range growing in
      // the opposite direction from |of|
      let bound = if (same-sign?(in.range-by, of.range-by))
		    in.last
		  else
		    in.range-from
		  end if;
      let n = truncate/(bound - of.range-from, of.range-by);
      n * of.range-by + of.range-from
    end method;

define method id?-intersection (r1 :: <finite-range>, r2 :: <finite-range>)
  finite-intersection(r1.range-from, r1.range-by, r1.last,
		      r2.range-from, r2.range-by, r2.last)
end method id?-intersection;

define method id?-intersection (r1 :: <finite-range>, r2 :: <infinite-range>)
  finite-intersection(r1.range-from, r1.range-by, r1.last,
		      r2.range-from, r2.range-by, last-of-in(r2, r1))
end method id?-intersection;

define method id?-intersection (r1 :: <infinite-range>, r2 :: <finite-range>)
  finite-intersection(r1.range-from, r1.range-by, last-of-in(r1, r2),
		      r2.range-from, r2.range-by, r2.last)
end method id?-intersection;

define method id?-intersection (r1 :: <infinite-range>, r2 :: <infinite-range>)
  let by1 = r1.range-by;
  let by2 = r2.range-by;
  let from1 = r1.range-from;
  let from2 = r2.range-from;
  if (same-sign?(by1, by2))
    let from = first-intersection(from1, by1, from2, by2);
    if (from)
      make(<range>, from: from, by: lcm(by1, by2))
    else
      $empty-range
    end if
  else
    finite-intersection(from1, by1, last-of-in(r1, r2),
			from2, by2, last-of-in(r2, r1))
  end if
end method id?-intersection;

define method intersection
    (range-1 :: <range>, range-2 :: <range>, #key test = \==)
 => (result :: <range>)
  if (test == \==)
    id?-intersection(range-1, range-2)
  else
    next-method()
  end if;
end method intersection;


define method reverse
    (range :: <range>) => (result :: <range>)
  reverse!(copy-sequence(range))
end method reverse;

define sealed method reverse!
    (range :: <empty-range>) => (result :: <empty-range>)
  $empty-range
end method reverse!;

define sealed method reverse!
    (range :: <infinite-range>) => (result :: <range>)
  error(make(<infinite-range-error>,
             format-string: "Cannot reverse! an infinite range"))
end method reverse!;

define sealed method reverse!
    (range :: <constant-range>) => (result :: <range>)
  unless (range.size)
    error(make(<infinite-range-error>,
               format-string: "Cannot reverse! an infinite range"))
  end unless;
  range
end method reverse!;

define sealed method reverse!
    (range :: <finite-range>) => (result :: <finite-range>)
  range.range-from := range.last;
  range.range-by := -range.range-by;
  range
end method reverse!; 


//// ITERATION PROTOCOL

define constant range-next-state
  = method (range :: <range>, state :: <number>)
     => (result :: <integer>)
      state + range.range-by
    end method;

define constant range-previous-state
  = method (range :: <range>, state :: <number>)
     => (result :: <integer>)
      state - range.range-by
    end method;

define constant empty-range-finished-state?
  = method (range :: <range>, state, limit) => (result :: <boolean>);
      #t
    end method;

define constant infinite-range-finished-state?
  = method (range :: <range>, state, limit) => (result :: <boolean>);
      #f
    end method;

define constant increasing-range-finished-state?
  = method (range :: <range>, state, limit) => (result :: <boolean>);
      state > limit
    end method;

define constant decreasing-range-finished-state?
  = method (range :: <range>, state, limit) => (result :: <boolean>);
      state < limit
    end method;

define constant range-current-key
  = method (range :: <range>, state :: <number>)
      floor/(state - range.range-from, range.range-by)
    end method;

define constant range-current-element
  = method (range :: <range>, state :: <number>)
      state
    end method;

define constant range-current-element-setter
  = method (new-value, range :: <range>, state)
     => (will-never-return :: <bottom>)
      error(make(<immutable-error>,
                 format-string: "range %= is immutable", 
                 format-arguments: list(range)))
    end method;

define constant constant-range-current-element
  = method (range :: <range>, state :: <number>)
      range.range-from
    end method;

define constant range-error
  = method (#rest ignored)
     => (will-never-return :: <bottom>)
      error("RANGE iteration protocol -- illegal operation")
    end method;

define method forward-iteration-protocol (range :: <empty-range>)
 => (initial-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(#f,
	 #f,
	 range-error,
	 empty-range-finished-state?,
	 range-error,
	 range-error,
	 range-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;

define method backward-iteration-protocol (range :: <empty-range>)
 => (final-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  forward-iteration-protocol(range)
end method backward-iteration-protocol;

define method forward-iteration-protocol (range :: <infinite-range>)
 => (final-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(range.range-from,
	 #f,
	 range-next-state,
	 infinite-range-finished-state?,
	 range-current-key,
	 range-current-element,
	 range-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;

define method backward-iteration-protocol (range :: <infinite-range>)
 => (final-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  error(make(<infinite-range-error>, 
             format-string: "Cannot iterate backwards over an infinite range"))
end method backward-iteration-protocol;

define method forward-iteration-protocol (range :: <finite-range>)
 => (initial-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(range.range-from,
	 range.last,
	 range-next-state,
	 if (range.range-by.positive?)
	   increasing-range-finished-state?
	 else
	   decreasing-range-finished-state?
	 end if,
	 range-current-key,
	 range-current-element,
	 range-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;

define method backward-iteration-protocol (range :: <finite-range>)
 => (final-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(range.last,
	 range.range-from,
	 range-previous-state,
	 if (range.range-by.positive?)
	   decreasing-range-finished-state?
	 else
	   increasing-range-finished-state?
	 end if,
	 range-current-key,
	 range-current-element,
	 range-current-element-setter,
	 identity-copy-state)
end method backward-iteration-protocol;

define method forward-iteration-protocol (range :: <constant-range>)
 => (initial-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(0,
	 range.size,
	 sequence-next-state,
	 if (range.size)
	   sequence-finished-state?
	 else
	   infinite-range-finished-state?
	 end if,
	 sequence-current-key,
	 constant-range-current-element,
	 range-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;

define method backward-iteration-protocol (range :: <constant-range>)
 => (final-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  unless (range.size)
    error(make(<infinite-range-error>, 
               format-string: "Cannot iterate backwards over an infinite range"))
  end unless;
  values(range.size - 1,
	 -1,
	 sequence-previous-state,
         sequence-finished-state?,
	 sequence-current-key,
	 constant-range-current-element,
	 range-current-element-setter,
	 identity-copy-state)
end method backward-iteration-protocol;
 
// NOT YET IMPLEMENTED
define method limited 
    (class == <range>, #key of, #all-keys) => (type :: <type>)
  class
end method;

// Finally, if we have an infinite range as source then we had better
// give up.

define method map-as-one
    (type :: <mutable-collection-type>,
     function :: <function>, collection ::  <infinite-range>)
        => new-collection :: <vector>; // actually :: type
  // TODO: make a proper error class
  error(make(<infinite-range-error>, format-string: "Cannot map over an infinite range"))
end method map-as-one;

define inline copy-down-method map-as-one
  (type == <list>, function :: <function>, collection ::  <infinite-range>) => 
  (new-collection :: <vector>); // actually :: type

/*
define method map-as-one
    (type == <deque>,
     function :: <function>, collection ::  <infinite-range>)
        => new-collection :: <vector>; // actually :: type
  // TODO: make a proper error class
  error(make(<infinite-range-error>, format-string: "Cannot map over an infinite range"))
end method map-as-one;

define method map-as-one
    (type == <object-deque>,
     function :: <function>, collection ::  <infinite-range>)
        => new-collection :: <vector>; // actually :: type
  // TODO: make a proper error class
  error(make(<infinite-range-error>, format-string: "Cannot map over an infinite range"))
end method map-as-one;

define method map-as-one
    (type == <list>,
     function :: <function>, collection ::  <infinite-range>)
        => new-collection :: <vector>; // actually :: type
  // TODO: make a proper error class
  error(make(<infinite-range-error>, format-string: "Cannot map over an infinite range"))
end method map-as-one;
*/

// eof
