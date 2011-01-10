module:    harp-utils
Synopsis:  Vectors of bits
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Revised Mar 12, 1998     Nosa Omo

// Implement bit-sets using simple-machine-word-vectors;
// These are limited vectors of raw-machine-words that the compiler
// can generate optimal machine-code for;
// The code below is written in a certain style to achieve this goal

define constant <vector-32bit> = <simple-machine-word-vector>;

define constant <bit-set> = <vector-32bit>;

define constant $empty-bit-set = empty(<simple-machine-word-vector>);

define constant $int-size$ = 32;  // Use 32 bits per word

define constant $log-int-size$ = 5;

define constant $int-size-mask$ = lognot(ash(-1, $log-int-size$));


// make bit set vectors in units of 1 words

define constant $bit-set-word-size$ = 1;

define constant $bit-unit-size$ = $bit-set-word-size$ * $int-size$;

define constant $bit-set-zero$ :: <machine-word> = as(<machine-word>, 0);

define constant $bit-set-not-zero$ :: <machine-word> =
  machine-word-lognot($bit-set-zero$);


define method make-bit-set (length :: <integer>) => (new :: <vector-32bit>)
  make(<vector-32bit>, size: ceiling/(length, $bit-unit-size$));
end;

define inline function bit-set-offset
    (num :: <integer>) => (offset :: <integer>)
  ash(num, - $log-int-size$)
end function;

define inline function bit-set-mask
    (num :: <integer>) => (mask :: <machine-word>)
    machine-word-unsigned-shift-left(coerce-integer-to-machine-word(1),
				     logand(num, $int-size-mask$));
end function;

define inline method set-bit-in-set 
    (bit-set :: <vector-32bit>, num :: <integer>) => (num :: <integer>)
  let offset :: <integer> = bit-set-offset(num);
  let the-bit :: <machine-word> = bit-set-mask(num);
  without-bounds-checks
    bit-set[offset] := machine-word-logior(bit-set[offset], the-bit);
  end;
  num;
end;

define inline method stretchy-bit-set(bit-set :: <vector-32bit>, new-size :: <integer>)
 => (new-bit-set :: <vector-32bit>)
  let bit-set-size :: <integer> = bit-set.size;
  let new-bit-set :: <vector-32bit> = make(<vector-32bit>, size: new-size);
  for (i :: <integer> from 0 below bit-set-size)
    without-bounds-checks
      new-bit-set[i] := bit-set[i]
    end;
  end for;
  new-bit-set
end method;

define method set-bit-in-set! 
    (bit-set :: <vector-32bit>, num :: <integer>) => (bit-set :: <vector-32bit>)
  let offset :: <integer> = bit-set-offset(num);
  let bit-set-size :: <integer> = bit-set.size;
  let bit-set :: <vector-32bit> =
    if (offset < bit-set-size)
      bit-set
    else
      stretchy-bit-set(bit-set, offset + 1);
    end if;
  let the-bit :: <machine-word> = bit-set-mask(num);
  without-bounds-checks
    bit-set[offset] := machine-word-logior(bit-set[offset], the-bit);
  end;
  bit-set;
end;

define inline method unset-bit-in-set 
    (bit-set :: <vector-32bit>, num :: <integer>) => (num :: <integer>)
  let offset :: <integer> = bit-set-offset(num);
  let the-bit :: <machine-word> = bit-set-mask(num);
  without-bounds-checks
    bit-set[offset] := machine-word-logand(bit-set[offset],
	  				   machine-word-lognot(the-bit));
  end;
  num;
end;

define inline method get-bit-from-set
     (bit-set :: <vector-32bit>, num :: <integer>) => (bit? :: <boolean>)
  let offset :: <integer> = bit-set-offset(num);
  let the-bit :: <machine-word> = bit-set-mask(num);
  without-bounds-checks
    let bit-val :: <machine-word> = machine-word-logand(bit-set[offset], the-bit);
    if (machine-word-equal?(bit-val, coerce-integer-to-machine-word(0))) #f
    else #t
    end if
  end;
end;

define inline method get-bit-from-set!
     (bit-set :: <vector-32bit>, num :: <integer>) => (bit? :: <boolean>)
  let offset :: <integer> = bit-set-offset(num);
  let bit-set-size :: <integer> = bit-set.size;
  if (offset < bit-set-size)
    let the-bit :: <machine-word> = bit-set-mask(num);
    without-bounds-checks
      let bit-val :: <machine-word> = machine-word-logand(bit-set[offset], the-bit);
      if (machine-word-equal?(bit-val, coerce-integer-to-machine-word(0))) #f
      else #t
      end if
    end;
  else
    #f
  end if;
end;

define inline method bit-set-or
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    without-bounds-checks
      set1[i] := machine-word-logior(set1[i], set2[i]);
    end;
  end for;
end;

define method bit-set-or!
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => (set1 :: <vector-32bit>)
  let set1-size :: <integer> = set1.size;
  let set2-size :: <integer> = set2.size;
  let set1 :: <vector-32bit> =
    if (set2-size > set1-size)
      stretchy-bit-set(set1, set2-size);
    else
      set1
    end if;
  for (i :: <integer> from 0 below set2-size)
    without-bounds-checks
      set1[i] := machine-word-logior(set1[i], set2[i]);
    end;
  end for;
  set1
end;


define inline method bit-set-and
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    without-bounds-checks
      set1[i] := machine-word-logand(set1[i], set2[i]);
    end;
  end for;
end;

define inline method bit-set-xor
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    without-bounds-checks
      set1[i] := machine-word-logxor(set1[i], set2[i]);
    end;
  end for;
end;

define inline method bit-set-andc2
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    without-bounds-checks
      set1[i] := machine-word-logand(set1[i], machine-word-lognot(set2[i]));
    end;
  end for;
end;

// set3 is a subset with all bits set, so only comes into play
// when in range

define inline method bit-set-or-andc2
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>, set3 :: <bit-subset>) => ()
  let bit-set3 :: <vector-32bit> = set3.bit-set;
  let bit-set3-start :: <integer> = set3.bit-set-start;
  let bit-set3-end :: <integer> = set3.bit-set-end;
  let bit-set3? = bit-set3-start > -1;
  let set-size :: <integer> = size(set1);

  without-bounds-checks
    if (bit-set3?)
      for (i :: <integer> from 0 below bit-set3-start)
	set1[i] := machine-word-logior(set1[i], set2[i]);
      end;
      for (i :: <integer> from bit-set3-start to bit-set3-end)
	set1[i] :=
	  machine-word-logior
	  (set1[i],
	   machine-word-logand(set2[i], bit-set3[i - bit-set3-start]));
      end;
      for (i :: <integer> from bit-set3-end + 1 below set-size)
	set1[i] := machine-word-logior(set1[i], set2[i]);
      end;
    else
      for (i :: <integer> from 0 below size(set1))
	set1[i] := machine-word-logior(set1[i], set2[i]);
      end;
    end;
  end;
end;

/* Define bit-set-update for tracking live names-set changes

   Detect whether a bit-set, set2, has changed; set1 is its 
   previous snapshot; update the snapshot at the same time
   for next time

   Nosa  Jan 25, 1999 */

define inline method bit-set-update
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>)
 => (changed? :: <boolean>)
  let changed? :: <boolean> = #f;
  for (i :: <integer> from 0 below size(set1))
    without-bounds-checks
      let word2 :: <machine-word> = set2[i];
      unless (changed?)
	changed? :=
	  ~ machine-word-equal?(machine-word-logxor(set1[i], word2),
				coerce-integer-to-machine-word(0));
      end;
      set1[i] := word2;
    end;
  end for;
  changed?
end;

define inline method set-bit-in-word
    (bit-set :: <vector-32bit>, offset :: <integer>,
     bit-mask :: <machine-word>)
 => ()
  without-bounds-checks
    bit-set[offset] := machine-word-logior(bit-set[offset], bit-mask);
  end;
end;

define inline method unset-bit-in-word
    (bit-set :: <vector-32bit>, offset :: <integer>,
     bit-mask :: <machine-word>)
 => ()
  without-bounds-checks
    bit-set[offset] := machine-word-logand(bit-set[offset], bit-mask);
  end;
end;

define inline method copy-bit-set (dst :: <vector-32bit>, src :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(dst))
    without-bounds-checks
      dst[i] := src[i];
    end;
  end for;
end;

define inline method clear-bit-set
    (set :: <vector-32bit>,
     #key end: last :: <integer> = set.size) => ()
  for (i :: <integer> from 0 below last)
    without-bounds-checks
      set[i] := coerce-integer-to-machine-word(0);
    end;
  end for;
end;

// For now, define a <machine-word> version of the logcount vector;
// This is a temporary hack to get around things like type-checks per
// vector access and achieve better inlined code because the compiler
// is not currently smart enough

define constant mw-bit-count-for-logcount :: <simple-machine-word-vector> =
  begin
    let size :: <integer> = size(bit-count-for-logcount);
    let smwv :: <simple-machine-word-vector> = make(<simple-machine-word-vector>, size: size);
    for (i :: <integer> from 0 below size)
      smwv[i] := coerce-integer-to-machine-word(bit-count-for-logcount[i]);
    end for;
    smwv
  end;

define inline method count-bits-in-byte (x :: <machine-word>) => (count :: <machine-word>)
  let index :: <integer> =
   coerce-machine-word-to-integer
    (machine-word-logand(x, coerce-integer-to-machine-word(#xff)));
  without-bounds-checks
    mw-bit-count-for-logcount[index];
  end;
end;

define inline method logcount (x :: <machine-word>) => (count :: <machine-word>)
  machine-word-add-signal-overflow
    (machine-word-add-signal-overflow
       (machine-word-add-signal-overflow
	  (count-bits-in-byte(x),
	   count-bits-in-byte(machine-word-shift-right(x, 8))),
	count-bits-in-byte(machine-word-shift-right(x, 16))),
     count-bits-in-byte(machine-word-shift-right(x, 24)));
end;

define inline method logcount (vec :: <simple-machine-word-vector>) => (count :: <integer>)
  let res :: <machine-word> = coerce-integer-to-machine-word(0);
  let vec-size :: <integer> = size(vec);
  let sum :: <integer> = 0;
  for (i :: <machine-word> in vec)
    sum := sum + coerce-machine-word-to-integer(logcount(i));
  end for;
  sum
end;


define method bit-set-as-vector(set :: <vector-32bit>) => (vector :: <simple-object-vector>)
  let size-set = size(set);
  let vector :: <simple-object-vector> = make(<vector>, size: size-set);
  for (i :: <integer> from 0 below size-set)
    without-bounds-checks
      let word :: <machine-word> = set[i];
      
      if (machine-word-equal?(machine-word-shift-right(word, 29),
			      coerce-integer-to-machine-word(0)))
	vector[i] := coerce-machine-word-to-integer(word);
      else
	vector[i] := word;
      end if;
    end;
  end for;
  vector
end method;

define method vector-as-bit-set(vector :: <simple-object-vector>) => (set :: <vector-32bit>)
  let size-vector = size(vector);
  let set :: <vector-32bit> = make(<vector-32bit>, size: size-vector);
  for (i :: <integer> from 0 below size-vector)
    without-bounds-checks
      let value = vector[i];
      select (value by instance?)
	<integer> =>
	  let new-value :: <integer> = value;
	  set[i] := coerce-integer-to-machine-word(new-value);
	<machine-word> =>
	  let new-value :: <machine-word> = value;
	  set[i] := new-value;
      end select;
    end;
  end for;
  set
end method;

define method pack-bitset(set :: <vector-32bit>) => (value)
  let vector :: <simple-object-vector> = bit-set-as-vector(set);
  select(vector.size by \=)
    0 => 0;
    1 => vector[0];
    otherwise => vector;
  end select;
end method;

define method unpack-value-as-bitset(value) => (set :: <vector-32bit>)
  select (value by instance?)
    <integer> =>
      let new-value :: <integer> = value;
      make(<vector-32bit>, size: 1,
	   fill: coerce-integer-to-machine-word(new-value));
    <machine-word> =>
      let new-value :: <machine-word> = value;
      make(<vector-32bit>, size: 1, fill: new-value);
    <simple-object-vector> => vector-as-bit-set(value);
  end select;
end method;



/* Define temporary bit-subsets for virtual-register ranges

   All virtual-registers defined by a basic-block are likely
   to be in close proximity of age

   This cuts down significantly on consing for huge lambdas

   All bits in subsets are initially set, until explicitly unset

   Nosa  Jan 25, 1999 */

define class <bit-subset>(<object>)
  slot bit-set-start :: <integer> = -1;
  slot bit-set-end :: <integer> = -1;
  slot bit-set :: <bit-set>, required-init-keyword: bit-set:;
end class;

define constant $empty-bit-subset =
  make(<bit-subset>, bit-set: $empty-bit-set);

define method make-bit-subset(#key size = 0, fill = $bit-set-zero$)
 => (bit-subset :: <bit-subset>)
  make(<bit-subset>,
       bit-set: make(<bit-set>, size: size, fill: fill));
end method;

// define inline function bit-subset-element
//     (set :: <bit-subset>, i :: <integer>)
//  => (word :: <machine-word>)
//   set.bit-set[i - set.bit-set-start]
// end function;

// define inline function bit-subset-range?
//     (set :: <bit-subset>, i :: <integer>)
//  => (in-range :: <boolean>)
//   (i >= set.bit-set-start) & (i <= set.bit-set-end)
// end function;

define inline method unset-bit-in-subset
    (set :: <bit-subset>, num :: <integer>) => (num :: <integer>)
  let offset :: <integer> = bit-set-offset(num);
  grow-bit-subset?(set, offset, $bit-set-not-zero$);
  let bit-set :: <vector-32bit> = set.bit-set;
  let the-bit :: <machine-word> = bit-set-mask(num);
  without-bounds-checks
    let offset :: <integer> = offset - set.bit-set-start;
    bit-set[offset] :=
    machine-word-logand(bit-set[offset],
			machine-word-lognot(the-bit));
  end;
  num;
end;

define inline method grow-bit-subset?
    (set :: <bit-subset>, i :: <integer>, fill :: <machine-word>)
 => ()
  
  let set-start :: <integer> = set.bit-set-start;
  let set-end :: <integer> = set.bit-set-end;
  let set-size :: <integer> = set.bit-set.size;

  let new-set-size :: <integer> =
    case
      (set-start = -1) =>
	set.bit-set-start := i;
	set.bit-set-end := i;
	set-size;
      (i < set-start) =>
	set.bit-set-start := i;
	set-start - i + set-size;
      (i > set-end) =>
	set.bit-set-end := i;
	i - set-end + set-size;
      otherwise => set-size;
    end;

  unless (new-set-size = set-size)
    let old-set :: <vector-32bit> = set.bit-set;
    let new-set :: <vector-32bit> =
      make(<vector-32bit>, size: new-set-size, fill: fill);  
    let new-set-start :: <integer> = set.bit-set-start;
    for (i :: <integer> from set-start to set-end)
      without-bounds-checks
	new-set[i - new-set-start] := old-set[i - set-start]
      end;
    end for;
    set.bit-set := new-set;
  end;

end method;

define method bitset-to-string(set :: <vector-32bit>)
 => (set :: <string>)
  reduce
    (method (string :: <string>, word :: <machine-word>)
       concatenate(string, ".",
		   machine-word-to-string(word));
     end,
     "",
     set);
end;
